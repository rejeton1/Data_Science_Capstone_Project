#For launch prediction program, you need to to following things
#1. Download and load several packages 
#2. Download MarkovData.RData (containing R Objects)
#3. 'final' folder from 'Coursera-Swiftkey.zip' (Data for testing)


#Install required packages
packages <- c('tm', 'lexicon', 'tokenizers', 'stringr', 'ds4psy', 'cld2', 'textclean',
              'data.table', 'dplyr', 'stopwords', 'LaF')

for(package in packages){
  if(!(package %in% installed.packages())){
    install.packages(package)
  }
  library(package, character.only = TRUE)
}



load("markov.RData")
load("MI30000.RData")


###############################################################################
#MI-N gram prediction model
preprocess_corpus_for_MI <- function(corpus){
  corpus <- tm_map(corpus, content_transformer(removeQuotation))
  corpus <- tm_map(corpus, content_transformer(tolower))
  corpus <- tm_map(corpus, content_transformer(replace_non_ascii))
  corpus <- tm_map(corpus, removePunctuation, ucp=TRUE, preserve_intra_word_contractions=TRUE,
                   preserve_intra_word_dashes=TRUE)
  corpus <- tm_map(corpus, removeNumbers)
  corpus <- tm_map(corpus, stripWhitespace)
  corpus <- tm_map(corpus, removeWords, profanity)
  corpus <- tm_map(corpus, stripWhitespace)
  corpus <- tm_map(corpus, content_transformer(function(x) str_remove(x, pattern="\\s$")))
  corpus <- tm_map(corpus, content_transformer(function(x) str_remove(x, pattern="^\\s")))
  return(corpus)
}



make_candidate_for_MI <- function(processed_text, wordpairvoca){
  Xs <- unlist(str_split(processed_text, "\\s"))
  Xs <- Xs[-c(length(Xs),length(Xs)-1, length(Xs)-2)]


  candidate_1 <- pred_with_keyword_matching(processed_text)$lastTerm
  
  candidate_2 <- unique(wordpairvoca[x %in% Xs, y])
  
  candidate <- unique(c(candidate_1, candidate_2))
  
  candidate_table <- data.table(Y=candidate)
  
  candidate_table <- candidate_table[,.(probability=getProbabilityFrom3Gram(paste(processed_text, Y))), by=Y] 
  
  return(candidate_table)                              
}


multiple_MI <- function(inputtext, candidate, wordpairvoca){
  Xs <- unlist(str_split(inputtext, "\\s"))
  Xs <- Xs[-c(length(Xs),(length(Xs)-1))]
  MIs <- c()
  
  for(X in Xs){
    matching_pair <- wordpairvoca[x==X & y==candidate]
    if(dim(matching_pair)[1]==0){
      MIs[length(MIs)+1] <- 1
    }else{
      MIs[length(MIs)+1] <- matching_pair[,PMI]
    }
  }
  
  if(is.null(MIs)){
    MIs <- 1
  }
  
  return(exp(sum(MIs)))
}



MI_N_Prediction <- function(inputtext, wordpairvoca=wordpairtable_important){
  inputtext <- text_to_sentences(inputtext, force_delim=TRUE)
  inputtext <- inputtext[length(inputtext)]
  
  candidates <- make_candidate_for_MI(inputtext, wordpairvoca=wordpairvoca)
  
  candidates <- candidates[,.(probability,
                  prob_MI=probability*multiple_MI(inputtext, Y, wordpairvoca=wordpairvoca)), by=Y][order(-prob_MI)]
  
  return(candidates)
}



combine_N_MI <-function(inputtext){
  input_len <- length(unlist(str_split(inputtext, "\\s")))
  if(input_len < 4){
    return(pred_with_keyword_matching(inputtext))
  }else if(input_len >= 4){
    if(grepl(pattern="\\s$", x=inputtext)){
      
      return(MI_N_Prediction(inputtext)[1:3])
    }else{
      keyword <- getLastTerms(inputtext, num=1)
      wordpairvoca <- wordpairtable_important[grepl(pattern=paste("^",keyword,sep=""), x=y)]
      
      if(dim(wordpairvoca)[1]==0){
        return(pred_with_keyword_matching(inputtext))
      }else{
        return(MI_N_Prediction(inputtext, wordpairvoca = wordpairvoca)[1:3])
      }
    }
  }
}


compare_two_model <- function(inputtext){
  print(pred_with_keyword_matching(inputtext))
  
  print(combine_N_MI(inputtext))
}



