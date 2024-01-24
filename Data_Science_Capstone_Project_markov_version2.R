library(LaF)
library(stringr)
# library(ggplot2)

set.seed(12345)

#For sampling random lines, use function 'sample_lines' in LaF package.
data <- sample_lines("./final/en_US/en_US.news.txt", n=1000)

#removing '\r' end of each lines.
data <- str_remove(data, pattern="\\r$")

#We will use 'str_split()' in stringr package as main function for split charater.

#can see about regular expressions by typing 
#vignette("regular-expressions")

#The steps of split
#First. split by blank (regex for any whitespace "\s")
#about this expression, 's' is not an specia; ex. so it didn't need escaper.
#so if we use "\s" to the command, it makes an error.
#However for using regex "\s", we need an literal "\" not escaper "\"
#so to use literal "\" we need additional "\" in front of "\".
#conclusionally, "\\s" is correct.

data <- str_split(data, "\\s")
#Then black is removed, and remaining things are splitted.


#For futher split, we need to look around regular expression entirely.

#Second, split the punctuation
#If i use str_split based on punctuation, that punctuations
#are removed. so i decided to write new function to split punctuation.
#-> i don't need to make a new function! i can do the same thing with existing functions

#to keep the delimitter, we can use the expression "(?=anything)".
#about this expression
#lookaround assertion
#there are 4 kinds : positive/negative, lookahead/lookbehind
#These assertions don't “consume” any characters.
#it means, it use "".
#positive look-ahead assertion. Matches if ... matches at the current input.
#so, in "a$", $ is following the a. and because assertion doesn't consume any characters,
#it is divided to "a", "$" not "a", ""(not consuming "$")
#also, in "$c", $ is following the "". so it divided into "", "$c" not "", "c"
#after this, you can guess the effect of the other 3 assertions.
#additionally, when matching, start from "a", but end of it, end to "" after the "$", not "$" itself.
#lookbehind go from right to left.


  data <- lapply(data, str_split, pattern="(?=[:punct:])")
  data <- lapply(data, unlist)
  data <- lapply(data, str_split, pattern="(?<=[:punct:])")
  data <- lapply(data, unlist)
  data <- lapply(data, str_split, pattern="(?=[$])")
  data <- lapply(data, unlist)
  data <- lapply(data, str_split, pattern="(?<=[$])")
  data <- lapply(data, unlist)
  data <- lapply(data, function(x) return(x[x !=""]))

#After doning this, we splited the punctuation.
  
  
#And let's split numbers
  data <- lapply(data, str_split, pattern="(?=[0-9])")
  data <- lapply(data, unlist)
  data <- lapply(data, str_split, pattern="(?<=[0-9])")
  data <- lapply(data, unlist)
  data <- lapply(data, function(x) return(x[x !=""]))


#removing punctuation
whether_punct <- lapply(data, grepl, pattern="([[:punct:]])")
whether_punct <- lapply(whether_punct, function(x) x = !x)

for(i in 1:length(data)){
  data[[i]] <- data[[i]][whether_punct[[i]]]
}

#removing numbers
whether_num <- lapply(data, grepl, pattern="([0-9])")
whether_num <- lapply(whether_num, function(x) x = !x)

for(i in 1:length(data)){
  data[[i]] <- data[[i]][whether_num[[i]]]
}

#to lower
#lower.
data <- lapply(data, tolower)


library(sentimentr)

#profanity lexicons are here
#'::' is used when we use function of specific package
#that is not loaded.
#source : https://towardsdatascience.com/how-to-catch-profanity-with-r-4a21e024d4db
# lexicon::profanity_alvarez
# lexicon::profanity_banned
# lexicon::profanity_arr_bad

#Preparing Profanity lexicons.
pro1 <- lexicon::profanity_alvarez
pro2 <- lexicon::profanity_banned
pro3 <- lexicon::profanity_arr_bad
pro4 <- lexicon::profanity_racist
pro5 <- lexicon::profanity_zac_anger

profanity <- union(union(union(pro1, pro2), union(pro3, pro4)), pro5)

#Profanity counting
firstcounting <- sum(profanity(unlist(data), profanity)$profanity_count)

#Removing profanity
remove_profanity <- function(text, profanity){
  for(word in profanity){
    matching <- grep(pattern=word, x=text, fixed = TRUE)
    if(length(matching) >= 1){
      text <- text[-matching]
    }
  }
  return(text)
}

data <- lapply(data, remove_profanity, profanity = profanity)


#Profanity 'Re'counting
recounting <- sum(profanity(unlist(data), profanity)$profanity_count)

#profanity that wasn't removed finding and adding it to lexicon and remove again with new lexicon
if(recounting != 0){
  profanity_index <- which(profanity(unlist(data), profanity)$profanity_count != 0)
  unremoved_profanity <- unlist(data)[profanity_index]
  unremoved_profanity
  profanity[length(profanity)+seq(from=1, by=1, length.out=length(unremoved_profanity))] <- unremoved_profanity
  
  data <- lapply(data, remove_profanity, profanity = profanity)
}



#last counting
lastcounting <- sum(profanity(unlist(data), profanity)$profanity_count)

data
c(firstcounting, recounting, lastcounting)


#By it, all profanity is removed.


#Lemmatization
library(textstem)
data <- lapply(data, lemmatize_words)


#Create N-grams
#Create new function for this.
make_bigrams <- function(text){
  tokens <- c()
  for(i in 1:length(text)-1){
    tokens[i] <- paste(text[i], text[i+1])
  }
  tokens
}

make_trigrams <- function(text){
  tokens <- c()
  for(i in 1:length(text)-2){
    tokens[i] <- paste(text[i], text[i+1], text[i+2])
  }
  tokens
}

#unigrams
unigrams <- data

#2-grams
bigrams <- lapply(data, make_bigrams)

#3-grams
trigrams <- lapply(data, make_trigrams)
# 
# ##################################################
#Add Nc column
add_Nc <- function(ngram){
  col_num <- ncol(ngram) + 1
  for(count in unique(ngram[,'count'])){
    ngram[ngram[,'count']==count, col_num] = length(unique(ngram[ngram[,'count']==count, 1]))
  }
  colnames(ngram)[col_num] <- 'Nc'
  return(ngram)
}

#Add count column
add_count <- function(ngram, ngrams){
  col_num <- ncol(ngram) + 1
  row_num <- 1
  for(word in ngram[,1]){
    ngram[row_num, col_num] = sum(unlist(ngrams) == word)
    row_num <- row_num + 1
  }
  colnames(ngram)[col_num] <- 'count'
  return(ngram)
}



trigram <- matrix(nrow=length(unique(unlist(trigrams))), ncol=4)
trigram <- as.data.frame(trigram)
trigram[,1] <- unique(unlist(trigrams))
trigram[,2:4] <- str_split(trigram[,1], "\\s", simplify = TRUE)[,1:3]
colnames(trigram) <- c('trigram', 'w1', 'w2', 'w3')
trigram <- add_count(trigram, trigrams)
trigram <- add_Nc(trigram)


#calculate dc
get_d <- function(trigram, k){
  col_num <- ncol(trigram)+1
  for(c in unique(trigram[,'count'])){
    if(c > k){
      trigram[trigram[,'count']==c, col_num] = 1
    }else{
      nc <- trigram[trigram[,'count']==c, 'Nc'][1]
      nc1 <- trigram[trigram[,'count']==(c+1), 'Nc'][1]
      nk1 <- trigram[trigram[,'count']==(k+1), 'Nc'][1]
      n1 <- trigram[trigram[,'count']==1, 'Nc'][1]
      numerator <- (((c+1)*nc1)/(c*nc)) - (((k+1)*nk1)/n1)
      denominator <- 1 - (((k+1)*nk1)/n1)
      dc <- numerator/denominator
        
      trigram[trigram[,'count']==c, col_num] <- dc
    }
  }
  colnames(trigram)[col_num] <- 'dc'
  return(trigram)
}

trigram <- get_d(trigram, 5)


#calculate c*
add_est_c <- function(trigram){
  col_num <- ncol(trigram)+1
  for(c in unique(trigram[,'count'])){
    trigram[trigram[,'count']==c, col_num] = c * trigram[trigram[,'count']==c, 'dc'][1]
  }
  colnames(trigram)[col_num] <- 'c*'
  return(trigram)
}

trigram <- add_est_c(trigram)


#calculate beta(B)
add_B <- function(trigram){
  col_num <- ncol(trigram)+1
  for(i in 1:dim(unique(trigram[,c('w1','w2')]))[1]){
    bigram <- as.character(unique(trigram[,c('w1','w2')])[i,])
    match <- (trigram[,'w1']==bigram[1]) & (trigram[,'w2']==bigram[2])
    
    nume <- sum(trigram[match, 'c*'])
    denom <- sum(trigram[match, 'count'])
  
    trigram[match, col_num] = 1-(nume/denom)
  }
  colnames(trigram)[col_num] <- 'B'
  return(trigram)
}



#caculate alpha(a)
add_a <- function(trigram){
  col_num <- ncol(trigram)+1
  for(i in 1:dim(unique(trigram[,c('w1','w2')]))[1]){
    bigram <- as.character(unique(trigram[,c('w1','w2')])[i,])
    match1 <- trigram[,'w1']==bigram[1] & trigram[,'w2']==bigram[2]
    match2 <- trigram[,'w1']!=bigram[1] & trigram[,'w2']==bigram[2]
    
    if(sum(match2)==0){
      trigram[match1, col_num] <- 0
    }else{
      B <- trigram[match1, 'B'][1]
      
      denom <- sum(trigram[match2,'c*'])/sum(trigram[match2, 'count'])
      
      trigram[match1, col_num] <- B/denom
    }
  }
 colnames(trigram)[col_num] <- 'a'
 return(trigram)
}




#calculate cbi
add_cbi <- function(trigram){
  col_num <- ncol(trigram)+1
  for(i in 1:dim(unique(trigram[,c('w1','w2')]))[1]){
    bigram <- as.character(unique(trigram[,c('w1','w2')])[i,])
    match <- trigram[,'w1']==bigram[1] & trigram[,'w2']==bigram[2]
    
    trigram[match, col_num] <- sum(trigram[match, 'count'])
    
  }
  colnames(trigram)[col_num] <- 'cbi'
  return(trigram)
}

#calculate cuni
add_cuni <- function(trigram){
  col_num <- ncol(trigram)+1
  for(unigram in unique(trigram[,'w2'])){
    match <- trigram[,'w2']==unigram
    
    trigram[match, col_num] <- sum(trigram[match, 'count'])
    
  }
  colnames(trigram)[col_num] <- 'cuni'
  return(trigram)
}


trigram <- add_B(trigram)
trigram <- add_a(trigram)
trigram <- add_cbi(trigram)
trigram <- add_cuni(trigram)


#calculate beta2(B2)
add_B2 <- function(trigram){
  col_num <- ncol(trigram)+1
  for(i in 1:length(unique(trigram[,'w2']))){
    unigram <- unique(trigram[,'w2'])[i]
    match <- (trigram[,'w2']==unigram)
    
    nume <- sum(trigram[match, 'c*'])
    denom <- sum(trigram[match, 'count'])
    
    trigram[match, col_num] = 1-(nume/denom)
  }
  colnames(trigram)[col_num] <- 'B2'
  return(trigram)
}



#caculate alpha2(a2)
add_a2 <- function(trigram){
  col_num <- ncol(trigram)+1
  for(i in 1:length(unique(trigram[,'w2']))[1]){
    unigram <- unique(trigram[,'w2'])[i]
    match1 <- trigram[,'w2']==unigram
    match2 <- trigram[,'w2']!=unigram
    

    B <- trigram[match1, 'B2'][1]
    denom <- sum(trigram[match2,'c*'])/sum(trigram[match2, 'count'])
    trigram[match1, col_num] <- B/denom
    }
  colnames(trigram)[col_num] <- 'a2'
  return(trigram)
}

trigram <- add_B2(trigram)
trigram <- add_a2(trigram)

unigram_freq <- matrix(ncol=2)
unigram_freq <- as.data.frame(unigram_freq)
colnames(unigram_freq) <- c('unigram', 'count')
row_num <- 1
for(unigram in unique(trigram[,'w3'])){
  unigram_freq[row_num,1] <- unigram
  unigram_freq[row_num,2] <- sum(trigram[,'w3']==unigram)
  row_num <- row_num + 1
}
unigram_freq <- unigram_freq[order(unigram_freq[,2],decreasing = TRUE),]

total_count <- sum(trigram[,'count'])


#predict next word
predict_next <- function(word1, word2){
  word1 <- lemmatize_words(word1)
  word2 <- lemmatize_words(word2)
  
  predictdata <- matrix(ncol=2)
  predictdata <- as.data.frame(predictdata)
  colnames(predictdata) <- c('word3', 'prob')
  match <- trigram[,'w1']==word1 & trigram[,'w2']==word2
  match2 <- trigram[,'w1']!=word1 & trigram[,'w2']==word2
  match2data <- trigram[match2,]
  match2data[,c('w1', 'w2')]
  
  match3 <- trigram[,'w2']==word2
  match3data <- trigram[match3,]
  num <- 1
  
  
  if(sum(match)!=0){
    for(i in which(match)){
      predictdata[num, 'word3'] <- trigram[i, 'w3']
      predictdata[num, 'prob'] <- trigram[i, 'c*']/trigram[i, 'cbi']
      num <- num + 1
    }
    if(sum(match2)!=0){
      for(t in which(match2)){
        if(trigram[t, 'w3'] %in% predictdata[,1]){
          next
        }else{
          predictdata[num, 'word3'] <- trigram[t, 'w3']
          predictdata[num, 'prob'] <- trigram[t, 'a']*
            (sum(match2data[match2data[,'w3']==trigram[t,'w3'], 'c*'])/trigram[t, 'cuni'])
          num <- num + 1
        }
      }
    }
  }else if(sum(match)==0){
    if(sum(match3)!=0){
      for(q in which(match)){
        predictdata[num, 'word3'] <- trigram[q, 'w3']
        predictdata[num, 'prob'] <- (sum(match3data[match3data[,'w3']==trigram[q,'w3'], 'c*'])/trigram[t, 'cuni'])
        num <- num + 1
      }
      for(word in head(unigram_freq,10)[,1]){
        if(word %in% predictdata[,'word3']){
          next
        }else{
          predictdata[num,'word3'] <- word
          predictdata[num, 'prob'] <- trigram[trigram[,'w2']==word2,'a2'][1]*
            (sum(trigram[trigram[,'w2']!=word2&trigram[,'w3']==word,'c*'])/total_count)
        }
        num <- num + 1
      }
      
      #last case
      
      
    }else if(sum(match3)==0){
      for(word in head(unigram_freq,10)[,1]){
        predictdata[num, 'word3'] <- word
        predictdata[num, 'prob'] <- sum(trigram[trigram[,'w3']==word,'c*'])/total_count
      }
    }
  }
  predictdata <- predictdata[order(predictdata[,'prob'],decreasing = TRUE),]
  return(predictdata)
}

