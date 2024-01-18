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


length(unlist(data))

unigram_freq <- matrix(ncol=2)
unigram_freq <- as.data.frame(unigram_freq)
colnames(unigram_freq) <- c('word', 'freq')
num <- 1

for(word in unique(unlist(data))){
  unigram_freq[num,1] <- word
  unigram_freq[num,2] <- sum(unlist(data)==word)
  num <- num + 1
}

unigram_freq <- unigram_freq[order(unigram_freq[,2], decreasing = TRUE),]

for(i in 1:dim(unigram_freq)[1]){
  unigram_freq[i,3] <- 100*sum(unigram_freq[1:i,2])/length(unlist(data))
}

colnames(unigram_freq)[3] <- 'cumul'

which(unigram_freq[,3] >= 80)[1]
length(unigram_freq[,1])





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
add_Nc <- function(ngrams){
  col_num <- ncol(ngrams) + 1
  for(count in unique(ngrams[,'count'])){
    ngrams[ngrams[,'count']==count, col_num] = length(unique(ngrams[ngrams[,'count']==count, 1]))
  }
  colnames(ngrams)[col_num] <- 'Nc'
  return(ngrams)
}



reduced_unigram <- data.frame(word=unigram_freq[1:412,1], count=unigram_freq[1:412,2])
reduced_unigram <- add_Nc(reduced_unigram)

reduced_bigram <- matrix(ncol=3)
reduced_bigram <- as.data.frame(reduced_bigram)

for(word1 in reduced_unigram[,1]){
  for(word2 in reduced_unigram[,1]){
    reduced_bigram[dim(reduced_bigram)[1]+1,1] <- paste(word1, word2)
  }
}

num2 <- 1
for(bigram in reduced_bigram[,1]){
  reduced_bigram[num2,2] <- sum(unlist(bigrams)==bigram)
  num2 <- num2 + 1
}
colnames(reduced_bigram)[2] <- 'count'






unigrams <- add_count(unigrams)
bigrams <- add_count(bigrams)
trigrams <- add_count(trigrams)




unigrams <- add_Nc(unigrams)
bigrams <- add_Nc(bigrams)
trigrams <- add_Nc(trigrams)

#Calculate coefficient 'd'(good-turing estimation)

# get_d <- function(ngram, data){
#   c <- data[data[,1]==ngram, 'count'][1]
#   Nc <- data[data[,1]==ngram, 'Nc'][1]
#   Nc1
# }

x <- unique(bigrams[,'count'])
y <- unique(trigrams[,'count'])

x[order(x)]
y[order(y)]
