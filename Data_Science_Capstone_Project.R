library(LaF)
library(stringr)
library(ggplot2)

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


#EDA
#First, the number of totoal lines.
length(data) #1000

#Second, number of tokens of each lines.
num_of_token <- lapply(data, length)
num_of_token_vec <- unlist(num_of_token)

#Create histogram.
h <- hist(num_of_token_vec, main = 'The number of tokens for each lines', 
     xlab='The number of tokens', ylab='Frequency', breaks = length(unique(num_of_token_vec))-1)
text(h$mids, h$counts, labels = h$counts, adj = c(0.5, -0.15))

#Third, Frequency of words without stopwords
library(stopwords)
stopwords <- c(stopwords(), "re", "d", "ll", "t", "s", "m", "ve", "us", "don", "st", "th", "u", "didn")
stopwords <- stopwords[-grep(pattern="\'", stopwords)]

data1 <- data

remove_stopword <- function(text, stopword){
  for(word in stopword){
    matching <- grep(pattern=paste("^", word, "$", sep=""), x=text)
    if(length(matching) >= 1){
      text <- text[-matching]
    }
  }
  return(text)
}

data1 <- lapply(data1, remove_stopword, stopword = stopwords)


#Create Word Frequency table
t <- table(unlist(data1))
head(t[order(t, decreasing=TRUE)], 100)
#Create histogram
df_t <- as.data.frame(t)
df_t <- df_t[order(df_t[,2], decreasing = TRUE),]
b <- barplot(df_t$Freq[1:20], names.arg=as.character(df_t$Var1[1:20]), main="Frequency of words in news", 
        xlab='Words', ylab='Frequency', cex.names = 0.55)
text(x=b, y=df_t$Freq[1:20], labels=df_t$Freq[1:20], adj=c(0.5, 1))


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

#2-grams
bigrams <- lapply(data, make_bigrams)

#3-grams
trigrams <- lapply(data, make_trigrams)


##Word Embedding
library(word2vec)
embed_model <- word2vec(x=data, dim=5, iter=20, window = 2, min_count = 1)
word_vectors <- as.matrix(embed_model)


#Find vector of words
find_vec <- function(word){
  return(word_vectors[word,])
}

##replace 3-grams with vectors
trigramvec <- matrix(nrow=23293, ncol=19)
trigramvec <- as.data.frame(trigramvec)
dim(trigramvec) 
trigramvec[,1] <- unlist(trigrams)
colnames(trigramvec)[1] <- 'trigram'

trigramvec[,2:4] <- str_split(trigramvec[,1], "\\s", simplify = TRUE)[,1:3]
for(i in 1:dim(trigramvec)[1]){
  trigramvec[i,5:9] <- find_vec(trigramvec[i,2])
  trigramvec[i,10:14] <- find_vec(trigramvec[i,3])
  trigramvec[i,15:19] <- find_vec(trigramvec[i,4])
}

##3grams to vector has been completed.

#modeling rnn
library(abind)

input <- abind(trigramvec[5:9], trigramvec[10:14], along = 3)
input <- aperm(input, c(1, 3, 2))
# output <- abind(trigramvec[10:14], trigramvec[15:19], along = 3)
# output <- aperm(output, c(1, 3, 2))
output <- trigramvec[15:19]


library(caret)

model <- keras_model_sequential() %>%
  layer_dense(input_shape = c(2, 5), units=2) %>%
  layer_simple_rnn(units=2) %>%
  layer_dense(units=1)

model %>% compile(loss='cosine_similarity',
                  optimizer = 'RMSprop',
                  metrics = c('cosine_similarity'))


trained_model <- model %>% fit(x = input,
                               y = output,
                               batch_size = 10,
                               epochs = 1,
                               )
