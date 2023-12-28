data <- readLines("./final/en_US/en_US.twitter.txt", 6)

data


#We will use 'str_split()' in stringr package
#as main function for split charater.

#can see about regular expressions by typing 
#vignette("regular-expressions")

#The turn of split
#First. split by blank ("\\s")

data <- str_split(data, "\\s")

#For futher split, we need to look around regular expression entirely.

#Second, split the punctuation
#If i use str_split based on punctuation, that punctuations
#are removed. so i decided to write new function to split punctuation.

lapply
grep("[[:punct:]]", data[[1]])
str_locate_all(data[[1]][3], "[[:punct:]]")
str_sub(data[[1]][3], 4,4)
append(벡터,삽입할데이터,after=n)

#With this basic functions, let's make a new function!

for(i in length(data)){
  index <- grep("[[:punct:]]", data[[i]])
  for(t in index){
    if(length(data[[i]][t])==1){
      next()
    }
    locate <- str_locate(data[[i]][t], "[[:punct:]]")
    if(locate[[1]][1] == 1){
      word1 <- str_sub(data[[i]][t], locate[[1]][1], locate[[1]][2])
      word2 <- str
    }else if(locate[[1]][1] > 1 && locate[[1]][2] < length(data[[i]][t])){
      
    }else if(locate[[1]][2] == length(data[[i]][t]))
    
    # punct <- str_sub(data[[1]][3], locate[[1]][1], locate[[1]][2])
    # append(data[[i]])
    
  }
}



