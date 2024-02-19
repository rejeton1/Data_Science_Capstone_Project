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



load("Markov.RData")
