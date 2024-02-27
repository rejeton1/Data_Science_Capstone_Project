#For launch prediction program, you need to to following things
#1. Download and load several packages 
#2. Download MarkovData.RData (containing R Objects)
#3. 'final' folder from 'Coursera-Swiftkey.zip' (Data for testing)


#Install required packages
packages <- c('tm', 'stringr', 'data.table', 'LaF', 'tokenizers', 'textclean')

for(package in packages){
  if(!(package %in% installed.packages())){
    install.packages(package, repos = "http://cran.us.r-project.org")
  }
  library(package, character.only = TRUE)
}

load(url("https://github.com/rejeton1/Data_Science_Capstone_Project/raw/main/markov.RData"))
