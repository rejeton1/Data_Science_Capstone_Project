library(LaF)
library(stringr)

set.seed(123)

#For sampling random lines, use function 'sample_lines' in LaF package.
data <- sample_lines("./final/en_US/en_US.twitter.txt", n=10)

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

#After doning this, we split the punctuation.
  
data


