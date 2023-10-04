wordExtract <- function(string){
  middle=stringr::str_sub(stringr::str_extract_all(string,
                                                   "\\*([^*]+)\\*"),2,-2)
  middleWords<-unlist(strsplit(middle, ','))
  endWord=stringr::str_sub(stringr::str_extract(string, "\\*([^*]+)\\*"), 2, -1)
  return(c(middleWords, endWord))
}

file <- textConnection("S(*BackTire*:prior=beta(1,2):test=TRUE:cost=$1:, *FrontTire*:prior=beta(3,2):test=TRUE:cost=$1:), *Tires*")

text=suppressWarnings(readLines(file))

wordExtract(text)




############## REWRITE WITH THE FOLLOWING EXAMPLE CODE ###########


file <- textConnection("S(*BackTire*:prior=beta(1,2):test=TRUE:cost=$1,000.52:, *FrontTire*:prior=beta(1,2):test=TRUE:cost=$1:), *Tires*")
text=suppressWarnings(readLines(file))


# REWRITTEN TO SUPPORT EACH COMPONENT BEING SURROUNDED BY ASTERISKS
# Pulls them because they are surrounded by asterisks, then 
wordExtract <- function(string){
  pattern <- "\\*([^*]+)\\*"
  matches <- str_extract_all(text, pattern)
  result_string <-""
  for (match_list in matches) {
    for (match in match_list) {
     result_string <- paste(result_string, match)
    }
  }
  cleaned_string <- gsub("\\*", "", result_string)
  cleaned_string <- stringr::str_trim(cleaned_string)
  stringr::str_replace_all(cleaned_string, " ", "")
  words <- unlist(strsplit(cleaned_string, ' '))
  if (length(words) > 1) {
    endWord <- words[length(words)]
    middleWords <- words[1:(length(words) - 1)]
  } else {
    endWord <- ""
    middleWords <- words
  }
  return(c(middleWords, endWord))
}

words <- wordExtract(text)


############################ EXTRACT TEST COST ########################
testCostExtract <- function(string) {
  pattern <- "\\$[0-9.,]+(?=[:])"
  result <- str_extract_all(string, pattern)[[1]]  
  return(result)
}

testCostExtract(text)

################## EXTRACT PRIORS ################
priorsExtract <- function(string) {
  pattern <- "beta\\((\\d+),(\\d+)\\)"
  matches <- str_extract_all(string, pattern)
  if (length(matches) > 0) {
    extracted_numbers <- unlist(matches)
    return(extracted_numbers)
  } else {
    return(NULL)
  }
}

priorsExtract(text)

# running into problem from before - how should I use this moving forward. 
# I could perform the sampling and store into their respective variable names? 

trueFalseTestExtract <- function(string) {
  pattern <- "test=([a-zA-Z]+)"
  matches <- str_extract_all(string, pattern)
  if (length(matches) > 0) {
    extracted_words <- unlist(matches)
    return(extracted_words)
  } else {
    return(NULL)
  }
}
trueFalseTestExtract(text)
# I could also remove "test=" from the output, but that may not matter just yet