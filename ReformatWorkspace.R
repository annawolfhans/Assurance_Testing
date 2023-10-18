wordExtract <- function(string){
  middle=stringr::str_sub(stringr::str_extract_all(string,
                                                   "\\*([^*]+)\\*"),2,-2)
  middleWords<-unlist(strsplit(middle, ','))
  endWord=stringr::str_sub(stringr::str_extract(string, "\\*([^*]+)\\*"), 2, -1)
  return(c(middleWords, endWord))
}

file <- textConnection("S(*TIRES*:cost=$4:*BackTire*:prior=beta(1,2):test=TRUE:cost=$1,
                       *FrontTire*:prior=beta(3,2):test=TRUE:cost=$1)")

# If you specify a cost, then it is testable. If not, then it is not testable, cost=Inf
# Somehow designate that x item has already been defined somewhere else, therefore not giving prior
## So for tires, you wouldn't give it a prior because it's comprised of BackTire and FrontTire
# Prior(a=1,b=2) and if it doesn't have a prior, find from previous components 
# textConnection() is like finding a file. The user could create/find a file instead, but it's easier to keep contained in one file
# Put in vector, and use positions

## Think about how to explain this to someone, how to go about it
# Spaces allowed? Character name? 
# Build in warnings as well for all of those :)


############## REWRITE WITH THE FOLLOWING EXAMPLE CODE ###########
file <- textConnection("S(*TIRES*:$2.00:,
	        *BackTire*:B(1,2):TRUE:$1:, 
          *FrontTire*:B(1,2):TRUE:$1:), 

          P(*BRAKES*:$55:
	        *BackBrake*:B(1,2):TRUE:$1:, 
          *FrontBrake*:B(1,2):TRUE:$1:), 

          S(*BIKE*:$55:
          *TIRES*:, 
          *BRAKES*:)")

text=suppressWarnings(readLines(file))
text <- paste(text, collapse = "")
text <- gsub("\\s+", "", text)
# file <- textConnection("S(*BackTire*:prior=B(1,2):test=TRUE:cost=$1,000.52:, *FrontTire*:prior=B(1,2):test=TRUE:cost=$1:prior=B(2,3)), *Tires*")
# text=suppressWarnings(readLines(file))


# REWRITTEN TO SUPPORT EACH COMPONENT BEING SURROUNDED BY ASTERISKS
# Pulls them because they are surrounded by asterisks, then 
wordExtract <- function(string){
  pattern <- "\\*([^*]+)\\*"
  matches <- stringr::str_extract_all(text, pattern)
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

needsBuilt<-stringr::str_sub(stringr::str_extract(text, ":[a-zA-Z0-9]+"), 2, -1)

if (length(needsBuilt)!=length(unique(needsBuilt)))stop("Non-unique definitions given to component (check right side of colon for non-unique values)")
## Anything is 'ready' if it is not on the RHS of a definition
ready<-setdiff(words, needsBuilt)

## Check to make sure all components only appear once on the LHS of the relations (enforce that data not used twice)
counts<-table(words)
countIndices<-names(counts)%in% names(table(needsBuilt))
if(any(counts[countIndices]>2) | !all(counts[!countIndices]==1))
  stop("Component names must be unique on LHS of equations")



############################ EXTRACT TEST COST ########################
testCostExtract <- function(string) {
  pattern <- "\\$[0-9.,]+(?=[:])"
  result <- stringr::str_extract_all(string, pattern)[[1]]  
  return(result)
}

testCostExtract(text)

################## EXTRACT PRIORS ################
## Based on B( , )
priorsExtract <- function(string) {
  pattern <- "B\\((\\d+),(\\d+)\\)"
  matches <- stringr::str_match_all(string, pattern)
  if (length(matches) > 0) {
    extracted_numbers <- matches[[1]][, 2:3]
    return(extracted_numbers)
  } else {
    return(NULL)
  }
}

priors <- priorsExtract(text)
colnames(priors) <- c("alpha", "beta")
priors
## turn this into a df, then name the rows as well...

# Should I keep it as a matrix, or switch to a vector of numbers?

priors <- as.integer(priorsExtract(text))
alpha <- numeric(0)
beta <- numeric(0)

for (i in seq_along(priors)) {
  if (i %% 2 == 1) {
    alpha <- c(alpha, priors[i])
  } else {
    beta <- c(beta, priors[i])
  }
}

made_samp <- FALSE
samp=NULL
mcsamps=1000
if(length(samp) == 0){
  samp <- list()
  for(i in 1:length(alpha)){
    samp[[i]] <- rbeta(mcsamps, alpha[i], beta[i])
  }
  made_samp <- TRUE
}

# this outputs samp[[1 through 4]]. I need a better way to associate them 

# running into problem from before - how should I use this moving forward. 
# I could perform the sampling and store into their respective variable names? 

 
 
#### TF EXTRACT: SHOULDN"T NEED THIS CODE ANY MORE ####
# trueFalseTestExtract <- function(string) {
#   pattern <- c("TRUE","FALSE", "true", "false", "True", "False")
#   matches <- stringr::str_extract_all(string, pattern)
#   if (length(matches) > 0) {
#     extracted_words <- unlist(matches)
#     return(extracted_words)
#   } else {
#     return(NULL)
#   }
# }
# trueFalseTestExtract(text)
 
#####################

# Rewrite WordExtract code
# Create a connection to the text data
file <- textConnection(
"BIKE: series
Tires: cost=$33
Brakes: cost=$45
Power: cost=$12

Tires: series
BackTire: B(1,2), cost=$12
FrontTire: B(1,2), cost=$12

Brakes: parallel
FrontBrakes: B(1,2), cost=$12
BackBrakes: B(1,2), cost=$Inf

Power: parallel
FrontBrakes: B(1,2), cost=$12
BackBrakes: B(1,2), cost=$Inf")

# Read the lines from the connection 
# IF use textConnection great. If not, make it work
#### come back to this - problem is that it splits it into separate strings ####
readInput <- function(file){
  if (inherits(file, "textConnection")){
    text <- suppressWarnings(readLines(file))
    close(file)
  }
  else if (is.character(file)) {
    text <- suppressWarnings(readLines(file))
  }
  else {
    stop("Input type not recognized. It should be a textConnection or character vector with file paths.")
  }
  return(text)
}

#### continue ####
text <- suppressWarnings(readLines(file))

# Close the connection
close(file)

# Initialize an empty data frame
data <- data.frame(Word = character(0))

# Extract the words on the left side of the colons and ignore lines with NA
for (line in text) {
  match <- regmatches(line, regexec("^(.*?):", line))
  if (length(match) > 0) {
    data <- rbind(data, data.frame(Word = match[[1]][2]))
  }
}

# Remove NAs from the data frame
words <- data[!is.na(data$Word), ]

# Print the data frame
words

line_count <- 0

# Iterate through the lines to count lines before a new line
for (line in text) {
  if (line == "") {
    break  # Exit the loop when an empty line is encountered
  }
  line_count <- line_count + 1
}
# Write a code to make sure the components in the first estrofa are listed again
line_count
words

# Iterate through the elements in words
for (i in 2:line_count) {
  word_to_check <- words[i]
  word_count <- sum(words == word_to_check)
  
  if (word_count != 2) {
    stop("Error: Subcomponent '", word_to_check, "' does not have its component information provided.")
  }
}

