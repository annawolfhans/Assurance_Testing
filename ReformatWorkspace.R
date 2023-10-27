#### EXAMPLE FILE ####
file <- textConnection(
"BIKE::PARALLEL:cost=$1999
Tires: cost=$33
Brakes: cost=$45
Power: cost=$12

Tires::Series
BackTire: B(1,2), cost=$12
FrontTire: B(1,2), cost=$12

Brakes::parallel
FrontBrakes: B(3,2), cost=$12
BackBrakes: B(1,4), cost=$Inf

Power::parallel
Pedaling: B(1,2), cost=$12
Electric: B(1,2), cost=$Inf")

#### ACCEPT EITHER TEXTCONNECTION() OR .TXT FILE ####
# Read the lines from the connection 
# IF use textConnection great. If not, make it work
#### come back to this - problem is that it splits it into separate strings ####
file_path <- path.expand("~/Research2023/Assurance_Testing/file.txt")
file_path <- as.character(file_path)  # Ensure it's a character string

processInput <- function(input_source) {
  if (inherits(input_source, "textConnection")) {
    input_string <- paste(readLines(input_source), collapse = " ")
  } else if (file.exists(input_source)) {
    input_string <- paste(readLines(input_source), collapse = " ")
  } else {
    stop("Invalid input source. Please provide a valid textConnection or file path.")
  }
  print(input_string)
}

processInput(file_path)
processInput(file_path)

#create a variable called file that's either textConnection or
# a string to the path of a file
# test is this a textConnection or a string
# If it's a string then you would go out and read the file
# even like read_csv (but requires tidyverse, so it would be better to use baseR) or readLines (best)
# needs to be formatted EXACTLy like you want it to be

#### BEGIN WARNINGS/PULLING INFORMATION #### 

text <- suppressWarnings(readLines(file))
if(length(text)==0)stop("Text file contained 0 elements")

## Checks to see if includes parallel or series relationship after::
# CASE INSENSITIVE
# pattern says it can't have a space - maybe specify in text or build a warning for it? 
for (sentence in text) {
  if (stringr::str_count(sentence, "::") == 1) {
    if (!grepl("(?i)::(parallel|series)", sentence)) {
      cat("Error: The sentence does not match the pattern: '", sentence, "'\n")
      stop("Pattern not found in the sentence.")
    }
  }
}

# Close the connection?
# close(file)


data <- data.frame(Word = character(0))
# Extract the words on the left side of the colons and ignore lines with NA
for (line in text) {
  match <- regmatches(line, regexec("^(.*?)::", line))
  if (length(match) > 0) {
    data <- rbind(data, data.frame(Word = match[[1]][2]))
  }
}

# Remove NAs from the data frame that come from the spaces
needsBuilt <- data[!is.na(data$Word), ]
needsBuilt

data1 <- data.frame(Word = character(0))
# Extract the words on the left side of the colons and ignore lines with NA
for (line in text) {
  match <- regmatches(line, regexec("^(.*?):", line))
  if (length(match) > 0) {
    data1 <- rbind(data1, data.frame(Word = match[[1]][2]))
  }
}

words <- data1[!is.na(data1$Word), ]
remove_colons <- function(input_string) {
  cleaned_string <- gsub(":", "", input_string)
  return(cleaned_string)
}
(words <- remove_colons(words))
ready <- setdiff(words, needsBuilt)
## MAKE SURE THE SUBSYSTEM PIECES HAVE THEIR INFO INCLUDED

line_count <- 0

# Iterate through the lines to count lines before a new line
for (line in text) {
  if (line == "") {
    break  # Exit the loop when an empty line is encountered
  }
  line_count <- line_count + 1
}
# Write a code to make sure the components in the first section are listed again
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

#### EXTRACT PRIORS ####
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
text <- paste(text, collapse = " ")
priors <- priorsExtract(text)
colnames(priors) <- c("alpha", "beta")
priors


#### EXTRACT COST ####
costExtract <- function(string) {
  pattern <- "\\$([0-9]+|Inf)"
  matches <- stringr::str_match_all(string, pattern)
  
  if (length(matches) > 0) {
    extracted_numbers <- as.numeric(matches[[1]][, 2])
    return(extracted_numbers)
  } else {
    stop("No cost values were found. Check costs formatting.")
  }
}
text <- paste(text, collapse = " ")
costs <- costExtract(text)
sys_only_price <- costs[1]
## First cost should be sys_only_price
costs <- costs[-1]

# <!--- eventually I may want to break up the costs for subsystem vs components -->

## Check to make sure all components only appear once on the LHS of the relations (enforce that data not used twice)
counts<-table(words)
countIndices<-names(counts)%in% names(table(needsBuilt))
if(any(counts[countIndices]>2) | !all(counts[!countIndices]==1))
  stop("Component names must be unique on LHS of equations")

## Check for circular relationship
done<-rep(F, line_count)
i=0
oneChange=FALSE
while(any(!done)){
  i=i+1
  if (i > line_count &!oneChange)stop("The diagram specified contains circular relationships that makes the model impossible")
  else if (i>line_count &oneChange){
    i =1
    oneChange=FALSE
  }
  if (done[i]) next
  compNeeded <- words[i]
  Name<-compNeeded[length(compNeeded)]
  compNeeded<-compNeeded[-length(compNeeded)]
  if (all(compNeeded%in% ready)){
    needsBuilt<-setdiff(needsBuilt, Name)
    ready<-c(ready, Name)
    oneChange=TRUE
    done[i]<-TRUE
  }
}


