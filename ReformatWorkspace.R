####### to do
## Fix circular relationship warning check
### Figure out merging_function input and output


#### EXAMPLE FILE ####
file <- textConnection(
"BI2KE::PARALLEL:cost=$1999
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

current_dir <- getwd()
file_name <- "file.txt"  
file_path <- file.path(current_dir, file_name)

if (file.exists(file_path)) {
  # File exists, read it using readLines
  text <- suppressWarnings(readLines(file_path))
  print("File read in as a file_path")
} else {
  # File doesn't exist, create a textConnection
  #text_con <- textConnection(file)
  text <- suppressWarnings(readLines(file))
  print("File read in as a text_connection")
}


#### BEGIN WARNINGS/PULLING INFORMATION #### 
assurance_testing_setup <- function(file){

text <- suppressWarnings(readLines(file))
if(length(text)==0)stop("Text file contained 0 elements")
# text <- text[!grepl("^\\s*$", text)] #### make sure this line of text works with all warnings. it should... ####


#### review where pasted them all together ####

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
# PARALLEL <- character(0)
# SERIES <- character(0)
# 
# is_parallel <- FALSE
# is_series <- FALSE
# 
# lines <- strsplit(text, "\n")
# 
# for (line in text) {
#   line_lower <- tolower(line)
#   
#   if ("::parallel" %in% line_lower) {
#     is_parallel <- TRUE
#     is_series <- FALSE
#   } else if ("::series" %in% line_lower) {
#     is_parallel <- FALSE
#     is_series <- TRUE
#   } else if (is_parallel) {
#     component <- strsplit(line, ":")[[1]][1]
#     PARALLEL <- c(PARALLEL, component)
#   } else if (is_series) {
#     component <- strsplit(line, ":")[[1]][1]
#     SERIES <- c(SERIES, component)
#   }
# # }
# 
# cat("PARALLEL:", PARALLEL, "\n")
# cat("SERIES:", SERIES, "\n")



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

## WRITE WARNING FOR NON-PERMITABLE CHARACTERS IN THE WORDS
check_invalid_characters <- function(word) {
  invalid_chars <- c("@", "#", "$", "%")
  if (any(sapply(invalid_chars, function(char) grepl(char, word)))) {
    cat("Invalid character detected in word:", word, "\n")
    stop("Invalid character found.")
  }
}

# Loop through the 'words' vector and check for invalid characters
for (word in words) {
  check_invalid_characters(word)
}

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

i <- 1
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

############# this isn't working. not creating needsBuilt and compNeeded ########
#################################################################################

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
  compNeeded <- words[1]
  Name<-compNeeded[length(compNeeded)]
  compNeeded<-compNeeded[-length(compNeeded)]
  if (all(compNeeded%in% ready)){
    needsBuilt<-setdiff(needsBuilt, Name)
    ready<-c(ready, Name)
    oneChange=TRUE
    done[i]<-TRUE
  }
}

compNeeded <- words[1:line_count]
ready
words
needsBuilt
## Series and parallel structure functions ##
######## double check that this will work for more than 3 or 4 components #########
###################################################################################

comps.in.series <- function(values) {
  result <- paste("(", paste(values, collapse = " * "), ")", sep = ' ')
  return(result)
}

comps.in.parallel <- function(values) {
  result <- paste("(1 - (1 -", paste("(1 -", values, ")", collapse = ") * (1 -"), "))", sep = ' ')
  return(result)
}

## Store names as strings into their name as a variable
component_variables <- list()

for (name in words) {
  name <- gsub("^\\s+|\\s+$", "", name)
  component_variables[[name]] <- name
}
for (name in names(component_variables)) {
  assign(name, component_variables[[name]])
}
##################################################################
############# rewrite to fix subsystems ##########################
##################################################################
line_count
component_variables
subsystem_names <- component_variables[1:line_count]
## Set up our series and parallel equations

for (sentence in text) {
  if (stringr::str_count(sentence, "::") == 1) {
    if (!grepl("(?i)::(parallel|series)", sentence)) {
      cat("Error: The sentence does not match the pattern: '", sentence, "'\n")
      stop("Pattern not found in the sentence.")
    }
  }
}

for (sentence in text) {
  print (sentence)
}

i = 1
merging_function <- list()
while (i <= line_count) {
  if (stringr::str_count(sentence, "::") == 1) {
  if (grepl("(?i)::series", sentence)) {
    merging_function[[i]] <- comps.in.series(compNeeded[2:length(compNeeded)])
  } else { 
    merging_function[[i]] <- comps.in.parallel(compNeeded[2:length(compNeeded)])
  }
  i <- i + 1
  }
}

while (i <= line_count){
  
}
# merging_function
names(merging_function) <- subsystem_names

replace_variables <- function(expression, values) {
  for (var in names(values)) {
    # Use \\b to match whole words only
    expression <- gsub(paste0("\\b", var, "\\b"), values[[var]], expression)
  }
  return(expression)
}

for (name in names(merging_function)) {
  merging_function[[name]] <- replace_variables(merging_function[[name]], merging_function)
}

}
#### maybe use this guy?
# output_strings <- list()
# 
# # code to replicate P( and S(
# for (line in text) {
#   line_lower <- tolower(line)
#   if (grepl("::", line)) {
#     match <- regmatches(line, gregexpr(".*(?=::)", line, perl = TRUE))
#     if (length(match[[1]]) > 0) {
#       if (grepl("parallel", line)) {
#         output_strings <- append(output_strings, paste0("P(", match[[1]]))
#       } else if (grepl("series", line)) {
#         output_strings <- append(output_strings, paste0("S(", match[[1]]))
#       }
#     }
#   }
# }
# output_strings
