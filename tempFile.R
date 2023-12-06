
# Example usage with the provided input
file <- textConnection(
  "BI2KE::PARALLEL:cost=$1999
Tires: cost=$33
Brakes: cost=$45

Tires::Series
BackTire: B(1,2), cost=$12
FrontTire: B(1,2), cost=$12

Brakes::parallel
FrontBrakes: B(3,2), cost=$12
BackBrakes: B(1,4), cost=$Inf")
# Make sure to be very explicit with every space 
# Ex: " On same line as BI2KE or " on line above 
# Try both the textConnection and the text file, the quotation marks might make a difference maybe...just try both

# Tell user it's okay to have spaces in between things if they'd like, it's built to remove them so it doesn't matter
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


assurance_testing_setup <- function(file){
  
  # Read in file
  text <- suppressWarnings(readLines(file))
  if(length(text)==0)stop("Text file contained 0 elements")
  
  # Check for grammar requirements
  for (sentence in text) {
    if (stringr::str_count(sentence, "::") == 1) {
      if (!grepl("(?i)::(parallel|series)", sentence)) {
        cat("Error: The sentence does not match the pattern: '", sentence, "'\n")
        stop("Pattern not found in the sentence.")
      }
    }
  }
  

  
  # Extract words... make sure it works with remaining
  # data <- data.frame(Word = character(0))
  # # Extract the words on the left side of the colons and ignore lines with NA
  # for (line in text) {
  #   match <- regmatches(line, regexec("^(.*?)::", line))
  #   if (length(match) > 0) {
  #     data <- rbind(data, data.frame(Word = match[[1]][2]))
  #   }
  # }
  # 
  # # Remove NAs from the data frame that come from the spaces
  # needsBuilt <- data[!is.na(data$Word), ]
  # needsBuilt
  # 
  # data1 <- data.frame(Word = character(0))
  # # Extract the words on the left side of the colons and ignore lines with NA
  # for (line in text) {
  #   match <- regmatches(line, regexec("^(.*?):", line))
  #   if (length(match) > 0) {
  #     data1 <- rbind(data1, data.frame(Word = match[[1]][2]))
  #   }
  # }
  # 
  # words <- data1[!is.na(data1$Word), ]
  # remove_colons <- function(input_string) {
  #   cleaned_string <- gsub(":", "", input_string)
  #   return(cleaned_string)
  # }
  # words <- remove_colons(words)
  # words <- gsub(" ", "", words)
  # ready <- setdiff(words, needsBuilt)
  # 
  # # Problems here...
  # 
  # # Warning for nonpermitable characters in strings
  # check_invalid_characters <- function(word) {
  #   invalid_chars <- c("@", "#", "$", "%")
  #   if (any(sapply(invalid_chars, function(char) grepl(char, word)))) {
  #     cat("Invalid character detected in word:", word, "\n")
  #     stop("Invalid character found.")
  #   }
  # }
  # for (word in words) {
  #   check_invalid_characters(word)
  # }
  # 
  # line_count <- 0
  # for (line in text) {
  #   if (line == "") {
  #     break  # Exit the loop when an empty line is encountered
  #   }
  #   line_count <- line_count + 1
  # }
  
  # i <- 1
  # # Iterate through the elements in words
  # for (i in 2:line_count) {
  #   word_to_check <- words[i]
  #   word_count <- sum(words == word_to_check)
  #   
  #   if (word_count != 2) {
  #     stop("Error: Subcomponent '", word_to_check, "' does not have its component information provided.")
  #   }
  # }
  
  # Read each line in as separate strings
  strings <- character(length(text))
  for (i in seq_along(text)) {
    strings[i] <- text[i]
  }
  
  # Identify every line that contains "::" as system or sysbsystem
  subsys_sys <- grep("::", strings, value = TRUE)

  ### Convert into an easier identifiable S() or P() structure
  outputs <- character(length(strings))
  component_tmp <- character(length(strings))
  
  for (i in seq_along(strings)) {
    if (stringr::str_count(strings[i], "::") == 1) {
      if (grepl("(?i)::parallel", strings[i])) {
        outputs[i] <- "P("
      } else if (grepl("(?i)::series", strings[i])) {
        outputs[i] <- "S("
      }
      
      # Extract the word immediately before "::" and store it in component_tmp
      components <- strsplit(strings[i], "::")[[1]]
      component_tmp[i] <- trimws(components[length(components) - 1])
    }
  
  # Fill empty quotes with the first word of the line (without the colon)
    if (outputs[i] == "") {
      first_word <- sub(":$", "", strsplit(trimws(strings[i]), "\\s+")[[1]][1])
      outputs[i] <- paste0(outputs[i], first_word)
    }
  }


  # print(outputs)
  # print(component_tmp)
  component_tmp <- component_tmp[component_tmp != ""]
  
  aggregated_outputs <- character(0)
  
  i_component <- 1  # Index for component_tmp
  
  for (i in seq_along(outputs)) {
    if (outputs[i] %in% c("P(", "S(")) {
      start <- i
      while (i <= length(outputs) && !grepl("NA", outputs[i]) && outputs[i] != "") {
        i <- i + 1
      }
      middle <- outputs[(start + 1):(i - 1)]
      first_word <- gsub("(^P\\()|(^S\\()", "", outputs[start])
      
      # Find the corresponding element in component_tmp
      component <- ifelse(is.na(component_tmp[i_component]), "", component_tmp[i_component])
      
      # Append the component to the end of the aggregated output
      aggregated_outputs <- c(aggregated_outputs, paste0(outputs[start], first_word, paste(middle, collapse = ", "), "):", component))
      
      # Increment the index for component_tmp
      i_component <- i_component + 1
    }
  }
  
  # Output an easier structure to work with for identifying parallel and series
  # There must be an easier way to do this, but it's working now
  # print(aggregated_outputs)

  ## From previous code
  # Extract words
  wordExtract <- function(string) {
    middle <- stringr::str_extract(string, "\\(([^)]+)\\)")
    middleWords <- unlist(strsplit(middle, ",\\s*"))
    middleWords <- gsub("[\\(\\)]", "", middleWords)  # Remove parentheses
    endWord <- stringr::str_extract(string, ":([a-zA-Z0-9]+)$")
    endWord <- gsub(":", "", endWord)  # Remove colon
    return(c(endWord, middleWords))
  }
  
  words<- wordExtract(aggregated_outputs)
  needsBuilt<-stringr::str_sub(stringr::str_extract(aggregated_outputs, ":[a-zA-Z0-9]+"), 2, -1)
  ready<-setdiff(words, needsBuilt)
  
  component_variables <- list()
  
  for (name in words) {
    name <- gsub("^\\s+|\\s+$", "", name)
    component_variables[[name]] <- name
  }
  for (name in names(component_variables)) {
    assign(name, component_variables[[name]])
  }
  
   
  subsystem_names <- component_variables[1:length(aggregated_outputs)]
  
  comps.in.series <- function(values) {
    result <- paste("(", paste(values, collapse = " * "), ")", sep = ' ')
    return(result)
  }
  
  comps.in.parallel <- function(values) {
    result <- paste("(1 - (1 -", paste("(1 -", values, ")", collapse = ") * (1 -"), "))", sep = ' ')
    return(result)
  }
  
  aggregated_outputs <- gsub("\\s+", "", aggregated_outputs)
  
  i = 1
  merging_function <- list()
  while (i <= length(aggregated_outputs)) {
    compNeeded <- wordExtract(aggregated_outputs[i])
    if (substr(aggregated_outputs[i],1,1)%in%c("S", "s")) {
      merging_function[[i]] <- comps.in.series(compNeeded[-1])
    } else { 
      merging_function[[i]] <- comps.in.parallel(compNeeded[-1])
    }
    i <- i + 1
  }
  merging_function
  compNeeded<-compNeeded[-length(compNeeded)]
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
  
  print(merging_function)
  
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
  text1 <- paste(text, collapse = " ")
  priors <- priorsExtract(text1)
  colnames(priors) <- c("alpha", "beta")
  # priors
  
}


#### OUTPUT NUMBERS ####
#### didn't work section... ####
BikeReliability <- function(BackTire, FrontTire, FrontBrakes, BackBrakes) {
  eval(parse(text = merging_function$BI2KE))
}

BikeReliability(BackTire = 0.8, FrontTire = 0.7, FrontBrakes = 0.5, BackBrakes = 0.6)

# Avoid for loop and try a vectorized function

BikeReliability<- function(input_vector) {
  eval(parse(text = merging_function[1]))
}
vector <- c(0.8,0.7,0.5,0.6)
ready
BikeReliability(input_vector)
# Try vectorized function to avoid a for loop ?

BikeReliability <- function(BackTire, FrontTire, FrontBrakes, BackBrakes) {
  eval(parse(text = merging_function$BI2KE))
}

# Function to evaluate the merging function with a vector input
VectorizedBikeReliability <- Vectorize(BikeReliability, vectorize.args = c("BackTire", "FrontTire", "FrontBrakes", "BackBrakes"))

# Input vector testing testing 
input_vector <- list(BackTire = 0.8, FrontTire = 0.7, FrontBrakes = 0.5, BackBrakes = 0.6)

# Evaluate the function with the input vector
result <- VectorizedBikeReliability(BackTire = input_vector$BackTire, FrontTire = input_vector$FrontTire, FrontBrakes = input_vector$FrontBrakes, BackBrakes = input_vector$BackBrakes)
result



BikeReliability <- function(BackTire, FrontTire, BackBrake, FrontBrake
){
  eval(parse(text = merging_function$Bike))
}

evaluate <- setdiff(words, compNeeded)
BikeReliability(BackTire = 0.8, FrontTire = 0.7, BackBrake=.6, FrontBrake=.5)

# lines below should become irrelevant 
stored <- list()
i <- 1  
while (i <= length(evaluate)) {
  stored[[i]] <- paste0(evaluate[i], "=", alpha[i])
  i <- i + 1  
}
# Right now, it's just putting in alpha but it is outputting a number which is good!! 
result <- paste("BikeReliability(", paste(stored, collapse = ", "), ")")
eval(parse(text = result))




######## this takes in a vector, assigns it to ######

vector <- c(0.8,0.7,0.5,0.6)
ready
variables <- setNames(vector, ready)
variables
evaluate_sys <- merging_function[1]

for (var in names(variables)) {
  evaluate_sys <- gsub(var, as.character(variables[var]), evaluate_sys)
}

result <- eval(parse(text = evaluate_sys))
print(result)
