########################## BIKE EXAMPLE ##########################
# test test

file <- textConnection("S(BackTire,FrontTire):Tires
P(BackBrake,FrontBrake):Brakes
S(Tires,Brakes):Bike")

## MORE COMPLEX EXAMPLE ##
# file <- textConnection("S(BackTire,FrontTire):Tires
# P(BackBrake,FrontBrake):Brakes
# P(Electric, Self): Power
# S(Tires,Brakes):Bike")

## SIMPLER EXAMPLE ##
# file <- textConnection("S(Tires, Brakes): Bike")

## BEGIN CHECKS FOR FORMATTING##
text=suppressWarnings(readLines(file))
if(length(text)==0)stop("Text file contained 0 elements")
text=stringr::str_replace_all(text, " ", "")

## Checks to see if valid
if(!all(stringr::str_sub(text, 1, 2) %in% c('p(', 'P(', 's(', 'S(')))stop("All expressions must start with P( or S(")


## Makes sure the pattern after the opening parenthesis is #,#,#,....):#
if(!(all(stringr::str_detect(stringr::str_sub(text, 3),
                             pattern = "^([a-zA-Z0-9]+,)+[a-zA-Z0-9]+\\):[a-zA-Z0-9]+$"))))
  stop("One or more strings did not meet grammar requirements (eg. P(V1,V2,V3):P1)")

## http://stla.github.io/stlapblog/posts/Numextract.html
wordExtract <- function(string){
  middle=stringr::str_sub(stringr::str_extract(string,
                                               '\\([a-zA-Z0-9,]+\\)'),2,-2)
  middleWords<-unlist(strsplit(middle, ','))
  endWord=stringr::str_sub(stringr::str_extract(string, ":[a-zA-Z0-9]+"), 2, -1)
  return(c(middleWords, endWord))
}

words<-wordExtract(text)

## The first time through we will see if it is logically defined
## The 2nd time through we will perform the computations
needsBuilt<-stringr::str_sub(stringr::str_extract(text, ":[a-zA-Z0-9]+"), 2, -1)

if (length(needsBuilt)!=length(unique(needsBuilt)))stop("Non-unique definitions given to component (check right side of colon for non-unique values)")
## Anything is 'ready' if it is not on the RHS of a definition
ready<-setdiff(words, needsBuilt)

## Check to make sure all components only appear once on the LHS of the relations (enforce that data not used twice)
counts<-table(words)
countIndices<-names(counts)%in% names(table(needsBuilt))
if(any(counts[countIndices]>2) | !all(counts[!countIndices]==1))
  stop("Component names must be unique on LHS of equations")

## Check for circular relationship
done<-rep(F, length(text))
i=0
oneChange=FALSE
while(any(!done)){
  i=i+1
  if (i > length(text) &!oneChange)stop("The diagram specified contains circular relationships that makes the model impossible")
  else if (i>length(text) &oneChange){
    i =1
    oneChange=FALSE
  }
  if (done[i]) next
  compNeeded <- wordExtract(text[i])
  Name<-compNeeded[length(compNeeded)]
  compNeeded<-compNeeded[-length(compNeeded)]
  if (all(compNeeded%in% ready)){
    needsBuilt<-setdiff(needsBuilt, Name)
    ready<-c(ready, Name)
    oneChange=TRUE
    done[i]<-TRUE
  }
}

## Series and parallel structure functions ##
comps.in.series <- function(values) {
  result <- paste("(", paste(values, collapse = " * "), ")", sep = ' ')
  return(result)
}

comps.in.parallel <- function(values) {
  result <- paste("(1 - (1 -", paste("(1 -", values, ")", collapse = ") * (1 -"), "))", sep = ' ')
  return(result)
}

# tmpList <- list() # POSSIBLY DELETE
# priorList <- list() # POSSIBLY DELETE
# dataList <- NULL # POSSIBLY DELETE

done<-rep(F, length(text)) #Done matches up with the lines in variable text
i=0
oneChange=FALSE
# Main loop
# merging_function = ifelse(substr(text[1],1,1)%in%c("S", "s"), comps.in.series, comps.in.parallel)
# compNeeded <- wordExtract(text[2])
# first_component<- compNeeded[1]
# first_component <- gsub("\"", "", first_component)
# comp1<- get(compNeeded[1])
# eval(parse(merging_function(compNeeded[1], compNeeded[2])))
# where should I put the eval(parse()) part of the code?
merging_function<- list()
i <- 1 # POSSIBLY DELETE
# component_variables <- list()
# 
# ##This will create the variables as their respective string names
# for (name in words) {
#   name <- gsub("^\\s+|\\s+$", "", name)
#   component_variables[[name]] <- name
# }
# for (name in names(component_variables)) {
#   assign(name, component_variables[[name]])
# }

# while(any(!done)){ # continues until at least one element in the 'done' vector is not true
#   i=i+1 # increments i by one value fo reach iteration
#   # Reset i if necessary
#   if (i>length(text) &oneChange){
#     i =1
#     oneChange=FALSE
#   }
#   # Skip if done 
#   if (done[i]) next
#   
#   #Extract component information
#   compNeeded <- wordExtract(text[i])
#   # Select merging function based on First character of text[i]
#   merging_function=ifelse(substr(text[i],1,1)%in%c("S", "s"), comps.in.series, comps.in.parallel)
#   # Get the name
#   Name<-compNeeded[length(compNeeded)]
#   compNeeded<-compNeeded[-length(compNeeded)]
#   # Check if all components are ready
#   if (all(compNeeded%in% ready)){
#     ##We are ready to perform the computations needed in order to provide the prior for Name
# 
#     #This handles the case where there are more than two components in the list
#   temp = tmpList[[compNeeded[[1]]]]
#     for (j in 2:length(compNeeded)){
#       temp<-(merging_function(temp, tmpList[[compNeeded[j]]]))
#       #d<<-temp
#       #if(compNeeded[j]=="generator7")skldf
#     }
#   
#     # Store the results in priorList
#     priorList[[Name]]<-temp
# 
#   needsBuilt<-setdiff(needsBuilt, Name)
#   ready<-c(ready, Name)
#   oneChange=TRUE
#   done[i]<-TRUE
#   output <- eval(parse(merging_function(compNeeded[i], compNeeded[i-1])))
#   print(output)
#   #   if (!is.null(dataList[[Name]])){
#   #     tmpList[[Name]]<-(priorList[[Name]])
#   #   }else{ #if there's no data to augment it, just pass the prior through as the posterior
#   #     tmpList[[Name]]<-priorList[[Name]]
#   #   }
#   # }
# }
# }

## Store names as strings into their name as a variable
component_variables <- list()
words<-wordExtract(text) # POSSIBLY DELETE

for (name in words) {
  name <- gsub("^\\s+|\\s+$", "", name)
  component_variables[[name]] <- name
}
for (name in names(component_variables)) {
  assign(name, component_variables[[name]])
}

## Set up our series and parallel equations
i = 1
merging_function <- list()
while (i <= length(text)) {
  compNeeded <- wordExtract(text[i])
  if (substr(text[i],1,1)%in%c("S", "s")) {
    merging_function[[i]] <- comps.in.series(compNeeded[-length(text)])
  } else { 
    merging_function[[i]] <- comps.in.parallel(compNeeded[-length(text)])
  }
  i <- i + 1
}

## Store the equations into their respective subsystem names
temp <- list()
temporary <- list()
i = 1
while (i <= length(text)) {
  compNeeded <- wordExtract(text[i])
  temp[i] <- paste(merging_function[i])
  temporary[i] <- paste0(compNeeded[length(text)], " = temp[", i, "]")
  eval(parse(text = temporary[i]))
  i = i+1
}

## Identify duplicate values to recognize components and subcomponents
# I need to make sure 1. this will work if there's a simpler option (no duplicates)
# 2. a more complex three subsystem system will also work


duplicated_indices <- which(duplicated(words))
duplicates <- words[duplicated_indices]
## This line ensures that it won't require the system name to be listed last. 
system_name <- words[(max(duplicated_indices) +1)]
system_name <- eval(parse(text = system_name))
i = 1
while (i < length(text)){
  system_name <- gsub(duplicates[i], eval(merging_function[i]), system_name)
  i = i+1
}
## This outputs our "equation 13". Our compressed equation.
# I ALSO NEED TO GO BACK AND STORE THIS INTO BIKE NOT SYSTEM_NAME
system_name
