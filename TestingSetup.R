########################## BIKE EXAMPLE ##########################

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

## BEGIN CHECKS FOR FORMATTING ##
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
if (length(duplicates) > 0) {
  system_name <- words[(max(duplicated_indices) +1)]
  system_name <- eval(parse(text = system_name))
  i = 1
  while (i < length(text)){
    system_name <- gsub(duplicates[i], eval(merging_function[i]), system_name)
    i = i+1
  }
} else { # this else statement should make it so if there's a simple function, 
          # the last word (which should be the system name), is the system name
  system_name <- words[length(words)]
  system_name <- eval(parse(text = system_name))
}

## This outputs our "equation 13". Our compressed equation.
# I ALSO NEED TO GO BACK AND STORE THIS INTO BIKE NOT SYSTEM_NAME
# tmp <- eval(parse(text = words[length(words)])) 
# tmp <- system_name
# not sure if this line will be right - consult Dr. Warr