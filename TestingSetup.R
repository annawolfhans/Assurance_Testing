########################## BIKE EXAMPLE ##########################

file <- textConnection("S(BackTire,FrontTire):Tires
P(BackBrake,FrontBrake):Brakes
S(Tires,Brakes):Bike")

## MORE COMPLEX EXAMPLE ##
file <- textConnection("S(BackTire,FrontTire):Tires
P(BackBrake,FrontBrake):Brakes
P(Electric, Self): Power
S(Tires,Brakes,Power):Bike")

## SIMPLER EXAMPLE ##
file <- textConnection("S(Tires, Brakes): Bike")

## EXAMPLE WITH PRIORS - note that additional checks with priors are still needed ##
file <- textConnection("S(FrontBrakes<- c(8,8), BackBrakes <- c(4,4)):Brakes")
file <- textConnection("S(BackTire<- c(1,2),FrontTire<- c(3,4)):Tires
P(BackBrake<- c(7,8),FrontBrake<- c(9,8)):Brakes
S(Tires,Brakes):Bike")
file <- textConnection("S(BackTire:(1,2):T:$1:, FrontTire:(1,2):T:$1:), Tires")

file <- textConnection("S(BackTire:prior=beta(1,2):TRUE:$1:, FrontTire:prior=beta(1,2):TRUE:$1:), Tires
P(BackBrake:prior=beta(1,2):TRUE:$1:, FrontBrake:prior=beta(1,2):TRUE:$1:), Brakes
S(Tires:prior=beta(1,2):TRUE:$1:, Brakes:prior=beta(1,2):TRUE:$1:), Bike")
# make sure it pulls first word separated by comma 
# If there's priors, it's a component

assurance_testing_setup <- function(file){
## BEGIN CHECKS FOR FORMATTING ##
rbd_text=suppressWarnings(readLines(file))
if(length(rbd_text)==0)stop("Text file contained 0 elements")
rbd_text=stringr::str_replace_all(rbd_text, " ", "")

## Checks to see if begins with P or S
if(!all(stringr::str_sub(rbd_text, 1, 2) %in% c('p(', 'P(', 's(', 'S(')))stop("All expressions must start with P( or S(")

## Remove <-c(#,#) information from the text
text <- gsub("<-c\\([^)]+\\)", "", rbd_text)
# from here on out, rbd_text will include prior information, text will not

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

## Extract priors and put in alpha and beta vectors

Numextract <- function(string){
  unlist(regmatches(string,gregexpr("[[:digit:]]+\\.*[[:digit:]]*",string)))
}

priors <- as.integer(Numextract(rbd_text))
alpha <- numeric(0)
beta <- numeric(0)

for (i in seq_along(priors)) {
  if (i %% 2 == 1) {
    alpha <- c(alpha, priors[i])
  } else {
    beta <- c(beta, priors[i])
  }
}
# Name items of the list 
# priorparams$FrontBrake 8 8 
# or FrontBrakes$alpha 8
# FrontBrakes$beta 8
# alpha
# beta

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

for (name in words) {
  name <- gsub("^\\s+|\\s+$", "", name)
  component_variables[[name]] <- name
}
for (name in names(component_variables)) {
  assign(name, component_variables[[name]])
}

subsystem_names <- component_variables[(length(component_variables) - length(text) + 1):length(component_variables)]
## Set up our series and parallel equations
i = 1
merging_function <- list()
while (i <= length(text)) {
  compNeeded <- wordExtract(text[i])
  if (substr(text[i],1,1)%in%c("S", "s")) {
    merging_function[[i]] <- comps.in.series(compNeeded[-length(compNeeded)])
  } else { 
    merging_function[[i]] <- comps.in.parallel(compNeeded[-length(compNeeded)])
  }
  i <- i + 1
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

## Print the updated merging_function
print(merging_function)
}

assurance_testing_setup(file)

### ENSURE THIS CAN PERFORM CALCULATIONS ### 
# change it so line 170 is dynamic instead 
# BikeReliability pasted with line 173 text, then eval parse the whole thing
# system name should only be used once, you can use that fact s
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
