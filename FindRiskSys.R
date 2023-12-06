set.seed(16)


find_system_risks <- function(alpha, beta, cvec, nvec, p.rrl, p.arl, mcsamps=10, samp = NULL) {
  #if we haven't taken a prior sample, take one now
  made_samp <- FALSE
  alpha <- as.numeric(priors[,1])
  beta <- as.numeric(priors[,2])
  i <- 1
  if(length(samp) == 0){
    samp <- list()
    for(i in 1:length(priors[,1])){
      samp[[i]] <- rbeta(mcsamps, alpha[i], beta[i])
      i <- i + 1
    }
    made_samp <- TRUE
    
  }
  
  vector <- c(0.8,0.7,0.5,0.6)
  ready
  variables <- setNames(samp, ready)
  variables
  evaluate_sys <- merging_function[1]
  
  for (var in names(variables)) {
    evaluate_sys <- gsub(var, as.character(variables[var]), evaluate_sys)
  }
  
  result <- eval(parse(text = evaluate_sys))
  print(result)
  # After this, I have n number of samps in the order of how it appears in the file variable
  # the problem is that merging_function may not be in the same order of how it appears in the file
  # I could go BACK to rbd_text and associate it there? possibly do a eval(parse(text )) thing?
  cvec <- rep(0,length(alpha))
  nvec <- c(rep(0,(length(alpha)-1)), 1)
  
  for(i in 1:(length(alpha))){
    assign(paste0('test',i), pbinom(cvec[i], nvec[i], 1-samp[[i]]))
  }
  testSys <- pbinom(cvec[length(alpha)], nvec[length(beta)], 1 - sampSys)
  
  
  
  
  
}