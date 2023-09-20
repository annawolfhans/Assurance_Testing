set.seed(16)

find_system_risks <- function(alpha, beta, cvec, nvec, p.rrl, p.arl, mcsamps=100, samp = NULL) {
  #if we haven't taken a prior sample, take one now
  made_samp <- FALSE
  if(length(samp) == 0){
    samp <- list()
    for(i in 1:length(alpha)){
      samp[[i]] <- rbeta(mcsamps, alpha[i], beta[i])
    }
    made_samp <- TRUE
  }
  # After this, I have n number of samps in the order of how it appears in the file variable
  # the problem is that merging_function may not be in the same order of how it appears in the file
  # I could go BACK to rbd_text and associate it there? possibly do a eval(parse(text )) thing?
  
  
  #define system priors
  sampSys <- samp[[1]] * samp[[2]] # include output from parsing
  sampSys <- merging_function[1]
  merging_function
  # dumb question but there would only be a prior associated with Brakes bc it goes into Bike? Maybe
  
  
  
  
  
}