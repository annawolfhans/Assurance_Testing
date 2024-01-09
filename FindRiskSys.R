#### QUESTIONS FOR DR WARR
### check on cvec and nvec
### Consider changing samp and samps because that's a little confusing 
### I already changed alpha_vec to priors_alpha

set.seed(16)
# priors - this will have been given in the set up function :)
# alpha_vec <- c(8,8,0)
# beta_vec <- c(4,4,0)

p.rrl <- .9
p.arl <- .95
mcsamps <- 1000000

alpha <- .05
beta <- .05

both <- TRUE
iter <- 0
iter_c <- 0
nlistsys_prel <- 1
clistsys_prel <- 0
samps <- NULL
samp = NULL
# May change the names here for clarity

merging_function
cvec
nvec
find_system_risks <- function(merging_function, priors_alpha, priors_beta, cvec, nvec, p.rrl, p.arl, mcsamps=10, samp = NULL) {
  # IF WE HAVEN'T TAKEN A PRIOR SAMPLE, TAKE ONE NOW
  made_samp <- FALSE
  priors_alpha <- as.numeric(priors[,1])
  priors_beta <- as.numeric(priors[,2])
  i <- 1
  if(length(samp) == 0){
    samp <- list()
    for(i in 1:length(priors_alpha)){
      samp[[i]] <- rbeta(mcsamps, priors_alpha[i], priors_beta[i])
      i <- i + 1
    }
    made_samp <- TRUE
    
  }
  #ready
  variables <- setNames(samp, ready)
  #variables
  evaluate_sys <- merging_function[1]
  
  for (var in names(variables)) {
    evaluate_sys <- gsub(var, as.character(variables[var]), evaluate_sys)
  }
  # DEFINE SYSTEM PRIORS
  sampSys <- eval(parse(text = evaluate_sys))
  # print(sampSys)
  
  # Check and see if this note is still important ??
  # After this, I have n number of samps in the order of how it appears in the file variable
  # the problem is that merging_function may not be in the same order of how it appears in the file
  # I could go BACK to rbd_text and associate it there? possibly do a eval(parse(text )) thing?
  cvec <- rep(0,length(priors_alpha))
  nvec <- c(rep(0,(length(priors_alpha)-1)), 1)
  i <- 1
  # GET PROBABILITIES OF TESTS PASSING
  test_list <- list()  # I want this to output test1, test2 etc so I can access it for the substitution
  
  for (i in 1:(length(priors_alpha))) {
    test_var <- pbinom(cvec[i], nvec[i], 1 - samp[[i]])
    test_name <- paste0('test', i)
    assign(test_name, test_var)
    test_list[[test_name]] <- test_var
    print(get(test_name))  ## check if working correctly
  }
  testSys <- pbinom(cvec[length(priors_alpha)], nvec[length(priors_beta)], 1 - sampSys)
  
  TIP <- testSys * 
    test1 *
    test2
  merging_function[[1]]
  
  # Rewrite dynamically
  
  numerPostConsumRisk <- mean((sampSys<=p.rrl)*TIP)
  denomPostConsumRisk <- ifelse(mean(TIP) == 0, 1, mean(TIP))
  numerPostProducRisk <- mean((sampSys>=p.arl)*(1-TIP))  
  denomPostProducRisk <- ifelse(denomPostConsumRisk==1,1,1-denomPostConsumRisk)
  
  #return different lists depending on if samps already exists
  if(made_samp){
    return(list(PPR=numerPostProducRisk/denomPostProducRisk,
                PCR=numerPostConsumRisk/denomPostConsumRisk,
                samps=list(samp)))
  }
  
  list(PPR=numerPostProducRisk/denomPostProducRisk,
       PCR=numerPostConsumRisk/denomPostConsumRisk)
}
  

# while(both){
#   
#   
#   
#   #check PCR and PPR, save prior sample if need be
#   risk_list <- find_system_risks(merging_function, priors_alpha, priors_beta,cvec,nvec,p.rrl,p.arl,mcsamps, samps)
#   if(length(samps) == 0){
#     samps <- risk_list$samps[[1]] ## 
#   }
#   
#   #do flowchart
#   if(risk_list['PPR'] > alpha){
#     if(cvec[length(cvec)] < (nvec[length(nvec)] - 1)){
#       cvec[length(cvec)] <- cvec[length(cvec)] + 1
#     }
#     else{
#       nvec[length(nvec)] <- nvec[length(nvec)] + 1
#       cvec[length(cvec)] <- cvec[length(cvec)] + 1
#     }
#     
#   }
#   else if(risk_list['PCR'] > beta){
#     nvec[length(nvec)] <- nvec[length(nvec)] + 1
#   }
#   
#   if((risk_list['PPR'] < alpha) & (risk_list['PCR'] < beta)){
#     both <- FALSE
#     print(paste0('nsys is ', nvec[length(nvec)]))
#     print(paste0('csys is ', cvec[length(cvec)]))
#     print(risk_list)
#   }
#   #save vector of system tests and failures
#   clistsys_prel[iter_c + 1] <- cvec[length(cvec)]
#   nlistsys_prel[iter_c + 1] <- nvec[length(nvec)]
#   iter_c <- iter_c + 1
#   iter <- iter + 1
#   print(iter)
# }
# 
# max_dist <- function(risk_list, alpha, beta){
#   if((risk_list['PPR'] >= alpha) & (risk_list['PCR'] >= beta)){
#     distance <- sqrt((risk_list[['PPR']] - alpha)^2 + (risk_list[['PCR']] - beta)^2)
#   }
#   else if(risk_list['PPR'] >= alpha){
#     distance <- risk_list[['PPR']] - alpha
#   }
#   else{
#     distance <- risk_list[['PCR']] - beta
#   }
# }
# 
# nlistcomp_prel <- lapply(1:(length(nvec)-1), function(x) nvec[x])
# clistcomp_prel <- lapply(1:(length(cvec)-1), function(x) cvec[x])
# j <- iter + 2
# iter <- 0
# both <- TRUE
# clistsys_prel <- c(0, clistsys_prel)
# nlistsys_prel <- c(0, nlistsys_prel)
# # prices <- c(1,1,4) # this will now come out of Testing Setup
# prices[-length(prices)] <- prices[-length(prices)]
# price_order <- order(prices)
# hold_iter <- 1
# sys_only_price <- prices[length(prices)] * nvec[length(nvec)]
# component_prices <- 0
# old_price <- sys_only_price
# hold_min <- sys_only_price
# hold_min_test <- hold_min_fail <- 0
# costs <- numeric()
# stop_test <- FALSE
# iter_hold <- 0
# 
# while (!stop_test) {
#   cvec[length(cvec)] <- clistsys_prel[j - 1]
#   nvec[length(nvec)] <- nlistsys_prel[j - 1]
#   j <- j - 1
#   r <- 1
#   r_out <- FALSE
#   
#   both <- TRUE
#   while(both & !r_out){
#     if(r >= 10){
#       r_out <- TRUE
#       next
#     } # clarify that it is skipping this next esctoin
#     risk_list <- find_system_risks(priors,cvec,nvec,p.rrl,p.arl,mcsamps, samps)
#     if((risk_list['PPR'] < alpha) & (risk_list['PCR'] < beta)){
#       both <- FALSE
#       next
#     }
#     
#     cvec_2 <- c(ifelse(cvec[-length(cvec)] == 0,cvec[-length(cvec)],cvec[-length(cvec)] - 1),cvec[length(cvec)])
#     risk_list_p2 <- FindRisksSys(priors_alpha,priors_beta,cvec_2,nvec,p.rrl,p.arl,mcsamps, samps)
#     nvec_1 <- c(ifelse(nvec[-length(nvec)] <= (cvec[-length(cvec)] + 1), nvec[-length(nvec)] + 1, nvec[-length(nvec)]),nvec[length(cvec)])
#     cvec_1 <- c(cvec[-length(cvec)] + 1,cvec[length(cvec)])
#     nvec_1 <- c(ifelse(nvec_1[-length(nvec)] <= (cvec_1[-length(cvec)] + 1), nvec[-length(nvec)] + 1, nvec[-length(nvec)]),nvec[length(cvec)])
#     risk_list_p1 <- FindRisksSys(priors_alpha,priors_beta,cvec_1,nvec_1,p.rrl,p.arl,mcsamps, samps)
#     
#     if((max_dist(risk_list,alpha,beta) <= max_dist(risk_list_p2,alpha,beta)) & (max_dist(risk_list,alpha,beta) <= max_dist(risk_list_p1,alpha,beta))){
#       nvec[-length(nvec)] <- nvec[-length(nvec)] + 1
#     }
#     else if(max_dist(risk_list_p1,alpha,beta) < max_dist(risk_list_p2,alpha,beta)){
#       cvec <- cvec_1
#       nvec <- nvec_1
#     }
#     else{
#       cvec <- cvec_2
#     }
#     nlistcomp_prel <- lapply(1:(length(nvec) - 1), function(x) c(nlistcomp_prel[[x]], nvec[x]))
#     clistcomp_prel <- lapply(1:(length(cvec) - 1), function(x) c(clistcomp_prel[[x]], cvec[x]))
#     r <- r + 1
#   }
#   for(i in 1:(length(nvec) - 1)){
#     if(r_out) next
#     all2 <- TRUE
#     comp_loc <- which(price_order == (length(nvec) - i))
#     
#     while(all2){
#       
#       if(nvec[comp_loc] == 0){
#         all2 <- FALSE
#         next
#       }
#       
#       nvec[comp_loc] <- nlistcomp_prel[[comp_loc]][length(nlistcomp_prel[[comp_loc]]) - 1]
#       cvec[comp_loc] <- clistcomp_prel[[comp_loc]][length(clistcomp_prel[[comp_loc]]) - 1]
#       
#       risk_list <- FindRisksSys(priors_alpha,priors_beta,cvec,nvec,p.rrl,p.arl,mcsamps, samps)
#       
#       if(!((risk_list['PPR'] < alpha) & (risk_list['PCR'] < beta))){
#         nvec[comp_loc] <- nlistcomp_prel[[comp_loc]][length(nlistcomp_prel[[comp_loc]])]
#         cvec[comp_loc] <- clistcomp_prel[[comp_loc]][length(clistcomp_prel[[comp_loc]])]
#         all2 <- FALSE
#       }
#       else{
#         clistcomp_prel[[comp_loc]] <- clistcomp_prel[[comp_loc]][1:(length(clistcomp_prel[[comp_loc]]) - 1)]
#         nlistcomp_prel[[comp_loc]] <- nlistcomp_prel[[comp_loc]][1:(length(nlistcomp_prel[[comp_loc]]) - 1)]
#       }
#     }
#   }
#   if(r_out) next
#   component_prices <- sum(nvec * prices)
#   costs[iter] <- component_prices
#   iter_hold <- iter_hold + 1
#   if(component_prices < hold_min){
#     hold_min <- component_prices
#     hold_min_test <- nvec
#     hold_min_fail <- cvec
#   }
#   if(component_prices <= old_price){
#     iter_hold <- 0
#   }
#   old_price <- component_prices
#   if(iter_hold == 5){
#     stop_test <- TRUE
#   }
#   if(nvec[length(nvec)] == 0){
#     stop_test <- TRUE
#   }
#   iter <- iter + 1
#   print(iter)
#   print(paste0('component_price: ',component_prices))
#   print(paste0('system_only_price: ',sys_only_price))
#   print(paste0('min_value: ', hold_min))
#   print(paste0('min_config_n: ', hold_min_test))
#   print(paste0('min_config_c: ', hold_min_fail))
#   print(paste0('n components is ', nvec))
#   print(paste0('c components is ', cvec))
#   print(risk_list)
# }
# 
# risk_list <- FindRisksSys(priors,hold_min_fail,hold_min_test,p.rrl,p.arl,mcsamps, samps)
# print(risk_list)
# out_frame <- cbind(costs, nlistsys_prel[length(nlistsys_prel):2][2:(length(costs) + 1)], clistsys_prel[length(nlistsys_prel):2][2:(length(costs) + 1)])
# saveRDS(out_frame, 'parallel_out.RDS')
