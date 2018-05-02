alpha_Measures <- function(nuggets, R, reviews, verbose = TRUE, alpha = 0.5, beta = 2, gama = 2){
  
  ### Params
  ### nuggets: binary data.frame of content descriptors for each item
  ### R: matrix with top N recommended items for each user
  ### reviews: matrix with all visited items for each user
  ### verbose: if true, print the process out
  ### alpha: parameter of evaluation
  ### beta: parameter of evaluation
  ### gama: parameter of evaluation
  
  N <- ncol(R)
  
  tt <- nrow(nuggets)
  
  aCG <- NULL
  nCG <- NULL
  sCG <- NULL
  aCGi <- NULL
  aCGii <- NULL
  aDCG <- NULL
  nDCG <- NULL
  sDCG <- NULL
  aDCGi <- NULL
  aDCGii <- NULL
  aDCGs <- NULL
  anDCGs <- NULL
  nnDCGs <- NULL
  snDCGs <- NULL
  dnDCGs <- NULL
  
  n_users <- nrow(R)
  warn = 0
  
  for (user in 1:n_users) {
    
    top_R = matrix(data = 0, nrow = ncol(R), ncol = ncol(nuggets))
    
    for (reco in 1:nrow(top_R)) {
      
      if (is.na(R[user,reco])) # random selection for NAs recommendations 
        top_R[reco,] = sample(0,ncol(nuggets),replace=TRUE)
      else
        top_R[reco,] <- unlist(nuggets[which(rownames(nuggets) == R[user,reco]),])           
    }
    
    exp1 <- NULL
    exp2 <- NULL    
    
    Nu <- length(reviews[[user]])
    
    for (i in 1:ncol(nuggets)) {
      
      if (sum(nuggets[,i] == tt)) {
        exp1[i] = -1  
      } 
      else {
        exp1[i] = log(tt/(sum(nuggets[,i])+1), base = tt) - 1  
      }
      
      if (Nu == 0){
        
        if (warn == 0) {
          cat("Warning: Some Users Have Not Rated Items Yet, Cold Start Problem.")
          warn = warn + 1
        }
        exp2[i] = 0 # Cold start Effect (user has not rated any items yet)
      } 
      else{
        if (sum(nuggets[reviews[[user]],i]) > 0){
          if (sum(nuggets[reviews[[user]],i]) == Nu) {
            exp2[i] = -1
          }
          else {
            exp2[i] = log(Nu/(sum(nuggets[reviews[[user]],i])+1), base = Nu) - 1  
          }
          
        } 
        else {
          exp2[i] = 0
        }
      }
      
    }
    
   
    r_real = matrix(data = 0, nrow = ncol(top_R), ncol = nrow(top_R))
    
    for (i in 1:nrow(top_R)) {
      for (j in 1:ncol(top_R)) {
        r_real[j,i] = sum(top_R[1:i,j])
      }      
    }
    
    
    for (i in 1:N) {           
      
      aCGi[i] <- sum(top_R[i,]*alpha^r_real[,i])          
      nCG[i] <- sum(top_R[i,]*alpha^r_real[,i]*(beta^exp1)) #Alpha.Beta-NDCG
      sCG[i] <- sum(top_R[i,]*alpha^r_real[,i]*(gama^exp2)) #Alpha.Gamma-NDCG
      aCG[i] <- sum(top_R[i,]*alpha^r_real[,i]*(beta^exp1)*(gama^exp2)) #Alpha.Beta.Gamma-NDCG
      
    }
    
    aCGii <- sort(aCGi, decreasing = TRUE)
    
    if (N > 1) {
      for (i in 2:length(aCGi)) {
        aCGi[i] = aCGi[i - 1] + aCGi[i]
        
        aCG[i] = aCG[i - 1] + aCG[i]
        nCG[i] = nCG[i - 1] + nCG[i]
        sCG[i] = sCG[i - 1] + sCG[i]
        
        aCGii[i] = aCGii[i - 1] + aCGii[i]
      }  
    }
    
    aDCG[1] <- aCG[1]
    nDCG[1] <- nCG[1]
    sDCG[1] <- sCG[1]
    aDCGi[1] <- aCGi[1]
    aDCGii[1] <- aCGii[1]
    
    if (N > 1) {
      for (i in 2:length(aCGi)) {
        aDCG[i] <- aDCG[i - 1] + (aCG[i]/log2(i))
        nDCG[i] <- nDCG[i - 1] + (nCG[i]/log2(i))
        sDCG[i] <- sDCG[i - 1] + (sCG[i]/log2(i))
        aDCGi[i] <- aDCGi[i - 1] + (aCGi[i]/log2(i))
        aDCGii[i] <- aDCGii[i - 1] + (aCGii[i]/log2(i))
      }  
    }
    
    if (sum(aDCGi[1:N]) > 0){
      aDCGs[user] <- sum(aDCG[1:N])
      anDCGs[user] <- sum(aDCG[1:N])/sum(aDCGi[1:N])
      nnDCGs[user] <- sum(nDCG[1:N])/sum(aDCGi[1:N])
      snDCGs[user] <- sum(sDCG[1:N])/sum(aDCGi[1:N])  
    }
    else {
      aDCGs[user] <- 0
      anDCGs[user] <- 0
      nnDCGs[user] <- 0
      snDCGs[user] <- 0
    }
    
    if (sum(aDCGii[1:N]) > 0)
      dnDCGs[user] <- sum(aDCGi[1:N])/sum(aDCGii[1:N])
    else
      dnDCGs[user] <- 0
    
    if (verbose) cat("user",user,"a-DCG:",aDCGs[user],"a-NDCG:",dnDCGs[user],"ab-NDCG:",nnDCGs[user],"ag-NDCG:",snDCGs[user],"abg-NDCG:",anDCGs[user],"\n")
    
  }
  
  abg_NDCG <- mean(anDCGs)
  ab_NDCG <- mean(nnDCGs)
  a_NDCG <- mean(dnDCGs)
  ag_NDCG <- mean(snDCGs)
  a_DCG <- mean(aDCGs)
  
  uniq_items <- names(which(table(R) == 1))
  uniq_items_u <- NULL
  
  for (i in 1:length(uniq_items)) {
    uniq_items_u <- c(uniq_items_u, which(R == uniq_items[i], arr.ind = T)[1])
  }
  
  TotDiv <- a_NDCG*(length(unique(uniq_items_u))/n_users)
  abg_TotDiv <- abg_NDCG*TotDiv
  res <- lapply(list(a_DCG,a_NDCG, ab_NDCG, ag_NDCG, abg_NDCG, TotDiv, abg_TotDiv),round,4)
  names(res) = c("a-DCG","a-NDCG","ab-NDCG","ag-NDCG","abg-NDCG","a-TOT","Utility") 
  if (verbose) cat("a-DCG:",a_DCG,"a-NDCG:",a_NDCG,"ab-NDCG:",ab_NDCG,"ag-NDCG:",ag_NDCG,"abg-NDCG:",abg_NDCG,"TotDiv:",TotDiv,"abg_TotDiv:",abg_TotDiv)
  return(res)
}