Content_Novelty <- function(topics, recommendations, rated, verbose = TRUE, alpha = 0.5, beta = 2, gama = 2){

  ### Params
  ### topics: binary data.frame of nuggets for each item
  ### recommendations: matrix with top N recommended items for each user
  ### rated: matrix with all visited items for each user
  ### verbose: if true, print the process out
  ### alpha: parameter of evaluation
  ### beta: parameter of evaluation
  ### gama: parameter of evaluation
    
  N <- ncol(recommendations)
  tt <- nrow(topics)
  NoT <- NULL
  for (i in 1:ncol(topics)) NoT[i] <- sum(topics[,i]) # Number of each particular topic
  
  aCG <- NULL
  nCG <- NULL
  sCG <- NULL
  aCGi <- NULL
  aDCG <- NULL
  nDCG <- NULL
  sDCG <- NULL
  aDCGi <- NULL
  aDCGii <- NULL
  anDCGs <- NULL
  nnDCGs <- NULL
  snDCGs <- NULL
  dnDCGs <- NULL
  
  n_users <- nrow(recommendations)
  
  for (user in 1:n_users) {
    
    top_recommended = matrix(data = 0, nrow = ncol(recommendations), ncol = ncol(topics))
    
    for (reco in 1:nrow(top_recommended)) {
      top_recommended[reco,] <- unlist(topics[which(rownames(topics) == recommendations[user,reco]),])
      #top_recommended[reco,] <- topics[which(rownames(topics) == recommendations[user,reco]),]
    }
    
    if (length(rated[[user]]) > 1) {
      topic_consumed <- colSums(topics[rated[[user]],]) # Number of items for each topic consumed by user
    } 
    else {
      topic_consumed <- topics[rated[[user]],]
    }
    
    exp1 <- NULL
    for (i in 1:ncol(topics)) {
      exp1[i] = log(tt/sum(topics[,i]), base = tt) - 1
    }
    
    exp2 <- NULL
    Nu <- length(rated[[user]])
    
    for (i in 1:ncol(topics)) {
      
      if ( (sum(topics[rated[[user]],i]) > 0) & (Nu > 1) ) {
        exp2[i] = log(Nu/sum(topics[rated[[user]],i]), base = Nu) - 1
      } else {
        exp2[i] = 0
      }
      
    }
    
    exp2[ exp2 == Inf ] <- 0
    
    r_real = matrix(data = 0, nrow = ncol(top_recommended), ncol = nrow(top_recommended))
    
    for (i in 1:nrow(top_recommended)) {
      for (j in 1:ncol(top_recommended)) {
        r_real[j,i] = sum(top_recommended[1:i,j])
      }      
    }
    
    for (i in 1:N) {
      aCGi[i] <- sum(top_recommended[i,]*alpha^r_real[,i])
      nCG[i] <- sum(top_recommended[i,]*alpha^r_real[,i]*(beta^exp1)) # Alpha.Beta-NDCG
      sCG[i] <- sum(top_recommended[i,]*alpha^r_real[,i]*(gama^exp2)) # Alpha.Gamma-NDCG
      aCG[i] <- sum(top_recommended[i,]*alpha^r_real[,i]*(beta^exp1)*(gama^exp2)) # Alpha.Beta.Gamma-NDCG
      
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
    
    anDCG <- aDCG/aDCGi # Total Diversity (Inter-Diversity)
    nnDCG <- nDCG/aDCGi # Alpha.Beta-NDCG
    snDCG <- sDCG/aDCGi # Alpha.Gamma-NDCG
    inDCG <- aDCGi/aDCGii # Alpha-NDCG (Intra-Diversity)
    
    anDCGs[user] <- anDCG[N]
    nnDCGs[user] <- nnDCG[N]
    snDCGs[user] <- snDCG[N]
    dnDCGs[user] <- inDCG[N]
    
    if (verbose) cat("user",user,"a-NDCG:",dnDCGs[user],"ab-NDCG:",nnDCGs[user],"ag-NDCG:",snDCGs[user],"abg-NDCG:",anDCGs[user],"\n")
    
  }
  
  abg_NDCG <- mean(anDCGs)
  ab_NDCG <- mean(nnDCGs)
  a_NDCG <- mean(dnDCGs)
  ag_NDCG <- mean(snDCGs)
  
  uniq_items <- names(which(table(recommendations) == 1))
  uniq_items_u <- NULL
  
  for (i in 1:length(uniq_items)) {
    uniq_items_u <- c(uniq_items_u, which(recommendations == uniq_items[i], arr.ind = T)[1])
  }
  
  a_TOT <- a_NDCG * (length(unique(uniq_items_u))/n_users)
  abg_TOT <- abg_NDCG * a_TOT
  Averages <- list(a_NDCG, ab_NDCG, ag_NDCG, abg_NDCG, a_TOT, abg_TOT)
  names(Averages) = c("a-NDCG","ab-NDCG","ag-NDCG","abg-NDCG","Tot-Div","abg-Tot-Div") 
  if (verbose) cat("a-NDCG:",a_NDCG,"ab-NDCG:",ab_NDCG,"ag-NDCG:",ag_NDCG,"abg-NDCG:",abg_NDCG,"Tot-Div:",a_TOT,"abg-Tot-Div:",abg_TOT)
  return(Averages)
}