
Bernoulli <- function(N,k,p) {
  return(choose(N,k)*p^k*(1-p)^(N-k))
}

Probability <- function(nuggets){
  p <- matrix(data = 0, nrow = ncol(nuggets), ncol = 1)
  for (i in 1:ncol(nuggets)) {
    p[i] = sum(nuggets[,i])/nrow(nuggets)
  }
  rownames(p) <- colnames(nuggets)
  return(p)
}

BinomDiv <- function(nuggets,R) {
  
  cov <- NULL
  non <- NULL
  bin <- NULL
  
  N <- ncol(R)
  n_users <- nrow(R)
  p <- Probability(nuggets)
  G <- colnames(nuggets)
  
  for (user in 1:n_users) {
    
    G_R <- NULL
    
    k_g_R <- colSums(nuggets[R[user,],],na.rm = TRUE)
    for (g in 1:length(k_g_R)){
      if (k_g_R[[g]] > 0){
        for (gg in 1:k_g_R[[g]]){
          G_R <- c(G_R, names(k_g_R[g]))
        }
      }
    }
    
    G_NR <- setdiff(G, G_R)
    
    Coverage = 1
    for (i in 1:length(G_NR)){
      Coverage = Coverage * Bernoulli(N,0,p[G_NR[i],][[1]])^(1/length(G))
    }
    
    g_r <- unique(G_R)
    
    NonRed = 1
    for (i in 1:length(g_r)){
      k_g = length(which(G_R == g_r[i]))
      sumat = 0
      if (k_g > 1){
        for (l in 1:(k_g-1)){
          sumat = sumat + Bernoulli(N,l,p[g_r[i],][[1]])/(1-Bernoulli(N,0,p[g_r[i],][[1]]))
        }  
      }
      NonRed = NonRed * (1 - sumat)^(1/length(g_r))
    }
    if (is.na(NonRed)) NonRed = 1
    cov[user] <- signif(Coverage,4)
    non[user] <- signif(NonRed,4)
    bin[user] <- signif(Coverage*NonRed,4)
  }
  
  res <- list(mean(cov),mean(non),mean(bin))
  names(res) <- c('Coverage','NonRed','BinomDiv')
  #cat(res)
  return(res)
}