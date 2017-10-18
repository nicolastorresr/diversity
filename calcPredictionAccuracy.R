setMethod("calcPredictionAccuracy", signature(x= "realRatingMatrix", 
                                              data = "realRatingMatrix"),
          
          function(x, data, rank=FALSE, byUser=FALSE, goodRating=NA, ...) {
            
            if(rank){
              # top_NN : List of n_test_users with top-N recommended items (could be smaller than N)
              top_NN <- getTopNLists(x, n = rank, minRating = 0)
              
              real_mat <- as(data, "matrix")
              
              # In real_mat for each user we must to retrieve all viewed items
              views <- list()
              
              for (pe in 1:nrow(real_mat)){
                views[[pe]] <- names(which(!is.na(real_mat[pe,])))
              }
              
              # top_NN@items : IDs of N-Best items for each user
              # top_NN@ratings : ratings of N-Best items for each user
              
              reco = matrix(NA,nrow = length(top_NN@items),ncol = rank)
              
              for (i in 1:length(top_NN@items)) {
                reco[i,] = colnames(real_mat)[top_NN@items[[i]]]
              }
              
              # Nuggets Matrix is hard-coded !
              if (!file.exists("Nuggets_ML100K.dat")) stop("File of nuggets does not exist.")
              nuggets <- read.table("Nuggets_ML100K.dat")
              
              l <- Content_Novelty(nuggets, reco, views, verbose = FALSE)
              
              drop(cbind(a_NDCG = l[[1]], ab_NDCG = l[[2]], ag_NDCG = l[[3]], abg_NDCG = l[[4]], TOT = l[[5]], abg_TOT = l[[6]]))
              
            }else{
              
              if(byUser) fun <- rowMeans
              else fun <- mean
              
              MAE <- fun(abs(as(x, "matrix") - as(data,"matrix")), 
                         na.rm=TRUE)
              MSE <- fun((as(x, "matrix") - as(data,"matrix"))^2, 
                         na.rm=TRUE)
              RMSE <- sqrt(MSE)
              drop(cbind(RMSE, MSE, MAE))
              
            }
                                        
            
          })

setMethod("calcPredictionAccuracy", signature(x= "topNList", 
                                              data = "realRatingMatrix"),
          
          function(x, data, byUser=FALSE, given=NULL, goodRating=NA, ...) {
            if(is.na(goodRating)) stop("You need to specify goodRating!")
                      
            data <- binarize(data, goodRating)
            calcPredictionAccuracy(x, data, byUser, given,...)
          })

setMethod("calcPredictionAccuracy", signature(x= "topNList", 
                                              data = "binaryRatingMatrix"),
          
          function(x, data, byUser=FALSE, given=NULL, ...) {
            if(is.null(given)) stop("You need to specify how many items were given for the prediction!")
            
            TP <- rowSums(as(x, "ngCMatrix") * as(data, "ngCMatrix"))
            TP_FN <- rowCounts(data)
            TP_FP <- rowCounts(x)
            FP <- TP_FP - TP
            FN <- TP_FN - TP
            TN <-  ncol(data) - given - TP - FP - FN
            
            ## calculate some important measures
            precision <- TP / (TP + FP) 
            recall <- TP / (TP + FN) 
            TPR <- recall 
            FPR <- FP / (FP + TN)                                              
                                  
            res <- cbind(TP, FP, FN, TN, precision, recall, FPR)
            
            if(!byUser) res <- colMeans(res, na.rm=TRUE)
            
            res
          })