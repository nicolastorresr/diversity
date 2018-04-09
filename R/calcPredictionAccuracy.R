setMethod("calcPredictionAccuracy", signature(x= "realRatingMatrix",
                                              data = "realRatingMatrix"),
          
          function(x, data, rank=FALSE, byUser=FALSE, ...) {
            
            if(rank){
              
              resNDCG <- NDCG(x,data,rank)
              
              cat("",resNDCG,"")
              
              drop(cbind(resNDCG))
              
            } else {
              
              if(byUser) fun <- rowMeans
              else fun <- mean
              
              ## we use matrix to make sure NAs are accounted for correctly
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
          
          function(x, data, original=NULL, byUser=FALSE, given=NULL, goodRating=NA, t='topNList', method=NULL, rank=NULL, nMatrix, ...) {
            
            
            if(t=="Novelty"){

              cat("Content Novelty Top -",rank,":")

              real_mat <- as(original, "matrix")

              recomendaciones = matrix(NA,nrow = length(x@items),ncol = rank)

              for (i in 1:length(x@items)) {

                if (length(x@items[[i]])==rank)
                  recomendaciones[i,] = colnames(real_mat)[x@items[[i]]]
                else
                  recomendaciones[i,] = sample(NA, rank, replace=TRUE)

              }
             
              views <- list()

              for (pe in 1:nrow(real_mat)){
                views[[pe]] <- names(which(!is.na(real_mat[pe,])))
              }

              if (!file.exists(nMatrix)) stop("File topics does not exist.")
              topics <- read.table(nMatrix)

              l <- Content_Novelty(topics, recomendaciones, views, verbose = FALSE)
              
              cat("",l[[1]],l[[2]],l[[3]],l[[4]],l[[5]],l[[6]],l[[7]],"")

              drop(cbind(a_DCG=l[[1]], a_NDCG = l[[2]], ab_NDCG = l[[3]], ag_NDCG = l[[4]], abg_NDCG = l[[5]], TOT_DIV = l[[6]], abg_TOT_DIV = l[[7]]))                            
              
            }else{
              
              if(is.na(goodRating)) stop("You need to specify goodRating!")
              
              data <- binarize(data, goodRating)
              calcPredictionAccuracy(x, data, byUser, given,...)
              
            }
            
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
            
            res <- cbind(TP, FP, FN, TN, precision, recall, TPR, FPR)
            
            if(!byUser) res <- colMeans(res, na.rm=TRUE)
            
            res
          })
