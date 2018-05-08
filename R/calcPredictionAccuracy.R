setMethod("calcPredictionAccuracy", signature(x= "realRatingMatrix",
                                              data = "realRatingMatrix"),
          
          function(x, data, rank=FALSE, byUser=FALSE, ...) {
              
            if(byUser) fun <- rowMeans
            else fun <- mean
            
            ## we use matrix to make sure NAs are accounted for correctly
            MAE <- fun(abs(as(x, "matrix") - as(data,"matrix")),
                       na.rm=TRUE)
            MSE <- fun((as(x, "matrix") - as(data,"matrix"))^2,
                       na.rm=TRUE)
            RMSE <- sqrt(MSE)
            
            drop(cbind(RMSE, MSE, MAE))
            
          })

setMethod("calcPredictionAccuracy", signature(x= "topNList",
                                              data = "realRatingMatrix"),
          
          function(x, data, rawData=NULL, byUser=FALSE, given=NULL, goodRating=NA, subtype="topNList", method=NULL, rank=NULL, nMatrix, ...) {
            
            if(subtype=="a-nDCG"){
              
              cat("a-nDCG Measures (top-",rank,"): ", sep='')
              
              realMatrix <- as(rawData, "matrix")
              
              R = matrix(NA,nrow = length(x@items),ncol = rank)
              
              for (i in 1:length(x@items)) {
                
                if (length(x@items[[i]])==rank)
                  R[i,] = colnames(realMatrix)[x@items[[i]]]
                else
                  R[i,] = sample(NA, rank, replace=TRUE)
                
              }
              
              reviews <- list()
              
              for (pe in 1:nrow(realMatrix)){
                reviews[[pe]] <- names(which(!is.na(realMatrix[pe,])))
              }
              
              if (!file.exists(nMatrix)) stop("File nuggets does not exist.")
              nuggets <- read.table(nMatrix)
              
              l <- alpha_Measures(nuggets, R, reviews, verbose = FALSE)
              
              cat("",l[[1]],l[[2]],l[[3]],l[[4]],l[[5]],l[[6]],l[[7]],"")
              
              drop(cbind(a_DCG=l[[1]], a_NDCG = l[[2]], ab_NDCG = l[[3]], ag_NDCG = l[[4]], abg_NDCG = l[[5]], TotDiv = l[[6]], abg_TotDiv = l[[7]]))                            
              
            } else if(subtype=="BinomDiv"){
              
              cat("Binomial Diversity (top-",rank,"): ",sep='')
              
              realMatrix <- as(rawData, "matrix")
              
              R = matrix(NA,nrow = length(x@items),ncol = rank)
              
              for (i in 1:length(x@items)) {
                
                if (length(x@items[[i]])==rank)
                  R[i,] = colnames(realMatrix)[x@items[[i]]]
                else
                  R[i,] = sample(NA, rank, replace=TRUE)
                
              }
              
              if (!file.exists(nMatrix)) stop("File nuggets does not exist.")
              nuggets <- read.table(nMatrix)
              
              l <- BinomDiv(nuggets, R)
              
              cat("",l[[1]],l[[2]],l[[3]],"")
              
              drop(cbind(Coverage=l[[1]], NonRed = l[[2]], BinomDiv = l[[3]]))                            
              
            } else {
              
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
