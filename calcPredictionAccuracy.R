setMethod("calcPredictionAccuracy", signature(x= "realRatingMatrix", 
                                              data = "realRatingMatrix"),
          
          function(x, data, rank=FALSE, byUser=FALSE, goodRating=NA, ...) {
            
            if(rank){
              #Coverage <- coverage(x, data, rank)
              #drop(cbind(Coverage))
              ## ADD: Normalized Discounted Cumulated Gain (NDCG)
              #NDCG <- NDCG(x, data, rank)  
              ## ADD: alpha Normalized Discounted Cumulated Gain (aNDCG)
              #l <- aNDCG(x, data, rank)
              ## ADD: Coverage, Diversity, Novelty, Serendipity, Utility
              #l <- abNDCG(x, data, rank)
              #print(rank)
              
              #top_NN : List de n_test_users con los N items recomendados (puede ser menor a N)
              top_NN <- getTopNLists(x, n = rank, minRating = 0)
              
              real_mat <- as(data, "matrix")
              #predicted_mat <<- as(x, "matrix")
              
              #En real_mat para cada usuario se debe recuperar las peliculas que vio
              views <- list()
              
              for (pe in 1:nrow(real_mat)){
                views[[pe]] <- names(which(!is.na(real_mat[pe,])))
              }
              
              
              # top_NN@items[[1]] : las ID's de los N mejores items del usuario 1
              # top_NN@ratings[[1]] : los ratings de los N mejores items del usuario 1
              
              recomendaciones = matrix(NA,nrow = length(top_NN@items),ncol = rank)
              
              for (i in 1:length(top_NN@items)) {
                #recomendaciones[i,] = top_NN@items[[i]]
                recomendaciones[i,] = colnames(real_mat)[top_NN@items[[i]]]
                #top_recommended[reco,] <- topics[which(rownames(topics) == recommendations[user,reco]),]
              }
             
              #stop("alto.")
              #rec <- recomendaciones
              if (!file.exists("MT.dat")) stop("File topics does not exist.")
              topics <- read.table("MT.dat")
              
              l <- utility(topics, recomendaciones, views, verbose = FALSE)
              
              #best_NN <<- bestN(top_NN, rank)
              
              #stop("stop")
              
              drop(cbind(a_NDCG = l[[1]], ab_NDCG = l[[2]], ag_NDCG = l[[3]], abg_NDCG = l[[4]], a_TOT = l[[5]], Utility = l[[6]]))
              #drop(cbind(Coverage=l[[1]],NDCG,aDCG=l[[2]],anDCG=l[[3]],abDCG=l[[4]],abnDCG=l[[5]]))
              #drop(cbind(Coverage=l[[1]],NDCG,Diversity=l[[2]],Novelty=l[[3]],Serendipity=l[[4]],Utility=l[[5]]))
              #drop(cbind(NDCG,aDCG=l[[1]],anDCG=l[[2]]))
              
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