setMethod("evaluate", signature(x = "evaluationScheme", method = "character"),
          function(x, method, nMatrix, type="topNList", subtype="topNList", n=1:10, parameter=NULL, 
                   progress = TRUE, keepModel=FALSE) {
            
            scheme <- x
            runs <- 1:scheme@k
            
            if(progress) cat(method, "run fold/sample [model time/prediction time]")
            
            cm <- list()
            for(r in runs) {
              if(progress) cat("\n\t",r, " ")
              
              cm[[r]] <- .do_run_by_n(scheme, method, nMatrix=nMatrix,
                                      run=r, type=type, subtype=subtype, n=n, parameter=parameter, 
                                      progress=progress, keepModel=keepModel)
            }
            
            if(progress) cat("\n")
            
            new("evaluationResults", results = cm, 
                method=recommenderRegistry$get_entry(method)$method)
          })

setMethod("evaluate", signature(x = "evaluationScheme", method = "list"),
          function(x, method, nMatrix, type="topNList", subtype="topNList", n=1:10, parameter=NULL, 
                   progress = TRUE, keepModel=FALSE) {
            
            ## method is a list of lists
            #list(RANDOM = list(name = "RANDOM", parameter = NULL), 
            #  POPULAR = list(...
            
            results <- lapply(method, FUN = function(a) try(evaluate(x, a$n,
                                                                     n = n , type=type, parameter = a$p)))	
            
            ## handle recommenders that have failed
            errs <- sapply(results, is, "try-error")
            if(any(errs)) {
              warning(paste("\n  Recommender '", names(results)[errs], 
                            "' has failed and has been removed from the results!", sep=''))
              results[errs] <- NULL
            }
            
            as(results, "evaluationResultList")
          })


## evaluation work horse
.do_run_by_n <- function(scheme, method, nMatrix, run, type, subtype, n, parameter = NULL,
                         progress=FALSE, keepModel=TRUE) {
  
  ## prepare data
  train <- getData(scheme, type="train", run=run)
  test_known <- getData(scheme, type="known", run=run)
  test_unknown <- getData(scheme, type="unknown", run=run)
  
  ## train recommender
  time_model <- system.time(
    r <- Recommender(train, method, parameter=parameter)
  )
  
  
  time_predict <- system.time(
    pre <- predict(r, test_known, n=max(n), type=type)
  )
  
  if(is(pre, "topNList")) {
    
    for(i in 1:length(n)) {
      NN <- n[i]
      
      ## get best N
      topN <- bestN(pre, NN)
      
      r <- calcPredictionAccuracy(
        topN, test_unknown, test_known, byUser=FALSE, given=scheme@given, goodRating=scheme@goodRating, 
        subtype=subtype, method=method, rank=NN, nMatrix=nMatrix
      )
      
      if(i==1) res <- rbind(r)
      else res <- rbind(res, r)
    }
    rownames(res) <- n
    
  }else{
    
    if(type == "ratings"){
      res <- calcPredictionAccuracy(pre, test_unknown, byUser=FALSE, rank=FALSE,
                                    given=scheme@given, goodRating=scheme@goodRating)        
      res <- rbind(res)
      
    }else{
      
      for(i in 1:length(n)) {
        NN <- n[i]
        
        r <-  calcPredictionAccuracy(pre, test_unknown, byUser=FALSE, rank=NN,
                                     given=scheme@given, goodRating=scheme@goodRating)
        if(i==1) res <- rbind(r)
        else res <- rbind(res, r)
      }
      rownames(res) <- n
    }
    
  }
  
  time_usage <- function(x) x[1]+x[2]
  
  if(progress) cat("[", 
                   time_usage(time_model), "sec/",
                   time_usage(time_predict),"sec] ", sep="")
  
  new("confusionMatrix", cm = res, model = 
        if(keepModel) getModel(r) else NULL)
}
