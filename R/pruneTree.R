pruneTree <- function(stump.model){
  
  if(class(stump.model) != "adaStump") stop("Not an adaStump object")
  
  model.frame <- stump.model$model
  
  model.frame_splitted <- split(model.frame, model.frame[,c("var","condition","value")], drop = T)
  
  reduced.frame <- lapply(model.frame_splitted, function(x){
    
  
    probtrue <- apply(x,1, function(y) getAlphaProbability(as.numeric(y["probT"]),as.numeric(y["alpha"]),stump.model$type))
    probtrue <- sum(probtrue)
    
    probfalse <- apply(x,1, function(y) getAlphaProbability(as.numeric(y["probF"]),as.numeric(y["alpha"]),stump.model$type))
    probfalse <- sum(probfalse)
    
    
    default <- as.logical(names(which.max(table(x$default))))
    
    ret <- unique(x[,c("var","condition","value")])
    ret$probT <- probtrue
    ret$probF <- probfalse
    ret$default <- default
    
    return(ret)
    
  })
  
  reduced.frame <- do.call("rbind",reduced.frame)
  
  row.names(reduced.frame) <- c()
  
  ret <- list(model = reduced.frame, type = stump.model$type)
  class(ret) <- "prunedadaStump"
  return(ret)
  
}