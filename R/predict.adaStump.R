predict.adaStump <-
function(stump.model, newdata, n.iter = nrow(stump.model$model)){
  
  if(class(stump.model) != "adaStump") stop("Not an adaStump Model")
  
  if(nrow(stump.model$model) == 0) return(rep(NA,nrow(newdata)))
  
  cols <- unique(stump.model$model$var)
  
  newdata_reduced <- newdata[,cols, drop = F]
  rm(newdata)
  
  stumps.probs <- sapply(1:n.iter, function(x) {
    
    args.st <- as.list(stump.model$model[x,])
    args.st$newdata <- newdata_reduced
    
    args.st$value <- unlist(strsplit(args.st$value, split = ","))
    if(all(grepl("^[0-9\\.]+$",args.st$value))) args.st$value <- as.numeric(args.st$value)
    
    args.st$type <- stump.model$type
    
    ret <- do.call("getCoefficientLine",args.st)
    
    return(ret)
    
  })
  
  coefs <- apply(stumps.probs,1,sum)
  
  final.prob <- sapply(coefs, function(x){
    
    mod.coef <- ifelse(x > 0, exp(-2*x), exp(2*x))
    ret <- mod.coef/(1+mod.coef)
    return(ret)
    
  })
  
  return(final.prob)
  
}
