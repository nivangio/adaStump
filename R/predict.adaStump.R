#' Predict from an \code{adaStump} object 
#' 
#' 
#' @param stump.model object of class \code{adaStump}
#' @param newdata Data to be fit
#' @param n.iter Amount of stumps to consider in the prediction
#' 
#' @return 
#' A vector of length \code{nrow(newdata)} expressing the probability of ocurrence of the event modelled.
#' 
#' @examples
#' #Load Iris
#' data(iris)
#' 
#' #Create Variable is Iris as numerical
#' iris$isSetosa <- as.numeric(iris$Species == "setosa")
#' 
#' #Split sample in 70 train - 30 test
#' train.ind <- sample(nrow(iris), nrow(iris) * 0.7)
#' 
#' #Train model. For obvious reasons, Species variable is not included in the fit
#' fit <- adaStump(isSetosa ~ Sepal.Length + Sepal.Width + Petal.Length + Petal.Width, iris[train.ind,], 
#'                 type = "discrete", iter = 10, nu = 0.05, bag.frac = 0.6)
#' #Prediction
#' predict(fit,iris[-train.ind,])
#'
#' @export 

 
predict.adaStump <-
function(stump.model, newdata, n.iter = nrow(stump.model$model)){
  
  if(!class(stump.model) %in% c("adaStump","prunedadaStump")) stop("Not an adaStump or prunedadaStump Model")
  
  if(nrow(stump.model$model) == 0) return(rep(NA,nrow(newdata)))
  
  cols <- unique(stump.model$model$var)
  
  newdata_reduced <- newdata[,cols, drop = F]
  rm(newdata)
  
  stumps.probs <- sapply(1:n.iter, function(x) {
    
    args.st <- as.list(stump.model$model[x,])
    args.st$newdata <- newdata_reduced
    
    args.st$value <- unlist(strsplit(args.st$value, split = ","))
    if(all(grepl("^[0-9\\.]+$",args.st$value))) args.st$value <- as.double(args.st$value)
    
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
