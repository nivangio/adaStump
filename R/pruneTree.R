#' Merge stumps with repeated conditions
#' 
#' In order to reduce execution times, stumps with the same condition are collapsed. 
#' If a pruned model is used, n.iter can no longer be specified and all the stumps are used.
#' 
#' @param stump.model Object of class "adaStump" to be reduced.
#' 
#' @return
#' 
#' An item of class \emph{prunedadaStump} containing the following:
#' 
#' \item{model }{a data.frame describing the pruned stumps} 
#' \item{type }{Type of ada execution performed.}
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
#'                 
#' #Prune Tree and predict
#' 
#' fit_pruned <- pruneTree(fit)
#' predict(fit_pruned,iris[-train.ind,])
#' @export

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