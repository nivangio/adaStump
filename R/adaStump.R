adaStump <- function(formula, data, ...){
  
  args <- list(formula = formula, data = data, control = rpart.control(maxdepth=1,cp=-1,minsplit=0,xval=0), ...)
  
  model <- do.call("ada", args)
  
  ret <- createStumpFrame(model)
  ret <- list(model = ret, type = model$model$lossObj$type)
  class(ret) <- "adaStump"
  return(ret)
  
}