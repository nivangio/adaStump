adaStump <-
function(formula, data, type, iter){
  
  args <- list(formula = formula,data = data,type = type,iter = iter, control = rpart.control(maxdepth=1,cp=-1,minsplit=0,xval=0))
  
  model <- do.call("ada", args)
  
  ret <- createStumpFrame(model)
  ret <- list(model = ret, type = type)
  class(ret) <- "adaStump"
  return(ret)
  
}
