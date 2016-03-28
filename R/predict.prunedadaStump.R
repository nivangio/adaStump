predict.prunedadaStump <- function(stump.model, newdata){
  
  ret <- predict.adaStump(stump.model,newdata)
  
  return(ret)
  
}