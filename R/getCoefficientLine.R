getCoefficientLine <-
function(var,condition,value,probT,probF,alpha=NULL,newdata,default,type){
  
  meets.condition <- do.call(condition, list(newdata[[var]],value))
  meets.condition[is.na(newdata[[var]])] <- default
  tree.value <- ifelse(meets.condition, probT,probF)
  
  if(is.null(alpha)) return(tree.value)
  
  ret <- getAlphaProbability(tree.value, alpha, type)
  
  return(ret)
  
}
