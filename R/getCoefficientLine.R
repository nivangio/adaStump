getCoefficientLine <-
function(var,condition,value,probT,probF,alpha,newdata,default,type){
  
  meets.condition <- do.call(condition, list(newdata[[var]],value))
  meets.condition[is.na(newdata[[var]])] <- default
  tree.value <- ifelse(meets.condition, probT,probF)
  
  
  switch(type,
  
      real = {   
        
        ind <- tree.value == 1 | (1 - tree.value) == 1
        if (length(ind) > 1) {
          tree.value[ind] = (1 - tree.value[ind]) * 1e-04 + tree.value[ind] * 0.9999
        }
        
      modified.value <- 0.5 * log(tree.value/(1-tree.value))
      ret <- modified.value * alpha},
      
      discrete = {
        ret <- tree.value * alpha
        
      },
      
      gentle = {
        ret <- tree.value * alpha
        
      }
      
      
  
  )
  
  return(ret)
  
}
