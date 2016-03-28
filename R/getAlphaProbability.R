#' Internal - Transform tree value and multiply with alpha according its type
#' @keywords internal
#' @export

getAlphaProbability <- function(tree.value, alpha, type){
  
  
  ret <- switch(type,
         
         real = {   
           
           #ind <- tree.value == 1 | (1 - tree.value) == 1
           tree.value[tree.value == 0] <- 1e-04
           tree.value[tree.value == 1] <- 0.9999
           
           # if (length(ind) >= 1) {
           #   tree.value[ind] = (1 - tree.value[ind]) * 1e-04 + tree.value[ind] * 0.9999
           # }
           
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