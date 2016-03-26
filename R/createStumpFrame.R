createStumpFrame <-
function(ada.model){
  
  if(class(ada.model) != "ada") stop("Not an ada object")
  
  trees <- ada.model$model$trees
  
  stumps.decomp <- sapply(trees, function(x){
    
    frame <- x$frame
    
    condition.var <- as.character(frame$var[1])
    
    #Get operation and condition of operation
    
    condition.value <- path.rpart(x,nodes = 2, print.it = F)
    condition.value <- condition.value[[1]][2]
    condition.value <- gsub(condition.var,"",condition.value)
    
    condition.operation <- regmatches(condition.value, regexpr("(<|>|=){1,2}",condition.value))
    
    condition.value <- gsub(condition.operation,"",condition.value)
    condition.value <- gsub(" ","",condition.value)
    
    switch(ada.model$model$lossObj$type,
    
      real = {
      probabilityT <- frame$yval2[2,5]
      probabilityF <- frame$yval2[3,5]},
      
      discrete = {
        values <- c(-1,1)
        probabilityT <- values[frame$yval[2]]
        probabilityF <- values[frame$yval[3]]
        },
      
      gentle = {
        probabilityT <- frame$yval[2]
        probabilityF <- frame$yval[3]
      }
      
        
    )
      
    
    
  
    names(probabilityT) <- c()
    names(probabilityF) <- c()
    
    default <- ifelse(frame$n[2] > frame$n[3],T,F)
    
    ret <- c(condition.var,condition.operation,condition.value,
             probabilityT,probabilityF, default)
    
    return(ret)
      
  })
  
  stumps.decomp <- t(stumps.decomp)
 
  alphas <-  ada.model$model$alpha
  
  stumps.decomp <- cbind(stumps.decomp, alphas)
  
  colnames(stumps.decomp) <- c("var","condition","value","probT","probF","default","alpha")
  
  stumps.decomp <- as.data.frame(stumps.decomp, stringsAsFactors = F)
  
  stumps.decomp$probT <- as.numeric(stumps.decomp$probT)
  stumps.decomp$probF <- as.numeric(stumps.decomp$probF)
  stumps.decomp$alpha <- as.numeric(stumps.decomp$alpha)
  
  stumps.decomp$condition[stumps.decomp$condition == "="] <- "%in%"
  
  return(stumps.decomp)
  
}
