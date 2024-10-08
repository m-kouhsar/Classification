train.test.split <- function(grouping.data , class.variable, factor.grouping.variables , 
                             numeric.grouping.variables, train.percentage=0.75,use.anticlust=T){
  if(train.percentage==1){
    grouping.data$Train <- T
  }else if(train.percentage==0){
    grouping.data$Train <- F
  }else if(use.anticlust){
    
    grouping.data$group <- anticlust::anticlustering(x=grouping.data[,numeric.grouping.variables],
                                                     K = round(1/(1-train.percentage),digits = 0),
                                                     objective = "variance",
                                                     categories = grouping.data[,c(class.variable,factor.grouping.variables)])
    grouping.data$Train <- T
    grouping.data$Train[grouping.data$group == 1] <- F
    
    grouping.data <- grouping.data[,!(names(grouping.data)=="group")]
    
    
  }else{
    grouping.data$Train <- F
    t <- caret::createDataPartition(y = grouping.data[,class.variable],times = 1,p = train.percentage)[[1]]
    grouping.data$Train[t] <- T
  }
  
  
  return(grouping.data)
}

###################################################################################
maxN <- function(x, N=2, arr.ind = F){
  x = as.matrix(x)
  y=unique(sort(x,decreasing = T))
  len <- length(y)
  if(N>len){
    warning('N out of bounds.  Setting N=length(unique(x))')
    N <- length(y)
  }
  
  if(arr.ind){
    return(which(x==y[N],arr.ind = arr.ind))
  }else{
    return(y[N])
  }
}

##################################################################################
minN <- function(x, N=2, arr.ind = F){
  x = as.matrix(x)
  y=unique(sort(x,decreasing = F))
  len <- length(y)
  if(N>len){
    warning('N out of bounds.  Setting N=length(unique(x))')
    N <- length(y)
  }
  
  if(arr.ind){
    return(which(x==y[N],arr.ind = arr.ind))
  }else{
    return(y[N])
  }
}
