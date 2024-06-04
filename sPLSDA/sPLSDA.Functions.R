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
  x=as.matrix(x)
  len <- length(x)
  if(N>len){
    warning('N greater than length(x).  Setting N=length(x)')
    N <- length(x)
  }
  
  out = unique(sort(x,decreasing = T))[N]
  
  if(arr.ind){
    return(which(x==out,arr.ind = arr.ind))
  }else{
    return(out)
  }
  
}

##################################################################################
minN <- function(x, N=2, arr.ind = F){
  x=as.matrix(x)
  len <- length(x)
  if(N>len){
    warning('N greater than length(x).  Setting N=length(x)')
    N <- length(x)
  }
  
  out = unique(sort(x,decreasing = F))[N]
  
  if(arr.ind){
    return(which(x==out,arr.ind = arr.ind))
  }else{
    return(out)
  }
}
