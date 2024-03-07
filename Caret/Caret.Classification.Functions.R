train.test.split <- function(grouping.data , factor.grouping.variables , 
                             numeric.grouping.variables, train.percentage=0.75){
  
  grouping.data$group <- anticlust::anticlustering(x=grouping.data[,numeric.grouping.variables],
                                             K = round(1/(1-train.percentage),digits = 0),
                                             objective = "variance",
                                             categories = grouping.data[,factor.grouping.variables])
  grouping.data$Train <- T
  grouping.data$Train[grouping.data$group == 1] <- F
  
  grouping.data <- grouping.data[,!(names(grouping.data)=="group")]
  
  return(grouping.data)
}

##########################################################################
corr.based.FC <- function(feature.data,class.vect,cut.off=0.9){
  cor.f <- cor(feature.data,method = "pearson")
  diag(cor.f) <- 0
  high.cor.f <- apply(cor.f, 1, function(x){which(x > cut.off)})
  
  cor.c <- apply(feature.data, 2, function(x){return(cor(x,as.numeric(as.factor(class.vect)) , method = "spearman"))})
  
  rm.index <- vector(mode = "numeric")
  if(length(high.cor.f)>0)
    for (i in 1:length(high.cor.f)) {
      if(length(high.cor.f[[i]]) > 0){
        cor.c.1 <- data.frame(cor.c=abs(cor.c[c(i,high.cor.f[[i]])]),index=c(i,high.cor.f[[i]]))
        cor.c.1 <- cor.c.1[order(cor.c.1$cor.c , decreasing = T) , ]
        rm.index <- append(rm.index , values = cor.c.1$index[-1])
      }
    }
  return(unique(rm.index))
}

###########################################################################
t.test.score <- function(feature.data,class.vect){
  result <- data.frame(statistic=vector(mode = "numeric",length = ncol(feature.data)),p.value=vector(mode = "numeric",length = ncol(feature.data)))
  rownames(result) <- colnames(feature.data)
  for (i in 1:ncol(feature.data)) {
    t <- t.test(x ~ y , data = data.frame(x=feature.data[,i],y=class.vect))
    result$statistic[i] <- t$statistic
    result$p.value[i] <- t$p.value
  }
  return(result)
}
