train.test.split <- function(grouping.data , factor.grouping.variables , 
                             numeric.grouping.variables, train.percentage=0.75){
  
  grouping.data <- minDiff::create_groups(grouping.data,criteria_scale = numeric.grouping.variables,
                criteria_nominal = factor.grouping.variables, sets_n = round(1/(1-train.percentage),digits = 0),
                equalize=list(mean, sd),exact=F,talk = T)
  grouping.data$Train <- T
  grouping.data$Train[grouping.data$newSet == 1] <- F
  
  grouping.data <- grouping.data[,!(names(grouping.data)=="newSet")]
  
  return(grouping.data)
}

##########################################################################
corr.based.FC <- function(feature.data,class.vect,cut.off=0.9){
  cor.f <- cor(feature.data,method = "pearson")
  diag(cor.f) <- 0
  high.cor.f <- apply(cor.f, 1, function(x){which(x > cut.off)})
  
  cor.c <- apply(feature.data, 2, function(x){return(cor(x,as.numeric(as.factor(class.vect)) , method = "spearman"))})
  
  rm.index <- vector(mode = "numeric")
  for (i in 1:length(high.cor.f)) {
    if(length(high.cor.f[[i]]) > 0){
      cor.c.1 <- data.frame(cor.c=abs(cor.c[c(i,high.cor.f[[i]])]),index=c(i,high.cor.f[[i]]))
      cor.c.1 <- cor.c.1[order(cor.c.1$cor.c , decreasing = T) , ]
      rm.index <- append(rm.index , values = cor.c.1$index[-1])
    }
  }
  return(unique(rm.index))
}

