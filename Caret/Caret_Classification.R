
library(caret)
library(anticlust)
library(data.table)
options(datatable.fread.datatable = F)
library(stringr)
library(ggplot2)
library(pROC)
library("funr")

dirScript <- dirname(sys.script())
source(paste0(dirScript,"/Caret.Classification.Functions.R"))
args <- commandArgs(T)

data.feature.file= args[1] 
data.class.file= args[2] 
OutPrefix = args[3] 
class.column=args[4] 
grouping.columns.num=args[5] 
grouping.columns.fact=args[6] 
scale=args[7] 
feature.selection=args[8]
saveModel=args[9]
models= args[10]
Do.Parellel=args[11]

######################### Input Arguments ###########################################
cat("Feature data file: ",data.feature.file,"\n")
cat("Class labels file: ",data.class.file,"\n")
cat("Output files prefix: ",OutPrefix,"\n")
cat("Class variable: ",class.column,"\n")
cat("Numeric variables for train-test split: ",grouping.columns.num,"\n") 
cat("Categorical variables for train-test split: ",grouping.columns.fact,"\n")
cat("Do you want to scale data? ",scale,"\n")
cat("Do you want to run feature selection? ",feature.selection,"\n") 
cat("Do you want to save trained model? ",saveModel,"\n")
cat("Models: ",models,"\n")
cat("Do you want to run training in parallel?",Do.Parellel,"\n")
######################## Reading Inputs ############################################
cat("\n")
cat("Reading inputs...")
cat("\n")

scale=ifelse(tolower(trimws(scale))=="yes",T,F) 
feature.selection=ifelse(tolower(trimws(feature.selection))=="yes",T,F) 
saveModel=ifelse(tolower(trimws(saveModel))=="yes",T,F)
models= str_split(trimws(models),pattern = ",",simplify = T)[1,]
Do.Parellel=ifelse(tolower(trimws(Do.Parellel))=="yes",T,F)

data.feature <- fread(file = data.feature.file , stringsAsFactors = F , header = T)
rownames(data.feature) <- data.feature[,1]
data.feature <- data.feature[,-1]

data.class <- read.csv(file=data.class.file , stringsAsFactors = F , header = T , row.names = 1)
if(!identical(rownames(data.class),colnames(data.feature)))
  stop("Row names in class data must be equal to colnames in feature data")

data.feature <- as.data.frame(t(data.feature))
######################## Scaling ###################################################
if(scale){
  cat("Scalling feature data...")
  cat("\n")
  data.feature <- as.data.frame(scale(data.feature))
}

######################## Feature Selection #########################################
if(feature.selection){
  cat("Feature selection...")
  cat("\n")
  nzv <- nearZeroVar(data.feature, saveMetrics= TRUE)
  data.feature <- data.feature[,!nzv$nzv]
  
  rm.f <- corr.based.FC(feature.data = data.feature , class.vect = data.class[,class.column] , cut.off = 0.9)
  if(length(rm.f)>0)
    data.feature <- data.feature[,-rm.f]  
  
}

######################## Train-Test splitting ######################################
cat("Train-Test splitting...")
cat("\n")
grouping.columns.num <- str_trim(str_split(grouping.columns.num , pattern = ",",simplify = T)[1,],side = "both")
grouping.columns.fact <- str_trim(str_split(grouping.columns.fact , pattern = ",",simplify = T)[1,],side = "both")

data.class <- train.test.split(grouping.data = data.class , factor.grouping.variables = c(grouping.columns.fact , class.column),
                               numeric.grouping.variables = grouping.columns.num,train.percentage = 0.75)
data.train <- cbind.data.frame(data.feature[data.class$Train ,],Class=as.factor(data.class[data.class$Train , class.column]))
data.test <- cbind.data.frame(data.feature[!data.class$Train ,],Class=as.factor(data.class[!data.class$Train , class.column]))

model.all <-  getModelInfo()
for(i in 1:length(models)){
    model <- models[i]
    ######################## Training #################################################
    model.label <- model.all[[model]]$label
    
    cat(paste0("Train ",model," (",model.label,") ","model on train data..."))
    cat("\n")
  
  set.seed(1234)
  fitControl <- trainControl(
    method = "cv",
    number = 5, 
    classProbs = T,
    savePredictions = T,
    selectionFunction = best,
    allowParallel = Do.Parellel,
    verboseIter = F)
  
  tryCatch(expr = {
    
    if(Do.Parellel){
      library(doParallel)
      cores <- detectCores()
      cl <- makePSOCKcluster(cores)
      registerDoParallel(cl)
    }
    
    Grid <- model.all[[model]]$grid(x = data.train[,-ncol(data.train)],y = data.train[,ncol(data.train)],len = 10)
    fit <- train(Class ~ ., data = data.train, 
                 method = model, 
                 trControl = fitControl,
                 tuneGrid = Grid
                 )
    
    if(Do.Parellel)
      stopCluster(cl)
    
    if(saveModel)
      saveRDS(fit,file = paste0(OutPrefix,".",model,".rds"))
    cat("Plot training results...")
    cat("\n")
    if(nrow(Grid)>1){
      p <- ggplot(fit) + ggtitle(paste(model.label,"- parameters tuning"))+ theme_bw()
      tiff(filename = paste0(OutPrefix,".",model,".parameters.tuning.tiff"),type="cairo", units = "in", height = 10,width = 10,res = 300)
      print(p)
      graphics.off()
    }
    
    fit.pred <- fit$pred[!is.na(fit$pred$pred),]
    folds <- unique(fit.pred$Resample)
    train.roc.obj <- vector(mode = "list", length = length(folds))
    train.roc.labels <- vector(mode = "character", length = length(folds))
    names(train.roc.obj) <- folds
    
    for(j in 1:length(folds)){
      train.roc.obj[[j]] <- roc(response=fit.pred$obs[fit.pred$Resample==folds[j]],predictor=fit.pred[,4][fit.pred$Resample==folds[j]])
      train.roc.labels[j] <- paste0(folds[j]," (AUC=",round(train.roc.obj[[j]]$auc,digits = 2),")")
    }
    
    p <- ggroc(train.roc.obj) + ggtitle(paste(model.label,"accuracy on train data")) + labs(color = "Fold") +
      scale_color_discrete(labels=train.roc.labels)+ theme_bw()
    
    tiff(filename = paste0(OutPrefix,".",model,".accuracy.on.train.data.tiff"),type="cairo", units = "in", height = 10,width = 10,res = 300)
    print(p)
    graphics.off()
    
    imp.var <- varImp(fit ,scale=T,useModel=F)
    p <- plot(imp.var, top = 20)
    
    tiff(filename = paste0(OutPrefix,".",model,".variable.importance.tiff"),type="cairo", units = "in", height = 10,width = 10,res = 300)
    print(p)
    graphics.off()
    ####################### Testing ####################################################
    cat(paste0("Applying the ",model," model on test data..."))
    cat("\n")
    pred <- predict(fit , newdata = data.test , type = "prob")
    pred$Predict <- predict(fit , newdata = data.test , type = "raw")
    pred$Reference <- data.test$Class
    
    cat("Plot test results...")
    cat("\n")
    obj.roc <- roc(response=pred$Reference,predictor=pred[,1])
    
    p <- ggroc(obj.roc)+ggtitle(paste(model.label,"accuracy on test data")) +
      theme_bw() + annotate("text",x = 0.1,y = 0.1,label=paste0("AUC=",round(obj.roc$auc,digits = 2)))
    
    tiff(filename = paste0(OutPrefix,".",model,".accuracy.on.test.data.tiff"),type="cairo", units = "in", height = 10,width = 10,res = 300)
    print(p)
    graphics.off()
    
    cat("Save results summary...")
    cat("\n")
    results.summ <- confusionMatrix(data =as.factor(pred$Predict) , reference = as.factor(pred$Reference))
    sink(paste0(OutPrefix,".",model,".txt"))
    cat(model,"(",model.label,")",sep = "",":\n")
    print(results.summ)
    sink()
  },
  error=function(e){
    print(e)
    })
  cat("#########################################################################")
  cat("\n")
}
