
library(caret)
library(minDiff)
library(dplyr)
library(data.table)
options(datatable.fread.datatable = F)
library(stringr)
library(ggplot2)
library(pROC)

source("Scripts/Caret.Classification.Functions.R")
args <- commandArgs(T)

data.feature.file= args[1] #"Raw/NorCog.TotalRNA.logCPM.Scaled0.csv"
data.class.file= args[2] #"Raw/NorCog.Pheno.csv"
OutPrefix = args[3] #"NorCog.TotalRNA"
class.column=args[4] #"Phenotype"
grouping.columns.num=args[5] #"Age"
grouping.columns.fact=args[6] #"Sex"
scale=ifelse(tolower(trimws(args[7]))=="yes",T,F) #F
feature.selection=ifelse(tolower(trimws(args[8]))=="yes",T,F) #T
saveModel=ifelse(tolower(trimws(args[7]))=="yes",T,F)
models= str_split_1(trimws(args[9]),pattern = ",") #"adaboost,awtan,bartMachine,bayesglm,earth,extraTrees,gaussprLinear,gbm,glm,glmnet,gpls,kknn,LMT,lssvmLinear,lssvmPoly,lssvmRadial,rf,RRF,svmBoundrangeString,svmExpoString,svmLinear,svmPoly,svmRadial"

######################## Reading Inputs ############################################
sink(paste0(OutPrefix,".log"))

cat("Reading inputs...")
cat("\n")
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
  data.feature <- data.feature[,-rm.f]  
  
}

######################## Train-Test splitting ######################################
cat("Train-Test splitting...")
cat("\n")
grouping.columns.num <- str_trim(str_split_1(grouping.columns.num , pattern = ","),side = "both")
grouping.columns.fact <- str_trim(str_split_1(grouping.columns.fact , pattern = ","),side = "both")

data.class <- train.test.split(grouping.data = data.class , factor.grouping.variables = c(grouping.columns.fact , class.column),
                               numeric.grouping.variables = grouping.columns.num,train.percentage = 0.75)
data.train <- cbind.data.frame(data.feature[data.class$Train ,],Class=data.class[data.class$Train , class.column])
data.test <- cbind.data.frame(data.feature[!data.class$Train ,],Class=data.class[!data.class$Train , class.column])

model.all <-  getModelInfo()
for(model in models){
  ######################## Training #################################################
  model.label <- model.all[[model]]$label
  cat(paste0("Train ",model," (",model.label,") ","on train data..."))
  cat("\n")
  
  #caret.models <- modelLookup()
  #caret.models$label.model = NA
  #for (i in 1:nrow(caret.models)) {
  #  caret.models$label.model[i] = model.all[[caret.models$model[i]]]$label
  #}
  #caret.models <- caret.models[,c(1,7,2,3,4,5,6)]
  #write.csv(caret.models,file = "Raw/Caret.Models.csv", row.names = F)
  
  set.seed(1234)
  fitControl <- trainControl(
    #method = "repeatedcv",
    method = "cv",
    number = 5, 
    #repeats = 3,
    classProbs = T,
    savePredictions = T,
    #summaryFunction = twoClassSummary,
    selectionFunction = tolerance)
  
  Grid <- model.all[[model]]$grid(x = data.train[,-ncol(data.train)],y = data.train[,ncol(data.train)],len = 10)
  
  fit <- train(Class ~ ., data = data.train, 
               method = model, 
               trControl = fitControl,
               #metric="ROC",
               tuneGrid = Grid,
               verbose = F
               )
  
  if(saveModel)
    saveRDS(fit,file = paste0(OutPrefix,".",model,".rds"))
  cat("Plot training results...")
  cat("\n")
  tiff(filename = paste(OutPrefix,".",model,".parameters.tuning.tiff"), units = "in", height = 10,width = 10,res = 300)
  ggplot(fit) + ggtitle(paste(model.label,"- parameters tuning"))+ theme_bw()
  graphics.off()
  folds <- unique(fit$pred$Resample)
  train.roc.obj <- vector(mode = "list", length = length(folds))
  train.roc.labels <- vector(mode = "character", length = length(folds))
  names(train.roc.obj) <- folds
  for(i in 1:length(folds)){
    train.roc.obj[[i]] <- roc(response=fit$pred$obs[fit$pred$Resample==folds[i]],predictor=fit$pred[,4][fit$pred$Resample==folds[i]])
    train.roc.labels[i] <- paste0(folds[i]," (AUC=",round(train.roc.obj[[i]]$auc,digits = 2),")")
  }
  tiff(filename = paste(OutPrefix,".",model,".accuracy.on.train.data.tiff"), units = "in", height = 10,width = 10,res = 300)
  ggroc(train.roc.obj) + ggtitle(paste(model.label,"accuracy on train data")) + labs(color = "Fold") +
                         scale_color_discrete(labels=train.roc.labels)+ theme_bw()
  graphics.off()
  imp.var <- varImp(fit ,scale=T,useModel=F)
  tiff(filename = paste(OutPrefix,".",model,".variable.importance.tiff"), units = "in", height = 10,width = 10,res = 300)
  plot(imp.var, top = 20)
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
  tiff(filename = paste(OutPrefix,".",model,"accuracy.on.test.data.tiff"), units = "in", height = 10,width = 10,res = 300)
  ggroc(obj.roc)+ggtitle(paste(model.label,"accuracy on test data")) +
    theme_bw() + annotate("text",x = 0.1,y = 0.1,label=paste0("AUC=",round(obj.roc$auc,digits = 2)))
  graphics.off()
  cat("Performance summary on test data:")
  confusionMatrix(data =as.factor(pred$Predict) , reference = as.factor(pred$Reference))
}
sink()
