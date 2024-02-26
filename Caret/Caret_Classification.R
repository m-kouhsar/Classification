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

data.feature.file=args[1]
data.class.file=args[2]
class.column=args[3]
grouping.columns.num=args[4]
grouping.columns.fact=args[5]
scale=args[6]
feature.selection=ifelse(trimws(tolower(args[7]),which = "both")=="yes",T,F)
method=args[8]

######################## Reading Inputs ############################################
data.feature <- fread(file = data.feature.file , stringsAsFactors = F , header = T)
rownames(data.feature) <- data.feature[,1]
data.feature <- data.feature[,-1]

data.class <- read.csv(file=data.class.file , stringsAsFactors = F , header = T , row.names = 1)
if(!identical(rownames(data.class),colnames(data.feature)))
  stop("Row names in class data must be equal to colnames in feature data")

data.feature <- as.data.frame(t(data.feature))
######################## Scaling ###################################################
if(scale){
  
  data.feature <- as.data.frame(scale(data.feature))
}

######################## Feature Selection #########################################
if(feature.selection){
  
  nzv <- nearZeroVar(data.feature, saveMetrics= TRUE)
  data.feature <- data.feature[,!nzv$nzv]
  
  rm.f <- corr.based.FC(feature.data = data.feature , class.vect = data.class[,class.column] , cut.off = 0.9)
  data.feature <- data.feature[,-rm.f]  
  
}

######################## Train-Test splitting ######################################
grouping.columns.num <- str_trim(str_split_1(grouping.columns.num , pattern = ","),side = "both")
grouping.columns.fact <- str_trim(str_split_1(grouping.columns.fact , pattern = ","),side = "both")

data.class <- train.test.split(grouping.data = data.class , factor.grouping.variables = c(grouping.columns.fact , class.column),
                               numeric.grouping.variables = grouping.columns.num,train.percentage = 0.75)
data.train <- cbind.data.frame(data.feature[data.class$Train ,],Class=data.class[data.class$Train , class.column])
data.test <- cbind.data.frame(data.feature[!data.class$Train ,],Class=data.class[!data.class$Train , class.column])
######################## Training ##################################################
model.all <-  getModelInfo()
model.label <- model.all[[method]]$label

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

Grid <- model.all[[method]]$grid(x = data.train[,-ncol(data.train)],y = data.train[,ncol(data.train)],len = 10)

fit <- train(Class ~ ., data = data.train, 
             method = method, 
             trControl = fitControl,
             #metric="ROC",
             tuneGrid = Grid,
             verbose = F
             )

ggplot(fit) + ggtitle(paste(model.label,"- parameters tuning"))+ theme_bw()

folds <- unique(fit$pred$Resample)
train.roc.obj <- vector(mode = "list", length = length(folds))
train.roc.labels <- vector(mode = "character", length = length(folds))
names(train.roc.obj) <- folds
for(i in 1:length(folds)){
  train.roc.obj[[i]] <- roc(response=fit$pred$obs[fit$pred$Resample==folds[i]],predictor=fit$pred[,4][fit$pred$Resample==folds[i]])
  train.roc.labels[i] <- paste0(folds[i]," (AUC=",round(train.roc.obj[[i]]$auc,digits = 2),")")
}

ggroc(train.roc.obj) + ggtitle(paste(model.label,"accuracy on train data")) + labs(color = "Fold") +
                       scale_color_discrete(labels=train.roc.labels)+ theme_bw()

####################### Testing ####################################################
pred <- predict(fit , newdata = data.test , type = "prob")
pred$Predict <- predict(fit , newdata = data.test , type = "raw")
pred$Reference <- data.test$Class

obj.roc <- roc(response=pred$Reference,predictor=pred[,1])
ggroc(obj.roc)+ggtitle(paste(model.label,"accuracy on test data")) +
  theme_bw() + annotate("text",x = 0.1,y = 0.1,label=paste0("AUC=",round(obj.roc$auc,digits = 2)))

cat("Performance summary on test data:")
confusionMatrix(data =as.factor(pred$Predict) , reference = as.factor(pred$Reference))






