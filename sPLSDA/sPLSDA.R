
##############################################################################
# This script is writen using a case study explained in the following URL:
# http://mixomics.org/case-studies/splsda-srbct-case-study/

##############################################################################

library(mixOmics)
library("funr")
library(stringr)
library(parallel)

dirScript <- dirname(sys.script())
source(paste0(dirScript,"/sPLSDA.Functions.R"))

args <- commandArgs(T)

data.feature.file= args[1] 
data.class.file= args[2] 
OutPrefix = args[3] 
class.column=args[4] 
grouping.columns.num=args[5] 
grouping.columns.fact=args[6] 
save.final.model=args[7]
data.split = args[8]
n.comp = as.numeric(trimws(args[9]))
dist.method =trimws(args[10])   #max.dist, centroids.dist, mahalanobis.dist
n.fold = as.numeric(args[11])
n.repeat = as.numeric(args[12])

######################### Input Arguments ###########################################
cat("Feature data file: ",data.feature.file,"\n")
cat("Class labels file: ",data.class.file,"\n")
cat("Output files prefix: ",OutPrefix,"\n")
cat("Class variable: ",class.column,"\n")
cat("Numeric variables for train-test split: ",grouping.columns.num,"\n") 
cat("Categorical variables for train-test split: ",grouping.columns.fact,"\n")
cat("Do you want to save final model? ",ifelse(tolower(trimws(save.final.model))=="yes","Yes","No"),"\n")
cat("Do you want to split data into train and test? ",ifelse(tolower(trimws(data.split))=="yes","Yes","No"),"\n")
cat("Number of component in initial model: ",n.comp,"\n") 
cat("distance method: ",dist.method,"\n") 
cat("Number of folds in tuning: ",n.fold,"\n") 
cat("Number of repeats in tuning: ",n.repeat,"\n") 
#####################################################################################
set.seed(12345)

cat("\n")
cat("Reading inputs...")
cat("\n")
save.final.model <- ifelse(tolower(trimws(save.final.model))=="yes",T , F)
data.split <- ifelse(tolower(trimws(data.split))=="yes",T , F)

data.feature <- readRDS(file = data.feature.file)
data.class <- read.csv(file=data.class.file , stringsAsFactors = F , header = T , row.names = 1)

if(!identical(rownames(data.class),rownames(data.feature)))
  stop("Row names in class data must be equal to rownames in feature data")

######################## Train-Test splitting ######################################
if(data.split){
  cat("Train-Test splitting...")
  cat("\n")
  grouping.columns.num <- str_trim(str_split(grouping.columns.num , pattern = ",",simplify = T)[1,],side = "both")
  grouping.columns.fact <- str_trim(str_split(grouping.columns.fact , pattern = ",",simplify = T)[1,],side = "both")
  
  data.class <- train.test.split(grouping.data = data.class , class.variable = class.column,factor.grouping.variables = grouping.columns.fact ,
                                 numeric.grouping.variables = grouping.columns.num,train.percentage = 0.8,use.anticlust=T)
  
  X.train <- data.matrix(data.feature[data.class$Train ,])
  Y.train=as.factor(data.class[data.class$Train , class.column])
  X.test <- data.matrix(data.feature[!data.class$Train ,])
  Y.test=as.factor(data.class[!data.class$Train , class.column])
}else{
  X.train <- data.matrix(data.feature)
  Y.train=as.factor(data.class[ , class.column])
}

###################### Viewing the data #####################################################
cat("looking at train data ...")
cat("\n")

pca.features <- pca(X.train, ncomp = n.comp, center = TRUE, scale = TRUE)

pdf(file =paste0(OutPrefix,".sPLSDA.RawPCA.pdf"), height = 8, width = 8)
plot(pca.features)

plotIndiv(pca.features, comp = c(1,2), 
          group = Y.train, 
          ind.names = F, 
          legend = TRUE, title = 'Train data PCA plot', legend.title = class.column)
graphics.off()

initial.model <- mixOmics::splsda(X.train, Y.train, ncomp = n.comp)

background = background.predict(initial.model, comp.predicted=2, dist = "max.dist")

pdf(file =paste0(OutPrefix,".sPLSDA.InitialModelPCA.pdf"), height = 8, width = 8)

plotIndiv(initial.model , comp = 1:2, 
          group = Y.train, ind.names = FALSE,  
          ellipse = TRUE, 
          legend = TRUE, title = 'sPLS-DA with confidence ellipses on train data')

plotIndiv(initial.model, comp = 1:2,
          group = Y.train, ind.names = FALSE, 
          background = background, 
          legend = TRUE, title = "sPLS-DA with prediction background on train data")
graphics.off()

###################### Parameter tuning #####################################################
cat("Parameter tuning...")
cat("\n")

ncpu <- detectCores()

cat("Running perf function to find optimal number of comp...\n")
perf.initial.model <- perf(initial.model, validation = "Mfold", 
                          folds = n.fold, nrepeat = n.repeat, 
                          dist = dist.method,
                          progressBar = FALSE, auc = TRUE, cpus =ncpu-1) 

optimal.ncomp <- minN(perf.initial.model$error.rate$BER,N = 1 , arr.ind = T)[1,1]
if(optimal.ncomp==1){
  optimal.ncomp <- minN(perf.initial.model$error.rate$BER,N = 2 , arr.ind = T)[1,1]
}
cat("Optimal number of components in perf function: ",optimal.ncomp, "\n")

list.keepX <- c(1:10,  seq(20, 500, 10))

cat("Running tune function to find optimal keepx...\n")
tune.initial.model <- tune.splsda(X.train, Y.train, ncomp = optimal.ncomp, 
                                 validation = 'Mfold',
                                 folds = n.fold, nrepeat = n.repeat, 
                                 dist = dist.method, 
                                 measure = "BER", 
                                 test.keepX = list.keepX,
                                 cpus =ncpu-1 ) 

pdf(file =paste0(OutPrefix,".Tuning.pdf"), height = 8, width = 12)

plot(perf.initial.model, sd = TRUE,legend.position = "horizontal")

plot(tune.initial.model) 
graphics.off()

optimal.ncomp <- minN(tune.initial.model$error.rate,N = 1 , arr.ind = T)[1,2]
if(optimal.ncomp==1){
  optimal.ncomp <- minN(tune.initial.model$error.rate,N = 2 , arr.ind = T)[1,2]
}
optimal.keepX <- tune.initial.model$choice.keepX[1:optimal.ncomp]
cat("Final optimal ncomp: ",optimal.ncomp,"\n")
cat("Optimal keepx: ",optimal.keepX,"\n")

###################### Generating final model #####################################################
cat("Generating final model...")
cat("\n")
final.model <- mixOmics::splsda(X.train, Y.train, 
                       ncomp = optimal.ncomp, 
                       keepX = optimal.keepX)
if(save.final.model){
  save(final.model, X.train, Y.train, perf.initial.model, tune.initial.model,file = paste0(OutPrefix,".FinalModel.rdat"))
}
pdf(file =paste0(OutPrefix,".FinalModelPCA.pdf"), height = 8, width = 8)
plotIndiv(final.model, comp = c(1,2), 
          group = Y.train, ind.names = FALSE, 
          ellipse = TRUE, legend = TRUE, 
          title = 'sPLS-DA optimal mpodel on train data, comp 1 & 2')
if(optimal.ncomp > 2){
  plotIndiv(final.model, comp = c(1,3), 
            group = Y.train, ind.names = FALSE,  
            ellipse = TRUE, legend = TRUE, 
            title = 'sPLS-DA optimal mpodel on train data, comp 1 & 3')
  
  plotIndiv(final.model, comp = c(2,3), 
            group = Y.train, ind.names = FALSE,  
            ellipse = TRUE, legend = TRUE, 
            title = 'sPLS-DA optimal mpodel on train data, comp 2 & 3')
}
graphics.off()

tiff(filename = paste0(OutPrefix,".ROC.Train.tif"),height = 5,width = 8, units = "in",res = 300)
auc = auroc(final.model , print = F , plot = T)
auc[[optimal.ncomp+1]] + ggtitle("Final model ROC on train data")
graphics.off()

###################### Performance on test data #####################################################
if(data.split){
  cat("Performance on test data...")
  cat("\n")
  
  tiff(filename = paste0(OutPrefix,".ROC.Test.tif"),height = 5,width = 8, units = "in",res = 300)
  auc = auroc(final.model , print = F,newdata = X.test,outcome.test = Y.test)
  auc[[optimal.ncomp+1]] + ggtitle("Final model ROC on test data")
  graphics.off()
}

cat("All done!\n")
