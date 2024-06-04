#!/bin/bash
#SBATCH -A Research_Project1 # research project to submit under.
#SBATCH --export=ALL # export all environment variables to the batch job.
#SBATCH -D . # set working directory to .
#SBATCH -p mrcq
#SBATCH --time=50:00:00 # Maximum wall time for the job
#SBATCH --nodes=1 # specify number of nodes.
#SBATCH --ntasks-per-node=16 # specify number of processors.
#SBATCH --mail-type=END # send email at job completion
#SBATCH --mail-user=m.kouhsar@exeter.ac.uk # email address


data_feature_file=./Classification/Raw/FeatureMatrix.rds
data_class_file=./Classification/Raw/PhenotypeData.csv 
OutPrefix=./Classification/Caret.Out
class_column="Phenotype" 
grouping_columns_num="Age"
grouping_columns_fact="Sex"
scale=no
feature_selection="nzv,rfe"  ## feature.selection=nzv,rfe,cor,ttest
rfe_size="c(5,10,20,50)"     ## An R numeric vector in string type
saveModel=yes
models=rf
do_parallel=yes

ScriptDir=./Caret



Rscript ${ScriptDir}/Caret.Classification.R $data_feature_file $data_class_file $OutPrefix $class_column $grouping_columns_num $grouping_columns_fact $scale $feature_selection $rfe_size $saveModel $models $do_parallel 
