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
OutPrefix="Out.sPLSDA"
class_column="Phenotype" 
grouping_columns_num="Age"
grouping_columns_fact="Sex"
saveModel=yes
TrainTesSplit=no
ncomp=25
dist_method="max.dist"   #max.dist, centroids.dist, mahalanobis.dist
n_fold=5
n_repeat=10

ScriptDir=./sPLSDA

Rscript ${ScriptDir}/sPLSDA.R $data_feature_file $data_class_file $OutPrefix $class_column $grouping_columns_num $grouping_columns_fact $saveModel $TrainTesSplit $ncomp $dist_method $n_fold $n_repeat 
