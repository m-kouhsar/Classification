#!/bin/bash
#SBATCH -A Research_Project-MRC164847 # research project to submit under.
#SBATCH --export=ALL # export all environment variables to the batch job.
#SBATCH -D . # set working directory to .
#SBATCH -p mrcq
#SBATCH --time=50:00:00 # Maximum wall time for the job
#SBATCH --nodes=1 # specify number of nodes.
#SBATCH --ntasks-per-node=16 # specify number of processors.
#SBATCH --mail-type=END # send email at job completion
#SBATCH --mail-user=m.kouhsar@exeter.ac.uk # email address


data_feature_file=/lustre/projects/Research_Project-191391/Morteza/Classification/Raw/NorCog.miRNA.LogCPM.rds
data_class_file=/lustre/projects/Research_Project-191391/Morteza/Classification/Raw/NorCog.Pheno.csv 
OutPrefix=/lustre/projects/Research_Project-191391/Morteza/Classification/NorCog.miRNA
class_column="Phenotype" 
grouping_columns_num="Age"
grouping_columns_fact="Sex"
scale=no
feature_selection="nzv,rfe"
rfe_size="c(5,10,20,50)"
saveModel=yes
models=rf
do_parallel=yes

ScriptDir=/lustre/projects/Research_Project-191391/Morteza/Classification/Scripts



Rscript ${ScriptDir}/Caret.Classification.R $data_feature_file $data_class_file $OutPrefix $class_column $grouping_columns_num $grouping_columns_fact $scale $feature_selection $rfe_size $saveModel $models $do_parallel 
