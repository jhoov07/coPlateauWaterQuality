#!/bin/bash

#SBATCH --job-name=asXGB_LTE
#SBATCH -o %J-%x.out
#SBATCH -e %J-%x.err 
#SBATCH --nodes=1
#SBATCH --ntasks=94
#SBATCH --mem=470gb
#SBATCH --time=24:00:00
#SBATCH --partition=standard
#SBATCH --account=hoover
#SBATCH --mail-type=BEGIN,END
#SBATCH --mail-user=aaronnuanez@hpc.arizona.edu

module load R/4.4
#echo 'R_LIBS=~/R/library_R_v4.4/' >> ~/.Renviron

in_path="/home/u29/aaronnuanez/arsenic/data/Cleaned_As_GIS_Filtered.csv"
out_pathDir="/home/u29/aaronnuanez/arsenic/data/"
#out_name="/home/u29/aaronnuanez/arsenic/data/xgb_as_model"

alpha = 1
labmda = 0
gamma = 0
max_delta_step = 0
min_child_weight = 1

eta = 0.005  #Shrinkage Step size shrinkage used in update to prevent overfitting. After each boosting step, we can directly get the weights of new features, and eta shrinks the feature weights to make the boosting process more conservative.
max_depth = 6 #Maximum depth of a tree. Increasing this value will make the model more complex and more likely to overfit. 0 indicates no limit on depth. 
nrounds = 750
subsample = 0.5 #Subsample ratio of the training instances. Setting it to 0.5 means that XGBoost would randomly sample half of the training data prior to growing trees and this will prevent overfitting. 
colsample_bytree = 0.75


Rscript /home/u29/aaronnuanez/arsenic/scripts/as_XGBforHPC.R $in_path $out_pathDir
