#!/bin/bash

#SBATCH --job-name=asXGB_LTE10
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
tune_var = "a2"
alpha = 1
lambda = 0
gamma = 0
max_delta_step = 0
min_child_weight = 1
eta = 0.005
max_depth = 6 
nrounds = 750
subsample = 0.5 
colsample_bytree = 0.75

Rscript /home/u29/aaronnuanez/arsenic/scripts/as_XGBforHPC_SecondaryTuning.R $in_path $out_pathDir $tune_var $alpha $lambda $gamma $max_delta_step $min_child_weight $eta $max_depth $nrounds $subsample $colsample_bytree
