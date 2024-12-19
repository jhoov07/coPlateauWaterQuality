#!/bin/bash

#SBATCH --job-name=LTE10_g1
#SBATCH -o %J-%x.out
#SBATCH -e %J-%x.err 
#SBATCH --nodes=1
#SBATCH --ntasks=2
#SBATCH --mem=10gb
#SBATCH --time=24:00:00
#SBATCH --partition=standard
#SBATCH --account=hoover
#SBATCH --mail-type=BEGIN,END
#SBATCH --mail-user=aaronnuanez@arizona.edu

module load R/4.4
#echo 'R_LIBS=~/R/library_R_v4.4/' >> ~/.Renviron

#in_path="/home/u29/aaronnuanez/arsenic/data/All_As_Data.csv"
#out_pathDir="/home/u29/aaronnuanez/arsenic/data/"

Rscript /home/u29/aaronnuanez/arsenic/scripts/as_XGBforHPC_SecondaryTuning.R $in_path $out_pathDir "g0" 0 1

#Rscript /home/u29/aaronnuanez/arsenic/scripts/as_XGBforHPC_SecondaryTuning.R $in_path $out_pathDir "g1" 1 1
#Rscript /home/u29/aaronnuanez/arsenic/scripts/as_XGBforHPC_SecondaryTuning.R $in_path $out_pathDir "g2" 2 1
#Rscript /home/u29/aaronnuanez/arsenic/scripts/as_XGBforHPC_SecondaryTuning.R $in_path $out_pathDir "g3" 3 1
#Rscript /home/u29/aaronnuanez/arsenic/scripts/as_XGBforHPC_SecondaryTuning.R $in_path $out_pathDir "g4" 4 1
#Rscript /home/u29/aaronnuanez/arsenic/scripts/as_XGBforHPC_SecondaryTuning.R $in_path $out_pathDir "g5" 5 1


#$in_path 
#$out_pathDir 
#tune_var = "a2"
#gamma = 0

#min_child_weight = 1
#eta = 0.005
#max_depth = 6 
#nrounds = 750
#subsample = 0.5 
#colsample_bytree = 0.75
