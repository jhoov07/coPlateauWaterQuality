#!/bin/bash

#SBATCH --job-name=asXGB10
#SBATCH -o %x_%A_%a.out
#SBATCH -e %J-%x.err 
#SBATCH --nodes=1
#SBATCH --ntasks=94
#SBATCH --mem=470gb
#SBATCH --time=24:00:00
#SBATCH --partition=standard
#SBATCH --account=hoover
#SBATCH --mail-type=BEGIN,END
#SBATCH --mail-user=jhoover@hpc.arizona.edu

module load R/4.4
#echo 'R_LIBS=~/R/library_R_v4.4/' >> ~/.Renviron

n_path="/home/u18/jhoover/arsenic/data/All_As_Data.csv"
out_path="/home/u18/jhoover/arsenic/data/"

#in_path="/home/u29/aaronnuanez/arsenic/data/Cleaned_As_GIS_Filtered.csv"
#out_pathDir="/home/u29/aaronnuanez/arsenic/data/"
#out_name="/home/u29/aaronnuanez/arsenic/data/xgb_as_model"

Rscript /home/u18/jhoover/arsenic/scripts/RF/20250623_as_XGBforHPC_as10ugL.R $in_path $out_path
#Rscript /home/u29/aaronnuanez/arsenic/scripts/as_XGBforHPC.R $in_path $out_pathDir
