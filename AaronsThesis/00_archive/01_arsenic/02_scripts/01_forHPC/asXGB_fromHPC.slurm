#!/bin/bash

#SBATCH --job-name=as_XGB_3CatCV10
#SBATCH -o %J-%x.out
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
#echo 'R_LIBS=~/R/lx86_64-pc-linux-gnu-library/4.4' >> ~/.Renviron

in_path="/home/u18/jhoover/arsenic/data/Cleaned_As_GIS_Filtered.csv"
out_pathDir="/home/u18/jhoover/arsenic/data/"
#out_name="/home/u18/jhoover/arsenic/data/xgb_as_model"

Rscript /home/u18/jhoover/arsenic/scripts/as_XGB_10ugL.R $in_path $out_pathDir
