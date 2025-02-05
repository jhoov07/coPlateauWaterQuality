#!/bin/bash

#SBATCH --job-name=as_RF_CV10
#SBATCH -o %x.%X.out
#SBATCH -e %x.err 
#SBATCH --nodes=1
#SBATCH --ntasks=36
#SBATCH --mem=180gb
#SBATCH --time=24:00:00
#SBATCH --partition=standard
#SBATCH --account=hoover
#SBATCH --mail-type=BEGIN,END
#SBATCH --mail-user= aaronnuanez@hpc.arizona.edu

module load R/4.4
#echo 'R_LIBS=~/R/library_R_v4.4/' >> ~/.Renviron

in_path="/home/u29/aaronnuanez/arsenic/data/All_As_Data.csv"
out_path="/home/u29/aaronnuanez/arsenic/data/"


Rscript /home/u29/aaronnuanez/arsenic/scripts/20250131_As_rRF_forHPC_V1_5ugL.R $in_path $out_path
