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
#SBATCH --mail-user=jhoover@hpc.arizona.edu

module load R/4.4
#echo 'R_LIBS=~/R/library_R_v4.4/' >> ~/.Renviron

in_path="/home/u18/jhoover/arsenic/data/All_As_Data.csv"
out_path="/home/u18/jhoover/arsenic/data/"


Rscript /home/u18/jhoover/arsenic/scripts/RF/20250131_As_rRF_forHPC_V1.R $in_path $out_path
