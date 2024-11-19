#!/bin/bash

#SBATCH --job-name=as_RF
#SBATCH -o %x.%X.out
#SBATCH -e %x.err 
#SBATCH --nodes=1
#SBATCH --ntasks=36
#SBATCH --mem=180gb
#SBATCH --time=12:00:00
#SBATCH --partition=standard
#SBATCH --account=hoover
#SBATCH --mail-type=BEGIN,END
#SBATCH --mail-user=jhoover@hpc.arizona.edu

module load R/4.4
#echo 'R_LIBS=~/R/library_R_v4.0/' >> ~/.Renviron

in_path="/home/u18/jhoover/arsenic/data/2_20240724_randomForest_As_dataClean.csv"
out_path="/home/u18/jhoover/arsenic/data/"


Rscript /home/u18/jhoover/arsenic/scripts/as_randomForest.R $in_path $out_path
