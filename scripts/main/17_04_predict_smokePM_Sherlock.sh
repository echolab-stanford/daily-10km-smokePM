#!/bin/bash
#
#SBATCH --job-name=predict_smokePM
#
#SBATCH --partition=serc
#SBATCH --nodes=1
#SBATCH --ntasks-per-node=1
#SBATCH --cpus-per-task=30
#SBATCH --time=1-06:00:00
#SBATCH --mem-per-cpu=16G
#SBATCH --output=logs/predict_smokePM.log
#SBATCH --mail-type=ALL

# load modules
ml physics gdal/2.2.1 udunits proj/4.9.3 geos 
ml R/4.0.2

# execute script
Rscript scripts/main/17_04_predict_smokePM_Sherlock.R
