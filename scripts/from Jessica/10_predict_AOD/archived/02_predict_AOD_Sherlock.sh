#!/bin/bash
#
#SBATCH --job-name=predict_AOD
#
#SBATCH --partition=serc
#SBATCH --nodes=1
#SBATCH --ntasks-per-node=1
#SBATCH --cpus-per-task=33
#SBATCH --time=1-00:00:00
#SBATCH --mem-per-cpu=20G
#SBATCH --output=predict_AOD_5.log
#SBATCH --mail-type=ALL

# load modules
ml physics gdal udunits proj geos
ml R/4.0.2

# execute script
Rscript 02_predict_AOD_Sherlock.R
