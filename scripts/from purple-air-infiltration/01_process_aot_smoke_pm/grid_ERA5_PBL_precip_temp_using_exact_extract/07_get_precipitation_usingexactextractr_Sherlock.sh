#!/bin/bash
#
#SBATCH --job-name=griddingprecip-1
#
#SBATCH --partition=serc
#SBATCH --nodes=1
#SBATCH --ntasks-per-node=1
#SBATCH --cpus-per-task=80
#SBATCH --time=3-00:00:00
#SBATCH --mem-per-cpu=10G
#SBATCH --output=griddingprecip-1.log
#SBATCH --mail-type=ALL

# load modules
ml R/4.0.2
ml load physics gdal udunits proj geos
ml load devel netcdf

# execute script
Rscript 07_get_precipitation_usingexactextractr_Sherlock.R
