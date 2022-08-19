#!/bin/bash
#
#SBATCH --job-name=griddingtemp-1
#
#SBATCH --partition=serc
#SBATCH --nodes=1
#SBATCH --ntasks-per-node=1
#SBATCH --cpus-per-task=80
#SBATCH --time=3-00:00:00
#SBATCH --mem-per-cpu=10G
#SBATCH --output=griddingtemp-1.log
#SBATCH --mail-type=ALL

# load modules
ml R/4.0.2
ml load physics gdal udunits proj geos
ml load devel netcdf

# execute script
Rscript 08_get_temperature_usingexactextractr_Sherlock.R
