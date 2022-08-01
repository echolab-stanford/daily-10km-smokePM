#!/bin/bash
#
#SBATCH --job-name=get_HYSPLIT_over_grid
#
#SBATCH --partition=normal
#SBATCH --nodes=1
#SBATCH --ntasks-per-node=1
#SBATCH --cpus-per-task=32
#SBATCH --time=1-00:00:00
#SBATCH --mem-per-cpu=7G
#SBATCH --output=get_HYSPLIT_over_grid.log
#SBATCH --mail-type=ALL
#SBATCH --no-requeue

# load modules
ml physics gdal udunits proj geos
ml R/4.0.2

# execute script
Rscript 15_get_HYSPLIT_over_grid_Sherlock.R
