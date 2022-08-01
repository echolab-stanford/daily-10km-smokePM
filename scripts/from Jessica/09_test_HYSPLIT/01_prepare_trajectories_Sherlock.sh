#!/bin/bash
#
#SBATCH --job-name=prepare_trajectories
#
#SBATCH --partition=serc
#SBATCH --nodes=1
#SBATCH --ntasks-per-node=1
#SBATCH --cpus-per-task=128
#SBATCH --time=21:00:00
#SBATCH --mem-per-cpu=7G
#SBATCH --output=prepare_trajectories_8.log
#SBATCH --mail-type=ALL

# load modules
ml physics gdal udunits proj geos
ml R/4.0.2

# execute script
Rscript 01_prepare_trajectories_Sherlock.R
