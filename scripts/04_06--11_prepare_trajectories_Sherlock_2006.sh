#!/bin/bash
#
#SBATCH --job-name=prepare_trajectories_20060419-20061231
#
#SBATCH --partition=serc
#SBATCH --nodes=1
#SBATCH --ntasks-per-node=1
#SBATCH --cpus-per-task=128
#SBATCH --time=1-00:00:00
#SBATCH --mem-per-cpu=7G
#SBATCH --output=prepare_trajectories_20060419-20061231.log
#SBATCH --mail-type=ALL
#SBATCH --no-requeue

# load modules
ml physics gdal udunits proj geos
ml R/4.0.2

# execute script
Rscript 11_prepare_trajectories_Sherlock.R 20060419 20061231
