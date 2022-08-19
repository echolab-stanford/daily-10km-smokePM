#!/bin/bash
#
#SBATCH --job-name=aod_run1
#
#SBATCH --partition=serc
#SBATCH --nodes=1
#SBATCH --ntasks-per-node=1
#SBATCH --cpus-per-task=128
#SBATCH --time=7-00:00:00
#SBATCH --mem-per-cpu=4G
#SBATCH --output=aod_run1.log
#SBATCH --mail-type=ALL

# load modules
ml R/3.5.1

# execute script
Rscript 13_regress_random_forest_AOD_Sherlock.R
