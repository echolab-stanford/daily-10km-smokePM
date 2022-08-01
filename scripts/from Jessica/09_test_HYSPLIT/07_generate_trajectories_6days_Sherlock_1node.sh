#!/bin/bash
#
#SBATCH --job-name=run2005_12-2010_05-missing
#
#SBATCH --partition=serc
#SBATCH --nodes=1
#SBATCH --ntasks-per-node=1
#SBATCH --cpus-per-task=128
#SBATCH --time=12:00:00
#SBATCH --mem-per-cpu=4G
#SBATCH --output=run2005_12-2010_05-missing.log
#SBATCH --mail-type=ALL

# load modules
ml R/3.5.1

# execute script
Rscript 07_generate_trajectories_6days_Sherlock_1node.R
