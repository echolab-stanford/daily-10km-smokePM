#!/bin/bash
#
#SBATCH --job-name=run20060419-20201231
#
#SBATCH --partition=serc
#SBATCH --nodes=1
#SBATCH --ntasks-per-node=1
#SBATCH --cpus-per-task=73
#SBATCH --time=7-00:00:00
#SBATCH --mem-per-cpu=4G
#SBATCH --output=run20060419-20201231.log
#SBATCH --mail-type=ALL

# load modules
ml R/3.5.1

# execute script
Rscript 10_generate_trajectories_6days_Sherlock_1node.R
