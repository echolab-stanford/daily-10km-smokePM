#!/bin/bash
#
#SBATCH --job-name=run2015_08
#
#SBATCH --partition=serc
#SBATCH --nodes=1
#SBATCH --ntasks-per-node=1
#SBATCH --cpus-per-task=128
#SBATCH --time=5-00:00:00
#SBATCH --mem-per-cpu=7G
#SBATCH --output=run2015_08.log
#SBATCH --mail-type=ALL

# load modules
ml R/3.5.1

# execute script
Rscript 03_generate_trajectories_6wk_Sherlock_1node.R 2015 8
