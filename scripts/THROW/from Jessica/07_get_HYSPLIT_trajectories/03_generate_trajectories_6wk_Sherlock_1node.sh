#!/bin/bash
#
#SBATCH --job-name=rerun02
#
#SBATCH --partition=serc
#SBATCH --nodes=1
#SBATCH --ntasks-per-node=1
#SBATCH --cpus-per-task=50
#SBATCH --time=12:00:00
#SBATCH --mem-per-cpu=4G
#SBATCH --output=rerun02.log
#SBATCH --mail-type=ALL

# load modules
ml R/3.5.1

# execute script
Rscript 03_generate_trajectories_6wk_Sherlock_1node.R
