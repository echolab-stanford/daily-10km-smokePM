#!/bin/bash
#
#SBATCH --job-name=run11
#
#SBATCH --partition=serc
#SBATCH --nodes=1
#SBATCH --ntasks-per-node=1
#SBATCH --cpus-per-task=24
#SBATCH --time=2-00:00:00
#SBATCH --mem-per-cpu=6G
#SBATCH --array=1-10
#SBATCH --output=run11.log
#SBATCH --mail-type=ALL

# load modules
ml R/3.5.1

# execute script
Rscript 04_generate_trajectories_6wk_Sherlock_multinode.R
