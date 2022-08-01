#!/bin/bash
#
#SBATCH --job-name=matching08
#
#SBATCH --partition=serc
#SBATCH --nodes=1
#SBATCH --ntasks-per-node=1
#SBATCH --cpus-per-task=60
#SBATCH --time=1-00:00:00
#SBATCH --mem-per-cpu=10G
#SBATCH --output=matching08.log
#SBATCH --mail-type=ALL

# load modules
ml R/3.5.1

# execute script
Rscript 16_match_trajectories_EPA_Sherlock.R
