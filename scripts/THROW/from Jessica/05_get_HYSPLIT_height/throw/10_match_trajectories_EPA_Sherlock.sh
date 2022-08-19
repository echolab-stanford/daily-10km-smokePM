#!/bin/bash
#
#SBATCH --job-name=matching02
#
#SBATCH --partition=serc
#SBATCH --nodes=1
#SBATCH --ntasks-per-node=1
#SBATCH --cpus-per-task=100
#SBATCH --time=3-00:00:00
#SBATCH --mem-per-cpu=9G
#SBATCH --output=matching02.log
#SBATCH --mail-type=ALL

# load modules
ml R/3.5.1

# execute script
Rscript 10_match_trajectories_EPA_Sherlock.R
