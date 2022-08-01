#!/bin/bash
#
#SBATCH --job-name=matching10
#
#SBATCH --partition=serc
#SBATCH --nodes=1
#SBATCH --ntasks-per-node=1
#SBATCH --cpus-per-task=80
#SBATCH --time=0-00:20:00
#SBATCH --mem-per-cpu=10G
#SBATCH --output=matching10.log
#SBATCH --mail-type=ALL

# load modules
ml R/3.5.1

# execute script
Rscript 12_match_trajectories_EPA_Sherlock.R
