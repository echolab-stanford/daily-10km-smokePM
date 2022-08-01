#!/bin/bash
#
#SBATCH --job-name=run06
#
#SBATCH --partition=serc
#SBATCH --nodes=1
#SBATCH --ntasks-per-node=1
#SBATCH --cpus-per-task=70
#SBATCH --time=12:00:00
#SBATCH --mem-per-cpu=10G
#SBATCH --output=run06.log
#SBATCH --mail-type=ALL

# load modules
ml R/3.5.1

# execute script
Rscript 04_generate_trajectories_72hr_Sherlock_1node.R