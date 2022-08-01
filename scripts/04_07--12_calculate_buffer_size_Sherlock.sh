#!/bin/bash
#
#SBATCH --job-name=calculate_buffer_size
#
#SBATCH --partition=serc
#SBATCH --nodes=1
#SBATCH --ntasks-per-node=1
#SBATCH --cpus-per-task=1
#SBATCH --time=00:30:00
#SBATCH --mem-per-cpu=100G
#SBATCH --output=calculate_buffer_size.log
#SBATCH --mail-type=ALL
#SBATCH --no-requeue

# load modules
ml R/4.0.2

# execute script
Rscript 12_calculate_buffer_size_Sherlock.R
