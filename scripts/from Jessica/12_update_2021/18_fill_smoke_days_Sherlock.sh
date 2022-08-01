#!/bin/bash
#
#SBATCH --job-name=fill_smoke_days
#
#SBATCH --partition=normal
#SBATCH --nodes=1
#SBATCH --ntasks-per-node=1
#SBATCH --cpus-per-task=32
#SBATCH --time=00:20:00
#SBATCH --mem-per-cpu=7G
#SBATCH --output=fill_smoke_days.log
#SBATCH --mail-type=ALL
#SBATCH --no-requeue

# load modules
ml R/4.0.2

# execute script
Rscript 19_fill_smoke_days_Sherlock.R 50
