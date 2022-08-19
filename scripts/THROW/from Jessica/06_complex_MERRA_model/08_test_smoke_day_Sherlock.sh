#!/bin/bash
#
#SBATCH --job-name=sample_test_run1
#
#SBATCH --partition=serc
#SBATCH --nodes=1
#SBATCH --ntasks-per-node=1
#SBATCH --cpus-per-task=120
#SBATCH --time=6-00:00:00
#SBATCH --mem-per-cpu=5G
#SBATCH --array=1-10
#SBATCH --output=sample_test_run1.log
#SBATCH --mail-type=ALL

# load modules
ml R/3.5.1

# execute script
Rscript 08_test_smoke_day_Sherlock.R
