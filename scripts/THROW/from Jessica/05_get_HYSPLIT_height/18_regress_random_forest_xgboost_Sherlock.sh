#!/bin/bash
#
#SBATCH --job-name=HYSPLIT_xgboost
#
#SBATCH --partition=serc
#SBATCH --nodes=1
#SBATCH --ntasks-per-node=1
#SBATCH --cpus-per-task=128
#SBATCH --time=2-00:00:00
#SBATCH --mem-per-cpu=5G
#SBATCH --output=HYSPLIT_xgboost_1.log
#SBATCH --mail-type=ALL

# load modules
ml R/3.5.1

# execute script
Rscript 18_regress_random_forest_xgboost_Sherlock.R
