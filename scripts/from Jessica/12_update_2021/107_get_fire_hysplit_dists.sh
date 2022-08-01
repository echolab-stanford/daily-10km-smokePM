#!/bin/bash
#
#SBATCH --job-name=get_fire_hysplit_dists
#
#SBATCH --partition=serc
#SBATCH --nodes=1
#SBATCH --ntasks-per-node=1
#SBATCH --cpus-per-task=31
#SBATCH --time=1-00:00:00
#SBATCH --mem-per-cpu=20G
#SBATCH --output=get_fire_hysplit_dists.log
#SBATCH --mail-type=ALL

# load modules
ml physics gdal/2.2.1 udunits proj/4.9.3 geos 
ml R/4.0.2

# execute script
Rscript get_fire_hysplit_dists.R
