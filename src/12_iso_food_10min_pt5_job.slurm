#!/bin/bash
#SBATCH -p bii
#SBATCH -A biocomplexity
#SBATCH --job-name=isochrones_teja
#SBATCH --time=1-10:00:00
#SBATCH -c 2
#SBATCH --mem=250G

module load goolf R
module load gcc/7.1.0
module load openmpi/3.1.4
module load proj/4.9.2
module load gdal/2.4.0-py36

Rscript /home/tp2sk/Git/extension/src/12_iso_foodretail.R