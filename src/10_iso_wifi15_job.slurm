#!/bin/bash
#SBATCH -p bii
#SBATCH -A biocomplexity
#SBATCH --job-name=isochrones_teja
#SBATCH --time=1-10:00:00
#SBATCH -c 2
#SBATCH --mem=250G
module load goolf R/4.0.0
Rscript /home/tp2sk/Git/extension/src/10_iso_wifi.R