#!/bin/bash
#SBATCH -p batch
#SBATCH -t 22:00:00
#SBATCH --constraint=[ivb|hsw]
#SBATCH -n 1
#SBATCH --mem=3500
#SBATCH -o create_figures.out
module load r/3.4.3-python-2.7.14
export R_LIBS=$WRKDIR/R/$EBVERSIONR
echo $R_LIBS
srun Rscript --vanilla create_figures.R
