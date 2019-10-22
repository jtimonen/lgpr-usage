#!/bin/bash
#SBATCH -p short
#SBATCH -t 01:00:00
#SBATCH --constraint=[ivb|hsw]
#SBATCH -n 1
#SBATCH --mem=2000
#SBATCH -o postproc.out
module load r/3.4.3-python-2.7.14
export R_LIBS=$WRKDIR/R/$EBVERSIONR
echo $R_LIBS
srun Rscript --vanilla postproc.R
