#!/bin/bash
#SBATCH -p debug
#SBATCH -t 00:15:00
#SBATCH --constraint=[ivb|hsw]
#SBATCH -n 1
#SBATCH --mem=2500
#SBATCH -o post/post.out
module load r/3.4.3-python-2.7.14
export R_LIBS=$WRKDIR/R/$EBVERSIONR
echo $R_LIBS
srun Rscript --vanilla 02_post.R
