#!/bin/bash
#SBATCH -p batch
#SBATCH -t 0-10:00:00
#SBATCH --constraint=[ivb|hsw]
#SBATCH -n 1
#SBATCH --mem=2000
#SBATCH -o out/post.out
module load r/3.6.1-python3
srun Rscript --vanilla post.R
