#!/bin/bash
#SBATCH -p short
#SBATCH -t 04:00:00
#SBATCH --constraint=[ivb|hsw]
#SBATCH -n 1
#SBATCH --mem=2500
#SBATCH -o out/post.out
module load r/3.6.1-python3
srun Rscript --vanilla post_lgpr.R
