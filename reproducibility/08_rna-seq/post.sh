#!/bin/bash
#SBATCH -p short
#SBATCH -t 03:15:00
#SBATCH --constraint=[ivb|hsw]
#SBATCH -n 1
#SBATCH --mem=2500
#SBATCH -o out/post.out
module load r/3.6.1-python3
n=$SLURM_ARRAY_TASK_ID
srun Rscript --vanilla post.R
