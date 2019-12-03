#!/bin/bash
#SBATCH -p batch
#SBATCH -t 2-00:00:00
#SBATCH --constraint=[ivb|hsw]
#SBATCH -n 1
#SBATCH --mem=3000
#SBATCH --array=359,785,1265
#SBATCH -o out/out-%a.out
module load r/3.6.1-python3
n=$SLURM_ARRAY_TASK_ID
srun Rscript --vanilla job.R $n 8000 0.99
