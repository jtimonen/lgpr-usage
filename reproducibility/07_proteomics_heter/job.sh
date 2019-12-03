#!/bin/bash
#SBATCH -p batch
#SBATCH -t 1-10:00:00
#SBATCH --constraint=[ivb|hsw]
#SBATCH -n 1
#SBATCH --mem=2000
#SBATCH --array=1-1538
#SBATCH -o out/out-%a.out
module load r/3.6.1-python3
n=$SLURM_ARRAY_TASK_ID
srun Rscript --vanilla job.R $n 4000 0.99
