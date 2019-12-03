#!/bin/bash
#SBATCH -p batch
#SBATCH -t 10:00:00
#SBATCH --constraint=[ivb|hsw]
#SBATCH -n 1
#SBATCH --mem=2500
#SBATCH --array=1-200
#SBATCH -o out/lgpr-%a.out
n=$SLURM_ARRAY_TASK_ID

echo "---------- PART1 ----------"
module load r/3.6.1-python3
srun Rscript --vanilla job_lgpr.R $n

