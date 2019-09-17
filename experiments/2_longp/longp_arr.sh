#!/bin/bash
#SBATCH -p batch
#SBATCH -t 1-20:00:00
#SBATCH --constraint=[ivb|hsw]
#SBATCH -n 1
#SBATCH --mem=2500
#SBATCH --array=1-200
#SBATCH -o out/longp-%a.out
n=$SLURM_ARRAY_TASK_ID

echo "---------- PART2 ----------"
module load matlab
srun matlab -nosplash -r "main_longp($n); exit(0)"

