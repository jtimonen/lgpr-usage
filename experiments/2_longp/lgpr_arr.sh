#!/bin/bash
#SBATCH -p short
#SBATCH -t 04:00:00
#SBATCH --constraint=[ivb|hsw]
#SBATCH -n 1
#SBATCH --mem=2500
#SBATCH --array=1-200
#SBATCH -o out/lgpr-%a.out
n=$SLURM_ARRAY_TASK_ID

echo "---------- PART1 ----------"
module load r/3.4.3-python-2.7.14
export R_LIBS=$WRKDIR/R/$EBVERSIONR
srun Rscript --vanilla main_lgp.R $n

