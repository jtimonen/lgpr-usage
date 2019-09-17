#!/bin/bash
#SBATCH -p short
#SBATCH -t 04:00:00
#SBATCH --constraint=[ivb|hsw]
#SBATCH -n 1
#SBATCH --mem=1500
#SBATCH --array=1-300
#SBATCH -o out/out-%a.out
module load r/3.4.3-python-2.7.14
export R_LIBS=$WRKDIR/R/$EBVERSIONR
echo $R_LIBS
n=$SLURM_ARRAY_TASK_ID
srun Rscript --vanilla exp_nb.R $n
