#!/bin/bash
#SBATCH -p batch
#SBATCH -t 3-23:00:00
#SBATCH --constraint=[ivb|hsw]
#SBATCH -n 1
#SBATCH --mem=2200
#SBATCH --array=244,246,359,576,785,961,1066,1098,1371,1374,1422
#SBATCH -o out/heter_rerun-%a.out
module load r/3.4.3-python-2.7.14
export R_LIBS=$WRKDIR/R/$EBVERSIONR
echo $R_LIBS
n=$SLURM_ARRAY_TASK_ID
srun Rscript --vanilla 06B_run.R $n 8000 0.99
