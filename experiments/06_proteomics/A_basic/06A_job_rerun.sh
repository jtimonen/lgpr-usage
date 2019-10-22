#!/bin/bash
#SBATCH -p batch
#SBATCH -t 1-23:00:00
#SBATCH --constraint=[ivb|hsw]
#SBATCH -n 1
#SBATCH --mem=2500
#SBATCH --array=109,112,147,152,160,175,190,197,216,231,509,560,576,601,665,725,764,829,855,929,939,957,989,992,1023,1040,1066,1067,1068,1141,1173,1180,1319,1422,1513
#SBATCH -o out/basic_rerun-%a.out
module load r/3.4.3-python-2.7.14
export R_LIBS=$WRKDIR/R/$EBVERSIONR
echo $R_LIBS
n=$SLURM_ARRAY_TASK_ID
srun Rscript --vanilla 06A_run.R $n 4000 0.99
