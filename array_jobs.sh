#!/bin/bash
 
#SBATCH --job-name=ls_array4
#SBATCH --chdir=/work/chowdhus
#SBATCH --output=/work/%u/%A-%a.out
#SBATCH --time=0-00:05:00

#SBATCH --mem-per-cpu=1G

echo hello world $SLURM_ARRAY_TASK_ID