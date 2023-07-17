#!/bin/bash
 
#SBATCH --job-name=ls_task1
#SBATCH --chdir=/work/chowdhus
#SBATCH --output=/work/%u/%x-%j.out
#SBATCH --time=0-00:03:00

#SBATCH --mem-per-cpu=1G

echo hello world 