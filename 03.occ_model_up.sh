#!/bin/bash

#SBATCH --job-name=03.occ_model_carabid_2yr
#SBATCH --chdir=/home/chowdhus/occ_model_de
#SBATCH --output=/work/%u/%x-%A-%a.out
#SBATCH --time=14-00:00:00

#SBATCH --mem-per-cpu=128G

module load foss/2022b R/4.2.2

species="$1"

array_or_job_id=${SLURM_ARRAY_JOB_ID:-$SLURM_JOB_ID}
output_dir="/work/$USER/$SLURM_JOB_NAME"
mkdir -p "$output_dir"

Rscript /home/chowdhus/occ_model_de/03.occ_model_carabid_2yr.R "$species" "$output_dir"