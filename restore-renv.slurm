#!/bin/bash
#
#SBATCH --job-name=restore-renv-ordinal-ITCI
#SBATCH --ntasks=1 --cpus-per-task=2
#SBATCH --time=00:30:00
#SBATCH --cluster=wice
#SBATCH --mail-type=BEGIN,END,FAIL
#SBATCH -A lp_doctoralresearch

export OMP_NUM_THREADS=1

module use /apps/leuven/rocky8/icelake/2023a/modules/all
module load GSL
module load CMake
module load R/4.4.0-gfbf-2023a


Rscript -e "renv::status()" -e "renv::restore()" - "renv::status()"




