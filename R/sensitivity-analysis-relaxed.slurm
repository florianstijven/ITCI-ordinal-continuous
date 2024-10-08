#!/bin/bash
#
#SBATCH --job-name=sensitivity-analysis-ordinal-ITCI
#SBATCH --ntasks=1 --cpus-per-task=72 --nodes=1
#SBATCH --time=05:00:00
#SBATCH --cluster=wice
#SBATCH --mail-type=BEGIN,END,FAIL
#SBATCH -A lp_doctoralresearch

export OMP_NUM_THREADS=1

module use /apps/leuven/rocky8/icelake/2023a/modules/all
module load GSL
module load CMake
module load R/4.4.0-gfbf-2023a

# This slurm file is located in ./R. This causes issues with renv because renv 
# expects scripts to be executed from the root directory. We therefore first go
# back to the root directory, from where we run the scripts. 
cd ..

Rscript -e "renv::restore()" -e "Sys.setenv(TZ='Europe/Brussels')"
Rscript R/sensitivity-analysis-relaxed.R 71



