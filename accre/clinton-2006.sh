#!/bin/bash
#SBATCH --mail-user=brenton.kenkel@gmail.com
#SBATCH --mail-type=ALL
#SBATCH --nodes=4
#SBATCH --ntasks=4
#SBATCH --time=1:00:00
#SBATCH --mem-per-cpu=250M
#SBATCH --output=clinton-2006.out

setpkgs -a gcc_compiler
setpkgs -a R_3.1.1
setpkgs -a openmpi_gcc
setpkgs -a mpiexec

mpirun Rscript /home/kenkelb/parallel-r-workshop/run.r
