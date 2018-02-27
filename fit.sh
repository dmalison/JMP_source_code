#!/bin/bash
#PBS -N ~/log/qsub_log
#PBS -l nodes=1:ppn=8,mem=16gb
#PBS -j oe

cd $PBS_O_WORKDIR

R CMD BATCH ~/bin/R/FragileFamilies/CurrentVersion/ME_RE_fit_v4_1.R


