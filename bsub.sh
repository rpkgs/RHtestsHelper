#!/bin/bash

#BSUB -J RHtests2023
#BSUB -o log_o.txt
#BSUB -e log_e.txt
#BSUB -n 16
#BSUB -m c001
#BSUB -R span[hosts=1]
#BSUB -u kongdd.sysu@gmail.com
#BSUB -N 
#BSUB -I 

source /etc/profile.d/modules.sh

## 加载模块
# module load gcc/9.3.0
# module load netcdf-c/4.7.4-gcc930
export PATH="/share/opt/.conda/envs/r-4.2/bin:$PATH"
# ~/.profile works
# /share/opt/R/4.1.0/bin/R --file "~/github/bsub/hello.R"

# Rscript hello.R
Rscript scripts/ChinaHW_cluster/run_withRef_V2.R
