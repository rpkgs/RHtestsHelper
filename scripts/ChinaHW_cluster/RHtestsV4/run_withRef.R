#! /usr/bin/Rscript --no-init-file
# Dongdong Kong ----------------------------------------------------------------
# Copyright (c) 2022 Dongdong Kong. All rights reserved.
source('scripts/ChinaHW_cluster/main.R')

InitCluster(10)

f_input <- "data-raw/INPUT/INPUT_met2474_Tmax&RHmax_for_HImax_1951-2022_V2.fst"
df = import_fst(f_input)

version = "v20230331"
main_RHtests_met2481("RH_avg", version)
main_RHtests_met2481("Tair_max", version)
main_RHtests_met2481("Tair_avg", version)
