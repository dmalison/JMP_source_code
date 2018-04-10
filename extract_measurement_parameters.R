setwd("~/bin/JMP/JMP_source_code")

rm(list = ls())

library("rstan")     

# RStan recommends calling the following lines before use

rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

measurementPars = list()

for(
  filename in 
  c(
    "~/bin/JMP/work/fit_M_R_1",
    "~/bin/JMP/work/fit_M_N_1",
    "~/bin/JMP/work/fit_M_R_2",
    "~/bin/JMP/work/fit_M_N_2",
    "~/bin/JMP/work/fit_M_C_2",
    "~/bin/JMP/work/fit_M_R_3",
    "~/bin/JMP/work/fit_M_N_3",
    "~/bin/JMP/work/fit_M_C_3",
    "~/bin/JMP/work/fit_M_R_4",
    "~/bin/JMP/work/fit_M_N_4",
    "~/bin/JMP/work/fit_M_C_4"
  )
)
{
  load(filename)
  
  for (i in parNames){
    x <- extract(fit_stan, pars = i)[[1]]
    measurementPars[[i]] = colMeans(x)
    rm(i,x)
  }
  
  rm(fit_stan, stan_data, parNames)
  
}

save(list = "measurementPars", file = "~/bin/JMP/work/measurement_parameters")


