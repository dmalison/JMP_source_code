# Checks draws from model fitted to data

rm(list = ls())
library("rstan")
setwd("~/bin/JMP/JMP_source_code")
load('~/bin/JMP/work/fit')

# library("shinystan")
# launch_shinystan(fit_stan)

traceplot(fit_stan, "lp__", inc_warmup = F)

k = 1
i = parNames[k]
regex = grep(paste(paste(i,"[[].*[]]", sep = ""),"|","^",i,"$", sep = ""), names(fit_stan), value = T)
summary(fit_stan, pars = i, use_cache = F)[[1]][,c(1,6,4,8,9,10)]
traceplot(fit_stan, pars = regex[sort(sample(1:length(regex), min(length(regex),9)))], inc_warmup = F)
k = k + 1


