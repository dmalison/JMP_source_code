rm(list = ls())
library("rstan")
setwd("~/bin/JMP/JMP_source_code")
load('~/bin/JMP/work/sim')

# library("shinystan")
# launch_shinystan(fit_stan)

k = 1
i = parNames[k]
regex = grep(paste(paste(i,"[[].*[]]", sep = ""),"|","^",i,"$", sep = ""), names(fit_stan), value = T)

cbind(as.vector(t(parTrue[[i]])), summary(fit_stan, pars = i, use_cache = F)[[1]][,c(1,6,4,8,9,10)])
(t(parTrue[[i]]) >= summary(fit_stan, pars = i, use_cache = F)[[1]][,4])*(t(parTrue[[i]]) <= summary(fit_stan, pars = i, use_cache = F)[[1]][,8])
traceplot(fit_stan, pars = regex[sort(sample(1:length(regex), min(length(regex),9)))], inc_warmup = T)
k = k + 1

theta_1 <- extract(fit_stan, pars = "theta_1")[[1]]

traceplot(fit_stan, pars = "corr_1")