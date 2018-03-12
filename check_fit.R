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

i = paste("alpha_2[",1:stan_data$X_num, ',3]', sep = "")

sigma_M_C_2[1]
gamma_M_C_2[1]

gamma_M_C_3[1]
sigma_M_C_3[1]

sigma_M_C_4[1]
sigma_M_C_4[2]

gamma_M_C_4[1]


traceplot(fit_stan, pars = paste("theta_1[", sample(stan_data$N,9), ",2]", sep = ""), inc_warmup = T)
k = k + 1

theta_3 <- colMeans(extract(fit_stan, "theta_3")[[1]])
theta_2 <- colMeans(extract(fit_stan, "theta_2")[[1]])

which(theta_3[,3] == 0)

plot(theta_2[stan_data$R_3_ind1,1], theta_3[stan_data$R_3_ind1,1])

hist(theta_3[which(abs(theta_3[,3]) < .02),3])

table(theta_3[which(abs(theta_3[,3]) < .02),3])

data_raw$M_C_3_2[which(round(colMeans(theta_3)[,3],5) == round(-0.0189690263986446,5))]

ind <- which(round(colMeans(theta_3)[,3],5) == round(-0.0189690263986446,5))

theta_3 <- extract(fit_stan, "theta_3")[[1]]

data_raw$R_3[ind]

theta_3[,ind,3]

theta_3[,ind,3]

hist(theta_3[,3])

plot(theta_2[,3], theta_3[,3])

sum(theta_3[,3] == 0)

R_0 <- stan_data$R_0
R_1 <- rep(NA, stan_data$N)
R_1[stan_data$R_0_ind0] <- 0
R_1[stan_data$R_1_ind] <- stan_data$R_1
X <- stan_data$X

y <- theta_1[,2]

y <- data_raw$theta_N_1

summary(lm(y ~ X*R_0 + X*R_1))

