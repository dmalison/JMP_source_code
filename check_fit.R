# Checks draws from model fitted to data

rm(list = ls())
library("rstan")
library("foreign")
setwd("~/bin/JMP/JMP_source_code")
load('~/bin/JMP/work/fit')
data_raw <- read.dta("~/data/Fragile_Families/extract/extract_noretro.dta") 

# library("shinystan")
# launch_shinystan(fit_stan)

traceplot(fit_stan, pars = "lp__", inc_warmup = F)
summary(fit_stan, "lp__", use_cache = F)[[1]]

k = 1
i = parNames[k]
regex = grep(paste(paste(i,"[[].*[]]", sep = ""),"|","^",i,"$", sep = ""), names(fit_stan), value = T)
cbind(
  unlist(stan_data[[paste(i, "mean", sep = "_")]]),
  summary(fit_stan, pars = i, use_cache = F)[[1]][,c(1,6,4,8,9,10)]
)
traceplot(fit_stan, pars = regex[sort(sample(1:length(regex), min(length(regex),9)))], inc_warmup = F)
k = k + 1

i = paste("alpha_p_tilde[",1:stan_data$X_num, ',4]', sep = "")
summary(fit_stan, pars = i, use_cache = F)[[1]][,c(1,6,4,8,9,10)]
traceplot(fit_stan, pars = i, inc_warmup = F)

i = c("c_0", "c_1", "c_2", "c_3", "c_4")
traceplot(fit_stan, pars = i)

sampler_params <- get_sampler_params(fit_stan, inc_warmup = FALSE)

param = extract(fit_stan, pars = i, permuted = F, inc_warmup = F)
pairs(cbind(param[,1,],sampler_params[[1]][,"energy__"]))

pairs(sampler_params[[1]][,c("lp__", "energy__")])
pairs(fit_stan, pars = c("lp__", "gamma_3_raw"))

code <- get_stancode(fit_stan)
cat(code)

i = paste("alpha_3_tilde_raw[",1:8, ',3]', sep = "")
summary(fit_stan, pars = i, use_cache = F)[[1]][,c(1,6,4,8,9,10)]
traceplot(fit_stan, pars = i, inc_warmup = F)

theta_2 = colMeans(extract(fit_stan, pars = "theta_2")[[1]])
theta_3 = colMeans(extract(fit_stan, pars = "theta_3")[[1]])
plot(theta_2[,2], data_raw$theta_N_2)

theta_C_4 = colMeans(extract(fit_stan, pars = "theta_C_4")[[1]])

plot(theta_C_4, data_raw$theta_C_4)
ind = which(data_raw$R_2 == 1)

plot(test[ind],data_raw$theta_R_2[ind])

cbind(unlist(stan_data$c_M_R_2_cat3_mean), summary(fit_stan, pars = i, use_cache = F)[[1]][,c(1,6,4,8,9,10)])


i = "xi_3_raw"
summary(fit_stan, pars = i, use_cache = F)[[1]][,c(1,6,4,8,9,10)]
traceplot(fit_stan, pars = i, inc_warmup = F)

i = paste("alpha_3[",1:(stan_data$X_num), ',3]', sep = "")
summary(fit_stan, pars = i, use_cache = F)[[1]][,c(1,6,4,8,9,10)]
traceplot(fit_stan, pars = i, inc_warmup = F)

i = paste("alpha_p[",1:(stan_data$X_num), ',2]', sep = "")
summary(fit_stan, pars = i, use_cache = F)[[1]][,c(1,6,4,8,9,10)]
traceplot(fit_stan, pars = i, inc_warmup = F)

i = paste("alpha_p_tilde[",1:(stan_data$X_num), ',2]', sep = "")
summary(fit_stan, pars = i, use_cache = F)[[1]][,c(1,6,4,8,9,10)]
traceplot(fit_stan, pars = i, inc_warmup = F)

theta_3 <- extract(fit_stan, pars = "theta_3")[[1]]

theta_C_3 <- theta_3[,,3]

theta_C_3_bar <- colMeans(theta_C_3)

X <- stan_data$X
summary(lm(theta_C_3_bar ~ X))

theta_C_3= colMeans(theta_3[,,3])
theta_N_3= theta_3[,,2]

hist(apply(theta_N_2, 2, sd))

hist(theta_C_2[,3])

dim(theta_2)

i = paste("alpha_2[",1:stan_data$X_num, ',3]', sep = "")
traceplot(fit_stan, pars = i, inc_warmup = T)

sigma_M_C_2[1]
gamma_M_C_2[1]

gamma_M_C_3[1]
sigma_M_C_3[1]

sigma_M_C_4[1]
sigma_M_C_4[2]

gamma_M_C_4[1]


traceplot(fit_stan, pars = paste("theta_1[", sample(stan_data$N,9), ",2]", sep = ""), inc_warmup = T)
k = k + 1

epsilon_2 <- colMeans(extract(fit_stan, "epsilon_2")[[1]])

epsilon_2 <- colMeans(extract(fit_stan, "epsilon_2")[[1]])


epsilon_2[,3]
cor(cbind(epsilon_2[,3],stan_data$X))[1,]

theta_3 <- extract(fit_stan, "theta_3")[[1]]
theta_2 <- extract(fit_stan, "theta_2")[[1]]

hist(theta_3[,10,3])

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

