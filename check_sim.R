rm(list = ls())
library("rstan")
setwd("~/bin/R/JMP/JMP_source_code")
load('~/bin/R/JMP/work/sim')

# library("shinystan")
# launch_shinystan(fit_stan)

k = 1
i = parNames[k]
regex = grep(paste(paste(i,"[[].*[]]", sep = ""),"|","^",i,"$", sep = ""), names(fit_stan), value = T)

cbind(as.vector(t(parTrue[[i]])), summary(fit_stan, i)[[1]][,c(1,6,4,8,9,10)])
(t(parTrue[[i]]) >= summary(fit_stan, i)[[1]][,4])*(t(parTrue[[i]]) <= summary(fit_stan, i)[[1]][,8])
traceplot(fit_stan, pars = regex[sort(sample(1:length(regex), min(length(regex),9)))], inc_warmup = F)
k = k + 1

i = paste("lambda[", 1:stan_data$N, ",1]", sep = "")

traceplot(fit_stan, pars = "alpha_p[1,1]", inc_warmup = F)

draws = 
  extract(fit_stan,i, permuted = F, inc_warmup = F)

stan_hist(fit_stan,pars = "beta_3")

hist(draws[,c(1,2,3,4,6,7,8),1])

quantile(draws[500:750,c(1,2,3,5),1], c(.025, .05, .5, .95, .975))

j = 1 #sample(1:stan_data$N, 1)

plot(draws[500:750,1,j], type = "l", ylim = range(draws[500:750,c(1,2,3,5),j]))
lines(draws[500:750,2,j], type = "l", lty = 2)
lines(draws[500:750,3,j], type = "l", lty = 3)
lines(draws[500:750,5,j], type = "l", lty = 4)

j = j + 1



"alpha_1"
"c_1"

traceplot(fit_stan, 
          paste("c_p[",1:5, "]", sep = "")
          )

traceplot(fit_stan, "corr_lambda")
traceplot(fit_stan, "c_1")
traceplot(fit_stan, "c_2")
traceplot(fit_stan, "c_3")
traceplot(fit_stan, "c_4")

traceplot(fit_stan, "corr_1")
traceplot(fit_stan, "corr_2")
traceplot(fit_stan, "corr_3")
traceplot(fit_stan, "corr_4")

traceplot(fit_stan, "gamma_p_")

traceplot(fit_stan, 
          paste("gamma_M_N_1_cat5"),
          inc_warmup = T
)

traceplot(fit_stan, 
          paste("gamma_M_C_3"),
          inc_warmup = F
)

traceplot(fit_stan, 
          paste("corr_lambda"),
          inc_warmup = F
)

traceplot(fit_stan, 
          paste("lambda_R_sd"),
          inc_warmup = F
)

draws = 
  extract(fit_stan, "gamma_M_N_1_cat5", permuted = F, inc_warmup = T)

plot(draws[,1,1], type = "l", ylim = range(draws[250:750,,1]))
lines(draws[,2,1], type = "l", ylim = range(draws[250:750,,1]), lty = 2)
lines(draws[,3,1], type = "l", ylim = range(draws[250:750,,1]), lty = 2)
lines(draws[,4,1], type = "l", ylim = range(draws[250:750,,1]), lty = 3)
lines(draws[,5,1], type = "l", ylim = range(draws[250:750,,1]), lty = 4)
lines(draws[,6,1], type = "l", ylim = range(draws[250:750,,1]), lty = 5)
lines(draws[,7,1], type = "l", ylim = range(draws[250:750,,1]), lty = 5)
lines(draws[,8,1], type = "l", ylim = range(draws[250:750,,1]), lty = 5)



lines(draws[,3,1], type = "l", ylim = range(draws[250:750,,1]), lty = 1, col = "red")


abline(v = 500)

2,5 

4

5, 7

lambda <- extract(fit_stan, "lambda")[[1]]


apply(lambda[,,3], 1, sd)

dim(lambda)

c_p

traceplot(fit_stan, pars = regex[sort(sample(1:length(regex), min(length(regex),6)))], inc_warmup = F)



draws = extract(fit_stan, "c[2]", permuted = F)
hist(draws[,-c(2,4,7,8),1])

plot(draws[,1,1], type = "l")


#drop 2 4 7 8

chains = c(1,5,6)

plot(draws[,8,1], type = "l")


i = parNames[k]
regex = grep(paste(paste(i,"[[].*[]]", sep = ""),"|","^",i,"$", sep = ""), names(fit_stan), value = T)
#summary(fit_stan, i)[[1]][,c(1,6,4,8,9,10)]
traceplot(fit_stan, pars = regex[sort(sample(1:length(regex), min(length(regex),6)))], inc_warmup = T)
k = k + 1



cbind(parametersTrue[["alpha_0"]],
      summary(fit_stan, paste("alpha_0[",1:16, "]", sep = ""))[[1]][,c(1,4,8)])


#gamma_M_C_2_cat2[3] 

gamma_2_R2eq1[2,3]

j = 3
cbind(print(parametersTrue[[i]][,j]), 
      summary(fit_stan, paste(i, "[", 1:16, "," ,j, "]", sep = ""))[[1]][,c(1,4,8,9,10)])

alpha_1[1,1]
alpha_1 

i = "mu_M_C_2_1"
summary(fit_stan, i)[[1]][,c(1,6,4,8,9,10)]
print(parametersTrue[[i]])
traceplot(fit_stan, pars = i[sort(sample(1:length(i), min(length(i),6)))], inc_warmup = T)


i = paste("alpha_3[",1:16, ",3]", sep = "")
summary(fit_stan, i)[[1]][,c(1,6,4,8,9,10)]
print(parametersTrue[["alpha_3"]][,3])
traceplot(fit_stan, pars = i[sort(sample(1:length(i), min(length(i),6)))], inc_warmup = T)

k = k + 1

pairs(
  fit_stan, 
  pars = 
    c(
      "gamma_M_C_2_1",
      "sigma_M_C_2_1")
)

traceplot(fit_stan, pars = "gamma_M_N_1_cat5", inc_warmup = T)

params <- extract(fit_stan, pars = "gamma_4_r", permuted = F)

plot(sampler_params[[3]][,"energy__"], params[,3,4], cex = .1)

traceplot(fit_stan, pars = "l_c_p", inc_warmup = T)
summary(fit_stan, pars = "gamma_4_u")$summary

means = list()
rotation_matrices = list()

for(i in c(
  "c_p_",
  "alpha_p_tilde",
  "gamma_p_1",
  "gamma_p_2",
  "gamma_p_3",
  "gamma_p_4",
  "alpha_0_tilde",
  "alpha_1_tilde",
  "gamma_1_", 
  "c_1_", 
  "alpha_2_tilde",
  "beta_2_",
  "gamma_2_",
  "delta_2_",
  "c_2_",
  "alpha_3_tilde",
  "beta_3_",
  "gamma_3_",
  "delta_3_",
  "c_3_",
  "alpha_4_tilde",
  "beta_4_",
  "gamma_4_",
  "delta_4_",
  "c_4_"
  )
){
  
  param <- extract(fit_stan, pars = i)[[1]]
  param <- matrix(param, nrow = nrow(param))
  
  means[[paste("mu", i, sep = "_")]] <-  colMeans(param) # rep(0, ncol(param)) 
  rotation_matrices[[paste("L_Sigma", i, sep = "_")]] <- t(chol(cov(param))) # diag(ncol(param))
  
}

lambda <- colMeans(extract(fit_stan, pars = "lambda")[[1]])

plot(parametersTrue$lambda, lambda)
abline(a = 0, b = 1, col = "red")

draws = list()
draws_names = list()

for(i in c(
  "c_p_r",
  "alpha_p_tilde_r",
  "gamma_p_1_r",
  "gamma_p_2_r",
  "gamma_p_3_r",
  "gamma_p_4_r",
  "alpha_0_tilde_r",
  "alpha_1_tilde_r",
  "gamma_1_r", 
  "c_1_r", 
  "alpha_2_tilde_r",
  "beta_2_r",
  "gamma_2_r",
  "delta_2_r",
  "c_2_r",
  "alpha_3_tilde_r",
  "beta_3_r",
  "gamma_3_r",
  "delta_3_r",
  "c_3_r",
  "alpha_4_tilde_r",
  "beta_4_r",
  "gamma_4_r",
  "delta_4_r",
  "c_4_r"
)
){
  draws[[i]] = extract(fit_stan, pars = i, permuted = F)
  draws[[i]] = matrix(draws[[i]], nrow = nrow(draws[[i]])* ncol(draws[[i]]))
  draws_names[[i]] = paste(i, 1:ncol(draws[[i]]), sep = "_")
}

draws = list()
draws_names = list()

for(i in c(
  "c_p_",
  "alpha_p_tilde",
  "gamma_p_1",
  "gamma_p_2",
  "gamma_p_3",
  "gamma_p_4",
  "alpha_0_tilde",
  "alpha_1_tilde",
  "gamma_1_", 
  "c_1_", 
  "alpha_2_tilde",
  "beta_2_",
  "gamma_2_",
  "delta_2_",
  "c_2_",
  "alpha_3_tilde",
  "beta_3_",
  "gamma_3_",
  "delta_3_",
  "c_3_",
  "alpha_4_tilde",
  "beta_4_",
  "gamma_4_",
  "delta_4_",
  "c_4_"
)
){
  draws[[i]] = extract(fit_stan, pars = i, permuted = F)
  draws[[i]] = matrix(draws[[i]], nrow = nrow(draws[[i]])* ncol(draws[[i]]))
  draws_names[[i]] = paste(i, 1:ncol(draws[[i]]), sep = "_")
}

draws <- unlist(draws)
draws <- matrix(draws, nrow = 2000)
draws_names <- unlist(draws_names)
colnames(draws) <- draws_names

sampler_params <- get_sampler_params(fit_stan, inc_warmup = F)
energy <- as.numeric(sapply(sampler_params, function(x) x[,"energy__"]))

which(apply(draws, 2,function(x) cor(x, energy)) < -.3)

draws_cor <- 
draws_cor[upper.tri(draws_cor, diag = T)] <- NA

min(draws_cor, na.rm  = T)
max(draws_cor, na.rm  = T)

hist(draws_cor)

ind <- which(draws_cor < -.7, arr.ind = T)

cbind(draws_names[ind[,1]], draws_names[ind[,2]])

pairs(
  fit_stan, 
  pars = 
    c(
      paste(
      "lambda[",
      sample(stan_data$R_2_ind, 2), 
      "]", 
      sep = ""
    ),
  "l_c_p_[1]",
  "l_c_p_[2]")
)



pairs(
  fit_stan, 
  pars = 
    c("c_4_r", "lp__")
)

pairs(
  fit_stan, 
  pars = 
    c(paste("alpha_1_tilde[",sample(stan_data$X_num,5),",1]" ,sep = ""), "lp__")
)

pairs(
  fit_stan, 
  pars = 
    c(paste("alpha_3_tilde[",sample(stan_data$X_num,1), ",", 1:2, "]" ,sep = ""), "lp__")
)

pairs(
  fit_stan, 
  pars = 
    c("alpha_0_tilde", "lp__")
)

pairs(
  fit_stan, 
  pars = 
    c("l_c_p_r", "gamma_p_3[2]", "lp__")
)




par <- extract(fit_stan, pars = c("l_c_p_",  "lp__"))

par_cor <- cor(cbind(par[[1]], par[[2]], par[[3]]))

which(par_cor > .2, arr.ind = T)

traceplot(
  fit_stan, 
  pars = 
    c("l_c_p_")
)

summary(
  fit_stan, 
  pars = "l_c_p_"
)$summary

traceplot(
  fit_stan, 
  pars = "l_c_p_"
)

traceplot(
  fit_stan, 
  pars = 
    paste(
      "lambda[",
      sample(stan_data$R_1_ind1, 6), 
      "]", 
      sep = ""
    ), 
  inc_warmup = T
)

for (i in grep("alpha_p.*", names(parametersTrue), value = T)){
  draws <- extract(fit_stan, i)[[1]]
  if (length(dim(draws)) == 3){
    for(j in dim(draws)[3]){
      out <- matrix(nrow = stan_data$X_num, ncol = 5)
      out[,1] <- colMeans(draws[,,j])
      out[,2] <- parametersTrue[[i]][,j]
      out[,3:4] <- t(apply(draws, 2, function(x) quantile(x, probs = c(.025, .975))))
      out[,5] <- (out[,2] > out[,3]) & (out[,2] < out[,4])
      rownames(out) <- colnames(stan_data$X)
      print(i)
      print(format(out, digits = 3, scientific = F), quote = F)
    }
  }
  if (length(dim(draws)) == 2){
    out <- matrix(nrow = stan_data$X_num, ncol = 5)
    out[,1] <- colMeans(draws)
    out[,2] <- parametersTrue[[i]]
    out[,3:4] <- t(apply(draws, 2, function(x) quantile(x, probs = c(.025, .975))))
    out[,5] <- (out[,2] > out[,3]) & (out[,2] < out[,4])
    rownames(out) <- colnames(stan_data$X)
    print(i)
    print(format(out, digits = 3, scientific = F), quote = F)
}
}

for (i in grep("beta*", names(parametersTrue), value = T)){
  draws <- matrix(extract(fit_stan, i)[[1]], ncol = length(parametersTrue[[i]]))
  out <- matrix(nrow = length(parametersTrue[[i]]), ncol = 5)
  out[,1] <- colMeans(draws)
  out[,2] <- parametersTrue[[i]]
  out[,3:4] <- t(apply(draws, 2, function(x) quantile(x, probs = c(.025, .975))))
  out[,5] <- (out[,2] > out[,3]) & (out[,2] < out[,4])
  print(i)
  print(format(out, digits = 3, scientific = F), quote = F)
}

for (i in grep("gamma*", names(parametersTrue), value = T)){
  draws <- matrix(extract(fit_stan, i)[[1]], ncol = length(parametersTrue[[i]]))
  out <- matrix(nrow = length(parametersTrue[[i]]), ncol = 5)
  out[,1] <- colMeans(draws)
  out[,2] <- parametersTrue[[i]]
  out[,3:4] <- t(apply(draws, 2, function(x) quantile(x, probs = c(.025, .975))))
  out[,5] <- (out[,2] > out[,3]) & (out[,2] < out[,4])
  print(i)
  print(format(out, digits = 3, scientific = F), quote = F)
}

for (i in grep("delta*", names(parametersTrue), value = T)){
  draws <- matrix(extract(fit_stan, i)[[1]], ncol = length(parametersTrue[[i]]))
  out <- matrix(nrow = length(parametersTrue[[i]]), ncol = 5)
  out[,1] <- colMeans(draws)
  out[,2] <- parametersTrue[[i]]
  out[,3:4] <- t(apply(draws, 2, function(x) quantile(x, probs = c(.025, .975))))
  out[,5] <- (out[,2] > out[,3]) & (out[,2] < out[,4])
  print(i)
  print(format(out, digits = 3, scientific = F), quote = F)
}

for (i in grep("[Ss]igma*", names(parametersTrue), value = T)){
  draws <- matrix(extract(fit_stan, i)[[1]], ncol = length(parametersTrue[[i]]))
  out <- matrix(nrow = length(parametersTrue[[i]]), ncol = 5)
  out[,1] <- colMeans(draws)
  out[,2] <- parametersTrue[[i]]
  out[,3:4] <- t(apply(draws, 2, function(x) quantile(x, probs = c(.025, .975))))
  out[,5] <- (out[,2] > out[,3]) & (out[,2] < out[,4])
  print(i)
  print(format(out, digits = 3, scientific = F), quote = F)
}


for (i in grep("sigma_lambda", names(parametersTrue), value = T)){
  draws <- matrix(extract(fit_stan, i)[[1]], ncol = length(parametersTrue[[i]]))
  out <- matrix(nrow = length(parametersTrue[[i]]), ncol = 5)
  out[,1] <- colMeans(draws)
  out[,2] <- parametersTrue[[i]]
  out[,3:4] <- t(apply(draws, 2, function(x) quantile(x, probs = c(.025, .975))))
  out[,5] <- (out[,2] > out[,3]) & (out[,2] < out[,4])
  print(i)
  print(format(out, digits = 3, scientific = F), quote = F)
}