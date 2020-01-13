#Sex stratified abundance estimate
R_tab <-  table(dat19$R$sex)
C_tab <-  table(dat19$C$sex)
jags_dat_strat <- list(M = aggregate(tag ~ sex, dat19$M, FUN = function(x) {sum(!is.na(x))})[, 2],
                 R_weir = sum(dat19$R$Ytag),
                 #R_samp = sum(R_tab),
                 R_obs = as.vector(R_tab),
                 C_weir = max(dat19$weir$cum)[1],
                 C_samp = sum(C_tab),
                 C_male = C_tab[1]
)

#paramaters of interest
params_strat <- c("N", "C", "R")

#MCMC settings
nc <- 5; nb <- 15000; nt <- 1; ns <- 30000

post_strat <- jagsUI::jags(data = jags_dat_strat,
                           parameters.to.save = params_strat,
                           inits = list(list(R = c(14, 34)), list(R = c(14, 34)), list(R = c(14, 34)), list(R = c(14, 34)), list(R = c(14, 34))),
                           model.file = ".\\models\\mod_MR19_strat.txt",
                           n.chains = nc,
                           n.iter = ns,
                           n.burnin = nb,
                           n.thin = nt,
                           store.data = TRUE)
post_strat
plot(post_strat)



#Note: pooled model estimates higher N than Chapman
jags_dat_pooled <- list(M = sum(!is.na(dat19$M$tag)),
                        R = sum(dat19$R$Ytag),
                        C = max(dat19$weir$cum)[1]
)

#paramaters of interest
params_pooled <- c("N")

#MCMC settings
nc <- 5; nb <- 10000; nt <- 1; ns <- 20000

post_pooled <- jagsUI::jags(data = jags_dat_pooled,
                            parameters.to.save = params_pooled,
                            model.file = ".\\models\\mod_MR19_pooled.txt",
                            n.chains = nc,
                            n.iter = ns,
                            n.burnin = nb,
                            n.thin = nt,
                            store.data = TRUE)
post_pooled
jagsUI::traceplot(post_pooled)
