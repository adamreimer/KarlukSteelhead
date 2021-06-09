load(".\\data\\dat19.rda")


#Sex stratified abundance estimate
#model R directly
M_tab <-  table(dat19$M$sex)
R_tab <-  table(dat19$R$sex)
R_tab / sum(R_tab)
R_tab / M_tab
C_tab <-  table(dat19$C$sex)
C_tab / sum(C_tab)
jags_dat_strat <- list(M = aggregate(tag ~ sex, dat19$M, FUN = function(x) {sum(!is.na(x))})[, 2],
                       C_male = C_tab[1],
                       C_samp = sum(C_tab),
                       C_weir = max(dat19$weir$cum)[1],
                       R_male = R_tab[1],
                       R_samp = sum(R_tab),
                       R_weir = 49
)

#paramaters of interest
params_strat <- c("N", "C", "R", "N_all", "Surv")

#MCMC settings
nc <- 5; nb <- 100000; nt <- 200; ns <- 200000

post_strat <- jagsUI::jags(data = jags_dat_strat,
                           parameters.to.save = params_strat,
                           # inits = list(list(R = c(6, 43)),
                           #              list(R = c(31, 16)),
                           #              list(R = c(25, 24)),
                           #              list(R = c(10, 39)),
                           #              list(R = c(15, 30))),
                           model.file = ".\\models\\mod_MR19_strat.txt",
                           n.chains = nc,
                           n.iter = ns,
                           n.burnin = nb,
                           n.thin = nt,
                           store.data = TRUE)
post_strat
plot(post_strat)
#Model is sensitive to prior on N
#Uniform prior with upper end capped by weir count and historic survival seems reasonable
#S=.25 is lower than Begich ever saw, lower than our point estimate 49/133=.36 and ensure Rmale posterior excceeds observed R
sum(post_strat$sims.list$R[,1] < R_tab[1])
#What's troubling is that changing S changes the the posterior on N and .25 is arbitary.
#Point estimate with a normal prior is 6% higher.


###########################
#Model distribution of R and C using Chapman to get N

#add asl data to get accurate CI
jags_dat_strat2 <- list(M = aggregate(tag ~ sex, dat19$M, FUN = function(x) {sum(!is.na(x))})[, 2],
                       C_male = C_tab[1],
                       C_samp = sum(C_tab),
                       C_weir = max(dat19$weir$cum)[1],
                       R_male = R_tab[1],
                       R_samp = sum(R_tab),
                       R_weir = 49,
                       age = matrix(c(10, 15, 2, 0, 5, 68, 15, 2), nrow = 2, ncol = 4, byrow = TRUE),
                       age_sum = c(90, 27)
)

#paramaters of interest
params_strat2 <- c("N", "C", "R", "N_all", "Surv", "N_a", "N_am", "N_af", "theta_f", "theta_m", "theta_all")

#MCMC settings
nc <- 5; nb <- 500; nt <- 1; ns <- 1000;

post_strat2 <- jagsUI::jags(data = jags_dat_strat2,
                           parameters.to.save = params_strat2,
                           model.file = ".\\models\\mod_MR19_strat2.txt",
                           n.chains = nc,
                           n.iter = ns,
                           n.burnin = nb,
                           n.thin = nt,
                           store.data = TRUE)
post_strat2
plot(post_strat2)
post_strat19asl <- post_strat2
saveRDS(post_strat19asl, ".\\models\\post_strat19_asl.rds")
#This model is less satisfying but we dont have to defne the prior






#############################
#Note: pooled model estimates slightly higher N than Chapman

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
