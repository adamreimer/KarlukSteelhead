library(KarlukSteelhead)
jags_dat <- list(M = table(dat17$M$sex),
                 R = table(dat17$R$sex),
                 Rsum = sum(!is.na(dat17$R$sex)),
                 C = sum(dat17$weir$daily)
)

#paramaters of interest
params <- c("N", "m", "phi0", "phi", "PHI")

#MCMC settings
nc <- 5; nb <- 10000; nt <- 10; ns <- 20000

post <- jagsUI::jags(data = jags_dat,
                     inits = list(list(N = 8000), list(N = 8000), list(N = 8000), list(N = 8000), list(N = 8000)),
                     parameters.to.save = params,
                     model.file = ".\\models\\mod_MRp2_theta_17.txt",
                     n.chains = nc,
                     n.iter = ns,
                     n.burnin = nb,
                     n.thin = nt,
                     store.data = TRUE)
post
par(mfrow = c(2,2))
jagsUI::traceplot(post)
