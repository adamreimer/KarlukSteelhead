#Suspect biased high by handing mortality

jags_dat <- list(M = length(unique(dat17$M[dat17$M$sex %in% "F", ]$tag)),
                 R = length(unique(dat17$R[dat17$R$sex %in% "F", ]$tag)),
                 Call = sum(dat17$weir$daily),
                 samp_f = sum(dat17$C$sex == "F"),
                 samp_n = sum(!is.na(dat17$C$sex))
)

#paramaters of interest
params <- c("N", "C", "phi")

#MCMC settings
nc <- 5; nb <- 10000; nt <- 1; ns <- 20000

post <- jagsUI::jags(data = jags_dat,
                     parameters.to.save = params,
                     model.file = ".\\models\\mod_MRfemale17.txt",
                     n.chains = nc,
                     n.iter = ns,
                     n.burnin = nb,
                     n.thin = nt,
                     store.data = TRUE)
post
jagsUI::traceplot(post)
