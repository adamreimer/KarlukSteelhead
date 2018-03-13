library(KarlukSteelhead)
#pg 37 of Begich thesis.
plot(c(.62, .55, .42, .68, .36, .66),
     c(.63, .49, .47, .54, .36, .64),
     xlab = "female survival", ylab = "male survival")
abline(a = 0, b = 1)

surv <- c(0.667, 0.583, 0.507, 0.623, 0.357, .657)
surv_se <- c(0.023, 0.026, 0.029, 0.026, 0.034, 0.021, 0.027)
fit <- fitdistrplus::fitdist(surv, "beta")
hist(surv, xlim = c(0, 1), ylim = c( 0, 4))
lines(seq(0, 1, 0.01), dbeta(seq(0, 1, 0.01), fit$estimate[1], fit$estimate[2]))

jags_dat <- list(M = table(dat17$M$sex),
                 R = table(dat17$R$sex),
                 Rsum = dim(dat17$R)[1], #sum(!is.na(dat17$R$sex)),
                 C = sum(dat17$weir$daily),
                 beta_param = fit$estimate
)

#paramaters of interest
params <- c("N", "m", "rho", "phi", "PHI")

#MCMC settings
nc <- 3; nb <- 10000; nt <- 10; ns <- 30000

post <- jagsUI::jags(data = jags_dat,
                     #inits = list(list(N = 8000), list(N = 8000), list(N = 8000), list(N = 8000), list(N = 8000)),
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
