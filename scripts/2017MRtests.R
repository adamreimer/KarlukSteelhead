library(KarlukSteelhead)

test_ks <- function(lg1, lg2, title){
  name1 <- deparse(substitute(lg1))
  name2 <- deparse(substitute(lg2))
  plot(ecdf(lg1),
       main = title,
       ylab = "Proportion",
       xlab = "length")
  plot(ecdf(lg2), add = TRUE, col = "red")
  legend(quantile(lg1, .65, na.rm = TRUE), .40, legend = c(name1, name2), col = c("black", "red"), pch = 16)
  test <- ks.test(as.numeric(lg1),
                  as.numeric(lg2))
  text(quantile(lg1, .65, na.rm = TRUE), .1,
       labels = paste0(test$method, "\n p.val = ", format.pval(test$p.value, digits = 2, eps = 0.001, nsmall = 3)),
       adj = 0)
}
#no size selectivity
par(mfrow = c(2,2))
test_ks(dat17$C$length, dat17$R$length, "C vs. R")
test_ks(dat17$M$length, dat17$R$length, "M vs. R")
test_ks(dat17$M$length, dat17$C$length, "M vs. C")
par(mfrow = c(1,1))

#sex selectivity
#first event biased
R_sex <- dat17$R[!is.na(dat17$R$sex), ]
CR <- data.frame(strata= c(rep("C", dim(dat17$C)[1]), rep("R", dim(R_sex)[1])),
                 sex = c(dat17$C$sex, R_sex$sex))
aslpack::tab_lr(CR, "sex")

#second event biased, but weir should have equal prob capture by sex so really differnet mortality by sex.
M_sex <- dat17$M[!is.na(dat17$M$sex), ]
MR <- data.frame(strata= c(rep("M", dim(M_sex)[1]), rep("R", dim(R_sex )[1])),
                 sex = c(M_sex$sex, R_sex $sex))
aslpack::tab_lr(MR, "sex")

MC <- data.frame(strata= c(rep("M", dim(M_sex)[1]), rep("C", dim(dat17$C)[1])),
                 sex = c(M_sex$sex, dat17$C$sex))
aslpack::tab_lr(MC, "sex")

#Consistancy tests passed
mix_tab <- cbind(table(dat17$R$mark_strata, dat17$R$strata), table(dat17$M$strata) - table(dat17$R$mark_strata))
mix_tab
DescTools::GTest(mix_tab)

eqprop_tab <- rbind(table(dat17$R$strata), table(dat17$C$strata) - table(dat17$R$strata))
eqprop_tab
DescTools::GTest(eqprop_tab)

completemix_tab <- rbind(table(dat17$R$mark_strata), table(dat17$M$strata) - table(dat17$R$mark_strata))
completemix_tab
chisq.test(completemix_tab)

#females only
dat17f <- lapply(dat17[1:3], function(x) x[x$sex %in% "F", ])
mix_tab <- cbind(table(dat17f$R$mark_strata, dat17f$R$strata), table(dat17f$M$strata) - table(dat17f$R$mark_strata))
mix_tab
DescTools::GTest(mix_tab)

eqprop_tab <- rbind(table(dat17f$R$strata), table(dat17f$C$strata) - table(dat17f$R$strata))
eqprop_tab
DescTools::GTest(eqprop_tab)

completemix_tab <- rbind(table(dat17f$R$mark_strata), table(dat17f$M$strata) - table(dat17f$R$mark_strata))
completemix_tab
chisq.test(completemix_tab)

#pooled sex comp is sufficent
aslpack::tab_lr(dat17$C, "sex")

library(ggplot2)
#sexes are similarly distributed thru time at weir
ggplot(dat17$C, aes(x = as.Date(date))) +
  geom_histogram(binwidth = 1) +
  scale_x_date(date_labels = "%m/%d", date_breaks = "weeks") +
  labs(x = "Date") +
  facet_grid(sex ~ .)

#weir captures whole run
ggplot(dat17$weir, aes(x = as.Date(date), weight = daily)) +
  geom_histogram(binwidth = 1) +
  scale_x_date(date_labels = "%m/%d", date_breaks = "weeks") +
  labs(x = "Date")
