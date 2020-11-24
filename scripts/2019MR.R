library(magrittr)
library(ggplot2)
library(aslpack)
load(".\\data\\dat19.rda")
source(".\\scripts\\utils.R")

#Check text numbers
table(dat19$M$strata)
length(unique(dat19$M$tag))

range(dat19$weir$date)
max(dat19$weir$cum)
sum(dat19$R$Ytag)

#no size selectivity
R_lg <- dat19$R$length[!is.na(dat19$R$length)]
C_lg <- dat19$C$length[!is.na(dat19$C$length)]
M_lg <- dat19$M$length[!is.na(dat19$M$length)]
par(mfrow = c(2,2))
test_ks(C_lg, R_lg, "C vs. R")
test_ks(M_lg, R_lg, "M vs. R")
test_ks(M_lg, C_lg, "M vs. C")
par(mfrow = c(1,1))


length(R_lg); length(M_lg); length(C_lg);
#report version
data.frame(sample = "Recaptured (n=20)",
           Length = rep(R_lg, times = 2),
           test = rep(c("Captured vs. Recaptured", "Marked vs. Recaptured"), each = length(R_lg))) %>%
  rbind(data.frame(sample = "Marked (n=131)",
                   Length = rep(M_lg, times = 2),
                   test = rep(c("Marked vs. Recaptured", "Marked vs. Captured"), each = length(M_lg)))) %>%
  rbind(data.frame(sample = "Captured (n=244)",
                   Length = rep(C_lg, times = 2),
                   test = rep(c("Captured vs. Recaptured", "Marked vs. Captured"), each = length(C_lg)))) %>%
  ggplot(aes(x = Length, color = sample)) +
    stat_ecdf(geom = "step") +
    facet_grid(test ~ .) +
    geom_label(aes(x = x, y = y, label = text),
               data.frame(test = c("Captured vs. Recaptured", "Marked vs. Recaptured", "Marked vs. Captured"),
                          text = c("D = 0.22 \n p = 0.33", "D = 0.27 \n p = 0.16", "D = 0.11 \n p = 0.23"),
                          x = 425, y = .9),
               inherit.aes = FALSE) +
    ylab("Cumulative Relative frequency") +
    theme_bw(base_size = 15)


#sex selectivity
#first event unbiased
R_sex <- dat19$R$sex[!is.na(dat19$R$sex)]
C_sex <- dat19$C$sex[!is.na(dat19$C$sex)]
M_sex <- dat19$M$sex[!is.na(dat19$M$sex)]
CR <- data.frame(strata= c(rep("C", length(C_sex)), rep("R", length(R_sex))),
                 sex = c(C_sex, R_sex))
aslpack::tab_lr(CR, "sex")

#second event unbiased
MR <- data.frame(strata= c(rep("M", length(M_sex)), rep("R", length(R_sex ))),
                 sex = c(M_sex, R_sex))
aslpack::tab_lr(MR, "sex")

#sex comp differs by event
MC <- data.frame(strata= c(rep("M", length(M_sex)), rep("C", length(C_sex ))),
                 sex = c(M_sex, C_sex))
aslpack::tab_lr(MC, "sex")


#Consistancy tests passed
mix_tab <- cbind(table(dat19$R$mark_strata, dat19$R$strata), table(dat19$M$strata) - table(dat19$R$mark_strata))
mix_tab
DescTools::GTest(mix_tab)

eqprop_tab <- rbind(aggregate(Ytag ~ strata, dat19$R, sum)[[2]], table(dat19$C$strata) - aggregate(Ytag ~ strata, dat19$R, sum)[[2]])
eqprop_tab
DescTools::GTest(eqprop_tab)

completemix_tab <- rbind(table(dat19$R$mark_strata), table(dat19$M$strata) - table(dat19$R$mark_strata))
completemix_tab
DescTools::GTest(completemix_tab)


#Estimate
#Pooled Chapman
M <- sum(!is.na(dat19$M$tag)); M
C <- max(dat19$weir$cum)[1]; C
R <- sum(dat19$R$Ytag); R
N <- (M + 1) * (C + 1) / (R + 1) - 1; N
N_se <- sqrt((M + 1) * (C + 1) * (M - R) * (C - R) / (R + 1) ^2 / (R + 2)); N_se
N + N_se * qnorm(c(0.025, 0.975))
N_se*qnorm(0.975)/N

#Note R and C asl sampled at differet rates
Ms <- table(dat19$M$sex); Ms
Cs <- table(dat19$C$sex); Cs
Rs <- table(dat19$R$sex); Rs
Ns <- (Ms + 1) * (Cs + 1) / (Rs + 1) - 1; Ns
sum(Cs/C)
sum(Rs/R)

post_strat19 <- readRDS(".\\models\\post_strat19.rds")
post_strat19
post_strat19$sd$N_all*qnorm(0.975)/post_strat19$q50$N_all

######
#ASL on spawning grounds
#no abundacne by time lets check anyway
#Sex comp, age comp and spawning history do not across time strata
aslpack::tab_lr(dat19$M[!is.na(dat19$M$sex), ], "sex")
aslpack::tab_lr(dat19$M[!is.na(dat19$M$age_s), ], "age_s")
aslpack::tab_lr(dat19$M[!is.na(dat19$M$age_s), ], "spawn")
dat19$M$age2 = ifelse(!is.na(dat19$M$age_s),
                      ifelse(dat19$M$spawn %in% 1, paste0(dat19$M$age_s, "-Initial"), paste0(dat19$M$age_s, "-Repeat")),
                      NA)
aslpack::tab_lr(dat19$M[!is.na(dat19$M$age2), ], "age2")

#Age/Sex comps by trip
asl_sg_19 <-
  lapply(1:2, function(x) {
    dat19$M[dat19$M$strata %in% x, ] %>%
      dplyr::select(age = age_s, sex, length, spawn) %>%
      dplyr::filter(!is.na(sex) & !is.na(age)) %>%
      dplyr::mutate(age = ifelse(spawn %in% 1, paste0(as.character(age), "-Initial"), paste0(as.character(age), "-Repeat"))) %>%
      asl(data.frame(total = 10000, se_total = 0)) %>% #mock abundance to negate FPC
      test(totalname = "drop", output = "asl") %>%
      dplyr::filter(stat_lab != "drop(SE)")}
  )
(sg19_1 <- as.data.frame(asl_sg_19[1]))
(sg19_2 <- as.data.frame(asl_sg_19[2]))

#asl for spawning event
#Sex comp similar umongst age and unaged fish
dat19$M %>% dplyr::mutate(aged = ifelse(!is.na(length) & is.na(age_s), FALSE, TRUE)) %>% ggplot(aes(x = length)) + geom_histogram() + facet_grid(aged~.)
dat19$M %>% dplyr::mutate(aged = ifelse(!is.na(length) & is.na(age_s), FALSE, TRUE)) %>% ggplot(aes(x = sex)) + geom_histogram(stat = "count") + facet_grid(aged~.)

#Use this one. Note sex comps similar despite smaller sample size
sg_asl_tab <-
  dat19$M %>%
  dplyr::select(age = age_s, sex, length, spawn) %>%
  dplyr::filter(!is.na(sex) & !is.na(age)) %>%
  dplyr::mutate(age = ifelse(spawn %in% 1, paste0(age, "-Initial"), paste0(age, "-Repeat"))) %>%
  asl(data.frame(total = post_strat19$mean$N_all, se_total = post_strat19$sd$N_all))
(sg19 <- sg_asl_tab %>% test(totalname = "Spawners", output = "asl"))

# sg_asl_tab <-
#   dat19$M %>%
#   dplyr::select(age = age_s, sex, length, spawn) %>%
#   dplyr::filter(!is.na(sex) & !is.na(age)) %>%
#   dplyr::mutate(age2 = ifelse(spawn %in% 1, paste0(age, "-Initial"), paste0(age, "-Repeat")),
#                 sex2 = sex) %>%
#   asl(data.frame(sex2 = c("M", "F"), total = post_strat19$mean$N, se_total = post_strat19$sd$N), groupvars = "sex2") %>%
#   combine_strata()
# (sg19 <- sg_asl_tab %>% test(totalname = "Spawners", output = "asl"))
#
# #Sex comp for all fish (aged and unaged identical sex comp)
# sl_tab <-
#   dat19$M %>%
#   dplyr::select(age = age_s, sex, length) %>%
#   dplyr::filter(!is.na(sex)) %>%
#   asl(data.frame(total = post_strat19$mean$N_all, se_total = post_strat19$sd$N_all))
# sl_tab %>% test(totalname = "Kelts", output = "sl")



#Age/sex comps at weir
#weir captures whole run
ggplot(dat19$weir, aes(x = as.Date(date), weight = daily)) +
  geom_histogram(binwidth = 1) +
  scale_x_date(date_labels = "%m/%d", date_breaks = "weeks") +
  labs(x = "Date")

#sexes are similarly distributed thru time at weir
ggplot(dat19$C, aes(x = as.Date(date))) +
  geom_histogram(binwidth = 1) +
  scale_x_date(date_labels = "%m/%d", date_breaks = "weeks") +
  labs(x = "Date") +
  facet_grid(sex ~ .)

#But... ASL samples are not representative of emmigration in general
plot_w <- dat19$weir[, c("date", "daily")]
plot_w$source = "Weir count"

plot_age <-
  aggregate(lg_type ~ date, dat19$C, FUN = length) %>%
  setNames(c("date", "daily"))
plot_age$source <- "age samples"

dplyr::left_join(plot_w, plot_age, by = "date") %>%
  dplyr::select(date,
                emmigration = daily.x,
                samples = daily.y) %>%
  dplyr::mutate(samples = ifelse(is.na(samples), 0, samples)) %>%
  aslpack::plot_ks("emmigration")

#But... Sex and age comp are time invariant
#Redefine strata since major difference are early and late
aslpack::tab_lr(dat19$C[!is.na(dat19$C$sex), ], "sex")
aslpack::tab_lr(dat19$C[!is.na(dat19$C$age_s), ], "age_s")
aslpack::tab_lr(dat19$C[!is.na(dat19$C$age_s), ], "spawn")

dat19$C %>% dplyr::mutate(aged = ifelse(!is.na(length) & is.na(age_s), FALSE, TRUE)) %>% ggplot(aes(x = length)) + geom_histogram() + facet_grid(aged~.)
dat19$C %>% dplyr::mutate(aged = ifelse(!is.na(length) & is.na(age_s), FALSE, TRUE)) %>% ggplot(aes(x = sex)) + geom_histogram(stat = "count") + facet_grid(aged~.)

#Sex comp first since unaged fish skew sex comp
sl_tab <-
  dat19$C %>%
  dplyr::select(age = age_s, sex, length) %>%
  dplyr::filter(!is.na(sex)) %>%
  asl(data.frame(total = C, se_total = 0))
sl_tab %>% test(totalname = "Kelts", output = "sl")

weir_sex <- #Emmigration by sex
  sl_tab[sl_tab$age %in% "All", ] %>%
  dplyr::select(sex_strata = sex, total = t.z, se_total = sd_t.z)

age_clean <- #clean data with sex as a stratification varible
  dat19$C %>%
  dplyr::select(age = age_s, sex, length, spawn) %>%
  dplyr::filter(!is.na(age) & !is.na(sex)) %>%
  dplyr::mutate(age2 = ifelse(spawn %in% 1, paste0(age, "-Initial"), paste0(age, "-Repeat")))
age_clean$sex_strata = age_clean$sex

# asl(age_clean, weir_sex, c("sex_strata")) %>%
#   combine_strata() %>%
#   test(totalname = "Kelts", output = "asl", overall_se = 0)

age_clean2 <- age_clean #new dataset which combines age and inital/repeat spawning info in age label
age_clean2$age <- age_clean2$age2
w19 <-
  asl(age_clean2, weir_sex, c("sex_strata")) %>%
    combine_strata() %>%
    test(totalname = "Kelts", output = "asl", overall_se = 0)

WriteXLS::WriteXLS(c("sg19", "sg19_1", "sg19_2", "w19"), "Tables_19.xls")




# #Spawning survival: by initial/repeat spawning for now
# #need to expand recaps for sampling fraction
# #large sample approximation (probably not appropriate)
# psamp <- sum(!is.na(dat19$R$sex)) / sum(dat19$R$Ytag)
# V_psamp <- psamp * (1 - psamp) / (sum(dat19$R$Ytag) - 1)
#
# Rspawn_samp <- table(stringr::str_count(dat19$R$age, "s"))
# Rspawn_obs <- Rspawn_samp / psamp
# #From taylor series expansion. V(R/S) ~ varR/muS^2 -2muR*covRS/muS^3 +muR^2*varS/muS^4
# V_Rspawn_obs <- Rspawn_samp^2 / psamp^4 * V_psamp
# Mspawn_obs <- c(sum(dat19$M$spawn %in% "1"), sum(dat19$M$spawn %in% c("2", "3")))
# Rspawn_obs / Mspawn_obs
# sqrt(V_Rspawn_obs / Mspawn_obs^2)

#Spawning survival: by sex and initial/repeat spawning
#with bootstrap
#Note most of the estiamtes are wide with overlapping CI
fun_Ssurv2 <- function(dat){
  psamp <- sum(!is.na(dat$sex)) / sum(dat$Ytag)

  Rspawn_samp <- as.matrix(table(stringr::str_count(dat$age, "s"), dat$sex))
  if(dim(Rspawn_samp)[1] == 1) Rspawn_samp <- matrix(c(Rspawn_samp, 0, 0), 2, 2, byrow =TRUE)
  Rspawn_obs <- Rspawn_samp / psamp
  Mspawn_obs <- matrix(c(sum(dat19$M$spawn %in% "1" & dat19$M$sex %in% "M"),
                         sum(dat19$M$spawn %in% "1" & dat19$M$sex %in% "F"),
                         sum(dat19$M$spawn %in% c("2", "3") & dat19$M$sex %in% "M"),
                         sum(dat19$M$spawn %in% c("2", "3") & dat19$M$sex %in% "F")), 2, 2, byrow = TRUE)
  ifelse(Rspawn_obs / Mspawn_obs > 1, 1, Rspawn_obs / Mspawn_obs)
}
SsurvL <- rep(list(matrix(NA, 2,2)), times = 1000)
for(i in 1:1000){
  dat <- dat19$R[sample(1:dim(dat19$R)[1], size = dim(dat19$R)[1], replace = TRUE), ]
  SsurvL[[i]] <- fun_Ssurv2(dat)
}
Ssurv <- data.frame(M_initial = rep(NA, 1000),
                    F_initial = rep(NA, 1000),
                    M_repeat = rep(NA, 1000),
                    F_repeat = rep(NA, 1000))
for(i in 1:1000) Ssurv[i,1] <- SsurvL[[i]][1,1]
for(i in 1:1000) Ssurv[i,2] <- SsurvL[[i]][1,2]
for(i in 1:1000) Ssurv[i,3] <- SsurvL[[i]][2,1]
for(i in 1:1000) Ssurv[i,4] <- SsurvL[[i]][2,2]
# small sample sizes
table(stringr::str_count(dat19$R$age, "s"), dat19$R$sex)
table(stringr::str_count(dat19$M$age, "s"), dat19$M$sex)

# estimates
apply(Ssurv, 2, quantile, probs = c(0, 0.025, 0.5, 0.975, 1))
apply(Ssurv, 2, mean)
apply(Ssurv, 2, sd)

#Spawning survival: by initial/repeat spawning
#with bootstrap
fun_Ssurv <- function(dat){
  psamp <- sum(!is.na(dat$sex)) / sum(dat$Ytag)

  Rspawn_samp <- as.vector(table(stringr::str_count(dat$age, "s")))
  if(length(Rspawn_samp) == 1) Rspawn_samp <- c(Rspawn_samp, 0)
  Rspawn_obs <- Rspawn_samp / psamp
  Mspawn_obs <- c(sum(dat19$M$spawn %in% "1"), sum(dat19$M$spawn %in% c("2", "3")))
  ifelse(Rspawn_obs / Mspawn_obs >1, 1, Rspawn_obs / Mspawn_obs)
}
Ssurv <- matrix(NA, nrow = 1000, ncol = 2)
for(i in 1:1000){
  dat <- dat19$R[sample(1:dim(dat19$R)[1], size = dim(dat19$R)[1], replace = TRUE), ]
  Ssurv[i, ] <- fun_Ssurv(dat)
}
apply(Ssurv, 2, quantile, probs = c(0, 0.05, 0.5, 0.95, 1))
apply(Ssurv, 2, mean)
apply(Ssurv, 2, sd)

#Spawning survival: by sex
#with bootstrap
fun_Ssurv <- function(dat){
  psamp <- sum(!is.na(dat$sex)) / sum(dat$Ytag)

  Rspawn_samp <- as.vector(table(dat$sex))
  if(length(Rspawn_samp) == 1) Rspawn_samp <- c(Rspawn_samp, 0)
  Rspawn_obs <- Rspawn_samp / psamp
  Mspawn_obs <- table(dat19$M$sex)
  ifelse(Rspawn_obs / Mspawn_obs >1, 1, Rspawn_obs / Mspawn_obs)
}
Ssurv <- matrix(NA, nrow = 1000, ncol = 2)
for(i in 1:1000){
  dat <- dat19$R[sample(1:dim(dat19$R)[1], size = dim(dat19$R)[1], replace = TRUE), ]
  Ssurv[i, ] <- fun_Ssurv(dat)
}
apply(Ssurv, 2, quantile, probs = c(0, 0.05, 0.5, 0.95, 1))
apply(Ssurv, 2, mean)
apply(Ssurv, 2, sd)

#OVerall Spawning survival
#samples sizes too small to break down
S <- R / M; S
seS <- sqrt(S * (1 - S) / (M -1)); seS
binom::binom.confint(R, M, conf.level = 0.95, method = "wilson")



C_age0 <- dat19$C[!is.na(dat19$C$age_s), ]
C_age <- ifelse(C_age0$spawn %in% 1, paste0(C_age0$age_s, "-Initial"), paste0(C_age0$age_s, "-Repeat"))
M_age0 <- dat19$M[!is.na(dat19$M$age_s), ]
M_age <- ifelse(M_age0$spawn %in% 1, paste0(M_age0$age_s, "-Initial"), paste0(M_age0$age_s, "-Repeat"))
#saltwater age tests: no differences between events
MC <- data.frame(strata= c(rep("M", length(M_age)), rep("C", length(C_age))),
                 age_s = c(M_age, C_age))
aslpack::tab_lr(MC, "age_s", )



#Replicate figure 6 from FDS 97-6
plot_R <-
  aggregate(Ytag ~ date, dat19$R, FUN = sum) %>%
  setNames(c("date", "daily"))
plot_R$source <- "recaps"

dplyr::left_join(plot_w, plot_R, by = "date") %>%
  dplyr::mutate(unmarked = daily.x  - ifelse(is.na(daily.y), 0, daily.y)) %>%
  dplyr::select(date, marked = daily.y, unmarked) %>%
  tidyr::gather(group, daily, -date) %>%
  dplyr::mutate(week = gsub("[0-9]+-([0-9]+-[0-9]+)", "\\1", cut(date, "weeks"))) %>%
  dplyr::group_by(week, group) %>%
  dplyr::summarise(weekly = sum(daily, na.rm = TRUE)) %>%
  ggplot(aes(x = week, y = weekly, fill = group)) +
  geom_col(position = position_stack(reverse = TRUE)) +
  scale_fill_manual(values = c("marked" = "black", "unmarked" = "gray"))

#Alternative to prior plot
# rbind(plot_R, plot_w) %>% # rbind(rbind(plot_R, plot_w), plot_age) to include age
#   ggplot(aes(x = date, y = daily)) +
#   geom_col() +
#   facet_grid(source ~ ., scales = "free_y")


#Replicate figure 8 from FDS 97-6
weekly_sex <-
  dat19$C %>%
  dplyr::filter(!is.na(sex)) %>%
  dplyr::mutate(week = gsub("[0-9]+-([0-9]+-[0-9]+)", "\\1", cut(date, "weeks"))) %>%
  dplyr::group_by(week, sex) %>%
  dplyr::summarise(weekly = length(sex))

ggplot(weekly_sex, aes(x = week, y = weekly, fill = sex)) +
  geom_bar(position = position_fill(reverse = TRUE), stat = "identity") +
  scale_fill_manual(values = c("M" = "black", "F" = "gray")) +
  geom_text(data = aggregate(weekly ~ week, weekly_sex, FUN = sum),
            aes(x = week, y = 1, label = weekly),
            inherit.aes = FALSE,
            vjust = -.25)
chisq.test(weekly_sex$sex, weekly_sex$weekly)


#Length composition by total age
rbind(dat19$C[, c("length", "age_f", "age_s", "age_t", "spawn")], dat19$M[, c("length", "age_f", "age_s", "age_t", "spawn")]) %>%
  dplyr::filter(!is.na(age_f) & !is.na(length)) %>%
  dplyr::mutate(lg_g = cut(length, breaks = seq(0, 1000, 25))) %>%
  ggplot(aes(x = lg_g, fill = spawn)) +
  geom_bar() +
  facet_grid(age_t ~ .)

rbind(dat19$C[, c("length", "age_f", "age_s", "age_t", "spawn")], dat19$M[, c("length", "age_f", "age_s", "age_t", "spawn")]) %>%
  dplyr::filter(!is.na(age_s) & !is.na(length)) %>%
  dplyr::mutate(lg_g = cut(length, breaks = seq(0, 1000, 25))) %>%
  ggplot(aes(x = lg_g, fill = spawn)) +
  geom_bar() +
  facet_grid(age_s ~ .)

