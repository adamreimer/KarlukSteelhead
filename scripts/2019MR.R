library(magrittr)
library(ggplot2)
library(aslpack)
load(".\\data\\dat19.rda")

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
R_lg <- dat19$R$length[!is.na(dat19$R$length)]
C_lg <- dat19$C$length[!is.na(dat19$C$length)]
M_lg <- dat19$M$length[!is.na(dat19$M$length)]
par(mfrow = c(2,2))
test_ks(C_lg, R_lg, "C vs. R")
test_ks(M_lg, R_lg, "M vs. R")
test_ks(M_lg, C_lg, "M vs. C")
par(mfrow = c(1,1))

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

MC <- data.frame(strata= c(rep("M", length(M_sex)), rep("C", length(C_sex ))),
                 sex = c(M_sex, C_sex))
aslpack::tab_lr(MC, "sex")

#Consistancy tests passed
mix_tab <- cbind(table(dat19$R$mark_strata, dat19$R$strata), table(dat19$M$strata) - table(dat19$R$mark_strata))
mix_tab
DescTools::GTest(mix_tab)

eqprop_tab <- rbind(table(dat19$R$strata), table(dat19$C$strata) - table(dat19$R$strata))
eqprop_tab
DescTools::GTest(eqprop_tab)

completemix_tab <- rbind(table(dat19$R$mark_strata), table(dat19$M$strata) - table(dat19$R$mark_strata))
completemix_tab
DescTools::GTest(completemix_tab)

#Estimate
#Pooled Chapman is OK
M <- sum(!is.na(dat19$M$tag)); M
C <- max(dat19$weir$cum)[1]; C
R <- sum(dat19$R$Ytag); R
N <- (M + 1) * (C + 1) / (R + 1) - 1; N
N_se <- sqrt((M + 1) * (C + 1) * (M - R) * (C - R) / (R + 1) ^2 / (R + 2)); N_se


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
C19 <- dat19$C #new dataframe since aslpack::tab_lr looks for a varible named "strata"
C19$strata <- cut(dat19$C$date,
                       quantile(rep(dat19$weir$date, times = dat19$weir$daily), c(0, .5, 1)),
                       labels = FALSE)
aslpack::tab_lr(C19[!is.na(C19$sex), ], "sex")
aslpack::tab_lr(C19[!is.na(C19$age_s), ], "age_s")

#Convert aslpack::tab_asl to accept non-euro ages
test <-
  function(dat,
           totalname = "Total",
           output = "asl",
           display_cols = NULL,
           gather_cols = c("n.z", "p.z", "ci_p.z", "t.z", "lg.z", "range_lg.z"),
           age_labels = NULL,
           overall_se = NULL){
    stopifnot(!(display_cols %in% c(NULL, "stats")))

    if(!is.null(overall_se)) dat$sd_t.z[dat$age == "All" & dat$sex == "Both"] = overall_se
    if(is.null(age_labels)) age_labels <- c(unique(dat$age)[-length(unique(dat$age))], "All ages")

    temp <-
      dat %>%
      dplyr::mutate_at(c("p.z", "sd_p.z", "lci_p.z", "uci_p.z"), list(~ trimws(format(round(., 3), nsmall = 3)))) %>%
      dplyr::mutate_at(c("lg.z", "se_lg.z"), list(~ trimws(format(round(., 1), big.mark = ",", nsmall = 1)))) %>%
      dplyr::mutate_at(c("n.z", "min_lg.z", "max_lg.z"), as.integer) %>%
      dplyr::mutate(p.z = paste0(p.z, " (", ifelse(sex =="Both" & age == "All", 0, sd_p.z), ")"),
                    ci_p.z = ifelse(sex =="Both" & age == "All", "", paste0(lci_p.z, " - ", uci_p.z)),
                    t.z = paste0(trimws(format(round(t.z, 0), big.mark = ",", nsmall = 0)),
                                 " (",
                                 trimws(format(round(sd_t.z, 0), big.mark = ",", nsmall = 0)),
                                 ")"),
                    lg.z = paste0(lg.z, " (", se_lg.z, ")"),
                    range_lg.z = paste0("(", min_lg.z, "-", max_lg.z, ")")) %>%
      dplyr::select(sex, age, gather_cols) %>%
      tidyr::gather_("measure", "value", gather_cols = gather_cols) %>%
      dplyr::mutate(sex_lab = factor(sex, levels = c("F",       "M",     "Both"),
                                     labels = c("Females", "Males", "Both Sexes"),
                                     ordered = TRUE, exclude = NULL),
                    stat_lab = factor(measure, levels = c("n.z",
                                                          "p.z",
                                                          "ci_p.z",
                                                          "t.z",
                                                          "lg.z",
                                                          "range_lg.z"),
                                      labels = c("Sample size",
                                                 "Proportion (SE)",
                                                 "95% CI(Proportion)",
                                                 paste0(totalname, "(SE)"),
                                                 "Mean Length (SE)",
                                                 "Range(Length)"),
                                      ordered = TRUE),
                    age_lab = factor(age,
                                     levels = unique(dat$age),
                                     labels = age_labels,
                                     ordered = TRUE, exclude = NULL)) %>%
      dplyr::select(sex_lab, stat_lab, age_lab, value)

    switch(output,
           "al" = temp %>%
             dplyr::filter(sex_lab == "Both Sexes") %>%
             dplyr::select(-sex_lab) %>%
             tidyr::spread(if(is.null(display_cols)) age_lab else(stat_lab), value),
           "sl" = temp %>%
             dplyr::filter(age_lab == "All ages") %>%
             dplyr::select(-age_lab) %>%
             tidyr::spread(if(is.null(display_cols)) sex_lab else(stat_lab), value),
           "asl" = temp %>% tidyr::spread(if(is.null(display_cols)) age_lab else(stat_lab), value))
  }


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

asl(age_clean, weir_sex, c("sex_strata")) %>%
  combine_strata() %>%
  test(totalname = "Kelts", output = "asl", overall_se = 0)

age_clean2 <- age_clean #new dataset which combines age and inital/repeat spawning info in age label
age_clean2$age <- age_clean2$age2
asl(age_clean2, weir_sex, c("sex_strata")) %>%
  combine_strata() %>%
  test(totalname = "Kelts", output = "asl", overall_se = 0)


######
#ASL on spawning grounds
dat19$M %>%
  dplyr::select(age = age_s, sex, length) %>%
  dplyr::filter(!is.na(sex)) %>%
  asl(data.frame(total = N, se_total = N_se)) %>%
  test(totalname = "Spawners", output = "asl")


#Length composition by total age
# dat19$M %>%
#   dplyr::filter(!is.na(age_f) & !is.na(length)) %>%
#   dplyr::mutate(lg_g = cut(length, breaks = seq(0, 1000, 25))) %>%
#   ggplot(aes(x = lg_g, fill = spawn)) +
#   geom_bar() +
#   facet_grid(age_t ~ .)
#
# dat19$C %>%
#   dplyr::filter(!is.na(age_f) & !is.na(length)) %>%
#   dplyr::mutate(lg_g = cut(length, breaks = seq(0, 1000, 25))) %>%
#   ggplot(aes(x = lg_g, fill = spawn)) +
#   geom_bar() +
#   facet_grid(age_t ~ .)

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

#Spawning survival
#need to expand recaps for sampling fraction
psamp <- sum(!is.na(dat19$R$sex)) / sum(dat19$R$Ytag)
V_psamp <- psamp * (1 - psamp) / (sum(dat19$R$Ytag) - 1)

Rspawn_samp <- table(stringr::str_count(dat19$R$age, "s"))
Rspawn_obs <- Rspawn / psamp
#From taylor series expansion. V(R/S) ~ varR/muS^2 -2muR*covRS/muS^3 +muR^2*varS/muS^4
V_Rspawn_obs <- Rspawn^2 / psamp^4 * V_psamp
Mspawn_obs <- c(sum(dat19$M$spawn %in% "1"), sum(dat19$M$spawn %in% c("2", "3")))
Rspawn_obs / Mspawn_obs
sqrt(V_Rspawn_obs / Mspawn_obs^2)
