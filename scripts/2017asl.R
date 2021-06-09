library(magrittr)
library(ggplot2)
library(aslpack)
load(".\\data\\dat17.rda")
source(".\\scripts\\utils.R")

#Number sampled
length(unique(dat17$M$tag))
table(dat17$M$strata)

#ASL on spawning grounds
#Sex comp varies by sampling trip, age comp and spawning history do not
aslpack::tab_lr(dat17$M[!is.na(dat17$M$sex), ], "sex")
aslpack::tab_lr(dat17$M[!is.na(dat17$M$age_s), ], "age_s")
aslpack::tab_lr(dat17$M[!is.na(dat17$M$age_s), ], "spawn")

#Sex comps by trip
lapply(1:2, function(x) {
  dat17$M[dat17$M$strata == x, ] %>%
    dplyr::select(age = age_s, sex, length) %>%
    dplyr::filter(!is.na(sex)) %>%
    dplyr::mutate(age = as.character(age)) %>%
    asl(data.frame(total = 10000, se_total = 0)) %>% #mock abundance to negate FPC
    test(totalname = "Spawners", output = "sl")}
)

#Age/Sex comps by trip
#Use these. Note sex comps similar despite smaller sample size
asl_sg_17 <-
  lapply(1:2, function(x) {
    dat17$M[dat17$M$strata %in% x, ] %>%
      dplyr::select(age = age_s, sex, length, spawn) %>%
      dplyr::filter(!is.na(sex) & !is.na(age)) %>%
      dplyr::mutate(age = ifelse(spawn %in% 1, paste0(as.character(age), "-Initial"), paste0(as.character(age), "-Repeat"))) %>%
      asl(data.frame(total = 10000, se_total = 0)) %>% #mock abundance to negate FPC
      test(totalname = "drop", output = "asl") %>%
      dplyr::filter(stat_lab != "drop(SE)")}
)
sg17_1 <- as.data.frame(asl_sg_17[1]);
sg17_2 <- as.data.frame(asl_sg_17[2]);

dat17$M %>% dplyr::mutate(aged = ifelse(!is.na(length) & is.na(age_s), FALSE, TRUE)) %>% ggplot(aes(x = sex)) + geom_histogram(stat = "count") + facet_grid(aged~.)
table(dat17$M$sex[is.na(dat17$M$age)])
table(dat17$M$sex[!is.na(dat17$M$age)])
chisq.test(matrix(c(10, 44, 5, 16), nrow = 2))


#Age/sex comps at weir
#weir captures whole run
load(".\\data\\weir_daily.rda")
dplyr::filter(weir_daily, year %in% 2017:2019) %>%
  dplyr::mutate(date2 = as.Date(paste0("2017-",format(date, "%j")), "%Y-%j")) %>%
  ggplot(aes(x = date2, weight = daily)) +
    geom_histogram(stat = "count") +
    scale_x_date(date_labels = "%m/%d", date_breaks = "weeks") +
    labs(x = "Date", y = "Number of fish") +
    ggtitle("Karluk Weir Kelt Count") +
    facet_grid(year ~ .) +
    theme_bw(base_size = 16)

#sexes are similarly distributed thru time at weir
ggplot(dat17$C, aes(x = as.Date(date))) +
  geom_histogram(binwidth = 1) +
  scale_x_date(date_labels = "%m/%d", date_breaks = "weeks") +
  labs(x = "Date") +
  facet_grid(sex ~ .)

#But... ASL samples are not representative of emmigration in general
plot_w <- dat17$weir[, c("date", "daily")]
plot_w$source = "Weir count"
plot_w$date <- as.Date(plot_w$date)

plot_age <-
  aggregate(strata ~ date, dat17$C, FUN = length) %>%
  setNames(c("date", "daily"))
plot_age$source <- "age samples"
plot_age$date <- as.Date(plot_age$date)

dplyr::left_join(plot_w, plot_age, by = "date") %>%
  dplyr::select(date,
                emmigration = daily.x,
                samples = daily.y) %>%
  dplyr::mutate(samples = ifelse(is.na(samples), 0, samples),
                date = as.POSIXct(date)) %>%
  aslpack::plot_ks("emmigration")

#Sex comp, age comp and spawning history do not across time strata
aslpack::tab_lr(dat17$C[!is.na(dat17$C$sex), ], "sex")
aslpack::tab_lr(dat17$C[!is.na(dat17$C$age_s), ], "age_s")
aslpack::tab_lr(dat17$C[!is.na(dat17$C$age_s), ], "spawn")

#length and sex comp of aged and unaged fish
dat17$C %>% dplyr::mutate(aged = ifelse(!is.na(length) & is.na(age_s), FALSE, TRUE)) %>% ggplot(aes(x = length)) + geom_histogram() + facet_grid(aged~.)
dat17$C %>% dplyr::mutate(aged = ifelse(!is.na(length) & is.na(age_s), FALSE, TRUE)) %>% ggplot(aes(x = sex)) + geom_histogram(stat = "count") + facet_grid(aged~.)
table(dat17$C$sex, useNA = "ifany")
table(dat17$C$sex[is.na(dat17$C$age)])
table(dat17$C$sex[!is.na(dat17$C$age)])
DescTools::GTest(matrix(c(25, 217, 40, 128), nrow = 2))


#Sex comp first in include unaged fish
sl_tab <-
  dat17$C %>%
  dplyr::select(age = age_s, sex, length) %>%
  dplyr::filter(!is.na(sex)) %>%
  dplyr::mutate(age = as.character(age)) %>%
  asl(data.frame(total = sum(dat17$weir$daily), se_total = 0))
sl_tab %>% test(totalname = "Kelts", output = "sl")

weir_sex <- #Emmigration by sex
  sl_tab[sl_tab$age %in% "All", ] %>%
  dplyr::select(sex2 = sex, total = t.z, se_total = sd_t.z)

age_clean <- #clean data with sex as a stratification varible
  dat17$C %>%
  dplyr::select(age = age_s, sex, length, spawn) %>%
  dplyr::filter(!is.na(age) & !is.na(sex)) %>%
  dplyr::mutate(age = as.character(age),
                age2 = ifelse(spawn %in% 1, paste0(age, "-Initial"), paste0(age, "-Repeat")))
age_clean$sex2 = age_clean$sex

#wier age comp by saltwater age
# asl(age_clean, weir_sex, c("sex_strata")) %>%
#   combine_strata() %>%
#   test(totalname = "Kelts", output = "asl", overall_se = 0)

#wwir age comp by saltwater age and spawning history
age_clean2 <- age_clean #new dataset which combines age and inital/repeat spawning info in age label
age_clean2$age <- age_clean2$age2
w17 <-
  asl(age_clean2, weir_sex, c("sex2")) %>%
    combine_strata2() %>%
    test(totalname = "Kelts", output = "asl", overall_se = 0)

w17 <- w17[!(w17$stat_lab == "95% CI(Proportion)"), ]
WriteXLS::WriteXLS(c("sg17_1", "sg17_2", "w17"), "Tables_17.xls")



#Older/repeat spawing fish more prevelent in spawning population than kelt population
C_age0 <- dat17$C[!is.na(dat17$C$age_s), ]
C_age <- ifelse(C_age0$spawn %in% 1, paste0(C_age0$age_s, "-Initial"), paste0(C_age0$age_s, "-Repeat"))
M_age0 <- dat17$M[!is.na(dat17$M$age_s), ]
M_age <- ifelse(M_age0$spawn %in% 1, paste0(M_age0$age_s, "-Initial"), paste0(M_age0$age_s, "-Repeat"))

#saltwater age tests: differences between events
MC <- data.frame(strata= c(rep("M", length(M_age)), rep("C", length(C_age))),
                 age_s = c(M_age, C_age))
aslpack::tab_lr(MC, "age_s", )

#Initial/repeat spawning tests: differences between events
MC2 <- data.frame(strata= c(rep("M", length(M_age0$spawn)), rep("C", length(C_age0$spawn))),
                  spawn = c(M_age0$spawn, C_age0$spawn))
aslpack::tab_lr(MC2, "spawn", )

