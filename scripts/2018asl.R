library(magrittr)
library(ggplot2)
library(aslpack)
load(".\\data\\dat18.rda")
source(".\\scripts\\utils.R")

#Number sampled
length(unique(dat18$M$tag))
table(dat18$M$strata)

######
#ASL on spawning grounds
#no abundacne byt lets check anyway
#Sex comp, age comp and spawning history do not across time strata
aslpack::tab_lr(dat18$M[!is.na(dat18$M$sex), ], "sex")
aslpack::tab_lr(dat18$M[!is.na(dat18$M$age_s), ], "age_s")
aslpack::tab_lr(dat18$M[!is.na(dat18$M$age_s), ], "spawn")

#Sex comps
dat18$M %>%
  dplyr::select(age = age_s, sex, length = fl) %>%
  dplyr::filter(!is.na(sex)) %>%
  dplyr::mutate(age = as.character(age)) %>%
  asl(data.frame(total = 10000, se_total = 0)) %>% #mock abundance to negate FPC
  test(totalname = "Spawners", output = "sl")


#Age/Sex comps by trip
#Use these. Note sex comps similar despite smaller sample size
sg18 <-
  dat18$M %>%
    dplyr::select(age = age_s, sex, length = fl, spawn) %>%
    dplyr::filter(!is.na(sex) & !is.na(age)) %>%
    dplyr::mutate(age = ifelse(spawn %in% 1, paste0(as.character(age), "-Initial"), paste0(as.character(age), "-Repeat"))) %>%
    asl(data.frame(total = 10000, se_total = 0)) %>% #mock abundance to negate FPC
    test(totalname = "drop", output = "asl") %>%
    dplyr::filter(stat_lab != "drop(SE)")

dat18$M %>% dplyr::mutate(aged = ifelse(!is.na(length) & is.na(age_s), FALSE, TRUE)) %>% ggplot(aes(x = sex)) + geom_histogram(stat = "count") + facet_grid(aged~.)
table(dat18$M$sex[is.na(dat18$M$age)])
table(dat18$M$sex[!is.na(dat18$M$age)])
chisq.test(matrix(c(13, 80, 13, 34), nrow = 2))


######Age/sex comps at weir
#weir captures whole run
ggplot(dat18$weir, aes(x = as.Date(date), weight = daily)) +
  geom_histogram(binwidth = 1) +
  scale_x_date(date_labels = "%m/%d", date_breaks = "weeks") +
  labs(x = "Date")

#sexes are similarly distributed thru time at weir
ggplot(dat18$C, aes(x = as.Date(date))) +
  geom_histogram(binwidth = 1) +
  scale_x_date(date_labels = "%m/%d", date_breaks = "weeks") +
  labs(x = "Date") +
  facet_grid(sex ~ .)

#But... ASL samples are not representative of emmigration in general
plot_w <- dat18$weir[, c("date", "daily")]
plot_w$source = "Weir count"
plot_w$date <- as.Date(plot_w$date)

plot_age <-
  aggregate(strata ~ date, dat18$C, FUN = length) %>%
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

#Sex comp, age comp and spawning history do not differ across time strata
aslpack::tab_lr(dat18$C[!is.na(dat18$C$sex), ], "sex")
aslpack::tab_lr(dat18$C[!is.na(dat18$C$age_s), ], "age_s")
aslpack::tab_lr(dat18$C[!is.na(dat18$C$age_s), ], "spawn")

#Unaged fish and aged fish have similar sex comp
dat18$C %>% dplyr::mutate(aged = ifelse(!is.na(length) & is.na(age_s), FALSE, TRUE)) %>% ggplot(aes(x = length)) + geom_histogram() + facet_grid(aged~.)
dat18$C %>% dplyr::mutate(aged = ifelse(!is.na(length) & is.na(age_s), FALSE, TRUE)) %>% ggplot(aes(x = sex)) + geom_histogram(stat = "count") + facet_grid(aged~.)
table(dat18$C$sex, useNA = "ifany")
table(dat18$C$sex[is.na(dat18$C$age)])
table(dat18$C$sex[!is.na(dat18$C$age)])
DescTools::GTest(matrix(c(10, 99, 9, 55), nrow = 2))


#No differences, simple ASL
age_clean <- #clean data with sex as a stratification varible
  dat18$C %>%
  dplyr::select(age = age_s, sex, length, spawn) %>%
  dplyr::filter(!is.na(age) & !is.na(sex)) %>%
  dplyr::mutate(age = as.character(age),
                age2 = ifelse(spawn %in% 1, paste0(age, "-Initial"), paste0(age, "-Repeat")))
age_clean2 <- age_clean #new dataset which combines age and inital/repeat spawning info in age label
age_clean2$age <- age_clean2$age2

#weir age comp by saltwater age and spawning history
weir18 <-
  asl(age_clean2, data.frame(total = sum(dat18$weir$daily), se_total = 0)) %>%
  test(totalname = "Kelts", output = "asl")

WriteXLS::WriteXLS(c("sg18", "weir18"), "Tables_18.xls")



#Olderfish more prevelent in spawning population than kelt population
C_age0 <- dat18$C[!is.na(dat18$C$age_s), ]
C_age <- ifelse(C_age0$spawn %in% 1, paste0(C_age0$age_s, "-Initial"), paste0(C_age0$age_s, "-Repeat"))
M_age0 <- dat18$M[!is.na(dat18$M$age_s), ]
M_age <- ifelse(M_age0$spawn %in% 1, paste0(M_age0$age_s, "-Initial"), paste0(M_age0$age_s, "-Repeat"))

#saltwater age tests: differences between events
MC <- data.frame(strata= c(rep("M", length(M_age)), rep("C", length(C_age))),
                 age_s = c(M_age, C_age))
aslpack::tab_lr(MC, "age_s", )

#Initial/repeat spawning tests: differences between events
MC2 <- data.frame(strata= c(rep("M", length(M_age0$spawn)), rep("C", length(C_age0$spawn))),
                  spawn = c(M_age0$spawn, C_age0$spawn))
aslpack::tab_lr(MC2, "spawn", )
