library(magrittr)
M18 <-
  readxl::read_excel(".\\data\\data_raw\\Steelhead 2018.xlsx",
                     range = "Tagged Steelheads!A2:I142",
                     col_names = c("date", "card", "fish", "tag", "finclip", "sex", "length", "lgtype", "age"), #age breakdowns recreated in code
                     na = "-") %>%
  tidyr::fill(c("date"))
lapply(M18, table, useNA = "ifany")
#define strata
Mbreaks <- quantile(M18$date, probs = c(0, .5, 1))
Mbreaks
lubridate::day(Mbreaks[2]) <- 9
M18$strata <- cut(M18$date, Mbreaks, include.lowest = TRUE, labels = FALSE)
#standardize data
M18$sex <- ifelse(M18$sex == 1, "M", "F")
M18$age <- ifelse(M18$age %in% c("r", "m"), NA, M18$age)
M18$age_f <- as.numeric(ifelse(gsub("\\..*", "", M18$age) == "x", NA, gsub("\\..*", "", M18$age))) + 1
M18$age_s <- sapply(stringr::str_match_all(gsub(".*\\.", "", M18$age), "[0-9]+"), function(x) sum(as.numeric(x)))
M18$age_t <- M18$age_f + M18$age_s
M18$spawn <- as.character(stringr::str_count(M18$age, "s"))

#Check total ages
table(M18$age, M18$age_t, useNA = "ifany")
#check salt ages
table(M18$age_s, M18$spawn, useNA = "ifany")

M18$meft <- ifelse(M18$lgtype == "ME-F", as.numeric(M18$length),
                   ifelse(M18$lgtype == "ME-F/FL", as.numeric(gsub("/.*", "", M18$length)), NA))
M18$fl <- ifelse(M18$lgtype == "FL", as.numeric(M18$length),
                 ifelse(M18$lgtype == "ME-F/FL", as.numeric(gsub(".*/", "", M18$length)), NA))
plot(M18$fl, M18$meft)
mod_lg <- lm(fl~meft, data = M18)
summary(mod_lg)
par(mfrow = c(2,2))
plot(mod_lg)
par(mfrow = c(1,1))
M18$fl[M18$lgtype %in% "ME-F"] <- predict(mod_lg, newdata = data.frame(meft = M18$meft[M18$lgtype %in% "ME-F"]))

lapply(M18, table, useNA = "ifany")





weir_daily18 <-
  readxl::read_excel(".\\data\\data_raw\\Copy of 2018 kelt counts.xlsx",
                     range = "Daily_cumulative_Karluk-Steelhe!A2:C108",
                     col_names = c("date", "daily", "cum"))
weir_daily18
lubridate::year(weir_daily18$date) <- 2018
Cbreaks <- quantile(rep(weir_daily18$date, times = weir_daily18$daily)); Cbreaks
range(weir_daily18$date)
lubridate::month(Cbreaks[5]) <- 9
lubridate::day(Cbreaks[5]) <- 5
Cbreaks
weir_daily18$strata <- cut(weir_daily18$date, Cbreaks, labels = FALSE)
lapply(weir_daily18, table, useNA = "ifany")



###
#Don't read recap data since sample is biased
#techs only notes markes on fish in the trap and ignored marks on mirgating fish
###


C18 <-
readxl::read_excel(".\\data\\data_raw\\STEELHEAD 2018.xlsx",
                   range = "WeirASL!A2:J177",
                   col_names = c("date", "card", "fish", "vial", "lgtype", "tag", "finclip", "sex", "length", "age"),
                   col_types = c("date", rep("numeric", 3), "text", "numeric", "text", rep("numeric", 2), "text")) %>%
  tidyr::fill(c("date")) %>%
  dplyr::mutate(date = as.POSIXct(date,format = "%m/%d/%y"),
                strata = cut(date, Cbreaks, labels = FALSE))
lapply(C18, table, useNA = "always")
#standardize data
C18$sex <- ifelse(C18$sex == 1, "M", "F")
C18$age <- ifelse(C18$age %in% c("r", "m"), NA, C18$age)
C18$age_f <- as.numeric(ifelse(gsub("\\..*", "", C18$age) == "x", NA, gsub("\\..*", "", C18$age))) + 1
C18$age_s <- sapply(stringr::str_match_all(gsub(".*\\.", "", C18$age), "[0-9]+"), function(x) sum(as.numeric(x)))
C18$age_t <- C18$age_f + C18$age_s
C18$spawn <- as.character(stringr::str_count(C18$age, "s"))

#Check total ages
table(C18$age, C18$age_t, useNA = "ifany")
#check salt ages
table(C18$age_s, C18$spawn, useNA = "ifany")

lapply(C18, table, useNA = "ifany")




dat18 <- list(M = M18, C = C18, weir = weir_daily18)
save(dat18, file = ".\\data\\dat18.rda")

temp <-
  weir_daily18 %>%
  dplyr::select(-strata) %>%
  dplyr::mutate(year = format(date, "%Y"),
                date = as.Date(paste0(format(date, "%m"), "-", format(date, "%d"), "-", year), "%m-%d-%Y"),
                jday = as.numeric(format(date, "%j")),
                week = format(date, "%U"))
load(".\\data\\weir_daily.rda")
weir_daily <- rbind(weir_daily[!weir_daily$year == 2018,], temp) %>% dplyr::arrange(date)
save(weir_daily, file = ".\\data\\weir_daily.rda")

