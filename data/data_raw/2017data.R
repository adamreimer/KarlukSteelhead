library(magrittr)
M17 <-
  readxl::read_excel(".\\data\\data_raw\\STEELHEAD2017.xlsx",
                     range = "Tagged fish!A2:H77",
                     col_names = c("date", "card", "fish", "tag", "finclip", "sex", "length", "age")) %>% #age breakdowns recreated in code
  tidyr::fill(c("date", "card"))
lapply(M17, table)
#define strata
Mbreaks <- quantile(M17$date, probs = c(0, .5, 1))
Mbreaks
lubridate::day(Mbreaks[2]) <- 7
M17$strata <- cut(M17$date, Mbreaks, include.lowest = TRUE, labels = FALSE)
#standardize data
M17$finclip <- ifelse(M17$finclip == "X", NA, M17$finclip)
M17$sex <- ifelse(M17$sex == 1, "M", "F")
M17$age <- ifelse(M17$age %in% c("r", "m"), NA, M17$age)
M17$age_f <- as.numeric(ifelse(gsub("\\..*", "", M17$age) == "x", NA, gsub("\\..*", "", M17$age))) + 1
M17$age_s <- sapply(stringr::str_match_all(gsub(".*\\.", "", M17$age), "[0-9]+"), function(x) sum(as.numeric(x)))
M17$age_t <- M17$age_f + M17$age_s
M17$spawn <- as.character(stringr::str_count(M17$age, "s"))

#Check total ages
table(M17$age, M17$age_t, useNA = "ifany")
#check salt ages
table(M17$age_s, M17$spawn, useNA = "ifany")

lapply(M17, table, useNA = "ifany")




weir_daily17 <-
  readxl::read_excel(".\\data\\data_raw\\STEELHEAD2017.xlsx",
                     range = "Escapement!A3:C112") %>%
  setNames(tolower(names(.)))
weir_daily17
lubridate::year(weir_daily17$date) <- 2017
Cbreaks <- quantile(rep(weir_daily17$date, times = weir_daily17$daily)); Cbreaks
range(weir_daily17$date)
lubridate::day(Cbreaks[5]) <- 10
Cbreaks
weir_daily17$strata <- cut(weir_daily17$date, Cbreaks, labels = FALSE)
table(weir_daily17$strata)


###
#Don't read in recover data since weir crew failed to record recoveries not inside the trap.
# R17 <-
#   readxl::read_excel(".\\data\\data_raw\\STEELHEAD2017.xlsx",
#                      range = "Recoveries!A3:G23",
#                      col_names = c("date", "tag", "length", "finclip", "comment"),
#                      col_types = c("date", "numeric", "skip", "numeric", "text", "skip", "text"),
#                      na = c("", "n/a", "?")) %>%
#   tidyr::fill("date") %>%
#   dplyr::left_join(M17[, c("date", "tag", "length", "sex", "age", "age_f", "age_s", "age_t", "spawn")], "tag") %>%
#   dplyr::rename(length = length.y,
#                 date = date.x,
#                 mark_date = date.y) %>%
#   dplyr::select(-length.x) %>%
#   dplyr::mutate(strata = cut(date, Cbreaks, labels = FALSE),
#                 mark_strata = cut(mark_date, Mbreaks, include.lowest = TRUE, labels = FALSE))
# lapply(R17, table, useNA = "ifany")





C17 <-
readxl::read_excel(".\\data\\data_raw\\STEELHEAD2017.xlsx",
                   range = "WeirASL!A2:H411",
                   col_names = c("date", "card", "fish", "tag", "finclip", "sex", "length", "age")) %>%
  tidyr::fill(c("date", "card")) %>%
  dplyr::mutate(date = as.POSIXct(date,format = "%m/%d/%y"),
                strata = cut(date, Cbreaks, labels = FALSE))
lapply(C17, table, useNA = "always")
#standardize data
C17$sex <- ifelse(C17$sex == 1, "M", "F")
C17$age <- ifelse(C17$age %in% c("r", "m"), NA, C17$age)
C17$age_f <- as.numeric(ifelse(gsub("\\..*", "", C17$age) == "x", NA, gsub("\\..*", "", C17$age))) + 1
C17$age_s <- sapply(stringr::str_match_all(gsub(".*\\.", "", C17$age), "[0-9]+"), function(x) sum(as.numeric(x)))
C17$age_t <- C17$age_f + C17$age_s
C17$spawn <- as.character(stringr::str_count(C17$age, "s"))

#Check total ages
table(C17$age, C17$age_t, useNA = "ifany")
#check salt ages
table(C17$age_s, C17$spawn, useNA = "ifany")

lapply(M17, table, useNA = "ifany")




dat17 <- list(M = M17, C = C17, weir = weir_daily17)
save(dat17, file = ".\\data\\dat17.rda")

temp <-
  weir_daily17 %>%
  dplyr::select(-strata) %>%
  dplyr::mutate(year = format(date, "%Y"),
                date = as.Date(paste0(format(date, "%m"), "-", format(date, "%d"), "-", year), "%m-%d-%Y"),
                jday = as.numeric(format(date, "%j")),
                week = format(date, "%U"))
load(".\\data\\weir_daily.rda")
weir_daily <- rbind(weir_daily[!weir_daily$year == 2017,], temp) %>% dplyr::arrange(date)
save(weir_daily, file = ".\\data\\weir_daily.rda")

