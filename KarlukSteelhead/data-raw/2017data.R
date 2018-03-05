M17 <-
  readxl::read_excel(".\\KarlukSteelhead\\data_raw\\STEELHEAD2017.xlsx",
                     range = "Tagged fish!A1:M77") %>%
  tidyr::fill(c("date", "card"))
lapply(M17, table)
Mbreaks <- quantile(M17$date, probs = c(0, .5, 1))
Mbreaks
lubridate::day(Mbreaks[2]) <- 7
M17$strata <- cut(M17$date, Mbreaks, include.lowest = TRUE, labels = FALSE)
M17$sex <- ifelse(M17$sex == 1, "M", "F")

weir_daily17 <-
  readxl::read_excel(".\\KarlukSteelhead\\data_raw\\STEELHEAD2017.xlsx",
                     range = "Escapement!A3:C112") %>%
  setNames(tolower(names(.)))
weir_daily17
lubridate::year(weir_daily17$date) <- 2017
Cbreaks <- quantile(rep(weir_daily17$date, times = weir_daily17$daily))
weir_daily17$strata <- cut(weir_daily17$date, Cbreaks)

R17 <-
  readxl::read_excel(".\\KarlukSteelhead\\data_raw\\STEELHEAD2017.xlsx",
                     range = "Recoveries!A3:G23",
                     col_names = c("date", "tag", "length", "finclip", "comment"),
                     col_types = c("date", "numeric", "skip", "numeric", "text", "skip", "text"),
                     na = c("", "n/a", "?")) %>%
  tidyr::fill("date") %>%
  dplyr::left_join(M17[, c("date", "tag", "length", "sex", "age")], "tag") %>%
  dplyr::rename(length = length.y,
                date = date.x,
                mark_date = date.y) %>%
  dplyr::select(-length.x) %>%
  dplyr::mutate(strata = cut(date, Rbreaks, labels = FALSE),
                mark_strata = cut(mark_date, Mbreaks, include.lowest = TRUE, labels = FALSE))
lapply(R17, table)

C17 <-
readxl::read_excel(".\\KarlukSteelhead\\data_raw\\STEELHEAD2017.xlsx",
                   range = "WeirASL!A1:M411") %>%
  tidyr::fill(c("date", "card")) %>%
  dplyr::mutate(date = as.POSIXct(date,format = "%m/%d/%y"),
                strata = cut(date, Rbreaks, labels = FALSE))
lapply(C17, table)
C17$sex <- ifelse(C17$sex == 1, "M", "F")

dat17 <- list(M = M17, C = C17, R = R17, weir = weir_daily17)
devtools::use_data(dat17, pkg = ".\\KarlukSteelhead", overwrite = TRUE)

temp <-
  weir_daily17 %>%
  dplyr::select(-strata) %>%
  dplyr::mutate(year = format(date, "%Y"),
                date = as.Date(paste0(format(date, "%m"), "-", format(date, "%d"), "-", year), "%m-%d-%Y"),
                jday = as.numeric(format(date, "%j")),
                week = format(date, "%U"))
weir_daily <- rbind(weir_daily, temp)
devtools::use_data(weir_daily, pkg = ".\\KarlukSteelhead", overwrite = TRUE)
