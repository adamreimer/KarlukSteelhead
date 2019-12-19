library(magrittr)

weir_daily <-
  readxl::read_excel(".\\data\\data_raw\\Karluk Steelhead Counts 1976-2016.xlsx") %>%
  dplyr::rename(date = 'Month-Day') %>%
  tidyr::gather(year, cum, -date) %>%
  dplyr::group_by(year) %>%
  dplyr::mutate(daily = ifelse(is.na(dplyr::lag(cum)), 0, cum - dplyr::lag(cum)),
                date = as.Date(paste0(format(date, "%m"), "-", format(date, "%d"), "-", year), "%m-%d-%Y"),
                jday = as.numeric(format(date, "%j")),
                week = format(date, "%U")) %>%
  dplyr::ungroup() %>%
  dplyr::arrange(date)

save(weir_daily, file = ".\\data\\weir_daily.rda")
