library(KarlukSteelhead)

weir_daily <-
  readxl::read_excel(".\\KarlukSteelhead\\data_raw\\Karluk Steelhead Counts 1976-2016.xlsx") %>%
  dplyr::rename_(date = as.name('Month-Day')) %>%
  tidyr::gather(year, cum, -date) %>%
  dplyr::group_by(year) %>%
  dplyr::mutate(daily = ifelse(is.na(lag(cum)), 0, cum - lag(cum)),
                date = as.Date(paste0(format(date, "%m"), "-", format(date, "%d"), "-", year), "%m-%d-%Y"),
                jday = as.numeric(format(date, "%j")),
                week = format(date, "%U")) %>%
  dplyr::ungroup()

devtools::use_data(weir_daily, pkg = ".\\KarlukSteelhead", overwrite = TRUE)
