weir_daily <-
  readxl::read_excel(".\\data\\data_raw\\Karluk Steelhead Counts 1976-2016.xlsx") %>%
  dplyr::rename(date = 'Month-Day') %>%
  tidyr::gather(year, cum, -date) %>%
  dplyr::group_by(year) %>%
  dplyr::mutate(daily = ifelse(is.na(dplyr::lag(cum)), 0, cum - dplyr::lag(cum)),
                date = as.Date(paste0(format(date, "%m"), "-", format(date, "%d"), "-", year), "%m-%d-%Y"),
                jday = as.numeric(format(date, "%j")),
                week = format(date, "%U")) %>%
  dplyr::ungroup()

library(ggplot2)
weir_daily %>%
  ggplot(aes(x = jday, y = daily)) +
    geom_col() +
    facet_wrap(~year, scales = "free_y")

weir_daily %>%
  dplyr::filter((year >= 1991 & year <= 1996) | year >= 2011) %>%
  ggplot(aes(jday, weight = daily)) +
    geom_histogram(binwidth = 3) +
    facet_wrap(~year, ncol = 2, scales = "free_y")

weir_annual <-
  weir_daily %>%
  dplyr::group_by(year) %>%
  dplyr::summarise(year_sums = sum(daily, na.rm =TRUE))

weir_annual %>% dplyr::filter(year >= 2007)
mean(weir_annual$year_sums[weir_annual$year %in% 2012:2016])
weir_annual %>% dplyr::filter(year >= 1992 & year <= 1997)
mean(weir_annual$year_sums[weir_annual$year %in% 1992:1997])

#sampling rate
5*360

M <- seq(50, 250, by = 25)
C <- 1250
s <- seq(.3, .5, .05)
R <- sapply(M, function(x){x*s})
N <- sapply(1:9, function(x){(M[x] + 1)*(C + 1)/(R[, x] + 1)})
vN <- sapply(1:9, function(x){(M[x] + 1)*(C + 1)*(M[x] - R[, x])*(C - R[, x])/(R[, x] + 1)^2/(R[, x] +2)})
seN <- sqrt(vN)
rpN <- seN * 1.96 / N
rpN

fn <- function(D, alpha, A){
  abs(pnorm(A * sqrt(D) / (1 - A)) - pnorm(-A * sqrt(D) / (1 + A)) - (1 - alpha))
}
optimize(fn, interval = c(0, 1000), alpha = .05, A = .1)

n1 <- function(N, n2, D) {
  N/(n2*(N-1)/(N-n2)/D + 1)
}
n1(1253/seq(.3, .55, .05), 1253, optimize(fn, interval = c(0, 1000), alpha = .05, A = .25)[[1]])
n1(1253/seq(.3, .55, .05)/.8, 1253, optimize(fn, interval = c(0, 1000), alpha = .05, A = .25)[[1]])

#Chinook sample size
d2n95 <- 1.27359 #Thompson 1987 Table 1, alpha = 0.05
d2n90 <- 1.00635 #Thompson 1987 Table 1, alpha = 0.1

# goal under various scale regenration rates
n0 <- d2n90/c(.115)^2 # d
n <- sapply(n0, FUN=function(n) {n/c(.6)})
n

#sampling rate
1253/3/n

