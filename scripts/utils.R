#' @importFrom magrittr %>%
#' @export
magrittr::`%>%`

digits <-function(p){
  ps <- ifelse(p < 0.01, format(p, digits = 3, scientific = TRUE),
               ifelse(p < 1, format(round(p, 2), nsmall = 2),
                      ifelse(p < 100, format(round(p, 1), nsmall = 1), format(round(p, 0), nsmall = 0, scientific = FALSE, big.mark = ","))))
   return(ps)
}



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


#Compares 2 length comps
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
       labels = paste0("D = ", format(test$statistic[[1]], digits = 2), "\n p.val = ", format.pval(test$p.value, digits = 2, eps = 0.001, nsmall = 3)),
       adj = 0)
}

#combined proportion variance from ADF&G age comp appendix.
combine_strata2 <- function(dat){
  weight <-
    dat %>%
    dplyr::select(age, sex, t.z, lg.z) %>%
    dplyr::group_by(age, sex) %>%
    dplyr::summarize(sum_t.z = sum(t.z),
                     var = sum(lg.z * t.z))

  weight2 <-
    dat %>%
    dplyr::left_join(setNames(dat[dat$sex == "Both" & dat$age == "All", c("sex2", "t.z")], c("sex2", "total.sex")),
                     by = "sex2") %>%
    dplyr::mutate(total = sum(dat[dat$sex == "Both" & dat$age == "All", "t.z"])) %>%
    dplyr::select(age, sex, p.z, total.sex, total) %>%
    dplyr::mutate(p.k = total.sex/total*p.z) %>%
    dplyr::group_by(age, sex) %>%
    dplyr::summarize(p.k = sum(p.k))

  dplyr::left_join(dat, weight, by = c("age", "sex")) %>%
    dplyr::left_join(weight2, by = c("age", "sex")) %>%
    dplyr::left_join(setNames(dat[dat$sex == "Both" & dat$age == "All", c("sex2", "t.z", "sd_t.z")], c("sex2", "total.sex", "se_total.sex")),
                     by = "sex2") %>%
    dplyr::mutate(w_lg.z = lg.z * t.z/sum_t.z,
                  w_se_lg.z = se_lg.z^2 * (t.z/sum_t.z)^2 + (lg.z * sum_t.z - var)^2 / sum_t.z^4 * sd_t.z^2,
                  var_p.k = total.sex^2 * sd_p.z^2 / total.sex^2 + (p.z -p.k)^2 * se_total.sex^2 / total.sex^2) %>%
    dplyr::group_by(age, sex) %>%
    dplyr::summarise(n.z = sum(n.z),
                     t.z = sum(t.z),
                     lg.z = sum(w_lg.z),
                     sd_t.z = sqrt(sum(sd_t.z^2)),
                     se_lg.z = sqrt(sum(w_se_lg.z, na.rm = TRUE)),
                     min_lg.z = min(min_lg.z),
                     max_lg.z = max(max_lg.z),
                     p.k = mean(p.k),
                     var_p.k = sum(var_p.k)) %>%
    dplyr::mutate(total = weight[[which(weight$age == "All" & weight$sex == "Both"), "sum_t.z"]],
                  p.z = t.z/total,
                  var_p.z = sd_t.z^2 * total^-2,
                  #sd_p.z = sqrt(var_p.z),
                  sd_p.z = sqrt(var_p.k),
                  logitlci_p.z = log(p.z / (1-p.z)) - 1.96 * sqrt(1 / p.z^2 / (1-p.z)^2 * sd_p.z^2),
                  logituci_p.z = log(p.z / (1-p.z)) + 1.96 * sqrt(1 / p.z^2 / (1-p.z)^2 * sd_p.z^2),
                  lci_p.z = exp(logitlci_p.z)/(1 + exp(logitlci_p.z)),
                  uci_p.z = exp(logituci_p.z)/(1 + exp(logituci_p.z)))%>%
    dplyr::select(age, sex, n.z, p.z, sd_p.z, lci_p.z, uci_p.z, t.z, sd_t.z, lg.z, se_lg.z, min_lg.z, max_lg.z) %>%
    dplyr::ungroup()
}

