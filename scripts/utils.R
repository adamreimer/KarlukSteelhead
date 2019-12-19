#' @importFrom magrittr %>%
#' @export
magrittr::`%>%`

digits <-function(p){
  ps <- ifelse(p < 0.01, format(p, digits = 3, scientific = TRUE),
               ifelse(p < 1, format(round(p, 2), nsmall = 2),
                      ifelse(p < 100, format(round(p, 1), nsmall = 1), format(round(p, 0), nsmall = 0, scientific = FALSE, big.mark = ","))))
   return(ps)
}
