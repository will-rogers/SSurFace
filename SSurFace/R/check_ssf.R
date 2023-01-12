#' check_ssf
#'
#' This is a function to check whether a given object is a fitted
#' Step-Selection Function object
#'
#' @return Logical
#' @export
#'
#' @examples
#' library(lubridate)
#' library(amt)
#' data("deer")
#' deer
#'
#' data("sh_forest")
#' sh_forest
#'
#' ssf1 <- deer %>% steps_by_burst()
#' ssf1 <- ssf1 %>% random_steps(n_control = 15)
#' ssf1 <- ssf1 %>% extract_covariates(sh_forest)
#' ssf1 <- ssf1 %>%
#'   mutate(forest = factor(sh.forest, levels = 1:2, labels = c("forest", "non-forest")),
#'          cos_ta = cos(ta_),
#'          log_sl = log(sl_))
#' m2 <- ssf1 %>% fit_clogit(case_ ~ forest + forest:cos_ta + forest:log_sl + log_sl + cos_ta + strata(step_id_))
#'
#' check_ssf(m2)


check_ssf <- function(ssf.obj) {
  "fit_clogit" %in% class(ssf.obj)
}
