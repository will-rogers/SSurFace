#' step_distance
#'
#' @return Logical
#' @export
#'
#' @examples

step_distance <- function(ssf.obj, quantile) {
  if(!check_ssf(ssf.obj)) stop("Check that SSF model is valid")

  step <- qgamma(quantile, # user specified quantile
                 shape = ssf.obj$sl_$params$shape, # estimated from amt
                 scale = ssf.obj$sl_$params$scale) # estimated from amt

  return(step)
}
