#' get_cell_data
#'
#' @return Logical
#' @export
#'
#' @examples

get_cell_data <- function(ssf.obj, pred.data){
  if(!check_ssf(ssf.obj)) stop("Check that SSF model is valid")

  cells <- nrow(pred.data) # number of cells

  # relying on amt step-selection models, we can predict log-RSS
  # there are better ways to predict, but this is simple
  log.rss <- amt::log_rss(ssf.obj, # the model
                          pred.data, # the raster data (including missing values)
                          pred.data %>%
                            drop_na() %>%
                            sample_n(1),  # a row of the raster data (excluding missing values)
                          ci = NA)

  full.raster.data <- data.frame(pred.data, lRSS = log.rss$df$log_rss) # bind predictions to the original data

  return(full.raster.data)

}
