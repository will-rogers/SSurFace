#' get_cells
#'
#' @return Logical
#' @export
#'
#' @examples

get_cells <- function(ssf.obj, mock.surface, raster, accessory.x.preds = NULL){
  if(!check_ssf(ssf.obj)) stop("Check that SSF model is valid")

  pred.xy <- raster::coordinates(mock.surface) # get coordinates from grid we created

  predict.data <- data.frame(cbind(pred.xy, raster::extract(raster, pred.xy, df=TRUE))) # makes raster values a data frame

  predict.data$step_id_unique = ssf.obj$model$xlevels$`strata(step_id_)`[1] # fix the strata to something reasonable

  if(!is.null(accessory.x.preds)) {
    predict.data <- cbind(predict.data, accessory.x.preds) # this adds extraneous x values that are not in matrix
  }

  cells <- nrow(pred.xy) # number of cells

  predict.data$cellnr <- 1:cells # assign cell numbers, redundant of ID

  return(predict.data)
}
