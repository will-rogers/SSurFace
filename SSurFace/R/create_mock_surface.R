#' create_mock_surface
#'
#' @return Logical
#' @export
#'
#' @examples

create_mock_surface <- function(raster.list, multiple.extents = F, resolution = list(x = 100, y = 100)){

  # If rasters are all of the same extent, take the extent
  if(multiple.extents == F){
    xmin <- extent(raster.list)[1]
    xmax <- extent(raster.list)[2]
    ymin <- extent(raster.list)[3]
    ymax <- extent(raster.list)[4]
  }

  # If rasters are of different extent, take the overlap extent
  if(multiple.extents == T){
    xmin <- max(unlist(lapply(raster.list, function(x) {extent(x)[1]})))
    xmax <- max(unlist(lapply(raster.list, function(x) {extent(x)[2]})))
    ymin <- max(unlist(lapply(raster.list, function(x) {extent(x)[3]})))
    ymax <- max(unlist(lapply(raster.list, function(x) {extent(x)[4]})))
  }

  # Create new raster
  mock.surface <- raster(
    ncol=(xmax-xmin)/resolution$x, # raster automatically rounds, total cols
    nrow=(ymax-ymin)/resolution$y, # raster automatically rounds, total rows
    xmn=xmin, # min x exent
    xmx=xmax, # max x exent
    ymn=ymin, # min y exent
    ymx=ymax, # max y exent
    crs = crs(raster.list[[1]])) # take CRS from first raster, requires that rasters match in CRS

  values(mock.surface) <- 1:(ncol(mock.surface)*nrow(mock.surface)) # not important, just for visualization

  return(mock.surface)
}
