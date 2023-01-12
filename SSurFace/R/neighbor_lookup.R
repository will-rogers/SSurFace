#' neighbor_lookup
#'
#' @return Logical
#' @export
#'
#' @examples

neighbor_lookup <- function(mock.surface, cell.data, cell.data.list = NULL){
  cols <- mock.surface@ncols # columns in prediction
  rows <- mock.surface@nrows # rows in prediction
  cells <- cols*rows # number of cells
  index <- 1:cells # all index values in our prediction raster

  # create a matrix for each column in the first row and its comparisons to distance to all other cells

  # if(is.null(cell.data.list)){
  #   print("Splitting cell.data into list")
  #   cell.data.list. <- split(cell.data, cell.data$cellnr) # split the prediction data into row-wise lists to use lapply
  # }
  #
  # if(!is.null(cell.data.list)){
  #   print("Using inputted list of cell data")
  #   cell.data.list. <- cell.data.list # split the prediction data into row-wise lists to use lapply
  # }

  # this is a progress bar we can use in a for loop
  pb <- progress_bar$new(format = "(:spin) [:bar] :percent [Elapsed time: :elapsedfull || Estimated time remaining: :eta]", total = cols, complete = "=", incomplete = "-", current = ">", clear = FALSE, width = 100)

  for(i in 1:cols) { # step through columns
    dist <- pointDistance(cell.data[i,c("x","y")], # choose the first row cell by column (raster indices are row-wise)
                          cell.data[i:cells,c("x","y")], # choose all other cells
                          lonlat = F) # we are using UTM
    if(i == 1) mat.dist <- matrix(dist, ncol = 1)
    if(i > 1) mat.dist <- cbind(mat.dist, c(dist, rep(NA, i-1))) # for each additional column, there are i-1 comparisons that are repeated (unnecessary)
    pb$tick() # for progress bar
  }

  return(mat.dist)
}
