#' neighbor_finder
#'
#' @return
#' @export
#' @examples

neighbor_finder <- function(ssf.obj = m2, cell.data, neighbors.found, quantile = 0.99, cell.data.list = NULL, distance.override = NULL){

  if(is.null(distance.override)) neighborhood.distance <- step_distance(ssf.obj, quantile) # take the X% step distance as your neighborhood

  if(!is.null(distance.override)) neighborhood.distance <- distance.override

  cols <- ncol(neighbors.found) # columns of our call-up table
  differences <- nrow(neighbors.found) # number of differences in index values

  print("Creating neighbor comparisons")
  vector <- c(neighbors.found) # convert the neighbors to a vector we can index later, this is the purpose of all those NA's earlier based on i-1 unique distances

  print("Finding valid comparisons")
  valid <- vector < neighborhood.distance # T/F whether those neighborhood distances are less than our threshold

  if(is.null(cell.data.list)){
    print("Splitting cell.data into list")
    cell.data.list. <- split(cell.data, cell.data$cellnr) # split the prediction data into row-wise lists to use lapply
  }

  if(!is.null(cell.data.list)){
    print("Using inputted list of cell data")
    cell.data.list. <- cell.data.list # split the prediction data into row-wise lists to use lapply
  }

  print("Running comparisons")
  neighbor.mat <- pblapply(cell.data.list., function(x){ # step through each row (see list split above)
    focal <- as.numeric(x$cellnr) # value of row cell number
    delta <- abs(focal - cell.data$cellnr) # difference in row cell number vs all others
    index <- ifelse(focal < cell.data$cellnr, focal, cell.data$cellnr) # report the minimum cell index (based on our call-up structure)

    index.col <- index%%cols # use the remainder function to get the column number (see below, we have to force zeros to the column number because the remainder of the final column is zero)

    df <- data.frame(difference = delta + 1, # we have to add one because differences of 0 are stored in row 1, differences of 1 in row 2, etc.
                     col = ifelse(index.col == 0, cols, index.col), # forcing remainders of zero the number of columns
                     cell.nr = cell.data$cellnr) # just tracking the cell number we are comparing against for use later

    # filter the data frame based on whether our call-up values are less than the neighborhood
    df <- df %>%
      filter(valid[difference+((col-1)*differences)])

    # filter the data set to unique rows and columns for call-up (to accommodate memory issues)
    df.distinct <- df %>% distinct(difference, col)

    # find the unique distances (trims time down)
    df.distinct$distances <- vector[df.distinct$difference + ((df.distinct$col - 1)*differences)]

    # throw the unique distances back to the full data set
    df <- merge(df, df.distinct, by = c("difference","col"))

    # package into a nice data frame for export
    data.frame(row = focal, column = df$cell.nr, distance = df$distances)
  })

  # bind the output list
  neighbors <- rbindlist(neighbor.mat)

  # create a sparse matrix based on the focal id, alternate id, and distance
  sparse.neighbors <- sparseMatrix(i = neighbors$row, j = neighbors$column, x = neighbors$distance)

  # return both the matrix and the unbound list of neighbor cells
  return(list(matrix = sparse.neighbors, by.cell = neighbor.mat))
}
