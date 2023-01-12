#' compile_ssf_comparisons
#'
#' @return
#' @export
#' @examples

compile_ssf_comparisons <- function(sparse.neighbors, cell.data) {

  # this is why the export of the neighbors as individual lists was important
  ssf.comparisons <- lapply(sparse.neighbors$by.cell, function(x){
    baseline <- cell.data[x$column[which(x$distance == 0)],] # baseline will have a distance of zero (focal)

    baseline$step <- 0 # create a variable "step" that records this zero distance

    alternate <- cell.data[x$column,] # grab all the other cell.data for neighboring cells (including focal cell)

    alternate$step <- x$distance # force distance to this new variable step

    list(.given = baseline, .for = alternate) # return a list of focal and neigboring cell data

  })

  return(ssf.comparisons)
}
