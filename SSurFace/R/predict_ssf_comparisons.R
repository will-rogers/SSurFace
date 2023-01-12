#' predict_ssf_comparisons
#'
#' @return
#' @export
#' @examples

predict_ssf_comparisons <- function(ssf.obj = issf.fit, ssf.comparisons) {

  print("Estimating probability surface")

  prediction.list <- pbmclapply(ssf.comparisons, function(x){ # step through the list of SSF objects
    log.rss <- log_rss(ssf.obj, x$.for, x$.given, ci = "se") # get the log-RSS for each comparison
    x$.for$log_rss <- log.rss$df$log_rss
    x$.for$Prob <- exp(log.rss$df$log_rss)*(1/sum(exp(log.rss$df$log_rss))) # exponentiate and multiply against relative risk
    x$.for$Prob.l <- exp(log.rss$df$lwr)*(1/sum(exp(log.rss$df$lwr))) # exponentiate and multiply against relative risk
    x$.for$Prob.h <- exp(log.rss$df$upr)*(1/sum(exp(log.rss$df$upr))) # exponentiate and multiply against relative risk
    x$.for # return the data frame with probabilities
  })

  print("Compiling probability surface")

  for(i in 1:length(prediction.list)){
    prediction.list[[i]]$focal.cell <- i # specify the focal cell for each comparison
  }

  print("Making sparse matrix for transitions")
  bound <- rbindlist(prediction.list) # bind all data frames

  # use indexing to make a massive sparse matrix quickly
  Sparse.Matrix.lrss <- sparseMatrix(bound$focal.cell, bound$cellnr, x = bound$log_rss)
  Sparse.Matrix <- sparseMatrix(bound$focal.cell, bound$cellnr, x = bound$Prob)
  Sparse.Matrix.l <- sparseMatrix(bound$focal.cell, bound$cellnr, x = bound$Prob.l)
  Sparse.Matrix.h <- sparseMatrix(bound$focal.cell, bound$cellnr, x = bound$Prob.h)

  # return the prediction list and sparse matrix
  return(list(prob.surface = prediction.list, lrss.smatrix = Sparse.Matrix.lrss, sparse.matrix = Sparse.Matrix, sparse.matrix.l = Sparse.Matrix.l, sparse.matrix.h = Sparse.Matrix.h))
}
