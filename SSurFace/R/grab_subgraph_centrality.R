#' grab_subgraph_centrality
#'
#' @return
#' @export
#' @examples

grab_subgraph_centrality <- function(pred.surf.comp, new.neighborhood = NULL){

  # Combining predicted Surfaces
  combined <- rbindlist(pred.surf.comp$prob.surface)
  n <- length(pred.surf.comp$prob.surface)

  mat <- pred.surf.comp$sparse.matrix

  cent.vec <- rep(NA, n)

  pb <- progress_bar$new(format = "(:spin) [:bar] :percent [Elapsed time: :elapsedfull || Estimated time remaining: :eta]", total = n, complete = "=", incomplete = "-", current = ">", clear = FALSE, width = 100)

  for (i in 1:n) {
    if(!is.null(new.neighborhood)) closer <- pred.surf.comp$prob.surface[[i]] %>% filter(step < new.neighborhood)
    if(is.null(new.neighborhood)) closer <- pred.surf.comp$prob.surface[[i]]

    neighbors <- closer$ID

    mat.sub <- mat[neighbors,neighbors]

    g <- graph_from_adjacency_matrix(mat.sub, mode = "directed", weighted = T, diag = T)
    V(g)$name <- neighbors

    # g <- delete.edges(g, E(g)[is.na(E(g)$weight)])

    index <- which(neighbors == closer$focal.cell[1])

    # cent.vec. <- closeness(g, mode = "in")
    cent.vec. <- betweenness(g, directed = TRUE, weights = E(g)$weight, normalized = T)
    cent.vec[i] <- cent.vec.[index]

    pb$tick()
  }

  return(cent.vec)
}
