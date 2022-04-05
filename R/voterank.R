library(igraph)
library(igraphdata)
data(karate)

#' VoteRank algorithm
#'
#' @param g Graph object.
#' @param r Number of spreaders.
#' @export
#' @examples
#' library(igraphdata)
#'
#' data(karate)
#' voterank(karate, 2)
voterank <- function(g, r) {
  # Init
  spreaders <- c()
  graph_order <- gorder(g)
  if (graph_order < r) {
    stop(
      "Graph order must be greater than number of spreaders."
    )
  }

  # Set initial voting ability
  voting_ability <- rep(1, graph_order)

  for (ith_spreader in seq_len(r)) {
    # Set up scores for next iteration
    scores <- rep(0, graph_order)

    # Calculate scores
    for (ith_node in seq_len(graph_order)) {
      neighbours_of_ith_node <- neighbors(g, ith_node, mode = 'out')
      neighbours_indeces <- as.numeric(neighbours_of_ith_node)
      scores[ith_node] <- sum(voting_ability[neighbours_indeces])
    }

    # Identify spreader
    spreader <- which.max(scores)
    spreaders <- c(spreaders, spreader)

    # Update voting ability
    voting_ability[spreader] <- 0
    neighbours_of_spreader <- neighbors(g, spreader, mode = 'out')
    spreader_neighbours_indeces <- as.numeric(neighbours_of_spreader)
    voting_ability[spreader_neighbours_indeces] <- 1 / mean(degree(g))
  }

  return(V(g)[spreaders])
}


voterank(karate, 2)
