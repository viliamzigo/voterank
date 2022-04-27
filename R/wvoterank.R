#' @title WVoteRank
#' @description A voting approach to uncover multiple influential spreaders
#'              on weighted networks
#'
#' @param g Weighted graph object.
#' @param r Number of spreaders.
#' @param display Whether to display graph, default FALSE.
#' @param display_layout Custom layout of graph, dafault layout_nicely.
#' @param display_label Whether to display vertex.label, default TRUE.
#' @return Vector of \code{r} vertices identified as spreaders.
#' @examples
#' library(igraphdata)
#'
#' data(karate)
#' wvoterank(karate, 2, weight = E(karate)$weight)
#'
#' @references Csardi G, Nepusz T: The igraph software package for complex
#'             network research, InterJournal, Complex Systems 1695. 2006.
#'             \url{https://igraph.org}
#' @references Hong-liang, S. et al.: A voting approach to uncover multiple
#'             influential spreaders on weighted networks. Physica A:
#'             Statistical Mechanics and its Applications Volume 519, 2019,
#'             Pages 303-312. \url{https://doi.org/10.1016/j.physa.2018.12.001}
wvoterank <- function(g, r, display = F, display_layout = layout_nicely(g), display_label = T) {
  if (r <= 0) {
    stop("Number of spreaders must be positive integer.")
  }

  # Init
  spreaders <- c()
  graph_order <- igraph::gorder(g)
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
      neighbours_of_ith_node <- igraph::neighbors(g, ith_node, mode = "out")
      neighbours_indeces <- as.numeric(neighbours_of_ith_node)

      if (ith_node %in% spreaders) {
        scores[ith_node] <- -1
        next
      }

      for (ith_neighbour in seq_along(neighbours_indeces)) {
        scores[ith_node] <- scores[ith_node] +
          voting_ability[neighbours_indeces[ith_neighbour]] *
          igraph::E(g)[igraph::get.edge.ids(g, c(
            igraph::V(g)[ith_node],
            igraph::V(g)[neighbours_indeces[ith_neighbour]])
          )]$weight
      }
      scores[ith_node] <- sqrt(length(neighbours_indeces) * scores[ith_node])
    }

    # Identify spreader
    spreader <- which.max(scores)
    spreaders <- c(spreaders, spreader)

    # Update voting ability
    neighbours_of_spreader <- igraph::neighbors(g, spreader, mode = "out")
    spreader_neighbours_indeces <- as.numeric(neighbours_of_spreader)
    voting_ability[spreader_neighbours_indeces] <- 1 / mean(igraph::degree(g))
    voting_ability[spreaders] <- 0
  }

  if (display) {
    plot(g,
         vertex.size = 5,
         layout = display_layout,
         vertex.color = ifelse(igraph::V(g) %in% igraph::V(g)[spreaders], 'red', NA),
         vertex.label = if(display_label) igraph::V(g)$name else NA,
         vertex.label.dist = 1,
         vertex.label.font = 2,
         edge.width = 2,
         main = 'VoteRank'
    )
  }

  return(igraph::V(g)[spreaders])
}
