#' @title VoteRank
#' @description VoteRank algorithm for identifying a set of influential
#'              spreaders in complex networks.
#'
#' @param g Graph object.
#' @param r Number of spreaders.
#' @param display Whether to display graph, default FALSE.
#' @param display_layout Custom layout of graph, dafault layout_nicely.
#' @param display_label Whether to display vertex.label, default TRUE.
#' @return Vector of \code{r} vertices identified as spreaders.
#' @examples
#' library(igraphdata)
#'
#' data(karate)
#' voterank(karate, 2)
#'
#' @references Csardi G, Nepusz T: The igraph software package for complex
#'             network research, InterJournal, Complex Systems 1695. 2006.
#'             \url{https://igraph.org}
#' @references Zhang, JX., Chen, DB., Dong, Q. et al.: Identifying a set of
#'             influential spreaders in complex networks. Sci Rep 6, 27823
#'             (2016). \url{https://doi.org/10.1038/srep27823}
#'             (\url{https://www.nature.com/articles/srep27823.pdf})
voterank <- function(g, r, display = F, display_layout = layout_nicely(g), display_label = T) {
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
      scores[ith_node] <- ifelse(ith_node %in% spreaders, -1,
                                 sum(voting_ability[neighbours_indeces]))
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
