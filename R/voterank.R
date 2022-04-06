#' @title VoteRank
#' @description VoteRank algorithm for identifying a set of influential
#'              spreaders in complex networks.
#'
#' @param g Graph object.
#' @param r Number of spreaders.
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
#' @references Panfeng Liu, Longjie Li, Shiyu Fang, Yukai Yao: Identifying
#'             influential nodes in social networks: A voting approach, Chaos,
#'             Solitons & Fractals 152. 2021.
#'             \url{https://doi.org/10.1016/j.chaos.2021.111309}
#'             (\url{https://www.sciencedirect.com/science/article/pii/S0960077921006639})
#' @references Zhang, JX., Chen, DB., Dong, Q. et al.: Identifying a set of
#'             influential spreaders in complex networks. Sci Rep 6, 27823
#'             (2016). \url{https://doi.org/10.1038/srep27823}
#'             (\url{https://www.nature.com/articles/srep27823.pdf})
voterank <- function(g, r) {
  if (r <= 0) {S
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

  return(igraph::V(g)[spreaders])
}
