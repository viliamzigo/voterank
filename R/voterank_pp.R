#' @title VoteRank++
#' @description VoteRank++ algorithm for identifying a set of influential
#'              spreaders in complex networks.
#'
#' @param g Graph object.
#' @param r Number of spreaders.
#' @param lambda Suppressing factor, where is from interval [0, 1]
#' @return Vector of \code{r} vertices identified as spreaders.
#' @examples
#' library(igraphdata)
#'
#' data(karate)
#' voterank_pp(karate, 2, 0.8)
#'
#' @references Csardi G, Nepusz T: The igraph software package for complex
#'             network research, InterJournal, Complex Systems 1695. 2006.
#'             \url{https://igraph.org}
#' @references Panfeng Liu, Longjie Li, Shiyu Fang, Yukai Yao: Identifying
#'             influential nodes in social networks: A voting approach, Chaos,
#'             Solitons & Fractals 152. 2021.
#'             \url{https://doi.org/10.1016/j.chaos.2021.111309}
#'             (\url{https://www.sciencedirect.com/science/article/pii/S0960077921006639})
voterank_pp <- function(g, r, lambda, display = F, display_layout = layout_nicely(g)) {
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
  degrees <- igraph::degree(g)
  voting_ability <- rep(1, graph_order) - degrees / max(degrees)

  for (ith_spreader in seq_len(r)) {
    # Set up scores for next iteration
    scores <- rep(0, graph_order)

    # Calculate scores
    for (ith_node in seq_len(graph_order)) {
      neighbours_of_ith_node <- igraph::neighbors(g, ith_node, mode = "out")
      neighbours_indeces <- as.numeric(neighbours_of_ith_node)

      score <- 0
      for (ith_neighbour in seq_along(neighbours_indeces)) {
        ith_neighbour_neghbours <- igraph::neighbors(g, ith_neighbour, mode = "out")
        vp_neighbour <- igraph::degree(g, ith_node) / sum(
          igraph::degree(g, ith_neighbour_neghbours))
        va_neighbour <- voting_ability[neighbours_indeces[ith_neighbour]]
        score <- score + vp_neighbour * va_neighbour
      }

      score <- sqrt(length(neighbours_indeces) * score)
      scores[ith_node] <- ifelse(ith_node %in% spreaders, -1, score)
    }

    # Identify spreader
    spreader <- which.max(scores)
    spreaders <- c(spreaders, spreader)

    # Update voting ability
    neighbours_of_spreader <- igraph::neighbors(g, spreader, mode = "out")
    spreader_neighbours_indeces <- as.numeric(neighbours_of_spreader)
    voting_ability[spreader_neighbours_indeces] <- lambda * voting_ability[spreader_neighbours_indeces]
    for (neighbour_index in seq_along(spreader_neighbours_indeces)) {
      second_order_neighbours <- igraph::neighbors(g,
                                                   igraph::V(g)[neighbour_index],
                                                   mode = "out")
      second_order_neighbours_indeces <- as.numeric(second_order_neighbours)
      just_second_order_neighbours_indeces <- !(second_order_neighbours_indeces %in% spreader_neighbours_indeces)
      voting_ability[just_second_order_neighbours_indeces] <-
        sqrt(lambda) * voting_ability[just_second_order_neighbours_indeces]
    }
    voting_ability[spreaders] <- 0
  }


  if (display) {
    plot(g,
         vertex.size = 5,
         layout = display_layout,
         vertex.color = ifelse(igraph::V(g) %in% igraph::V(g)[spreaders], 'red', NA),
         vertex.label = NA,
         main = paste('VoteRank++ with suppressing factor: ', lambda)
    )
  }

  return(igraph::V(g)[spreaders])
}
