#' @title Susceptible(S) & Infected(I) model simulation
#' @description Initially, all nodes are susceptible status except for a set of
#'              r infected nodes selected as source spreaders. At each
#'              time step, infected node tries to infect one of its neighbors
#'              with given probability.
#'
#' @param g Graph object.
#' @param spreaders List of initial spreaders (graph vertices).
#' @param prob Probability of infecting neighbour.
#' @param layout Custom layout, default is layout_nicely.
#' @param plot_iter Boolean argument, default FALSE. If TRUE plot every iteration of spreading.
#' @return Number of iterations to infect all nodes.
#' @examples
#' library(igraphdata)
#'
#' data(karate)
#' spreaders <- voterank(karate, 2)
#' si_simulation(karate, spreaders, prob = 0.5)
si_simulation <- function(g, spreaders, prob, layout = layout_nicely(g), plot_iter = F) {
  iterations <- 0
  if(plot_iter) {
    plot(g,
         vertex.size = 5,
         layout = layout,
         vertex.color = ifelse(V(g)$name %in% spreaders, 'red', NA),
         vertex.label = NA,
         main = paste('SI iteration: ', iterations)
    )
    readline(prompt="Press [enter] to continue")
  }

  while(length(V(g)) > length(spreaders)) {
    if (runif(1) <= prob) {
      spreader <- sample(spreaders, 1)
      spreader_neighbors <- neighbors(g, V(g)[V(g)$name == spreader])$name
      susceptible <- spreader_neighbors[which(!(spreader_neighbors %in% spreaders))]

      if (length(susceptible) == 0) next;
      infected <- sample(susceptible, 1)
      spreaders <- c(spreaders, infected)

      if(plot_iter) {
        plot(g,
             vertex.size = 5,
             layout = layout,
             vertex.color = ifelse(V(g)$name %in% spreaders, 'red', NA),
             vertex.label = NA,
             main = paste('SI iteration: ', iterations)
        )
        readline(prompt="Press [enter] to continue")
      }
    }

    iterations <- iterations + 1
  }

  return(iterations)
}
