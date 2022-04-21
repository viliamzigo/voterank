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
#' @param seed Seed, default is 83 for no reason.
#' @return Number of iterations to infect all nodes.
#' @examples
#' library(igraphdata)
#'
#' data(karate)
#' spreaders <- voterank(karate, 2)
#' si_simulation(karate, spreaders, prob = 0.5)
si_simulation <- function(g, spreaders, prob, layout = layout_nicely(g), plot_iter = F, seed = 83) {
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
    infected <- c()
    for (spreader in spreaders) {
      if (runif(1) <= prob) {
        spreader_neighbors <- neighbors(g, V(g)[V(g)$name == spreader])
        susceptible <- spreader_neighbors[
          !(spreader_neighbor$name %in% spreaders) &
            !(spreader_neighbor$name %in% infected)]

        set.seed(seed)
        infected <- c(infected, sample(susceptible), 1)$name)
      }
    }

    spreaders <- c(spreaders, infected)
    iterations <- iterations + 1

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

  return(iterations)
}
