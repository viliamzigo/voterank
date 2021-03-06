% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/voterank_pp.R
\name{voterank_pp}
\alias{voterank_pp}
\title{VoteRank++}
\usage{
voterank_pp(
  g,
  r,
  lambda,
  display = F,
  display_layout = layout_nicely(g),
  display_label = T
)
}
\arguments{
\item{g}{Graph object.}

\item{r}{Number of spreaders.}

\item{lambda}{Suppressing factor, where is from interval [0, 1]}

\item{display}{Whether to display graph, default FALSE.}

\item{display_layout}{Custom layout of graph, dafault layout_nicely.}

\item{display_label}{Whether to display vertex.label, default TRUE.}
}
\value{
Vector of \code{r} vertices identified as spreaders.
}
\description{
VoteRank++ algorithm for identifying a set of influential
             spreaders in complex networks.
}
\examples{
library(igraphdata)

data(karate)
voterank_pp(karate, 2, 0.8)

}
\references{
Csardi G, Nepusz T: The igraph software package for complex
            network research, InterJournal, Complex Systems 1695. 2006.
            \url{https://igraph.org}

Panfeng Liu, Longjie Li, Shiyu Fang, Yukai Yao: Identifying
            influential nodes in social networks: A voting approach, Chaos,
            Solitons & Fractals 152. 2021.
            \url{https://doi.org/10.1016/j.chaos.2021.111309}
            (\url{https://www.sciencedirect.com/science/article/pii/S0960077921006639})
}
