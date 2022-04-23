# VoteRank

Voterank algorithm for identifying a set of influential spreaders in complex networks.

## Instalation

```r
install.packages("devtools")
devtools::install_github("viliamzigo/voterank")
```

## Exercises

Exercise 1:

```r
library(igraphdata)
data(karate)

# Find 2 most influential people in karate network using `voterank` and `voterank_pp`.

```

Exercise 2:

```r
g = read.graph(
file="http://users.dimi.uniud.it/~massimo.franceschet/teaching/datascience/network/R/dolphin.gml", format="gml")

# Find 5 most influential dolphins in dolphin network using `voterank` and `voterank_pp`.

```

## References

Csardi G, Nepusz T: The igraph software package for complex network research, InterJournal, Complex Systems 1695. 2006. https://igraph.org

Panfeng Liu, Longjie Li, Shiyu Fang, Yukai Yao: Identifying influential nodes in social networks: A voting approach, Chaos, Solitons & Fractals 152. 2021. https://doi.org/10.1016/j.chaos.2021.111309 (https://www.sciencedirect.com/science/article/pii/S0960077921006639)

Zhang, JX., Chen, DB., Dong, Q. et al.: Identifying a set of influential spreaders in complex networks. Sci Rep 6, 27823 (2016). https://doi.org/10.1038/srep27823 (https://www.nature.com/articles/srep27823.pdf)
