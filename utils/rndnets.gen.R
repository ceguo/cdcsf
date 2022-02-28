library(bnlearn)

random.network.el <- function(nnodes, elfilename, probfac=2)
{
  set.seed(20170322)
  network <- bnlearn::random.graph(paste(0:(nnodes-1)), num=1, prob=probfac/(nnodes-1))
  elheader = paste(length(nodes(network)), dim(arcs(network))[1], sep=" ")
  write.table(elheader, file=elfilename, sep=" ",
              append=F, col.names = F, row.names = F, quote=F)
  write.table(arcs(network), file=elfilename, sep=" ",
              append=T, col.names = F, row.names = F, quote=F)
}

random.network.el(1000, "../graphs/random1kf2.graph", 2)
random.network.el(1000, "../graphs/random1kf3.graph", 3)
random.network.el(1000, "../graphs/random1kf4.graph", 4)
random.network.el(5000, "../graphs/random5kf2.graph", 2)
random.network.el(5000, "../graphs/random5kf3.graph", 3)
random.network.el(5000, "../graphs/random5kf4.graph", 4)
