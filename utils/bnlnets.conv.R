library(bnlearn)

dsc_to_el <- function(inputfilename, elfilename)
{
  # Input
  network = read.dsc(inputfilename)
  # print(network)
  new_node_names = paste(0:(length(nodes(network))-1))
  nodes(network) = new_node_names
  # print(network)
  
  # EL output
  elheader = paste(length(nodes(network)), dim(arcs(network))[1], sep=" ")
  # print(elheader)
  write.table(elheader, file=elfilename, sep=" ",
              append=F, col.names = F, row.names = F, quote=F)
  write.table(arcs(network), file=elfilename, sep=" ",
              append=T, col.names = F, row.names = F, quote=F)
}

dsc_to_el_file = function(stem)
{
  ifn = paste("./bnlnets/", stem, ".dsc", sep="")
  ofn = paste("../graphs/", stem, ".graph", sep="")
  dsc_to_el(ifn, ofn)
  cat(ifn, " --> ", ofn, "\n", sep="")
}

data.id.vec = c("alarm",
                "win95pts",
                "andes",
                "hepar2",
          "hailfinder",
          "pigs",
          "link",
          "pathfinder",
          "munin",
          "diabetes")
lapply(data.id.vec, dsc_to_el_file)

