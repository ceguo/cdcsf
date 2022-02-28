library(conflicted)
library(pcalg)
library(bnlearn)
library(ParallelPC)
library(parallel)

source("cuPC.R")

nCoresUsed = 16
dump.pcalg.edges = function(g, rgfn)
{
  nedge = 0
  for (ei in g@graph@edgeL)
  {
    nedge = nedge + length(ei$edges)
  }
  
  write(c(length(g@graph@nodes), nedge), rgfn, append=FALSE, sep=" ")
  
  nodeidx = 0
  for (ei in g@graph@edgeL)
  {
    for (k in ei$edges)
    {
      write(c(nodeidx, k-1), rgfn, append=TRUE, sep=" ")
    }
    nodeidx = nodeidx + 1
  }
}


run.pcalg.compatible = function(dmat, mode, tool, addargs = list())
{
  vn = paste(0:(ncol(dmat)-1))
  al = 0.05
  pcenv = new.env()
  start.time = Sys.time()

  args = list(alpha = al, labels = vn, indepTest=gaussCItest)

  if (substring(tool, 1, 2) == "sf")
  {
    if (mode == "skeleton")
    {
      pcfun = pcalg::skeleton;
      args$method = "stable.fast"
    }
    else if (mode == "cpdag")
    {
      pcfun = pcalg::pc;
      args$skel.method = "stable.fast"
    }
  }
  else if (substring(tool, 1, 3) == "ppc")
  {
    if (mode == "skeleton")
    {
      pcfun = ParallelPC::skeleton_parallel
      args$method = "parallel"
      args$num_workers = nCoresUsed
    }
    else if (mode == "cpdag")
    {
      pcfun = ParallelPC::pc_parallel
      args$skel.method = "parallel"
      args$num.cores = nCoresUsed
    }
  }
  else if (substring(tool, 1, 4) == "cupc")
  {
    if (mode == "skeleton")
    {
      pcfun = cu_skeleton
    }
    else if (mode == "cpdag")
    {
      pcfun = cu_pc
    }
  }
  args = c(args, addargs)
  
  start.time = Sys.time()
  args$suffStat = list(C=cor(dmat), n=nrow(dmat))
  g = do.call(pcfun, args)
  end.time = Sys.time()
  exe.time = end.time - start.time
  exe.time.secs = as.numeric(exe.time, units = "secs")
  return(list(graph=g, time=exe.time.secs))
}


run.bnlearn.pc.stable = function(dmat, mode)
{
  numCores = nCoresUsed
  cl = makeCluster(numCores)  
  if (mode == "skeleton")
  {
    start.time = Sys.time()
    g = bnlearn::pc.stable(dmat, cluster = cl, test = "cor", undirected = TRUE)
    end.time = Sys.time()
  }
  else if (mode == "cpdag")
  {
    start.time = Sys.time()
    g = bnlearn::pc.stable(dmat, cluster = cl, test = "cor", undirected = FALSE)
    end.time = Sys.time()
  }
  end.time = Sys.time()
  exe.time = end.time - start.time
  exe.time.secs = as.numeric(exe.time, units = "secs")
  stopCluster(cl)
  return(list(graph=g, time=exe.time.secs))
}


main = function(data.prefix, result.prefix, mode = "skeleton",
                tool = "sf")
{
  data.filename = paste(data.prefix, ".matrix", sep="")
  
  result.common = paste(result.prefix, ".", mode, ".", tool, sep="")
  result.time.filename = paste(result.common, ".time", sep="")
  result.graph.filename = paste(result.common, ".graph", sep="")
  
  cat("Mode:", mode, "\n")
  cat("Tool:", tool, "\n")
  cat("Data file:", data.filename, "\n")
  cat("Output files:\n")
  cat("  ", result.time.filename, "\n")
  cat("  ", result.graph.filename, "\n")
  
  dmat = as.data.frame(t(read.table(data.filename, sep = " ", skip = 1)))
  names(dmat) = paste(0:(ncol(dmat)-1))
  
  if (tool == "bpcs")
  {
    # bnlearn pc stable
    res = run.bnlearn.pc.stable(dmat, mode)
    # plot(res$graph)
    write(res$time, file=result.time.filename, append=FALSE)
    write(c(length(res$graph$nodes), nrow(res$graph$arcs)),
          file=result.graph.filename, append=FALSE)
    write.table(res$graph$arcs,file=result.graph.filename, append=TRUE,
                quote = FALSE, col.names = FALSE, row.names = FALSE)
  }
  else
  {
    # pcalg compatible cases
    res = run.pcalg.compatible(dmat, mode, tool)
    write(res$time, file=result.time.filename, append=FALSE)
    dump.pcalg.edges(res$graph, result.graph.filename)
  }
  
  return(0)
}


test.run.single = function(data.id, tool.vec)
{
  dpf = paste("../data/", data.id, sep = "")
  rpf = paste("../sketch.tradeoff/", data.id, sep = "")
  for (mode in c("skeleton", "cpdag"))
  {
    for (tool in tool.vec)
    {
      main(dpf, rpf, mode, tool)
    }
  }
}


run.small = function()
{
  tool.vec=c("sf", "cupc", "ppc",  "bpcs")
  data.id.vec = c("andes", "diabetes")
  for (data.id in data.id.vec)
  {
    test.run.single(data.id, tool.vec)
  }
}

run.realworld = function()
{
  tool.vec=c("sf", "cupc", "ppc",  "bpcs")
  data.id.vec = c("hepar2", "andes", "win95pts", "hailfinder",
                  "link", "munin", "pigs", "win95pts")
  for (data.id in data.id.vec)
  {
    test.run.single(data.id, tool.vec)
  }
}

run.synthetic = function()
{
  tool.vec=c("sf", "cupc")
  data.id.vec = c("random5kf2", "random5kf3", "random5kf4")
  for (data.id in data.id.vec)
  {
    test.run.single(data.id, tool.vec)
  }
}

# run.realworld()
# run.synthetic()
