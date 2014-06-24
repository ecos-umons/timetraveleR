core.packages <- c("R", "base", "compiler", "datasets", "graphics",
                   "grDevices", "grid", "methods", "parallel", "profile",
                   "splines", "stats", "stats4", "tcltk", "tools",
                   "translations", "utils")

GlobalState <- function(packages) {
  rownames(packages) <- paste(packages$package, packages$version)
  packages[c("package", "version", "mtime")]
}

GetPackages <- function(packages, date) {
  packages[!is.na(packages$mtime) & as.Date(packages$mtime) <= as.Date(date), ]
}

GetLastPackages <- function(packages, date) {
  res <- GetPackages(packages, date)
  res <- res[with(res, order(as.character(mtime), decreasing=TRUE)), ]
  res[!duplicated(res["package"]), ]
}

Dependencies <- function(state, deps) {
  deps <- merge(deps, state)
  split(deps, sapply(deps$dependency, function(p) {
    if (p %in% core.packages) "core"
    else if (p %in% state$package) "contrib"
    else "missing"
  }))
}

DependencyGraph <- function(state, deps) {
  g <- graph.empty(directed=TRUE) + vertices(state$package)
  g + edges(apply(deps, 1, function(e) c(e["package"], e["dependency"])))
}

Init <- function(packages, deps, date,
                 deps.types=c("depends", "imports", "linkingto")) {
  packages <- GlobalState(packages)
  state <- GetLastPackages(packages, date)
  deps <- deps[deps$type %in% deps.types, ]
  deps <- Dependencies(state, deps)
  g <- DependencyGraph(state, deps$contrib)
  missings <- unique(deps$missing$package)
  installable <- shortest.paths(g, state$package, missings, "out")
  state$missing.deps <- !apply(installable, 1, function(x) all(x == Inf))
  state$installed <- FALSE
  state[topological.sort(g, "in"), ]
}

Update <- function(state, old.state) {
  old.state <- old.state[rownames(old.state) %in% rownames(state), ]
  state[rownames(old.state), ]$installed <- old.state$installed
  state
}
