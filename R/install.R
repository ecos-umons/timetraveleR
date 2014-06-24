PackageDir <- function(package, version, pkg.dir) {
  file.path(pkg.dir, package, version, package)
}

Install <- function(state, pkg.dir, lib.dir, log.dir=NULL, try.failed=FALSE) {
  res <- by(state, 1:nrow(state), function(package) {
    path <- PackageDir(package$package, package$version, pkg.dir)
    log.file <- file.path(log.dir, sprintf("%s_%s.rds", package$package,
                                           package$version))
    if (package$missing.deps) {
      message(sprintf("%s %s not installable",
                      package$package, package$version))
      character(0)
    } else if (package$installed) {
      message(sprintf("%s %s already installed", package$package,
                      package$version))
    } else if (try.failed | !file.exists(log.file)) {
      message(sprintf("Installing %s %s", package$package, package$version))
      args <- c("R", "CMD", "INSTALL", sprintf("--library=%s", lib.dir), path)
      res <- system2("xvfb-run", args, stdout=TRUE, stderr=TRUE)
      status <- attr(res, "status")
      if (!is.null(status)) {
        message("Error")
        message(paste(res, collapse="\n"))
        if (!is.null(log.dir)) {
          cat(paste(res, collapse="\n"), file=log.file)
        }
        res
      }
    } else {
      message(sprintf("%s %s already failed to install", package$package,
                      package$version))
      character(0)
    }
  })
  state$installed <- sapply(res, is.null)
  state
}
