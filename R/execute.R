Execute <- function(state, lib.dir, script, dest, ..., log.dir=NULL) {
  by(state, 1:nrow(state), function(package) {
    if (package$installed) {
      version <- package$version
      package <- package$package
      dest <- file.path(dest, sprintf("%s_%s.rds", package, version))
      if (!file.exists(dest)) {
        message(sprintf("Executing %s on package %s", script, package))
        args <- c("Rscript", script, package, version, lib.dir, dest, ...)
        res <- system2("xvfb-run", args, stdout=TRUE, stderr=TRUE)
        status <- attr(res, "status")
        if (!is.null(status)) {
          message("Error")
          message(paste(res, collapse="\n"))
          if (!is.null(log.dir)) {
            file <- file.path(log.dir, sprintf("%s.rds", package))
            cat(paste(res, collapse="\n"), file=file)
          }
          res
        }
      }
    } else {
      character(0)
    }
  })
}
