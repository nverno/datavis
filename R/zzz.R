##' @import devtools
NULL

.onLoad <- function(libname, pkgname) {
  pkglist <- list(
    c(name='sync.afs', url='nverno/sync.afs'),
    c(name='dirs', url='nverno/dirs')
  )

  for(pkg in pkglist)
    if(!suppressWarnings(suppressPackageStartupMessages(
      require(pkg['name'], quietly=TRUE,character.only=TRUE)))) {
      devtools::install_github(pkg['url'])
      suppressPackageStartupMessages( library(pkg['name'],character.only=TRUE))
    }

  invisible()
}
