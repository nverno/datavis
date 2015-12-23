##' Check that AFS is accessible
##' @title check_afs
##' @importFrom sync.afs get_afs
##' @export
check_afs <- function() file.exists(sync.afs::get_afs())


