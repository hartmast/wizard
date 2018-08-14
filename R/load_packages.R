#' @name load_packages
#' @export load_packages
#' @title Load packages (and install them, if need be)
#' @description Check if a package is installed. If not, install it before
#' @param ... the names of packages as unquoted, comma-separated strings
#' @param require If TRUE (the default), the packages are loaded. If FALSE, the function only checks whether they are installed and installs them if not.
#' @return inherited from require.
#' @examples
#' \dontrun{
#' load_packages(package1, package2, package3)
#' }


# main function: load multiple packages
# (depending on helper function in utils.R)
# (also applicable for loading only one package)

load_packages <- function(..., require = TRUE) {
  a <- eval(substitute(alist(...)))

  if(require) {
    sapply(1:length(a), function(i) .load_package(as.character(a[[i]]), unquote = F))
  } else {
    sapply(1:length(a), function(i) .load_package(as.character(a[[i]]), unquote = F, require = F))
  }

}


