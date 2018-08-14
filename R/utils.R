#' @importFrom utils install.packages
#' @importFrom utils installed.packages

# helper function: load one package ------------------------

.load_package <- function(pkg, dependencies = TRUE, unquote = TRUE, require = TRUE, ...) {
  if(unquote) {
    pkg <- substitute(pkg)
    pkg <- as.character(pkg)
  }

  if(!is.element(pkg, utils::installed.packages())) {
    utils::install.packages(pkg, dependencies = dependencies)
  }

  if(require) {
    base::require(pkg, character.only = T)
  }

}

