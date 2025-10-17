.onLoad <- function(libname, pkgname) {
    invisible(NULL)
}

.onAttach <- function(libname, pkgname) {
  packageStartupMessage(
    "\n",
    crayon::green("autolearnR "), "loaded successfully!\n",
    "--------------------------------------------------\n",
    "A package for streamlined machine learning data analysis.\n",
    "Use ", crayon::green("?autolearnR"), " for general help.\n",
    "--------------------------------------------------\n"
  )
}
