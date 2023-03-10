#' Enlist duplicated packages
#'
#' @export
duplicated_packages <- function(wide = TRUE) {
  pkgs <-
    installed.packages() %>%
    as_tibble %>%
    select(
      package = Package,
      libpath = LibPath,
      version = Version
    )
  pkgs_duplicated <-
    pkgs %>%
    count(package) %>%
    filter(n > 1) %>%
    semi_join(pkgs, ., by = "package")

  if (!wide) return(pkgs_duplicated) else {
    pkgs_duplicated %>%
      pivot_wider(names_from = libpath, values_from = version)
  }
}


#' Remove duplicated packages from first element of .libPaths()
#'
#'
#' @export
remove_duplicated_pkgs <- function() {
  to_remove <-
    duplicated_packages(wide = FALSE) %>%
    filter(libpath == .libPaths()[1]) %>%
    pull(package)
  if (interactive()) {
    message(
      "Will remove following duplicated packages from `",
      .libPaths()[1], "`: ",
      paste(to_remove, collapse = ", ")
      )
    a <- readline("Do you wish to continue? (y/n) ")
    if (grepl("^[Yy]", a)) {
      remove.packages(to_remove, lib = .libPaths()[1])
      return(invisible(NULL))
    } else {
      message("Aborting. Use `duplicated_packages()` for more information.")
      return(invisible(NULL))
    }
  }
}


