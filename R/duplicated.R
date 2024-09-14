#' Enlist duplicated packages
#'
#' @export
duplicated_packages <- function(wide = TRUE) {
  pkgs <-
    installed.packages() %>%
    as_tibble %>%
    mutate(version = as.package_version(Version)) %>%
    select(
      package = Package,
      libpath = LibPath,
      version
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

#' List user packages
#'
#' @export
user_packages <- function(libpath = .libPaths()[1], unique = TRUE, format_as_deb = FALSE) {
  message("Selecting the packages from ", libpath, ifelse(unique, " that are not present elsewhere", ""))
  pkgs <- installed.packages() %>%
    as_tibble %>%
    filter(LibPath == libpath) %>%
    mutate(version = as.package_version(Version)) %>%
    select(
      package = Package,
      version
    ) %>%
    {if (unique) dplyr::anti_join(., duplicated_packages(), by = "package") else .}

  if (format_as_deb) {
    format_as_deb(pkgs$package)
  } else pkgs
}

#' Serialize R package vector to debian r-cran packages
#'
#' @export
format_as_deb <- function(pkgs) {
  paste0("r-cran-", pkgs) |> tolower() |> cat()
}

#' Remove duplicated packages from first element of .libPaths()
#'
#'
#' @export
remove_duplicated_pkgs <- function(
    keep = c("sf", "terra", "ragg", "svglite", "lwgeom", "PROJ")) {
  to_remove <-
    duplicated_packages(wide = FALSE) %>%
    mutate(min_version = min(version), .by = package) %>%
    filter(
      libpath == .libPaths()[1],
      !(package %in% keep),
      version == min_version
    ) %>%
    pull(package)
  if (length(to_remove) == 0L) {
    message(
      "No packages to remove (taking into account the 'keep' argument).\n",
      "Use `duplicated_packages()` for more information."
    )
    return(invisible(NULL))
  }
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


