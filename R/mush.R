#' fuse multiple layers
#'
#' Combine polygon layers into one partition of the plane.
#'
#' Each layer (sf polygon object) is entered as an argument, may be 2 or more layers.
#'
#'
#' @param ... entered arguments of sf polygons layers
#'
#' @return sf polygon layer with attributes, a, b, c, ... as per length of '..."
#' @export
#'
#' @importFrom sf st_cast st_polygonize st_boundary st_geometry st_contains st_set_geometry
#' @importFrom tibble as_tibble
#' @examples
#' library(basf)
#' afile <- system.file("extdata/list_parcels_hobart.gdb", package = "mush", mustWork = TRUE)
#' bfile <- system.file("extdata/list_tasveg_30_hobart.gdb", package = "mush", mustWork = TRUE)
#' A <- sf::read_sf(afile)
#' B <- sf::read_sf(bfile)
#' x <-  mush(A, B)
mush <- function(...) {
  listies <- list(...)
  if (length(listies) > length(letters)) stop("our alphabet is not long enough")
  ## assume, sf layers of  polygons - can't be lines for st_boundary need to escape hatch
  resultants <- sf::st_cast(sf::st_polygonize(sf::st_union(do.call(c, lapply(listies, function(.x) sf::st_boundary(sf::st_geometry(.x)))))))
  r.pip <- sf::st_point_on_surface(resultants)
  n.id <- lapply(listies, function(.x) sf::st_contains(.x, r.pip))
  out <- vector("list", length(n.id))
  names(out) <- letters[seq_len(length(listies))]
  for (i in seq_along(out))  {
    nas <- rep(NA_character_, length(resultants))
    nas[unlist(n.id[[i]])] <- letters[i]
    out[[i ]] <- nas
  }
  out <- tibble::as_tibble(out)
  bad <- rowSums(do.call(cbind, lapply(out, as.factor)), na.rm = TRUE) < 1
  sf::st_set_geometry(out[!bad, ] , resultants[!bad])
}

