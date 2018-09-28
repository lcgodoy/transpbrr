#' Merge Viagens' Files
#'
#' @description Auxiliar function, internal use.
#'
#' @param list_df \code{list} of data.table's.
merge_viagens <- function(list_df) {
  Reduce(f = function(x, y) merge(x, y, by.x = names(y)[1],
                                  by.y = names(y)[1],
                                  allow.cartesian = T),
         x = list_df)
}
