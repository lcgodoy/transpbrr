#' Merge Viagens' Files
#'
#' @description Auxiliar function, internal use.
#'
#' @param list_df \code{list} of data.table's.
merge_viagens <- function(list_df) {
  Reduce(f = function(x, y) merge(x, y,
                                  by.x = names(y)[1],
                                  by.y = names(y)[1],
                                  allow.cartesian = T),
         x = list_df)
}

#' Merge Compras' Files
#'
#' @description Auxiliar function, internal use.
#'
#' @param list_df \code{list} of data.table's.
merge_compras <- function(list_df) {
  list_df[[4]][list_df[[3]], , allow.cartesian = T][list_df[[2]], , allow.cartesian = T]
}


#' Fix colnames
#'
#' Auxiliar function to standardize variables' names.
#'
#' @param x \code{colnaes(df)}
#'
#' @return a \code{string} vector with same length than \code{x}
fix_name <- function(x) {
  aux <- lapply(x,
                FUN = function(y) {
                  ifelse(Encoding(y) == 'latin1',
                         iconv(x = y, from = 'latin1', to = 'ASCII//TRANSLIT'),
                         y)
                })
  x <- unlist(unname(aux))
  x <- gsub(pattern = '-', replacement = '', x = x)
  x <- gsub(pattern = ' d(a|e|i|o|u) ', replacement = ' ', x = x)
  x <- gsub(pattern = '  ', replacement = ' ', x = x)
  x <- trimws(x = x, which = 'b')
  x <- gsub(pattern = '[ \t\r\n]', replacement = '_', x = x)
  x <- tolower(x)
  return(x)
}
