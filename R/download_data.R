#' Download Orcamento
#'
#' @param year \code{integer} between 2014 and 2018
#' @param ... additional parameters
#'
#' @return \code{data.frame}
#' @export
download_orcamento <- function(year = NULL, ...) {
  if(any(!is.numeric(year)) | any(!year %in% 2014:as.numeric(format(Sys.Date(), '%Y'))))
    stop('Year must be a integer value between 2014 and 2018.')

  temp_dir <- tempdir(check = T)
  link <- ('http://www.portaltransparencia.gov.br/download-de-dados/orcamento-despesa/%d')

  lapply(list.files(path = temp_dir, pattern = '.csv$', full.names = T),
         file.remove) %>% invisible()

  for(i in year) {
    file_name <- paste0(sprintf('orcamento_%d', i), '.zip')
    dest <- paste(temp_dir, file_name, sep = '/')
    file.create(dest)
    download.file(url = sprintf(link, i), destfile = dest, quiet = T, method = 'wget')
    # closeAllConnections()
    unzip(zipfile = dest, exdir = temp_dir)
  }

  out <- lapply(list.files(path = temp_dir, pattern = '.csv$', full.names = T),
                function(x) {
                  encoding <- readr::guess_encoding(file = x,
                                                    n_max = -1)[1,1] %>%
                    as.character()
                  output <- suppressWarnings(data.table::fread(x, dec = ',', sep = ';'))
                  colnames(output) <- iconv(colnames(output), from = encoding, to = 'ASCII//TRANSLIT')
                  output
                }) %>% dplyr::bind_rows()

  unlink(list.files(temp_dir, full.names = T), recursive = T)

  return(out)
}

#' Download Licitacoes e Compras
#'
#' @param year \code{integer} between 2014 and 2018
#' @param month \code{integer} between 1 and 12
#' @param type \code{character} must be 'licitacoes' or 'compras'
#' @param ... additional parameters
#'
#' @return \code{data.frame}
#' @export
download_lic_cont <- function(year = NULL, month = NULL, type = 'licitacoes', ...) {
  if(any(!is.numeric(year)) | any(!year %in% 2014:as.numeric(format(Sys.Date(), '%Y'))))
    stop('Year must be integer between 2014 and 2018.')

  if(any(!is.numeric(month)) | any(!month %in% 1:12))
    stop('Month must be integer between 1 and 12.')

  if(!type %in% c('licitacao', 'compras'))
    stop("c('licitacao', 'compras') are the only valid types")

  if(month < 10)
    month <- paste0('0', month)

  temp_dir <- tempdir(check = T)

  link <- ('http://www.portaltransparencia.gov.br/download-de-dados/%s/%d%s')


  lapply(list.files(path = temp_dir, pattern = '.csv$', full.names = T),
         file.remove) %>% invisible()

  for(i in year) {
    for(j in month) {
      file_name <- paste0(sprintf('%s_%d-%s', type, i, j), '.zip')
      dest <- paste(temp_dir, file_name, sep = '/')
      file.create(dest)
      download.file(url = sprintf(link, type, i, j), destfile = dest, quiet = T, method = 'wget')
      # closeAllConnections()
      unzip(zipfile = dest, exdir = temp_dir, list = T)
    }
  }

  out <- lapply(list.files(path = temp_dir, pattern = '.csv$', full.names = T),
                function(x) {
                  encoding <- readr::guess_encoding(file = x,
                                                    n_max = -1)[1,1] %>%
                    as.character()
                  output <- suppressWarnings(data.table::fread(x, dec = ',', sep = ';'))
                  colnames(output) <- iconv(colnames(output), from = encoding, to = 'ASCII//TRANSLIT')
                  output
                }) %>% dplyr::bind_rows()

  unlink(list.files(temp_dir, full.names = T), recursive = T)

  return(out)
}
