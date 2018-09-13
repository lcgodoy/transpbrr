#' Download Licitacoes e Compras
#'
#' @description This functions is not ready for use.
#'
#' @param year \code{integer} between 2014 and 2018
#' @param month \code{integer} between 1 and 12
#' @param type \code{character} must be 'licitacoes' or 'compras'
#' @param opt \code{character} there are three data frames in 'licitacoes' and 'compras',
#' this parameter controls which one do you want to download. If \code{NULL} (default)
#' then this three data frames will be merged. The options are 'Item', 'Compras', 'Licitacoes',
#' 'Termo' and 'Participantes'. Note that, 'Licitacoes' and 'Participantes' are valid only if
#' \code{type} = 'licitacoes'. 'Item' and \code{NULL} are the options allowd for both types.
#' @param ... additional parameters
#'
#' @return \code{data.frame}
#' @export
download_lic_cont <- function(year = NULL, month = NULL, type = 'licitacoes', opt = NULL, ...) {
  stop('This functions is not ready to use.')

  if(any(!is.numeric(year)) | any(!year %in% 2014:as.numeric(format(Sys.Date(), '%Y'))))
    stop('Year must be integer between 2014 and 2018.')

  if(any(!is.numeric(month)) | any(!month %in% 1:12))
    stop('Month must be integer between 1 and 12.')

  if(!type %in% c('licitacao', 'compras'))
    stop("c('licitacao', 'compras') are the only valid types")

  for(i in seq_along(month)) {
    if(month[i] < 10)
      month[i] <- paste0('0', month[i])
  }

  temp_dir <- tempdir(check = T)

  link <- ('http://www.portaltransparencia.gov.br/download-de-dados/%s/%d%s')


  lapply(list.files(path = temp_dir, pattern = '.csv$', full.names = T),
         file.remove) %>% invisible()

  for(i in year) {
    for(j in month) {
      file_name <- paste0(sprintf('%s_%d%s', type, i, j), '.zip')
      dest <- paste(temp_dir, file_name, sep = '/')
      file.create(dest)
      utils::download.file(url = sprintf(link, type, i, j), destfile = dest, quiet = T, method = 'auto')
      # closeAllConnections()
      if(is.null(opt)) {
        utils::unzip(zipfile = dest, exdir = temp_dir, list = T)
      }
    }
  }

  out <- lapply(list.files(path = temp_dir, pattern = '.csv$', full.names = T),
                function(x) {
                  readLines(x) %>%
                    iconv(from = 'ISO-8859-1', to = 'ASCII//TRANSLIT') %>%
                    writeLines(con = x)
                  output <- suppressWarnings(data.table::fread(x, dec = ',', sep = ';'))
                  colnames(output) <- iconv(colnames(output), from = encoding, to = 'ASCII//TRANSLIT')
                  output
                }) %>% data.table::rbindlist()

  unlink(list.files(temp_dir, full.names = T), recursive = T)

  return(out)
}
