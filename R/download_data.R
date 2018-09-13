#' Download Orcamento
#'
#' @family Orcamento Publico
#'
#' @param year \code{integer} between 2014 and 2018
#' @param ... additional parameters
#'
#' @examples
#'
#' (x <- download_orcamento(year = 2014))
#' (x <- download_orcamento(year = 2014:2015))
#'
#' \dontrun{
#'  (x <- download_orcamento(year = "2014"))
#'  (x <- download_orcamento(year = 2014, month = 2))
#' }
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
    dest <- file.path( temp_dir, file_name)
    file.create(dest)
    utils::download.file(url = sprintf(link, i), destfile = dest, quiet = T, mode = 'wb')
    # closeAllConnections()
    utils::unzip(zipfile = dest, exdir = temp_dir)
  }

  out <- lapply(list.files(path = temp_dir, pattern = '.csv$', full.names = T),
                function(x) {
                  readLines(x) %>%
                    iconv(from = 'ISO-8859-1', to = 'ASCII//TRANSLIT') %>%
                    writeLines(con = x)
                  output <- suppressWarnings(data.table::fread(x, dec = ',', sep = ';'))
                  output
                }) %>% data.table::rbindlist()

  unlink(list.files(temp_dir, full.names = T), recursive = T)

  return(out)
}

#' Download Transferencia
#'
#' @family Despesas Publicas
#'
#' @param year \code{integer} between 2013 and 2018
#' @param month \code{integer} between 1 and 12
#' @param ... additional parameters
#'
#' @examples
#'
#' (x <- download_transf(year = 2014, month = 1))
#' (x <- download_transf(year = 2014:2015, month = 1))
#' (x <- download_transf(year = 2015, month = 1:2))
#'
#' \dontrun{
#'  (x <- download_transf(year = "2014", month = 2))
#'  (x <- download_transf(year = 2014))
#' }
#'
#' @return \code{data.frame}
#' @export
download_transf <- function(year = NULL, month = NULL, ...) {
  if(any(!is.numeric(year)) | any(!year %in% 2013:as.numeric(format(Sys.Date(), '%Y'))))
    stop('Year must be integer between 2013 and 2018.')

  if(any(!is.numeric(month)) | any(!month %in% 1:12))
    stop('Month must be integer between 1 and 12.')

  for(i in seq_along(month)) {
    if(as.integer(month[i]) < 10) {
      month[i] <- paste0('0', month[i])
    }
  }


  temp_dir <- tempdir(check = T)

  link <- ('http://www.portaltransparencia.gov.br/download-de-dados/transferencias/%d%s')

  lapply(list.files(path = temp_dir, pattern = '.csv$', full.names = T),
         file.remove) %>% invisible()

  for(i in year) {
    for(j in month) {
      file_name <- paste0(sprintf('transferencias_%d%s', i, j), '.zip')
      dest <- file.path( temp_dir, file_name)
      file.create(dest)
      utils::download.file(url = sprintf(link, i, j), destfile = dest, quiet = T, mode = 'wb')
      # closeAllConnections()
      utils::unzip(zipfile = dest, exdir = temp_dir)
    }
  }

  out <- lapply(list.files(path = temp_dir, pattern = '.csv$', full.names = T),
                function(x) {
                  readLines(x) %>%
                    iconv(from = 'ISO-8859-1', to = 'ASCII//TRANSLIT') %>%
                    writeLines(con = x)
                  output <- suppressWarnings(data.table::fread(x, dec = ',', sep = ';'))
                  # colnames(output) <- iconv(colnames(output), )
                  output
                }) %>% data.table::rbindlist()

  unlink(list.files(temp_dir, full.names = T), recursive = T)

  return(out)
}

#' Download Execucao de Despesas
#'
#' @family Despesas Publicas
#'
#' @param year \code{integer} between 2013 and 2018
#' @param month \code{integer} between 1 and 12
#' @param ... additional parameters
#'
#' @examples
#'
#' (x <- download_exec_desp(year = 2014, month = 1))
#' (x <- download_exec_desp(year = 2014:2015, month = 1))
#' (x <- download_exec_desp(year = 2015, month = 1:2))
#'
#' \dontrun{
#'  (x <- download_exec_desp(year = "2014", month = 2))
#'  (x <- download_exec_desp(year = 2014))
#' }
#'
#' @return \code{data.frame}
#' @export
download_exec_desp <- function(year = NULL, month = NULL, ...) {
  if(any(!is.numeric(year)) | any(!year %in% 2014:as.numeric(format(Sys.Date(), '%Y'))))
    stop('Year must be integer between 2014 and 2018.')

  if(any(!is.numeric(month)) | any(!month %in% 1:12))
    stop('Month must be integer between 1 and 12.')

  for(i in seq_along(month)) {
    if(as.integer(month[i]) < 10) {
      month[i] <- paste0('0', month[i])
    }
  }

  temp_dir <- tempdir(check = T)

  link <- ('http://www.portaltransparencia.gov.br/download-de-dados/despesas-execucao/%d%s')

  lapply(list.files(path = temp_dir, pattern = '.csv$', full.names = T),
         file.remove) %>% invisible()

  for(i in year) {
    for(j in month) {
      file_name <- paste0(sprintf('transferencias_%d%s', i, j), '.zip')
      dest <- file.path( temp_dir, file_name)
      file.create(dest)
      utils::download.file(url = sprintf(link, i, j), destfile = dest, quiet = T, mode = 'wb')
      # closeAllConnections()
      utils::unzip(zipfile = dest, exdir = temp_dir)
    }
  }

  out <- lapply(list.files(path = temp_dir, pattern = '.csv$', full.names = T),
                function(x) {
                  readLines(x) %>%
                    iconv(from = 'ISO-8859-1', to = 'ASCII//TRANSLIT') %>%
                    writeLines(con = x)
                  output <- suppressWarnings(data.table::fread(x, dec = ',', sep = ';'))
                  # colnames(output) <- iconv(colnames(output), )
                  output
                }) %>% data.table::rbindlist()

  unlink(list.files(temp_dir, full.names = T), recursive = T)

  return(out)
}

#' Download Cartoes de Pagamentos
#'
#' @family Cartao de Pagamento
#'
#' @param year \code{integer} between 2013 and 2018
#' @param month \code{integer} between 1 and 12
#' @param type must be 'cpgf' (cartao de pagamentos do governo federal),
#' 'cpcc' (cartao de pagamentos do governo federal - compras centralizadas)
#' or 'cpdc' (cartao de pagamentos da defesa civil)
#' @param ... additional parameters
#'
#' @examples
#'
#' (x <- download_cp(year = 2014, month = 1, type = 'cpgf'))
#' (x <- download_cp(year = 2015, month = 1, type = 'cpcc'))
#' (x <- download_cp(year = 2015, month = 1:2, , type = 'cpdc'))
#'
#' \dontrun{
#'  (x <- download_cp(year = "2014", month = 2, type = 'cpdc'))
#'  (x <- download_cp(year = 2014, type = 'cpdc'))
#'  (x <- download_cp(year = 2014, month = 3, type = c('cpcc', 'cpdc')))
#'  (x <- download_cp(year = 2014, month = 3))
#' }
#'
#'
#' @return \code{data.frame}
#' @export
download_cp <- function(year = NULL, month = NULL, type = NULL, ...) {
  if(any(!is.numeric(year)) | any(!year %in% 2014:as.numeric(format(Sys.Date(), '%Y'))))
    stop('Year must be integer between 2014 and 2018.')

  if(any(!is.numeric(month)) | any(!month %in% 1:12))
    stop('Month must be integer between 1 and 12.')

  if(length(type) > 1)
    stop('You must provide just one type.')

  if(! type %in% c('cpgf', 'cpcc', 'cpdc'))
    stop('Type must be cpgf, cpcc or cpdc.')

  for(i in seq_along(month)) {
    if(as.integer(month[i]) < 10) {
      month[i] <- paste0('0', month[i])
    }
  }

  temp_dir <- tempdir(check = T)

  link <- ('http://www.portaltransparencia.gov.br/download-de-dados/%s/%d%s')

  lapply(list.files(path = temp_dir, pattern = '.csv$', full.names = T),
         file.remove) %>% invisible()

  for(i in year) {
    for(j in month) {
      file_name <- paste0(sprintf('%s_%d%s', type, i, j), '.zip')
      dest <- file.path( temp_dir, file_name)
      file.create(dest)
      utils::download.file(url = sprintf(link, type, i, j), destfile = dest, quiet = T, mode = 'wb')
      # closeAllConnections()
      utils::unzip(zipfile = dest, exdir = temp_dir)
    }
  }

  out <- lapply(list.files(path = temp_dir, pattern = '.csv$', full.names = T),
                function(x) {
                  readLines(x) %>%
                    iconv(from = 'ISO-8859-1', to = 'ASCII//TRANSLIT') %>%
                    writeLines(con = x)
                  output <- suppressWarnings(data.table::fread(x, dec = ',', sep = ';'))
                  # colnames(output) <- iconv(colnames(output), )
                  output
                }) %>% data.table::rbindlist()

  unlink(list.files(temp_dir, full.names = T), recursive = T)

  return(out)
}

#' Download Receitas
#'
#' @family Receitas Publicas
#'
#' @param year \code{integer} between 2013 and 2018
#' @param ... additional parameters
#'
#' @examples
#'
#' (x <- download_receitas(year = 2014))
#' (x <- download_receitas(year = 2014:2015))
#'
#' \dontrun{
#'  (x <- download_receitas(year = "2014"))
#'  (x <- download_receitas(year = 2014, month = 2))
#' }

#'
#' @return \code{data.frame}
#' @export
download_receitas <- function(year = NULL, ...) {
  if(any(!is.numeric(year)) | any(!year %in% 2013:as.numeric(format(Sys.Date(), '%Y'))))
    stop('Year must be integer between 2013 and 2018.')

  temp_dir <- tempdir(check = T)

  link <- ('http://www.portaltransparencia.gov.br/download-de-dados/receitas/%d')

  lapply(list.files(path = temp_dir, pattern = '.csv$', full.names = T),
         file.remove) %>% invisible()

  for(i in year) {
    file_name <- paste0(sprintf('receitas_%d', i), '.zip')
    dest <- file.path( temp_dir, file_name)
    file.create(dest)
    utils::download.file(url = sprintf(link, i), destfile = dest, quiet = T, mode = 'wb')
    # closeAllConnections()
    utils::unzip(zipfile = dest, exdir = temp_dir)
  }

  out <- lapply(list.files(path = temp_dir, pattern = '.csv$', full.names = T),
                function(x) {
                  readLines(x) %>%
                    iconv(from = 'ISO-8859-1', to = 'ASCII//TRANSLIT') %>%
                    writeLines(con = x)
                  output <- suppressWarnings(data.table::fread(x, dec = ',', sep = ';'))
                  # colnames(output) <- iconv(colnames(output), )
                  output
                }) %>% data.table::rbindlist()

  unlink(list.files(temp_dir, full.names = T), recursive = T)

  return(out)
}
