#' @title Download Bolsa Familia
#'
#' @family Beneficios ao cidadao
#'
#' @description Download of the Bolsa Familia payment and withdrawal data.
#'
#' @param year \code{integer} between 2013 and 2018
#' @param month \code{integer} between 1 and 12
#' @param query a \code{character} string indicating whether to download payment ('payment' or 'pagamento') data or withdrawal ('withdrawal' or 'saque') data
#' @param ... additional parameters
#'
#' @return \code{data.frame}
#' @export
download_bf <- function(year = NULL, month = NULL, query = 'payments', ...) {

  if(any(!is.numeric(year)) | any(!year %in% 2013:as.numeric(format(Sys.Date(), '%Y')))) {
    stop('Year must be a integer between 2013 and 2018.')
  }

  if(any(!is.numeric(month)) | any(!month %in% 1:12)) {
    stop('Month must be a integer between 1 and 12')
  }

  query_types <- c('payments' = 'bolsa-familia-pagamentos',
                   'withdrawal' = 'bolsa-familia-saques',
                   'pagamentos' = 'bolsa-familia-pagamentos',
                   'saques' = 'bolsa-familia-saques')

  if(any(!is.character(query)) | any(!query %in% names(query_types))) {
    stop('Query must be a character with: payment, withdrawal, pagamento or saque')
  }

  query_format <- query_types[query]
  month_format <- formatC(month, flag = 0, digits = 2, width = 2)
  year_month   <- expand.grid(year, month_format)
  year_month_format <- paste0(year_month[, 1], year_month[, 2])

  link <- 'http://www.portaltransparencia.gov.br/download-de-dados/%s/%s'
  url_built <- sprintf(link, query_format, year_month_format)

  invisible(lapply(list.files(path = temp_dir, pattern = '.csv$', full.names = T),
         file.remove))

  if (requireNamespace("RCurl", quietly = TRUE)) {
    ##-- File size ----
    file_info <-  RCurl::getURL(url_built, nobody = 1L, header = 1L)
    file_size <-  sapply(strsplit(file_info, "\r\n"), function(x) grep('Content-Length', x, value = T))
    file_size <-  as.numeric(gsub("\\D", "", file_size))
    file_size <-  round(file_size/(1024 * 1024), 1)
    total_size <- sum(file_size)
    size_type  <- "Mb"

    if(total_size > 1024) {
      total_size <- round(total_size/1024, 1)
      size_type  <- "Gb"
    }

    msg_file_size <- sprintf(" These files have ~%s %s. \n Do you want to continue the download?", total_size, size_type)
    ans <- utils::menu(choices = c("Yes", 'No'), title = msg_file_size)

    if(ans == 2) {
      return('Download has been canceled.')
    }
  }

  temp_dir  <- tempdir()
  file_name <- paste0(sprintf('bf_%s_%s', query, year_month_format), '.zip')
  dest_file <- file.path(temp_dir, file_name)
  utils::download.file(url = url_built, destfile = dest_file, quiet = T, mode = 'wb')

  if(.Platform$OS.type == "windows") {
    file_name_unzip <- sapply(dest_file, function(x) utils::unzip(zipfile = x, exdir = temp_dir, unzip = 'internal'))
  } else {
    file_name_unzip <- sapply(dest_file, function(x) utils::unzip(zipfile = x, exdir = temp_dir))
  }

  data_list <- suppressWarnings(lapply(file_name_unzip, data.table::fread, dec = ',', sep = ';', encoding = 'Latin-1', stringsAsFactors = F))
  dt <- data.table::rbindlist(data_list)

  unlink(list.files(temp_dir, full.names = T), recursive = T)

  return(dt)
}
