#' Download Licitacoes
#'
#' @description This functions is not ready for use.
#'
#' @param year \code{integer} between 2014 and 2018
#' @param month \code{integer} between 1 and 12
#' @param interactive a \code{boolean}. If \code{TRUE}, then
#' \code{file} will be ignored.
#' @param file \code{numeric} Which file do you want to access? The options are:
#' 1 for Pagamento, 2 for Passagem, 3 for Trecho, 4 for Viagem or
#' 5 for a merged file using all these information. This parameter
#' will only be used if \code{interactive = FALSE}.
#' @param ... additional parameters
#'
#' @return \code{data.frame}
#' @export
download_lic <- function(year = NULL, month = NULL,
                         interactive = NULL,
                         file = 3, ...) {
  if(any(!is.numeric(year)) | any(!year %in% 2014:as.numeric(format(Sys.Date(), '%Y'))))
    stop('Year must be integer between 2014 and 2018.')

  if(any(!is.numeric(month)) | any(!month %in% 1:12))
    stop('Month must be integer between 1 and 12.')

  month <- formatC(x = month, width = 2, flag = '0')

  temp_dir <- tempdir()
  link <- ('http://www.portaltransparencia.gov.br/download-de-dados/licitacoes/%d%s')


  unlink(list.files(path       = temp_dir,
                    pattern    = '.csv$',
                    full.names = T))

  if(is.null(interactive) & is.null(file) & !interactive()) {
    stop('you must provide a file value')
  }

  if(is.null(interactive) & interactive()) {
    cat('\n Which file do you want to load? \n')
    cat(' Options: \n')
    cat('\t 1. Item; \n')
    cat('\t 2. Licitacao; \n')
    cat('\t 3. Participantes; \n')
    user_def <- as.numeric(readline(prompt = 'Type the number corresponding to the desired option and press enter: '))
  } else {
    if(is.null(file) | file < 1 | file > 2) {
      stop('add a proper "file" value')
    }
    user_def <- file
  }

  for(i in year) {
    for(j in month) {
      file_name <- paste0(sprintf('licitacoes_%d%s', i, j), '.zip')
      dest <- paste(temp_dir, file_name, sep = '/')
      file.create(dest)
      utils::download.file(url = sprintf(link, i, j), destfile = dest, quiet = T, method = 'auto')
      # closeAllConnections()
      # if(user_def == 4) {
      #   cat('\n Warning: This functionality is under development. \n')
      #   utils::unzip(zipfile = dest, exdir = temp_dir, unzip = 'internal')
      #   invisible(
      #     file.rename(
      #       list.files(path = temp_dir, full.names = T),
      #       gsub(pattern = '\\?', replacement = '',
      #            x = iconv(list.files(temp_dir, full.names = T),
      #                      from = 'windows-1252',
      #                      to   = 'ASCII//TRANSLIT'))
      #     )
      #   )
      #
      #   to_wrt <- lapply(list.files(path = temp_dir, full.names = T, pattern = sprintf('^%d%d.*csv$', i, j)),
      #                    function(x) {
      #                      aux <- suppressWarnings(data.table::fread(x, dec = ',', sep = ';',
      #                                                                encoding = 'Latin-1', stringsAsFactors = F))
      #                      names(aux) <- trimws(iconv(names(aux), from = 'LATIN1', to = 'ASCII//TRANSLIT'), 'b')
      #                      char_fct <- which(sapply(aux, is.character))
      #                      aux[, c(char_fct) := lapply(.SD, function(x) {
      #                        iconv(x, from = 'LATIN1', to = 'ASCII//TRANSLIT')
      #                      }), .SDcols = char_fct]
      #                      aux
      #                    })
      #   to_wrt <- merge_viagens(to_wrt)
      #   data.table::fwrite(x = to_wrt, file = file.path(temp_dir, paste0(i, j,'_merged.csv')))
      # } else {
      #   file_names <- utils::unzip(zipfile = dest, exdir = temp_dir, unzip = 'internal', list = T)
      #   utils::unzip(zipfile = dest, exdir = temp_dir, unzip = 'internal', files = file_names[user_def, 1])
      #   invisible(
      #     file.rename(
      #       list.files(path = temp_dir, full.names = T),
      #       gsub(pattern = '\\?', replacement = '',
      #            x = iconv(list.files(temp_dir, full.names = T),
      #                      from = 'windows-1252',
      #                      to   = 'ASCII//TRANSLIT'))
      #     )
      #   )
      # }
      file_names <- utils::unzip(zipfile = dest, exdir = temp_dir, unzip = 'internal', list = T)
      utils::unzip(zipfile = dest, exdir = temp_dir, unzip = 'internal', files = file_names[user_def, 1])
      invisible(
        file.rename(
          list.files(path = temp_dir, full.names = T),
          gsub(pattern = '\\?', replacement = '',
               x = iconv(list.files(temp_dir, full.names = T),
                         from = 'windows-1252',
                         to   = 'ASCII//TRANSLIT'))
        )
      )
    }
  }

  out <- lapply(list.files(path = temp_dir, pattern = '.csv$', full.names = T),
                function(x) {
                  aux <- readLines(x)
                  aux <- iconv(aux, from = 'ISO-8859-1', to = 'ASCII//TRANSLIT')
                  output <- suppressWarnings(data.table::fread(text = aux, dec = ',', sep = ';'))
                  return(output)
                })

  out <- data.table::rbindlist(out)
  names(out) <- fix_name(out)

  unlink(list.files(temp_dir, full.names = T), recursive = T)

  return(out)
}
