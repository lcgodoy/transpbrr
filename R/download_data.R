#' Download Orcamento
#'
#' @family Orcamento Publico
#'
#' @param year \code{integer} between 2014 and 2019
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
#' @importFrom data.table ":="
#'
#' @return \code{data.table}
#' @export
download_orcamento <- function(year = NULL, ...) {
    current_year <- as.numeric(format(Sys.Date(), '%Y'))
    if(any(!is.numeric(year)) | any(!year %in% 2014:current_year))
        stop(sprintf('Year must be a integer value between 2014 and %d.',
                     current_year))

    temp_dir <- tempdir()
    link <- 'http://www.portaltransparencia.gov.br/download-de-dados/orcamento-despesa/%d'

    invisible(   
        lapply(
            list.files(path = temp_dir, pattern = '.csv$', full.names = T),
            file.remove
        )
    )

    file_name <- vector(mode   = "character",
                        length = length(year))
    dest <- vector(mode   = "character",
                   length = length(year))
    
    for(i in seq_along(year)) {
        file_name[i] <- sprintf('orcamento_%d.zip',
                                year[i])
        dest[i] <- file.path(temp_dir, file_name[i])
        file.create(dest[i])
        utils::download.file(url = sprintf(link,
                                           year[i]),
                             destfile = dest[i],
                             quiet = TRUE,
                             mode  = 'wb')
        
        if(.Platform$OS.type == "windows") {
            utils::unzip(zipfile = dest[i],
                         exdir   = temp_dir,
                         unzip   = 'internal')
        } else {
            utils::unzip(zipfile = dest[i],
                         exdir   = temp_dir)
        }
    }

    out <- lapply(list.files(path    = temp_dir,
                             pattern = '.csv$',
                             full.names = TRUE),
                  function(x) {
                      aux <- suppressWarnings(
                          data.table::fread(x, dec = ',', sep = ';',
                                            encoding = 'Latin-1',
                                            stringsAsFactors = FALSE)
                      )
                      data.table::setnames(x = aux,
                                           old = names(aux),
                                           new = fix_name(names(aux)))

                      char_fct <- which(sapply(aux, is.character))
                      aux[, c(char_fct) := lapply(.SD, function(x) {
                          iconv(x, from = 'LATIN1', to = 'ASCII//TRANSLIT')
                      }), .SDcols = char_fct]
                      aux
                  })
    out <- data.table::rbindlist(out)

    unlink(
        list.files(temp_dir,
                   full.names = TRUE),
        recursive = TRUE
    )

    return(out)
}

#' Download Transferencia
#'
#' @family Despesas Publicas
#'
#' @param ym \code{character} year-month, see examples.
#' @param ... additional parameters
#'
#' @examples
#'
#' (x <- download_transf(ym = "201401"))
#' (x <- download_transf(ym = c("201401", "201501")))
#' (x <- download_transf(ym = c("201501", "201502")))
#'
#' \dontrun{
#'  (x <- download_transf(year = 2014))
#' }
#'
#' @return a \code{data.table} containing the data
#' @export
download_transf <- function(ym = NULL, ...) {
    stopifnot(all(nchar(x = ym) == 6L))
    current_year <- as.numeric(format(Sys.Date(), '%Y'))

    year  <- as.numeric(substr(x = ym, start = 1, stop = 4))
    month <- as.numeric(substr(x = ym, start = 5, stop = 6))
    
    if(any(!year %in% 2014:current_year))
        stop(sprintf('Year must lie between 2014 and %d.',
                     current_year))
    
    if(any(!is.numeric(month)) | any(!month %in% 1:12))
        stop('Months must lie between 01 and 12.')

    temp_dir <- tempdir()

    link <- ('http://www.portaltransparencia.gov.br/download-de-dados/transferencias/%s')

    invisible(
        lapply(list.files(path = temp_dir,
                          pattern = '.csv$',
                          full.names = TRUE),
               file.remove)
    )

    file_name <- vector(mode   = "character",
                        length = length(ym))
    dest <- vector(mode   = "character",
                   length = length(ym))
    
    for(i in seq_along(ym)) {
        file_name[i] <- sprintf('transferencias_%s.zip',
                                ym[i])
        dest[i] <- file.path(temp_dir, file_name[i])
        file.create(dest[i])
        utils::download.file(url = sprintf(link,
                                           ym[i]),
                             destfile = dest[i],
                             quiet = TRUE,
                             mode = 'wb')
        if(.Platform$OS.type == "windows") {
            utils::unzip(zipfile = dest[i],
                         exdir   = temp_dir,
                         unzip   = 'internal')
        } else {
            utils::unzip(zipfile = dest[i],
                         exdir   = temp_dir)
        }
    }

    out <- lapply(list.files(path = temp_dir,
                             pattern = '.csv$',
                             full.names = TRUE),
                  function(x) {
                      aux <- suppressWarnings(
                          data.table::fread(x, dec = ',', sep = ';',
                                            encoding = 'Latin-1',
                                            stringsAsFactors = FALSE)
                      )
                      data.table::setnames(x = aux,
                                           old = names(aux),
                                           new = fix_name(names(aux)))
                      char_fct <- which(sapply(aux, is.character))
                      aux[, c(char_fct) := lapply(.SD, function(x) {
                          iconv(x, from = 'LATIN1', to = 'ASCII//TRANSLIT')
                      }), .SDcols = char_fct]
                      aux
                  })
    out <- data.table::rbindlist(out)

    unlink(
        list.files(temp_dir,
                   full.names = TRUE),
        recursive = TRUE
    )

    return(out)
}

#' Download Execucao de Despesas
#'
#' @family Despesas Publicas
#'
#' @param ym \code{character} year-month, see examples.
#' @param ... additional parameters
#'
#' @examples
#'
#' (x <- download_exec_desp(ym = "201401"))
#' (x <- download_exec_desp(ym = c("201401", "201501")))
#' (x <- download_exec_desp(ym = c("201501", "201502")))
#'
#' \dontrun{
#'  (x <- download_exec_desp(year = "2014", month = 2))
#'  (x <- download_exec_desp(year = 2014))
#' }
#'
#' @return \code{data.table}
#' @export
download_exec_desp <- function(ym = NULL, ...) {
    stopifnot(all(nchar(x = ym) == 6L))
    current_year <- as.numeric(format(Sys.Date(), '%Y'))

    year  <- as.numeric(substr(x = ym, start = 1, stop = 4))
    month <- as.numeric(substr(x = ym, start = 5, stop = 6))
    
    if(any(!year %in% 2014:current_year))
        stop(sprintf('Year must lie between 2014 and %d.',
                     current_year))
    
    if(any(!is.numeric(month)) | any(!month %in% 1:12))
        stop('Months must lie between 01 and 12.')

    temp_dir <- tempdir()

    link <- 'http://www.portaltransparencia.gov.br/download-de-dados/despesas-execucao/%s'

    invisible(
        lapply(list.files(path = temp_dir,
                          pattern = '.csv$',
                          full.names = TRUE),
               file.remove)
    )

    file_name <- vector(mode   = "character",
                        length = length(ym))
    dest <- vector(mode   = "character",
                   length = length(ym))
    
    for(i in seq_along(ym)) {
        file_name[i] <- sprintf('transferencias_%s.zip',
                                ym[i])
        dest[i] <- file.path(temp_dir, file_name[i])
        file.create(dest[i])
        utils::download.file(url = sprintf(link,
                                           ym[i]),
                             destfile = dest[i],
                             quiet = TRUE,
                             mode = 'wb')
        
        if(.Platform$OS.type == "windows") {
            utils::unzip(zipfile = dest[i],
                         exdir   = temp_dir,
                         unzip   = 'internal')
        } else {
            utils::unzip(zipfile = dest[i],
                         exdir   = temp_dir)
        }
    }

    out <- lapply(list.files(path = temp_dir,
                             pattern = '.csv$',
                             full.names = TRUE),
                  function(x) {
                      aux <- suppressWarnings(
                          data.table::fread(x, dec = ',', sep = ';',
                                            encoding = 'Latin-1',
                                            stringsAsFactors = FALSE)
                      )
                      data.table::setnames(x = aux,
                                           old = names(aux),
                                           new = fix_name(names(aux)))
                      char_fct <- which(sapply(aux, is.character))
                      aux[, c(char_fct) := lapply(.SD, function(x) {
                          iconv(x, from = 'LATIN1', to = 'ASCII//TRANSLIT')
                      }), .SDcols = char_fct]
                      aux
                  })
    out <- data.table::rbindlist(out)

    unlink(
        list.files(temp_dir,
                   full.names = TRUE),
        recursive = TRUE
    )

    return(out)
}

#' Download Cartoes de Pagamentos
#'
#' @family Cartao de Pagamento
#'
#' @param ym \code{character} year-month, see examples.
#' @param type must be 'cpgf' (cartao de pagamentos do governo federal),
#' 'cpcc' (cartao de pagamentos do governo federal - compras centralizadas)
#' or 'cpdc' (cartao de pagamentos da defesa civil)
#' @param ... additional parameters
#'
#' @examples
#'
#' (x <- download_cp(ym = "201401", type = 'cpgf'))
#' (x <- download_cp(ym = "201501", type = 'cpcc'))
#' (x <- download_cp(ym = c("201501", "201502"), type = 'cpdc'))
#'
#'
#' @return a \code{data.table} containing the data
#' @export
download_cp <- function(ym = NULL, type = NULL, ...) {
    stopifnot(all(nchar(x = ym) == 6L))
    current_year <- as.numeric(format(Sys.Date(), '%Y'))

    year  <- as.numeric(substr(x = ym, start = 1, stop = 4))
    month <- as.numeric(substr(x = ym, start = 5, stop = 6))
    
    if(any(!year %in% 2014:current_year))
        stop(sprintf('Year must lie between 2014 and %d.',
                     current_year))
    
    if(any(!is.numeric(month)) | any(!month %in% 1:12))
        stop('Months must lie between 01 and 12.')

    if(length(type) > 1)
        stop('You must provide only one type.')

    if(! type %in% c('cpgf', 'cpcc', 'cpdc'))
        stop('Type must be cpgf, cpcc or cpdc.')

    temp_dir <- tempdir()

    link <- 'http://www.portaltransparencia.gov.br/download-de-dados/%s/%s'

    invisible(
        lapply(
            list.files(path = temp_dir,
                       pattern = '.csv$',
                       full.names = TRUE),
            file.remove
        )
    )

    file_name <- vector(mode   = "character",
                        length = length(ym))
    dest <- vector(mode   = "character",
                   length = length(ym))

    
    for(i in seq_along(ym)) {
        file_name[i] <- sprintf('%s_%s.zip', type,
                                ym[i])
        dest[i] <- file.path(temp_dir, file_name[i])
        file.create(dest[i])
        utils::download.file(url = sprintf(link,
                                           type,
                                           ym[i]),
                             destfile = dest[i],
                             quiet = TRUE,
                             mode = 'wb')
        
        if(.Platform$OS.type == "windows") {
            utils::unzip(zipfile = dest[i],
                         exdir   = temp_dir,
                         unzip   = 'internal')
        } else {
            utils::unzip(zipfile = dest[i],
                         exdir   = temp_dir)
        }
    }

    out <- lapply(list.files(path = temp_dir,
                             pattern = '.csv$',
                             full.names = TRUE),
                  function(x) {
                      aux <- suppressWarnings(
                          data.table::fread(x, dec = ',', sep = ';',
                                            encoding = 'Latin-1',
                                            stringsAsFactors = FALSE)
                      )
                      data.table::setnames(
                                      x = aux,
                                      old = names(aux),
                                      new = fix_name(names(aux))
                                  )
                      char_fct <- which(sapply(aux, is.character))
                      aux[, c(char_fct) := lapply(.SD, function(x) {
                          iconv(x, from = 'LATIN1', to = 'ASCII//TRANSLIT')
                      }), .SDcols = char_fct]
                      aux
                  })
    out <- data.table::rbindlist(out)

    unlink(
        list.files(temp_dir,
                   full.names = TRUE),
        recursive = TRUE
    )

    return(out)
}

#' Download Receitas
#'
#' @family Receitas Publicas
#'
#' @param year \code{character} between 2013 and 2019
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
    current_year <- as.numeric(format(Sys.Date(), '%Y'))
    if(any(!is.numeric(year)) | any(!year %in% 2014:current_year))
        stop(sprintf('Year must be a integer value between 2014 and %d.',
                     current_year))

    temp_dir <- tempdir()

    link <- 'http://www.portaltransparencia.gov.br/download-de-dados/receitas/%d'

    invisible(
        lapply(list.files(path = temp_dir,
                          pattern = '.csv$',
                          full.names = TRUE),
               file.remove)
    )

    file_name <- vector(mode   = "character",
                        length = length(year))
    dest <- vector(mode   = "character",
                   length = length(year))
    
    for(i in seq_along(year)) {
        file_name[i] <- sprintf('receitas_%d.zip', year[i])
        dest[i] <- file.path(temp_dir, file_name[i])
        file.create(dest[i])
        utils::download.file(url = sprintf(link, year[i]),
                             destfile = dest[i],
                             quiet = TRUE,
                             mode = 'wb')
                                        # closeAllConnections()
        if(.Platform$OS.type == "windows") {
            utils::unzip(zipfile = dest[i],
                         exdir   = temp_dir,
                         unzip   = 'internal')
        } else {
            utils::unzip(zipfile = dest[i],
                         exdir   = temp_dir)
        }
    }

    out <- lapply(list.files(path = temp_dir,
                             pattern = '.csv$',
                             full.names = TRUE),
                  function(x) {
                      aux <- suppressWarnings(
                          data.table::fread(x, dec = ',', sep = ';',
                                            encoding = 'Latin-1',
                                            stringsAsFactors = FALSE)
                      )
                      data.table::setnames(x = aux,
                                           old = names(aux),
                                           new = fix_name(names(aux)))
                      char_fct <- which(sapply(aux, is.character))
                      aux[, c(char_fct) := lapply(.SD, function(x) {
                          iconv(x, from = 'LATIN1', to = 'ASCII//TRANSLIT')
                      }), .SDcols = char_fct]
                      aux
                  })
    out <- data.table::rbindlist(out)

    unlink(
        list.files(temp_dir,
                   full.names = TRUE),
        recursive = TRUE
    )

    return(out)
}

#' Download Viagens
#'
#' @family Viagens
#'
#' @param year \code{integer} between 2013 and 2019
#' @param interactive a \code{boolean}. If \code{TRUE}, then
#' \code{file} will be ignored.
#' @param file Which file do you want to access? The options are:
#' 1 for Pagamento, 2 for Passagem, 3 for Trecho, 4 for Viagem or
#' 5 for a merged file using all these information. This parameter
#' will only be used if \code{interactive = FALSE}.
#' @param ... additional parameters
#'
#' @examples
#'
#' \dontrun{
#'  (x <- download_viagens(year = 2018))
#'  (x <- download_viagens(year = 2017:2018))
#'
#'  (x <- download_viagens(year = 2014, interactive = F, file = 3))
#' }
#'
#' @return \code{data.table}
#' @export
download_viagens <- function(year = NULL, interactive = TRUE, file = 5, ...) {
    current_year <- as.numeric(format(Sys.Date(), '%Y'))
    if(any(!is.numeric(year)) | any(!year %in% 2014:current_year))
        stop(sprintf('Year must be a integer value between 2014 and %d.',
                     current_year))

    temp_dir <- tempdir()
    link <- ('http://www.portaltransparencia.gov.br/download-de-dados/viagens/%d')

    invisible(
        lapply(list.files(path = temp_dir,
                          pattern = '.csv$',
                          full.names = TRUE),
               file.remove)
    )

    file_name <- vector(mode   = "character",
                        length = length(year))
    dest <- vector(mode   = "character",
                   length = length(year))
    
    if(interactive) {
        cat('\n Which file do you want to load? \n')
        cat(' Options: \n')
        cat('\t 1. Pagamento; \n')
        cat('\t 2. Passagem; \n')
        cat('\t 3. Trecho; \n')
        cat('\t 4. Viagem; \n')
        cat('\t 5. All - Merged File.; \n')
        user_def <- as.numeric(readline(prompt = 'Type the number corresponding to the desired option and press enter: '))
    } else {
        user_def <- file
    }

    stopifnot(user_def %in% 1:5)

    file_name <- vector(mode   = "character",
                        length = length(year))
    dest <- vector(mode   = "character",
                   length = length(year))
    
    for(i in seq_along(year)) {
        file_name[i] <- sprintf('viagens_%d.zip',
                                year[i])
        dest[i] <- file.path(temp_dir, file_name[i])
        file.create(dest[i])
        cat('Trying to download files from year ', year[i], '. \n', sep = '')
        utils::download.file(url = sprintf(link,
                                           year[i]),
                             destfile = dest[i],
                             quiet = TRUE,
                             mode = 'wb')
        if(user_def == 5) {
            cat('\n Warning: This functionality is under development. \n')
            utils::unzip(zipfile = dest[i],
                         exdir   = temp_dir,
                         unzip   = 'internal')
            to_wrt <- lapply(list.files(path = temp_dir,
                                        full.names = TRUE,
                                        pattern = sprintf('[%d]*.csv$',
                                                          year[i])),
                             function(x) {
                                 aux <- suppressWarnings(
                                     data.table::fread(x, dec = ',', sep = ';',
                                                       encoding = 'Latin-1',
                                                       stringsAsFactors = FALSE)
                                 )
                                 data.table::setnames(x = aux,
                                                      old = names(aux),
                                                      new = fix_name(names(aux)))
                                 char_fct <- which(sapply(aux, is.character))
                                 aux[, c(char_fct) := lapply(.SD, function(x) {
                                     iconv(x, from = 'LATIN1', to = 'ASCII//TRANSLIT')
                                 }), .SDcols = char_fct]
                                 aux
                             })
            to_wrt <- merge_viagens(to_wrt)
            data.table::fwrite(x = to_wrt,
                               file = file.path(temp_dir,
                                                paste0(year[i], '_merged.csv')))
        } else {
            file_names <- utils::unzip(zipfile = dest[i],
                                       exdir   = temp_dir,
                                       unzip   = 'internal',
                                       list    = TRUE)
            utils::unzip(zipfile = dest[i],
                         exdir   = temp_dir,
                         unzip   = 'internal',
                         files   = file_names[user_def, 1])
        }
    }

    if(user_def == 5) {
        out <- lapply(list.files(path = temp_dir,
                                 pattern = '.*merge.*csv$',
                                 full.names = TRUE),
                      function(x) {
                          suppressWarnings(data.table::fread(x))
                      })
    } else {
        out <- lapply(list.files(path = temp_dir,
                                 pattern = 'csv$',
                                 full.names = TRUE),
                      function(x) {
                          aux <- suppressWarnings(
                              data.table::fread(x, dec = ',', sep = ';',
                                                encoding = 'Latin-1',
                                                stringsAsFactors = FALSE)
                          )
                          
                          data.table::setnames(x = aux,
                                               old = names(aux),
                                               new = fix_name(names(aux)))
                          char_fct <- which(sapply(aux, is.character))
                          aux[, c(char_fct) := lapply(.SD, function(x) {
                              iconv(x, from = 'LATIN1', to = 'ASCII//TRANSLIT')
                          }), .SDcols = char_fct]
                          aux
                      })
    }

    out <- data.table::rbindlist(out)
    unlink(
        list.files(temp_dir,
                   full.names = TRUE),
        recursive = TRUE
    )

    return(out)
}

#' Download Compras
#'
#' @description This functions is not ready for use.
#'
#' @param ym \code{character} year-month
#' @param interactive a \code{boolean}. If \code{TRUE}, then
#' \code{file} will be ignored.
#' @param file \code{numeric} Which file do you want to access? The options are:
#' 1 for Apostilamento, 2 for Compras, 3 for Item, 4 for Termo Aditivo or
#' 5 for a merged file using all these information. This parameter
#' will be used only if \code{interactive = FALSE}.
#' @param ... additional parameters
#'
#' @return \code{data.frame}
#' @export
download_compras <- function(ym          = NULL,
                             interactive = FALSE,
                             file        = NULL, ...) {
    stopifnot(all(nchar(x = ym) == 6L))
    current_year <- as.numeric(format(Sys.Date(), '%Y'))

    year  <- as.numeric(substr(x = ym, start = 1, stop = 4))
    month <- as.numeric(substr(x = ym, start = 5, stop = 6))
    
    if(any(!year %in% 2014:current_year))
        stop(sprintf('Year must lie between 2014 and %d.',
                     current_year))
    
    if(any(!is.numeric(month)) | any(!month %in% 1:12))
        stop('Months must lie between 01 and 12.')

    temp_dir <- tempdir()
    link <- ('http://www.portaltransparencia.gov.br/download-de-dados/compras/%d%s')

    unlink(list.files(path       = temp_dir,
                      pattern    = '.csv$',
                      full.names = T))

    if(interactive) {
        cat('\n Which file do you want to load? \n')
        cat(' Options: \n')
        cat('\t 1. Apostilamento; \n')
        cat('\t 2. Compras; \n')
        cat('\t 3. Item; \n')
        cat('\t 4. Termo Aditivo; \n')
        cat('\t 5. All - Merged File.; \n')
        user_def <- as.numeric(readline(prompt = 'Type the number corresponding to the desired option and press enter: '))
    } else {
        if(is.null(file) | file < 1 | file > 5) {
            stop('add a proper "file" value')
        }
        if(file == 4) {
            stop('apostilamento not working')
        }
        user_def <- file
    }

    file_name <- vector(mode   = "character",
                        length = length(year))
    dest <- vector(mode   = "character",
                   length = length(year))
    
    for(i in seq_along(ym)) {
            file_name[i] <- sprintf('compras_%s.zip', ym[i])
            dest[i]      <- file.path(temp_dir, file_name[i])
            file.create(dest[i])
            utils::download.file(url = sprintf(link, ym[i]),
                                 destfile = dest,
                                 quiet = TRUE,
                                 method = 'wb')

            ls_files <- utils::unzip(zipfile = dest[i],
                                     exdir = temp_dir,
                                     list = T)[, 1]
            if(user_def == 5) {
                cat('\n Warning: This functionality is under development. \n')
                utils::unzip(zipfile = dest[i],
                             exdir   = temp_dir,
                             unzip   = 'internal')
                to_wrt <- lapply(list.files(path = temp_dir,
                                            full.names = TRUE,
                                            pattern = sprintf('^%s.*csv$', ym[i])),
                                 function(x) {
                                     aux <- suppressWarnings(
                                         data.table::fread(x, dec = ',', sep = ';',
                                                           encoding = 'Latin-1',
                                                           stringsAsFactors = FALSE)
                                     )
                                     data.table::setnames(x = aux,
                                                          old = names(aux),
                                                          new = fix_name(names(aux)))
                                     char_fct <- which(sapply(aux, is.character))
                                     if(length(char_fct) > 0) {
                                         aux[, c(char_fct) := lapply(.SD, function(x) {
                                             iconv(x, from = 'LATIN1', to = 'ASCII//TRANSLIT')
                                         }), .SDcols = char_fct]
                                     }
                                     data.table::setkey(x = aux, 'numero_contrato',
                                                        'codigo_orgao', 'nome_orgao',
                                                        'codigo_ug', 'nome_ug')
                                     aux
                                 })

                to_wrt <- merge_compras(to_wrt)
                data.table::fwrite(x = to_wrt,
                                   file = file.path(temp_dir,
                                                    paste0(ym[i], '_merged.csv')))
            } else {
                file_names <- utils::unzip(zipfile = dest[i],
                                           exdir = temp_dir,
                                           unzip = 'internal',
                                           list = TRUE)
                utils::unzip(zipfile = dest[i],
                             exdir = temp_dir,
                             unzip = 'internal',
                             files = file_names[user_def, 1])
            }
    }

    if(user_def == 5) {
        out <- lapply(list.files(path = temp_dir, pattern = '.*merge.*csv$', full.names = T),
                      function(x) {
                          suppressWarnings(data.table::fread(x))
                      })
    } else {
        out <- lapply(list.files(path = temp_dir, pattern = '.csv$', full.names = T),
                      function(x) {
                          aux <- readLines(x)
                          aux <- iconv(aux, from = 'ISO-8859-1', to = 'ASCII//TRANSLIT')
                          output <- suppressWarnings(data.table::fread(text = aux, dec = ',', sep = ';'))
                          return(output)
                      })
    }
    out <- data.table::rbindlist(out)

    unlink(
        list.files(temp_dir,
                   full.names = TRUE),
        recursive = TRUE)

    return(out)
}
