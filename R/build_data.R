#' Title
#'
#' @param journal
#' @param issn
#' @param pdf_url
#' @param full_text
#' @param authors
#' @param filiation
#' @param title
#' @param abstract
#' @param keywords
#' @param references
#' @param pages
#' @param year
#' @param volume
#' @param number
#' @param language
#' @param doi
#' @param x
#'
#' @return
#'
#'
#' @examples
build_data <- function(
  authors,
  filiation,
  title,
  abstract,
  keywords,
  references,
  pages,
  year,
  volume,
  number,
  language,
  doi,
  x,
  pdf_url,
  full_text,
  journal,
  issn
) {

  if(isTRUE(full_text)) {

    ## N) Content

    suppressMessages(pdftools::pdf_text(pdf = pdf_url)) %>%
      readr::read_lines() %>%
      stringr::str_trim() %>%
      stringr::str_c(collapse = ' ') -> content

    tibble::tibble(
      AU = authors,
      OG = filiation,
      TI = title,
      AB = abstract,
      DE = keywords,
      CR = references,
      BP_EP = pages,
      PY = year,
      IS = paste0('v. ',volume,' n. ', number, ' (',year,')'),
      LA = language,
      DI = doi,
      SO = journal,
      SN = issn,
      URL = x,
      PDFURL = pdf_url,
      FULLTEXT = content
    )


  } else {

    tibble::tibble(
      AU = authors,
      OG = filiation,
      TI = title,
      AB = abstract,
      DE = keywords,
      CR = references,
      BP_EP = pages,
      PY = year,
      IS = paste0('v. ',volume,' n. ', number, ' (',year,')'),
      LA = language,
      DI = doi,
      SO = journal,
      SN = issn,
      URL = x,
      PDFURL = pdf_url,
    )
  }

}
