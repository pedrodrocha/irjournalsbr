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

  if(full_text) {

    ## N) Content

    suppressMessages(pdftools::pdf_text(pdf = pdf_url)) %>%
      readr::read_lines() %>%
      stringr::str_trim() %>%
      stringr::str_c(collapse = ' ') -> content

    tibble::tibble(
      autores = authors,
      filiacao = filiation,
      titulo = title,
      resumo = abstract,
      palavras_chave = keywords,
      referencias = references,
      paginas = pages,
      ano = year,
      edicao = paste0('v. ',volume,' n. ', number, ' (',year,')'),
      idioma = language,
      doi = doi,
      periodico = journal,
      issn = issn,
      url = x,
      pdf_url = pdf_url,
      texto_completo = content
    )


  } else {

    tibble::tibble(
      autores = authors,
      filiacao = filiation,
      titulo = title,
      resumo = abstract,
      palavras_chave = keywords,
      referencias = references,
      paginas = pages,
      ano = year,
      edicao = paste0('v. ',volume,' n. ', number, ' (',year,')'),
      idioma = language,
      doi = doi,
      periodico = journal,
      issn = issn,
      url = x,
      pdf_url = pdf_url
    )
  }

}
