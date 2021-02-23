#' Title
#'
#' @param year
#' @param volume
#' @param number
#' @param silence
#' @param full_text
#'
#' @return
#' @export
#'
#' @examples
austral <- function(
  year, volume, number, silence = TRUE, full_text = FALSE
) {
  # PART 0: ASSERTIONS

  assert(
    year = year,
    volume = volume,
    number - number,
    silence = silence,
    full_text = full_text
  )

  # PART 1: EDITIONS LINKS

  url_archive <- "https://seer.ufrgs.br/austral/issue/archive"

  url_archive_lido <- xml2::read_html(url_archive)

  url_archive_lido %>%
    rvest::html_nodes("h4 a") %>%
    rvest::html_attr("href") -> primary_url


  url_archive_lido %>%
    rvest::html_nodes("h4 a") %>%
    rvest::html_text() %>%
    stringr::str_remove(":(.*)") -> eds

  tibble::tibble(url = primary_url,
                 editions = eds) %>%
    dplyr::mutate(
      vol = stringr::str_extract(editions, "(Vol [0-9]{2})|(Vol [0-9]{1})") %>%
        stringr::str_replace_all(.,'Vol ','') %>%
        as.integer(.),
      n = stringr::str_extract(editions,'(No [0-9]{2})|(No [0-9]{1})') %>%
        stringr::str_replace_all(.,'No ','') %>%
        as.integer(.),
      ano = stringr::str_extract(editions,"[0-9]{4}") %>%
        as.double(.),
      url = paste0(url, "/showToc")
    ) %>%
    dplyr::filter(ano %in% year &
                    n %in% number &
                    vol %in% volume) %>%
    dplyr::pull(url) -> primary_url

  # PART II: ARTICLES LINKS

  articles_url <- purrr::map(primary_url, function(x) {

    url_lido <- xml2::read_html(x)

    url_lido %>%
      rvest::html_nodes('.tocTitle a') %>%
      rvest::html_attr('href')  -> links

    url_lido %>%
      rvest::html_nodes('.tocTitle a') %>%
      rvest::html_text()  %>%
      stringr::str_remove_all("\\n|\\t")-> names


    tibble::tibble(
      links = links,
      names = names
    ) %>%
      dplyr::filter(
        !stringr::str_detect(names, '(Complete Edition)|(Complete edition)')
      ) %>%
      dplyr::pull(links)

  }) %>%
    purrr::flatten_chr()

  # PART III: SCRAPPING METADATA

  austral <- purrr::map_dfr(articles_url, function(x) {
    if(!isTRUE(silence)) {
      usethis::ui_info(paste0('Currently scrapping: ', x))
    }

    url_lido <- xml2::read_html(x)

    ## A) Filiation

    url_lido %>%
      rvest::html_nodes('meta[name="citation_author_institution"]') %>%
      rvest::html_attr('content') -> filiation

    ## B) Authors

    url_lido %>%
      rvest::html_nodes('meta[name="citation_author"]') %>%
      rvest::html_attr('content') -> authors


    if(length(filiation) == 0){
      filiation <- NA
    } else if(length(filiation) != length(authors)) {
      filiation <- NA
    }

    ## C) Title

    url_lido %>%
      rvest::html_node('meta[name="DC.Title"]') %>%
      rvest::html_attr('content') -> title

    ## D) Abstract

    url_lido %>%
      rvest::html_node('meta[name="DC.Description"]') %>%
      rvest::html_attr('content') -> abstract


    if(length(abstract) == 0){abstract <- NA}

    ## E) DOI

    url_lido %>%
      rvest::html_nodes('meta[name="DC.Identifier.DOI"]') %>%
      rvest::html_attr('content') -> doi

    if(length(doi) == 0){ doi <- NA }

    ## F) Pages

    url_lido %>%
      rvest::html_nodes('meta[name="DC.Identifier.pageNumber"]') %>%
      rvest::html_attr('content') -> pages

    if(length(pages) == 0){ pages <- NA }

    ## G) Language

    url_lido %>%
      rvest::html_nodes('meta[name="DC.Language"]') %>%
      rvest::html_attr('content') -> language

    if(length(language) == 0){ language <- NA }

    ## H) Volume

    url_lido %>%
      rvest::html_nodes('meta[name="citation_volume"]') %>%
      rvest::html_attr('content') -> volume

    ## I) Number


    url_lido %>%
      rvest::html_nodes('meta[name="citation_issue"]') %>%
      rvest::html_attr('content') -> number

    ## J) Year

    url_lido %>%
      rvest::html_nodes('meta[name="citation_date"]') %>%
      rvest::html_attr('content') %>%
      stringr::str_extract(.,'[0-9]{4}')-> year

    ## K) Keywords

    url_lido %>%
      rvest::html_nodes('meta[name="DC.Subject"]') %>%
      rvest::html_attr('content') %>%
      paste0(., collapse = ', ')-> keywords

    if(keywords == ""){keywords <- NA}

    ## L) References

    references <- NA

    ## M) Url_pdf

    url_lido %>%
      rvest::html_nodes('meta[name="citation_pdf_url"]') %>%
      rvest::html_attr('content') -> pdf_url

    if(language == 'pt') { pdf_url <- pdf_url[1]} # Portuguese pdf
    else if(language == 'en') { pdf_url <- pdf_url[2]} # English pdf

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
        periodico = "Austral: Brazilian Journal of Strategy and International Relations",
        issn = '2238-6912',
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
        periodico = "Austral: Brazilian Journal of Strategy and International Relations",
        issn = '2316-8323',
        url = x,
        pdf_url = pdf_url
      )
    }

  })

  austral
}
