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
conjunturalaustral <- function(
  year, volume, number, silence = TRUE, full_text = FALSE
) {

  # PART 0A: ASSERTIONS

  assert(
    year = year,
    volume = volume,
    number = number,
    silence = silence,
    full_text = full_text
  )


  # PART 0B: SOLVING ISSUE NUMBER PROBLEM

  tibble::tibble(number = as.character(number)) %>%
    dplyr::mutate(number = dplyr::case_when(
      number %in% c("39","40") ~ "39-40",
      number %in% c("33","34") ~ "33-34",
      number %in% c("27","28") ~ "27-28",
      number %in% c("21","22") ~ "21-22",
      number %in% c("15","16") ~ "15-16",
      number %in% c("9","10") ~ "9-10",
      number %in% c("3","4") ~ "3-4",
      TRUE ~ number
    )
    ) %>%
    dplyr::distinct() %>%
    dplyr::pull(number) -> number




  # PART 1: EDITIONS LINKS
  url_archive <- "https://seer.ufrgs.br/ConjunturaAustral/issue/archive"

  url_archive <- xml2::read_html(url_archive)

  url_archive %>%
    rvest::html_nodes("h4 a") %>%
    rvest::html_attr("href") -> primary_url


  url_archive %>%
    rvest::html_nodes("h4 a") %>%
    rvest::html_text() %>%
    stringr::str_remove(":(.*)") -> eds

  tibble::tibble(url = primary_url,
                 editions = eds) %>%
    dplyr::mutate(
      vol = stringr::str_extract(editions, "(v. [0-9]{2})|(v. [0-9]{1})") %>%
        stringr::str_replace_all(.,'v. ','') %>%
        as.integer(.),
      n = stringr::str_extract(editions,'(n. [0-9]{2}-[0-9]{2})|(n. [0-9]{1}-[0-9]{2})|((n. [0-9]{1}-[0-9]{1}))|(n. [0-9]{2})|(n. [0-9]{1})') %>%
        stringr::str_replace_all(.,'n. ',''),
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
        !stringr::str_detect(names, 'EDIÇÃO COMPLETA')
      ) %>%
      dplyr::pull(links)

  }) %>%
    purrr::flatten_chr()

  conjunturaaustral <- purrr::map_dfr(articles_url, function(x) {
    if(!isTRUE(silence)) {
      usethis::ui_info(paste0('Currently scraping: ', x))
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
      rvest::html_nodes("#articleAbstract div") %>%
      rvest::html_text() -> abstract

    if(length(abstract) == 0){ abstract <- NA }

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

    ## H) Volume

    url_lido %>%
      rvest::html_nodes('meta[name="DC.Source.Volume"]') %>%
      rvest::html_attr('content') -> volume


    ## I) Number

    url_lido %>%
      rvest::html_nodes('meta[name="DC.Source.Issue"]') %>%
      rvest::html_attr('content') -> number


    ## J) Year

    url_lido %>%
      rvest::html_nodes('meta[name="citation_date"]') %>%
      rvest::html_attr('content') %>%
      stringr::str_extract('[0-9]{4}')-> year

    ## K) Keywords

    url_lido %>%
      rvest::html_nodes('meta[name="DC.Subject"]') %>%
      rvest::html_attr('content') -> keywords

    teste_keywords <- paste0(keywords, collapse = ', ')

    if(teste_keywords == ""){
      keywords <- NA
    } else if(length(teste_keywords) == 0) {
      keywords <- NA
    } else {
      url_lido %>%
        rvest::html_nodes('meta[name="DC.Subject"]') %>%
        rvest::html_attr('xml:lang') -> keywords_lang

      tibble::tibble(keywords = keywords, lang = keywords_lang) %>%
        dplyr::filter(lang == language) %>%
        dplyr::pull(keywords) %>%
        paste0(., collapse = ', ')-> keywords

    }

    ## L) References

    references <- NA

    ## M) Url_pdf

    url_lido %>%
      rvest::html_nodes('meta[name="citation_pdf_url"]') %>%
      rvest::html_attr('content') -> pdf_url

    build_data(
      authors = authors,
      filiation = filiation,
      title = title,
      abstract = abstract,
      keywords = keywords,
      references = references,
      pages = pages,
      year = year,
      volume = volume,
      number = number,
      language = language ,
      doi = doi,
      x = x,
      pdf_url = pdf_url,
      full_text = full_text,
      journal = "Conjuntura Austral: Journal of the Global South",
      issn = '2178-8839'
    )

  })

  conjunturaaustral
}
