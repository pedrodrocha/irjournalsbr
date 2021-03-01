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
#' @examples
estudosinternacionais <- function(
  year, volume, number, silence = TRUE, full_text = FALSE
){

  # PART 0: ASSERTIONS
  assert(
    year = year,
    volume = volume,
    number = number,
    silence = silence,
    full_text = full_text
  )

  # PART 1: EDITIONS LINKS

  url_archive <- "http://periodicos.pucminas.br/index.php/estudosinternacionais/issue/archive"

  url_archive_lido <-  xml2::read_html(url_archive)


  url_archive_lido %>%
    rvest::html_nodes("#pkp_content_main .title") %>%
    rvest::html_attr("href") -> primary_url

  url_archive_lido %>%
    rvest::html_nodes('.series') %>%
    rvest::html_text() %>%
    stringr::str_remove_all("\\n|\\t") -> eds


  tibble::tibble(
    url = primary_url,
    editions = eds
  ) %>%
    dplyr::mutate(
      vol = stringr::str_extract(editions, "v. [0-9]") %>%
        stringr::str_extract(.,"[0-9]") %>%
        as.numeric(),
      n = stringr::str_extract(editions, "n. [0-9]") %>%
        stringr::str_extract(.,"[0-9]") %>%
        as.numeric(),
      ano = as.numeric(stringr::str_extract(editions,"[0-9]{4}"))
    ) %>%
    dplyr::filter(ano %in% year &
                    n %in% number &
                    vol %in% volume) %>%
    dplyr::pull(url) -> primary_url


  # PART II: ARTICLES LINKS

  articles_url <- purrr::map(primary_url, function(x){


    url_lido <- xml2::read_html(x)

    url_lido %>%
      rvest::html_nodes(".title a") %>%
      rvest::html_attr("href") -> links

    url_lido %>%
      rvest::html_nodes(".title a") %>%
      rvest::html_text() %>%
      stringr::str_remove_all("\\n|\\t") -> names

    tibble::tibble(
      links = links,
      names = names
    ) %>%
      dplyr::filter(
        names != "Edição Completa",
        names != "Páginas Iniciais"
      ) %>%
      dplyr::pull(links)

  }) %>%
    purrr::flatten_chr()

  # PART III: SCRAPPING METADATA

  estudosinternacionais <- purrr::map_dfr(articles_url, function(x) {

    if(!isTRUE(silence)) {
      usethis::ui_info(paste0('Currently scraping: ', x))
    }

    url_lido <- xml2::read_html(x)


    ## A) Filiation

    url_lido %>%
      rvest::html_nodes('meta[name="citation_author_institution"]') %>%
      rvest::html_attr('content') -> filiation

    if (length(filiation) == 0) {
      url_lido %>%
        rvest::html_nodes('.affiliation') %>%
        rvest::html_text() %>%
        stringr::str_remove_all("\\n|\\t") -> filiation
    }



    ## B) Authors

    url_lido %>%
      rvest::html_nodes('meta[name="citation_author"]') %>%
      rvest::html_attr('content') %>%
      stringr::str_trim() -> authors

    if(length(filiation) == 0){
      filiation <- NA
    } else if(length(filiation) != length(authors)) {
      filiation <- NA

    }


    ## C) Title

    url_lido %>%
      rvest::html_nodes('meta[name="DC.Title"]') %>%
      rvest::html_attr('content') -> title


    ## D) Abstract

    url_lido %>%
      rvest::html_node(xpath = "//div[@class='item abstract']") %>%
      try(rvest::html_node("p")) %>%
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


    ## G) Language

    url_lido %>%
      rvest::html_nodes('meta[name="DC.Language"]') %>%
      rvest::html_attr('content') -> language

    if(length(language) == 0){ language <- NA }


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
      rvest::html_nodes(".keywords .value") %>%
      rvest::html_text() %>%
      stringr::str_remove_all("\\n|\\t") -> keywords

    if(length(keywords) == 0){keywords <- NA }

    ## L) References

    url_lido %>%
      rvest::html_nodes(".references .value") %>%
      rvest::html_nodes("div") %>%
      rvest::html_text() -> references

    if(length(references) == 0){ references <- NA }


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
      journal = "Estudos Internacionais: Revista de Relações Internacionais da PUC Minas",
      issn = '2317-773X'
    )

  })

  estudosinternacionais
}
