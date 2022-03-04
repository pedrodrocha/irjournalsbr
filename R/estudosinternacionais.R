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

  url_archive_1 <- "http://periodicos.pucminas.br/index.php/estudosinternacionais/issue/archive"
  url_archive_2 <- 'http://periodicos.pucminas.br/index.php/estudosinternacionais/issue/archive/2'

  url_archive_lido_1 <-  xml2::read_html(url_archive_1)
  url_archive_lido_2 <-  xml2::read_html(url_archive_2)



  url_archive_lido_1 %>%
    rvest::html_nodes('.series') %>%
    rvest::html_text() %>%
    stringr::str_remove_all("\\n|\\t") -> eds_1
  url_archive_lido_2 %>%
    rvest::html_nodes('.series') %>%
    rvest::html_text() %>%
    stringr::str_remove_all("\\n|\\t") -> eds_2
  eds <- c(eds_1, eds_2)


  url_archive_lido_1 %>%
    rvest::html_nodes('#pkp_content_main .title') %>%
    rvest::html_attr('href') -> primary_url_1

  url_archive_lido_2 %>%
    rvest::html_nodes('#pkp_content_main .title') %>%
    rvest::html_attr('href') -> primary_url_2
  primary_url <- c(primary_url_1,primary_url_2)




  tibble::tibble(url = primary_url,
                 editions = eds) %>%
    dplyr::mutate(
      vol = stringr::str_extract(editions, "(v. [0-9]{2})|(v. [0-9]{1})") %>%
        stringr::str_replace_all(.,'v. ','') %>%
        as.integer(.),
      n = stringr::str_extract(editions,'n. [0-9]{1}') %>%
        stringr::str_replace_all(.,'n. ','') %>%
        as.integer(.),
      ano = stringr::str_extract(editions,"[0-9]{4}") %>%  as.integer(.)
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

    if(length(pdf_url) == 0){pdf_url <- "NA"}
    if(pdf_url == ""){pdf_url <- "NA"}




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


  estudosinternacionais  %>%
    dplyr::group_by(TI) %>%
    dplyr::mutate(
      AU = stringr::str_c(AU,collapse = ";"),
      OG = stringr::str_c(OG,collapse = ";")
    ) %>%
    dplyr::distinct() %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      AB = stringr::str_squish(AB)
    )
}
