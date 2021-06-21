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
bjlas <- function(
  year, volume, number, silence = TRUE, full_text = FALSE
) {

  # PART 0: ASSERTIONS
  assert(
    year = year,
    volume = volume,
    number = number,
    silence = silence,
    full_text = full_text
  )


  # PART 1: EDITIONS LINKS

  url_archive <- c(
    "https://www.revistas.usp.br/prolam/issue/archive",
    "https://www.revistas.usp.br/prolam/issue/archive/2"
  )

  eds <- purrr::map(url_archive, .f = function(x){
    url_archive_lido <-  xml2::read_html(x)

    url_archive_lido %>%
      rvest::html_nodes('.obj_issue_summary .title') %>%
      rvest::html_text() %>%
      stringr::str_remove_all("\\n|\\t")
  }) %>%
    purrr::flatten_chr()

  tibble::tibble(eds = eds) %>%
    dplyr::mutate(
      eds = dplyr::case_when(
        stringr::str_detect(eds,"China") ~ "v. 19 n. 37 (2020)",
        stringr::str_detect(eds,"Movimentos") ~ "v. 17 n. 32 (2018)",
        TRUE ~ eds
      )
    ) %>%
    dplyr::pull(eds) -> eds



  primary_url <- purrr::map(url_archive, .f = function(x){
    url_archive_lido <-  xml2::read_html(x)

    url_archive_lido %>%
      rvest::html_nodes('.obj_issue_summary .title') %>%
      rvest::html_attr("href")
  }) %>%
    purrr::flatten_chr()


  tibble::tibble(url = primary_url,
                 editions = eds) %>%
    dplyr::mutate(
      vol = stringr::str_extract(editions, "(v. [0-9]{2})|(v. [0-9]{1})") %>%
        stringr::str_replace_all(.,'v. ','') %>%
        as.integer(.),
      n = stringr::str_extract(editions,'(n. [0-9]{2})|(n. [0-9]{1})') %>%
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
      rvest::html_attr("href")



  }) %>%
    purrr::flatten_chr()

  # PART III: SCRAPPING METADATA

  bjlas <- purrr::map_dfr(articles_url, function(x) {

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
      filiation <- "NA"
    } else if(length(filiation) != length(authors)) {
      filiation <- "NA"
    }

    ## C) Title

    url_lido %>%
      rvest::html_node('meta[name="DC.Title"]') %>%
      rvest::html_attr('content') -> title

    ## G) Language

    url_lido %>%
      rvest::html_nodes('meta[name="DC.Language"]') %>%
      rvest::html_attr('content') -> language

    if(length(language) == 0){ language <- "NA" }

    ## D) Abstract

    url_lido %>%
      rvest::html_nodes('meta[name="DC.Description"]') %>%
      rvest::html_attr('content') -> abstract_texto

    if(length(abstract_texto) == 0){
      abstract <- "NA"
    } else{
      url_lido %>%
        rvest::html_nodes('meta[name="DC.Description"]') %>%
        rvest::html_attr('xml:lang') -> abstract_lingua

      tibble::tibble(texto = abstract_texto, lingua = abstract_lingua) %>%
        dplyr::filter(lingua == language) %>%
        dplyr::pull(texto) -> abstract
    }

    if(abstract == "") {abstract <- "NA"}

    if(length(abstract) == 0){ abstract <- "NA" }

    ## E) DOI

    url_lido %>%
      rvest::html_nodes('meta[name="DC.Identifier.DOI"]') %>%
      rvest::html_attr('content') -> doi

    if(length(doi) == 0){ doi <- "NA" }

    ## F) Pages

    url_lido %>%
      rvest::html_nodes('meta[name="DC.Identifier.pageNumber"]') %>%
      rvest::html_attr('content') -> pages

    if(length(pages) == 0){ pages <- "NA" }



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
      rvest::html_nodes('meta[name="citation_keywords"]') %>%
      rvest::html_attr('content') %>%
      paste0(., collapse = ', ')-> keywords

    if(keywords == ""){
      url_lido %>%
        rvest::html_nodes('.keywords .value') %>%
        rvest::html_text() %>%
        stringr::str_remove_all("\\n|\\t") -> keywords

      if(keywords == ""){
        keywords <- "NA"
      } else if(length(keywords) == 0) {
        keywords <- "NA"
      }
    } else if(length(keywords) == 0) {

      url_lido %>%
        rvest::html_nodes('.keywords .value') %>%
        rvest::html_text() %>%
        stringr::str_remove_all("\\n|\\t") -> keywords

      if(keywords == ""){
        keywords <- "NA"
      } else if(length(keywords) == 0) {
        keywords <- "NA"
      }
    }

    ## L) References

    url_lido %>%
      rvest::html_nodes(".references") %>%
      rvest::html_text() -> references

    if(length(references) == 0){references <- "NA" }

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
      journal = "Brazilian Journal of Latin American Studies",
      issn = '1676-6288'
    )

    })

  bjlas %>%
    dplyr::group_by(TI) %>%
    dplyr::mutate(
      AU = toString(AU),
      OG = toString(OG)
    ) %>%
    dplyr::distinct() %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      AB = stringr::str_squish(AB)
    )

}
