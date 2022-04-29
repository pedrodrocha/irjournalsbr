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
  base <- "https://seer.ufrgs.br/index.php/ConjunturaAustral/issue/archive/"
  url_archive <- stringr::str_c(base,c('1','2','3','4'))


  grab_ed_links <- function(archive){

    url_lido <- xml2::read_html(archive)

    url_lido %>%
      rvest::html_nodes('.obj_issue_summary .title') %>%
      rvest::html_text() %>%
      stringr::str_remove_all("\\n|\\t") -> eds

    url_lido %>%
      rvest::html_nodes('.obj_issue_summary .title') %>%
      rvest::html_attr('href') -> primary_url

    tibble::tibble(url = primary_url,
                   editions = eds)

  }



  purrr::map_df(url_archive, grab_ed_links) %>%
    dplyr::mutate(
      editions = dplyr::case_when(
        stringr::str_detect(editions,"O SUL GLOBAL PENSADO") ~ "v. 12 n. 59 (2021)",
        stringr::str_detect(editions,"Dez anos de Conjuntura Austral") ~ "v. 11 n. 55 (2020)",
        stringr::str_detect(editions,"Especial Diplomacia") ~ "v. 11 n. 54 (2020)",
        stringr::str_detect(editions,"Especial BRICS") ~ "v. 11 n. 53 (2020)",
        TRUE ~ editions
      )
    ) %>%
    dplyr::mutate(
      vol = stringr::str_extract(editions, "(v. [0-9]{2})|(v. [0-9]{1})") %>%
        stringr::str_replace_all(.,'v. ','') %>%
        as.integer(.),
      n = stringr::str_extract(editions,'(n. [0-9]{2}-[0-9]{2})|(n. [0-9]{1}-[0-9]{2})|((n. [0-9]{1}-[0-9]{1}))|(n. [0-9]{2})|(n. [0-9]{1})') %>%
        stringr::str_replace_all(.,'n. ',''),
      ano = stringr::str_extract(editions,"[0-9]{4}") %>%
        as.double(.)
    ) %>%
    dplyr::filter(ano %in% year &
                    n %in% number &
                    vol %in% volume) %>%
    dplyr::pull(url) -> primary_url


  # PART II: ARTICLES LINKS

  articles_url <- purrr::map(primary_url, function(x) {

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

    if (length(filiation) == 0) {
      url_lido %>%
        rvest::html_nodes('.affiliation') %>%
        rvest::html_text() %>%
        stringr::str_remove_all("\\n|\\t") -> filiation
    }


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
      rvest::html_nodes(".abstract p+ p") %>%
      rvest::html_text() -> abstract

    if(length(abstract) == 0){
      url_lido %>%
        rvest::html_nodes(".abstract p") %>%
        rvest::html_text() -> abstract
    }

    if(length(abstract) == 0){
      abstract <- NA
    }

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
      rvest::html_nodes('meta[name="citation_keywords"]') %>%
      rvest::html_attr('content') -> keywords

    if(length(keywords) == 0){
      keywords <- NA
    } else {
      Filter(x = keywords, f = function(x) { x != "" }) %>%
        stringr::str_c(.,collapse = ';') -> keywords
    }



    ## L) References

    url_lido %>%
      rvest::html_nodes('meta[name="citation_reference"]') %>%
      rvest::html_attr('content') -> references

    if(length(references ) == 0){
      references  <- NA
    } else {
      Filter(x = references, f = function(x) { x != "" }) %>%
        stringr::str_c(.,collapse = '//t') -> references
    }


    ## M) Url_pdf

    url_lido %>%
      rvest::html_node('meta[name="citation_pdf_url"]') %>%
      rvest::html_attr('content') -> pdf_url

    if(length(pdf_url) == 0){pdf_url <- ""}

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

  conjunturaaustral %>%
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
