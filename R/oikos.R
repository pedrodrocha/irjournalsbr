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
oikos <- function(
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

  for (i in seq_along(year)) {

    tryCatch(
      assertthat::assert_that(year[i] > 2005),
      error = function(e) {
        stop("There isn't data before 2006", call. = FALSE)
      }
    )

  }



  # PART 1: EDITIONS LINKS

  url_archive <- "http://www.revistaoikos.org/seer/index.php/oikos/issue/archive"

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
      vol = stringr::str_extract(editions, "(Vol. [0-9]{2})|(Vol. [0-9]{1})") %>%
        stringr::str_replace_all(.,'Vol. ','') %>%
        as.integer(.),
      n = stringr::str_extract(editions,'No [0-9]{1}') %>%
        stringr::str_replace_all(.,'No ','') %>%
        as.integer(.),
      ano = stringr::str_extract(editions,"[0-9]{4}") %>%  as.integer(.)
    ) %>%
    dplyr::filter(ano %in% year &
                    n %in% number &
                    vol %in% volume) %>%
    dplyr::select(url) %>%
    purrr::flatten_chr() -> primary_url

  # PART II: ARTICLES LINKS

  articles_url <- purrr::map_dfr(primary_url,.f = function(x){

    xml2::read_html(x) %>%
      rvest::html_nodes(".tocTitle a") %>%
      rvest::html_attr("href") -> artigos

    tibble::tibble(links = artigos)
  }) %>%
    purrr::flatten_chr() %>%
    stringr::str_replace_all(.,'view','viewArticle')

  # PART III: SCRAPPING METADATA

  oikos <- purrr::map_dfr(articles_url, function(x) {
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
      rvest::html_nodes('meta[name="DC.Creator.PersonalName"]') %>%
      rvest::html_attr('content') -> authors

    if(length(filiation) == 0){filiation <- NA}
    else if(length(filiation) != length(authors)) { filiation <- NA }

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
      rvest::html_nodes('h2') %>%
      rvest::html_text() %>%
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
      rvest::html_nodes('.file') %>%
      rvest::html_attr('href') %>%
      stringr::str_replace(.,"view","download") %>%
      stringr::str_replace(.,'downloadIssue','download')-> pdf_url
<<<<<<< HEAD


    if(length(pdf_url) == 0){pdf_url <- "NA"}
    if(pdf_url == ""){pdf_url <- "NA"}
=======
>>>>>>> 309f90c0180efb21078b1ae1f88f110959b4a4b9


    if(length(pdf_url) == 0){ pdf_url <- NA }
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
      journal = "Oikos - Revista de Economia Politica Internacional",
      issn = '1808-0235'
    )

  })

  oikos %>%
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
