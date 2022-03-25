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
    number = number,
    silence = silence,
    full_text = full_text
  )

  # PART 1: EDITIONS LINKS

  url_archive <- "https://seer.ufrgs.br/index.php/austral/issue/archive"

  url_archive_lido <- xml2::read_html(url_archive)

  url_archive_lido %>%
    rvest::html_nodes(".obj_issue_summary .title") %>%
    rvest::html_attr("href") -> primary_url


  url_archive_lido %>%
    rvest::html_nodes(".obj_issue_summary .title") %>%
    rvest::html_text() %>%
    stringr::str_trim() -> eds

  tibble::tibble(url = primary_url,
                 editions = eds) %>%
    dplyr::mutate(
      vol = stringr::str_extract(editions, "(Vol. [0-9]{2})|(Vol. [0-9]{1})") %>%
        stringr::str_replace_all(.,'Vol. ','') %>%
        as.integer(.),
      n = stringr::str_extract(editions,'(No. [0-9]{2})|(No. [0-9]{1})') %>%
        stringr::str_replace_all(.,'No. ','') %>%
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



  # PART III: SCRAPPING METADATA

  austral <- purrr::map_dfr(articles_url, function(x) {
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
      rvest::html_attr('content') %>%
      stringr::str_to_title() -> title

    ## D) Abstract

    url_lido %>%
      rvest::html_nodes('meta[name="DC.Description"][xml\\:lang="en"]') %>%
      rvest::html_attr('content') -> abstract


    if(length(abstract) == 0){abstract <- NA}

    ## E) DOI

    url_lido %>%
      rvest::html_nodes('meta[name="DC.Identifier.DOI"]') %>%
      rvest::html_attr('content') -> doi

    if(length(doi) == 0){ doi <- NA }

    ## F) Pages


    pages <- NA

    ## G) Language

    url_lido %>%
      rvest::html_nodes('meta[name="citation_language"]') %>%
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
    query <- paste0("meta[name=citation_keywords][xml\\:lang='",language,"']" )
    query
    url_lido %>%
      rvest::html_nodes(query) %>%
      rvest::html_attr('content') %>%
      paste0(., collapse = '; ') -> keywords

    if(keywords == ""){keywords <- NA}

    ## L) References

    references <- NA

    ## M) Url_pdf

    url_lido %>%
      rvest::html_nodes('meta[name="citation_pdf_url"]') %>%
      rvest::html_attr('content') -> pdf_url

    if(length(pdf_url) == 0){pdf_url <- "NA"}
    if(language == 'pt') { pdf_url <- pdf_url[1]} # Portuguese pdf
    if(language == 'en') { pdf_url <- pdf_url[2]} # English pdf





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
      journal = "Austral: Brazilian Journal of Strategy and International Relations",
      issn = '2316-8323'
    )

  })

  austral %>%
  dplyr::group_by(TI) %>%
    dplyr::mutate(
      AU = stringr::str_c(AU,collapse = ";"),
      OG = stringr::str_c(OG,collapse = ";"),
      TI = stringr::str_to_title(TI)
    ) %>%
    dplyr::distinct() %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      AB = stringr::str_squish(AB)
    )
}
