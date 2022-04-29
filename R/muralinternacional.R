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
muralinternacional <- function(
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

  # PART 1: EDITIONS LINKS
  url_archive <-
    "https://www.e-publicacoes.uerj.br/index.php/muralinternacional/issue/archive"


  url_archive_lido <- xml2::read_html(url_archive)

  url_archive_lido %>%
    rvest::html_nodes("h4") %>%
    rvest::html_nodes("a") %>%
    rvest::html_attr("href") -> primary_url

  url_archive_lido %>%
    rvest::html_nodes("h4") %>%
    rvest::html_nodes("a") %>%
    rvest::html_text() %>%
    stringr::str_remove(":(.*)") -> eds

  tibble::tibble(url = primary_url,
                 editions = eds) %>%
    dplyr::mutate(
      vol = stringr::str_extract(editions, "(v. [0-9]{2})|(v. [0-9]{1})") %>%
        stringr::str_replace_all(.,'v. ','') %>%
        as.integer(.),
      n = stringr::str_extract(editions,'(n. [0-9]{2})|(n. [0-9]{1})') %>%
        stringr::str_replace_all(.,'n. ','') %>%
        as.integer(.),
      n =  ifelse(is.na(n),1,n),
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
      rvest::html_nodes('.tocTitle a') %>%
      rvest::html_attr('href')

  }) %>%
    purrr::flatten_chr()

  # PART III: SCRAPPING METADATA

  mural <- purrr::map_dfr(articles_url, function(x){
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
      rvest::html_attr('content') %>%
      stringr::str_remove("\\/(.*) | \\|(.*)") %>%
      stringr::str_trim() -> title

    ## D) Abstract

    url_lido %>%
      rvest::html_node('meta[name="DC.Description"]') %>%
      rvest::html_attr('content') -> abstract

    if(length(abstract) == 0){ abstract <- "NA" }
    if(abstract == ""){ abstract <- "NA" }

    ## E) DOI

    url_lido %>%
      rvest::html_node('meta[name="DC.Identifier.DOI"]') %>%
      rvest::html_attr('content') -> doi

    if(length(doi) == 0){ doi <- "NA" }

    ## F) Pages

    url_lido %>%
      rvest::html_nodes('meta[name="DC.Identifier.pageNumber"]') %>%
      rvest::html_attr('content') -> pages

    if(length(pages) == 0){ pages <- "NA" }
    if(stringr::str_detect(pages,'e')){ pages <- "NA" }


    ## G) Language

    url_lido %>%
      rvest::html_nodes('meta[name="DC.Language"]') %>%
      rvest::html_attr('content') -> language

    if(length(language) == 0){ language <- "NA" }

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
    ano_corte <- as.numeric(year)
    # From 2019 onward keywords change pattern
    if (ano_corte >= 2019) {
      url_lido %>%
        rvest::html_nodes('meta[name="DC.Subject"]') %>%
        rvest::html_attr('content') %>%
        paste0(., collapse = ';')-> keywords

    } else{
      url_lido %>%
        rvest::html_nodes('#articleAbstract p:nth-child(4)')  %>%
        rvest::html_text() %>%
        stringr::str_to_lower() %>%
        stringr::str_remove('-') %>%
        stringr::str_remove("(palavraschave:)|(keywords:)|(palabrasclave:)") %>%
        stringr::str_squish() %>%
        stringr::str_replace_all(.,'(\\.)|(,)',';') -> keywords
    }

    if(length(keywords) == 0){keywords <- "NA" }
    if(keywords == "") {keywords <- "NA" }

    ## L) References

    url_lido %>%
      rvest::html_nodes("#articleCitations p") %>%
      rvest::html_text() %>%
      paste0("\\t", .,"\\t") %>%
      toString() -> references

    if(length(references) == 0){references <- "NA" }
    if(references == "\\t\\t") {references <- "NA" }


    ## M) Url_pdf

    url_lido %>%
      rvest::html_nodes('meta[name="citation_pdf_url"]') %>%
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
      journal = "Mural Internacional",
      issn = '2177-7314'
    )


  })
  mural %>%
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
