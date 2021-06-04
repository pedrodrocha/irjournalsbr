#' Title
#'
#' @param year
#' @param volume
#' @param number Note: From 2016 editions have only volume and the number should be set to 0
#' @param silence
#' @param full_text
#'
#' @return
#' @export
#'
#' @examples
meridiano47 <- function(
  year, volume, number, silence = TRUE, full_text = FALSE
){
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
      number %in% c("71","72") ~ "71-72",
      number %in% c("52","53") ~ "52-53",
      number %in% c("50","51") ~ "50-51",
      number %in% c("44","45") ~ "44-45",
      number %in% c("42","43") ~ "42-43",
      number %in% c("40","41") ~ "40-41",
      number %in% c("38","39") ~ "38-39",
      number %in% c("36","37") ~ "36-37",
      number %in% c("34","35") ~ "34-35",
      number %in% c("32","33") ~ "32-33",
      number %in% c("4","5") ~ "4-5",
      number %in% c("20","21") ~ "20-21",
      number %in% c("23","24") ~ "23-24",
      number %in% c("8","9") ~ "8-9",
      number %in% c("12","10") ~ "10-12",
      number %in% c("14","15") ~ "14-15",
      number %in% c("29","29") ~ "28-29",
      number %in% c("30","31") ~ "30-31",
      TRUE ~ number
    )
    ) %>%
    dplyr::distinct() %>%
    dplyr::pull(number) -> number


  # PART 1: EDITIONS LINKS
  url_archive <- "https://periodicos.unb.br/index.php/MED/issue/archive"

  url_archive_lido <- xml2::read_html(url_archive)
  url_archive_lido %>%
    rvest::html_nodes("#main-content .title") %>%
    rvest::html_attr("href")  -> primary_url



  url_archive_lido %>%
    rvest::html_nodes(".media-heading .title") %>%
    rvest::html_text() %>%
    stringr::str_remove_all("\\n|\\t") %>%
    stringr::str_replace("\\\\", "-")  %>%
    Filter(x = ., function(x) { stringr::str_detect(x, '[0-9]{4}')}) %>%
    stringr::str_to_lower(.) -> eds1

  url_archive_lido %>%
    rvest::html_nodes(".lead") %>%
    rvest::html_text() %>%
    stringr::str_remove_all("\\n|\\t") %>%
    stringr::str_replace("\\\\", "-") %>%
    Filter(x = ., f = function(x) { !stringr::str_detect(x, 'v. [0-9]{2} \\([0-9]{4}\\)')})-> eds2
  eds <- c(eds1,eds2)

  tibble::tibble(url = primary_url,
                 editions = eds) %>%
    dplyr::mutate(
      vol = stringr::str_extract(editions, "(v. [0-9]{2})|(v. [0-9]{1})") %>%
        stringr::str_replace_all(.,'v. ','') %>%
        as.integer(.),
      n = stringr::str_extract(editions,'(n. [0-9]{3})|(n. [0-9]{2}-[0-9]{2})|(n. [0-9]{1}-[0-9]{2})|((n. [0-9]{1}-[0-9]{1}))|(n. [0-9]{2})|(n. [0-9]{1})') %>%
        stringr::str_replace_all(.,'n. ',''),
      ano = stringr::str_extract(editions,"[0-9]{4}") %>%
        as.double(.),
      n = ifelse(ano > 2015, 0,n)
    )  %>%
    dplyr::filter(ano %in% year &
                    n %in% number &
                    vol %in% volume) %>%
    dplyr::pull(url) -> primary_url

  # PART II: ARTICLES LINKS

  articles_url <- purrr::map(primary_url, function(x) {

    url_lido <- xml2::read_html(x)

    url_lido %>%
      rvest::html_nodes('.media-heading a') %>%
      rvest::html_attr('href')  -> links

    url_lido %>%
      rvest::html_nodes('.media-heading a') %>%
      rvest::html_text()  %>%
      stringr::str_remove_all("\\n|\\t")-> names


    tibble::tibble(
      links = links,
      names = names
    ) %>%
      dplyr::filter(
        !stringr::str_detect(names, '(edição completa)|(Edição Completa)')
      ) %>%
      dplyr::pull(links)

  }) %>%
    purrr::flatten_chr()


  # PART III: SCRAPPING METADATA

  meridiano47 <- purrr::map_dfr(articles_url, function(x) {
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
    if(length(title) == 0){ title <- "NA" }

    ## D) Abstract

    url_lido %>%
      rvest::html_nodes("#summary p") %>%
      rvest::html_text() %>%
      stringr::str_remove_all("\\n|\\t") -> abstract

    if(length(abstract) == 0){
      abstract <- "NA"
    } else if(length(abstract) == 2){
      abstract <- abstract[1]
    }

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

    url_lido %>%
      rvest::html_nodes('meta[name="citation_keywords"]') %>%
      rvest::html_attr('content') %>%
      paste0(., collapse = ', ')-> keywords

    if(keywords == ""){
      keywords <- "NA"
    } else if(length(keywords) == 0) {
      keywords <- "NA"
    }


    ## L) References

    url_lido %>%
      rvest::html_nodes(".article-references-content p") %>%
      rvest::html_text() %>%
      paste0("\\t", .,"\\t") %>%
      toString() -> references

    if(length(references) == 0){ references <- "NA" }


    ## M) Url_pdf

    url_lido %>%
      rvest::html_nodes('meta[name="citation_pdf_url"]') %>%
      rvest::html_attr('content') -> pdf_url

    if(length(pdf_url) == 0){ pdf_url <- "NA" }

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
      journal = "Meridiano 47 - Journal of Global Studies",
      issn = '1518-1219'
    )


  })

  meridiano47 %>%
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
