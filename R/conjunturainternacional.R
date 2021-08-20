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
conjunturainternacional <- function(
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
      assertthat::assert_that(year[i] > 2011),
      error = function(e) {
        stop("There isn't data before 2012", call. = FALSE)
      }
    )

    if(year[i] == 2012) {
      tryCatch(
        assertthat::assert_that(number > 4),
        error = function(e) {
          usethis::ui_warn('In 2012 there is only data for v.9, n.5 (2012)')
        }
      )

    }

  }

  # PART 1: EDITIONS LINKS
  url_archive1 <- "http://periodicos.pucminas.br/index.php/conjuntura/issue/archive/"
  url_archive2 <- "http://periodicos.pucminas.br/index.php/conjuntura/issue/archive/2"

  url_archive1_lido <- xml2::read_html(url_archive1)
  url_archive2_lido <- xml2::read_html(url_archive2)


  url_archive1_lido %>%
    rvest::html_nodes("#pkp_content_main .title") %>%
    rvest::html_attr("href") -> primary_url1

  url_archive2_lido %>%
    rvest::html_nodes("#pkp_content_main .title") %>%
    rvest::html_attr("href") -> primary_url2

  primary_url <- c(primary_url1, primary_url2)

  url_archive1_lido %>%
    rvest::html_nodes(".series") %>%
    rvest::html_text() %>%
    stringr::str_remove_all("\\n|\\t") -> eds_series1

  url_archive1_lido %>%
    rvest::html_nodes("#pkp_content_main .title") %>%
    rvest::html_text() %>%
    stringr::str_remove_all("\\n|\\t") %>%
    Filter(x = ., f = function(x) {
      !stringr::str_detect(x,'(CONJUNTURA INTERNACIONAL)|(Conjuntura Internacional)')
    })-> eds_title1

  eds1 <- c(eds_series1, eds_title1)

  url_archive2_lido %>%
    rvest::html_nodes("#pkp_content_main .title") %>%
    rvest::html_text() %>%
    stringr::str_remove_all("\\n|\\t") -> eds2

  eds <- c(eds1,eds2)

  tibble::tibble(url = primary_url,
                 editions = eds) %>%
    dplyr::mutate(
      vol = stringr::str_extract(editions, "(v. [0-9]{2})|(v. [0-9]{1})") %>%
        stringr::str_replace_all(.,'v. ','') %>%
        as.integer(.),
      n = stringr::str_extract(editions,'(n. [0-9]{2})|(n. [0-9]{1})') %>%
        stringr::str_replace_all(.,'n. ','') %>%
        as.integer(.),
      ano = stringr::str_extract(editions,"[0-9]{4}") %>%
        as.double(.)
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
        !stringr::str_detect(names,"(Edição Completa)|(Edição completa)"),
        !stringr::str_detect(names,"Páginas Iniciais"),
        !stringr::str_detect(names,"Sumário")
      ) %>%
      dplyr::pull(links)

  }) %>%
    purrr::flatten_chr()

  conjunturainternacional <- purrr::map_dfr(articles_url, function(x) {
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


    ## G) Language

    url_lido %>%
      rvest::html_nodes('meta[name="DC.Language"]') %>%
      rvest::html_attr('content') -> language

    if(length(language) == 0){ language <- NA }

    ## D) Abstract



    url_lido %>%
      rvest::html_nodes('meta[name="DC.Description"]') %>%
      rvest::html_attr('content') -> abstract_texto

    if(length(abstract_texto) == 0){
      abstract <- NA
    } else{
      url_lido %>%
        rvest::html_nodes('meta[name="DC.Description"]') %>%
        rvest::html_attr('xml:lang') -> abstract_lingua

      tibble::tibble(texto = abstract_texto, lingua = abstract_lingua) %>%
        dplyr::filter(lingua == language) %>%
        dplyr::pull(texto) -> abstract
    }

    if(abstract == "") {abstract <- NA}

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
      rvest::html_attr('content') %>%
      paste0(., collapse = ', ')-> keywords

    if(keywords == ""){
      keywords <- NA
    } else if(length(keywords) == 0) {
      keywords <- NA
    }

    ## L) References

    url_lido %>%
      rvest::html_nodes(xpath = "//div[@class='item references']") %>%
      rvest::html_nodes("div") %>%
      rvest::html_text() -> references


    if( length(references) == 0){references <- NA }

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
      journal = "Conjuntura Internacional",
      issn = '1809-6182'
    )


  })

  conjunturainternacional %>%
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
