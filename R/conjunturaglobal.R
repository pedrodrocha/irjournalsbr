conjunturaglobal <- function(
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


  url_archive1 <- "https://revistas.ufpr.br/conjgloblal/issue/archive?issuesPage=1#issues"
  url_archive2 <- "https://revistas.ufpr.br/conjgloblal/issue/archive?issuesPage=2#issues"

  url_archive1_lido <- xml2::read_html(url_archive1)
  url_archive2_lido <- xml2::read_html(url_archive2)

  url_archive1_lido %>%
    rvest::html_nodes("#issues h4 a") %>%
    rvest::html_attr("href") -> primary_url1

  url_archive2_lido %>%
    rvest::html_nodes("#issues h4 a") %>%
    rvest::html_attr("href") -> primary_url2

  primary_url <- c(primary_url1,primary_url2)

  url_archive1_lido %>%
    rvest::html_nodes("#issues h4 a") %>%
    rvest::html_text() -> eds1

  url_archive2_lido %>%
    rvest::html_nodes("#issues h4 a") %>%
    rvest::html_text() -> eds2

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

  articles_url <- purrr::map(primary_url, function(x) {

    url_lido <- xml2::read_html(x)

    url_lido %>%
      rvest::html_nodes('.tocTitle a') %>%
      rvest::html_attr('href')

  }) %>%
    purrr::flatten_chr()


  # PART III: SCRAPPING METADATA

  conjunturaglobal <- purrr::map_dfr(articles_url, function(x) {
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
      rvest::html_nodes(xpath = "//div[@id='articleAbstract']") %>%
      rvest::html_nodes("div") %>%
      purrr::map(xml2::xml_contents) -> abstract

    abstract[[1]][1] %>%
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
      rvest::html_nodes('meta[name="DC.Subject"]') %>%
      rvest::html_attr('content') %>%
      paste0(., collapse = ', ')-> keywords

    if(keywords == ""){keywords <- NA}
    else if(length(keywords) == 0) {keywords <- NA}

    url_lido %>%
      rvest::html_nodes("#articleCitations p") %>%
      rvest::html_text() %>%
      paste0("\\t", .,"\\t") %>%
      toString()  -> references

    if(length(references) == 0){references <- NA}
    else if(references == "\\t\\t"){references <- NA}

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
      journal = "Conjuntura Global",
      issn = '2317-6563'
    )

  })

  conjunturaglobal
}
