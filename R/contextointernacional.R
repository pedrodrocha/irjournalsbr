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
contextointernacional <- function(
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

  url_archive <- "http://www.scielo.br/j/cint/grid"

  xml2::read_html(url_archive) %>%
    rvest::html_nodes(".left .btn") %>%
    rvest::html_attr("href") %>%
    paste0("http://www.scielo.br",.) %>%
    Filter(x = ., f = function(x) { stringr::str_detect(x, "v[0-9]{2}n") & !stringr::str_detect(x, "goto=previous")})  -> primary_url

  xml2::read_html(url_archive) %>%
    rvest::html_nodes("tbody td:nth-child(1)") %>%
    rvest::html_text() -> anos

  xml2::read_html(url_archive) %>%
    rvest::html_nodes("tbody th") %>%
    rvest::html_text() %>%
    stringr::str_squish() -> volumes


  xml2::read_html(url_archive) %>%
    rvest::html_nodes("tbody .left") %>%
    rvest::html_text() %>%
    stringr::str_squish() %>%
    stringr::str_replace_all(.," ", ",") -> numeros


  tibble::tibble(
    ano  = anos,
    vol = volumes,
    numero = numeros
  )  %>%
    tidyr::separate_rows(numero, sep = ",") -> ano_vol_year

  tibble::tibble(
    url = primary_url
  ) %>%
    dplyr::distinct() %>%
    dplyr::bind_cols(ano_vol_year) %>%
    dplyr::filter(ano %in% year & numero %in% number & vol %in% volume) %>%
    dplyr::pull(url) -> primary_url

  # PART II: ARTICLES LINKS

  articles_url <- purrr::map(primary_url, function(x){
    url_lido <- xml2::read_html(x)

    url_lido %>%
      rvest::html_nodes(".links a") %>%
      rvest::html_attr("href") %>%
      Filter(x = .,f = function(x) { !stringr::str_detect(x,'abstract') & !stringr::str_detect(x,'pdf')})  %>%
      paste0("http://www.scielo.br",.)

  }) %>%
    purrr::flatten_chr()



  # PART III: SCRAPPING METADATA

  contextointernacional <- purrr::map_dfr(articles_url, function(x) {
    if(!isTRUE(silence)) {
      usethis::ui_info(paste0('Currently scraping: ', x))
    }

    url_lido <- xml2::read_html(x)


    url_lido %>%
      rvest::html_nodes('meta[name="citation_xml_url"]') %>%
      rvest::html_attr('content') %>%
      xml2::read_xml() -> xml



    #Qual o idioma?
    language <- stringr::str_sub(x, start= -2,end= -1)


    url_lido %>%
      rvest::html_node('h1.article-title') %>%
      rvest::html_text() %>%
      stringr::str_squish() -> article_title

    if(article_title == "Erratum"){
      build_data(
        authors = "NA",
        filiation = "NA",
        title = article_title,
        abstract = "NA",
        keywords = "NA",
        references = "NA",
        pages = "NA",
        year = "NA",
        volume = "NA",
        number = "NA",
        language = "NA" ,
        doi = "NA",
        x = x,
        pdf_url = "NA",
        full_text = "NA",
        journal = "Contexto Internacional",
        issn = "1982-0240"
      )


    } else {




      ## B) Authors
      xml %>%
        xml2::xml_find_all(., ".//contrib//surname") %>%
        xml2::xml_text() -> sobrenome

      xml %>%
        xml2::xml_find_all(., ".//contrib//given-names") %>%
        xml2::xml_text() -> primeiro_nome

      authors <- vector(mode = "character", length = length(sobrenome))
      for(i in seq_along(sobrenome)){
        authors[i] <- glue::glue("{primeiro_nome[i]} {sobrenome[i]}")
      }

      ## A) Filiation
      xml %>%
        xml2::xml_find_all(., "//institution[@content-type='orgname']") %>%
        xml2::xml_text() %>%
        stringr::str_remove(.,",") %>%
        stringr::str_trim() -> filiation

      for (i in seq_along(filiation)) {
        if(filiation[i] == "") {filiation <- "NA"}
      }

      if(length(filiation) != length(authors)){
        filiation <- filiation[1:length(authors)]
      }


      ## C) Title

      xml %>%
        # Pegar sempre só o primeiro, porque vai estar no idioma em que o artigo foi
        # escrito
        xml2::xml_find_first(., ".//article-title") %>%
        xml2::xml_text() -> title

      if (length(title) == 0) { title <- "NA"}
      else if(title == "") {title <- "NA"}

      ## D) Abstract
      xml %>%
        # Pegar sempre só o primeiro, porque vai estar no idioma em que o artigo foi
        # escrito
        xml2::xml_find_first(., ".//abstract") %>%
        xml2::xml_text() -> abstract

      if (length(abstract) == 0){abstract <- "NA"}

      ## E) DOI
      xml %>%
        xml2::xml_find_all(.,'.//article-meta//article-id[@pub-id-type="doi"]') %>%
        xml2::xml_text() -> doi

      if(length(doi) == 0){ doi <- "NA" }

      ## F) Pages

      xml %>%
        xml2::xml_find_all(.,'//fpage') %>%
        xml2::xml_text() -> fpage

      xml %>%
        xml2::xml_find_all(.,'//lpage') %>%
        xml2::xml_text() -> lpage


      if (length(fpage) > 1){
        pages <- "NA"
      } else if(length(fpage) == 0 | length(lpage) == 0) {
        pages <- "NA"
      } else if((is.na(fpage))|(is.na(lpage))){
        pages <- "NA"
      } else {
        pages <- paste0(fpage,"-",lpage)
      }


      ## G) Language

      language <- language

      ## H) Volume
      xml %>%
        xml2::xml_find_first(., ".//volume") %>%
        xml2::xml_text() -> volume

      ## I) Number
      xml %>%
        xml2::xml_find_first(., ".//issue") %>%
        xml2::xml_text() -> number

      ## J) Year
      xml %>%
        xml2::xml_find_first(., ".//pub-date//year") %>%
        xml2::xml_text() -> year


      ## K) Keywords
      xml %>%
        xml2::xml_find_all("//kwd-group[@xml:lang = 'en']") %>%
        xml2::xml_find_all(.,".//kwd")  %>%
        xml2::xml_text() %>%
        stringr::str_c(.,collapse = '; ')-> keywords



      if (length(keywords) == 0){ keywords <- "NA" }

      ## L) References
      xml %>%
        xml2::xml_find_all(.,".//ref") %>%
        xml2::xml_text() -> references_bruto

      if (length(references_bruto) == 0){
        references <- "NA"
      } else {

        references <- stringr::str_c(references_bruto, collapse =  '//t ') %>% stringr::str_squish()
      }


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
        journal = "Contexto Internacional",
        issn = "1982-0240"
      )



    }

  })
  contextointernacional %>%
    dplyr::group_by(TI) %>%
    dplyr::mutate(
      AU = stringr::str_c(AU,collapse = ";"),
      OG = stringr::str_c(OG,collapse = ";")
    ) %>%
    dplyr::distinct() %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      AB = stringr::str_squish(AB),
      DE = dplyr::na_if(DE, "")
    ) %>%
  dplyr::mutate(dplyr::across(where(is.character), ~dplyr::na_if(.,"NA")))

}
