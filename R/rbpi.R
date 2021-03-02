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
rbpi <- function(
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
  url_archive <- "https://www.scielo.br/scielo.php?script=sci_issues&pid=0034-7329&lng=en&nrm=iso"

  xml2::read_html(url_archive) %>%
    rvest::html_nodes("b a") %>%
    rvest::html_attr("href")  -> primary_url

  xml2::read_html(url_archive) %>%
    rvest::html_nodes("table") %>%
    rvest::html_table(fill = TRUE) %>%
    .[[5]] %>%
    dplyr::select(1:5) %>%
    dplyr::slice(-1) %>%
    tidyr::pivot_longer(cols = 3:5,
                        names_to = "X",
                        values_to = "numero",
                        values_drop_na = TRUE) %>%
    dplyr::select(-X) %>%
    dplyr::rename(ano = "X1",
                  vol = "X2") %>%
    dplyr::mutate(ano = as.numeric(ano),
                  vol = as.numeric(vol),
                  numero = as.numeric(dplyr::if_else(numero == "special issue","3", numero))) %>%
    dplyr::filter(numero %in% c("1","2","3")) %>%

    dplyr::mutate(url = primary_url) %>%
    dplyr::filter(ano %in% year &
                    numero %in% number &
                    vol %in% volume) %>%
    dplyr::pull(url)  -> primary_url

  # PART II: ARTICLES LINKS

  articles_url <- purrr::map(primary_url, function(x){
    url_lido <- xml2::read_html(x)

    url_lido %>%
      rvest::html_nodes(".content div a") %>%
      rvest::html_attr("href") %>%
      tibble::as_tibble() %>%
      dplyr::filter(!stringr::str_detect(value,"(.pdf)|(abstract)|(javascript)")) %>%
      dplyr::pull(value)

  }) %>%
    purrr::flatten_chr()

  # PART III: SCRAPPING METADATA

  rbpi <- purrr::map_dfr(articles_url, function(x) {
    if(!isTRUE(silence)) {
      usethis::ui_info(paste0('Currently scraping: ', x))
    }

    url_lido <- xml2::read_html(x)


    url_lido %>%
      rvest::html_nodes(xpath = "//div[@class='box']") %>%
      rvest::html_nodes("a") %>%
      rvest::html_attr("href") %>%
      tibble::tibble() %>%
      dplyr::filter(stringr::str_detect(.,"XML")) %>%
      purrr::flatten_chr() %>%
      xml2::read_xml() -> xml

    if(suppressWarnings(stringr::str_detect(xml[1], "Error"))){
      usethis::ui_warn(paste0('O xml do artigo ', x, " está quebrado"))

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
        journal ="Revista Brasileira de Política Internacional",
        issn = "0034-7329"
      )

    }




    #Qual o idioma?
    language <- stringr::str_sub(x, start= -2,end= -1)


    xml %>%
      # Pegar sempre sÃ³ o primeiro, porque vai estar no idioma em que o artigo foi
      # escrito
      xml2::xml_find_first(., ".//article-title") %>%
      xml2::xml_text() -> article_title

    if(article_title == "Erratum" | article_title == "Manual das organizações internacionais"){
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
        journal ="Revista Brasileira de Política Internacional",
        issn = "0034-7329"
      )


    } else {

      ## A) Filiation
      xml %>%
        xml2::xml_find_all(., ".//aff//institution") %>%
        xml2::xml_text() %>%
        stringr::str_remove(.,",") %>%
        stringr::str_trim() -> filiation

      for (i in seq_along(filiation)) {
        if(filiation[i] == "") {filiation <- "NA"}
      }



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

      ## C) Title

      xml %>%
        xml2::xml_find_all(.,'.//article-meta//article-title') %>%
        xml2::xml_text() -> title_texto

      xml %>%
        xml2::xml_find_all(.,'.//article-meta//article-title') %>%
        xml2::xml_attr('lang') -> title_lang


      if(length(title_texto) == 0){
        title <- "NA"
      } else{

        tibble::tibble(texto = title_texto, lingua = title_lang) %>%
          dplyr::filter(lingua == language) %>%
          dplyr::pull(texto) -> title
      }

      if (length(title) == 0) { title <- "NA"}
      else if(title == "") {title <- "NA"}

      ## D) Abstract
      xml %>%
        # Pegar sempre sÃ³ o primeiro, porque vai estar no idioma em que o artigo foi
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

      if(length(fpage) == 0 | length(lpage) == 0) {
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
        xml2::xml_find_first(., ".//numero") %>%
        xml2::xml_text() -> number

      ## J) Year
      xml %>%
        xml2::xml_find_first(., ".//pub-date//year") %>%
        xml2::xml_text() -> year


      ## K) Keywords
      xml %>%
        xml2::xml_find_all(., ".//kwd-group") %>%
        xml2::xml_find_all(.,".//kwd") -> keyword_bruto

      if (length(keyword_bruto) == 0){
        keywords <- "NA"
      } else {

        keyword_bruto[xml2::xml_attr(keyword_bruto, "lng")== language] %>%
          xml2::xml_text()-> keywords
        keywords %>%
          toString() -> keywords
      }

      ## L) References
      xml %>%
        xml2::xml_find_all(.,".//ref") %>%
        xml2::xml_text() -> references_bruto

      if (length(references_bruto) == 0){
        references <- "NA"
      } else {

        references <- vector(mode = "character", length = length(references_bruto))

        for (i in seq_along(references_bruto)){

          references[i] <- glue::glue("//t{references_bruto[i]}//t")
        }

        references %>%
          toString() -> references
      }


      ## M) Url_pdf
      url_lido %>%
        rvest::html_nodes(xpath = "//div[@class='box']") %>%
        rvest::html_nodes("a") %>%
        rvest::html_attr("href") %>%
        tibble::tibble() %>%
        dplyr::filter(stringr::str_detect(.,"pdf")) %>%
        purrr::flatten_chr() %>%
        stringr::str_c('https://www.scielo.br',.) -> pdf_url


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
        journal ="Revista Brasileira de Política Internacional",
        issn = "0034-7329"
      )



    }

  })
  rbpi

}
