library(htmltools)
library(stringr)
library(dplyr)
library(readr)
library(tidyverse)
# library(fontawesome)

knitr::opts_chunk$set(
  collapse = TRUE,
  warning = FALSE,
  message = FALSE,
  fig.retina = 3,
  comment = "#>"
)

gscholar_stats <- function(url) {
  cites <- get_cites(url)
  return(glue::glue(
    'Citations: {cites$citations} | h-index: {cites$hindex} | i10-index: {cites$i10index}'
  ))
}

get_cites <- function(url) {
  html <- xml2::read_html(url)
  node <- rvest::html_nodes(html, xpath='//*[@id="gsc_rsb_st"]')
  cites_df <- rvest::html_table(node)[[1]]
  cites <- data.frame(t(as.data.frame(cites_df)[,2]))
  names(cites) <- c('citations', 'hindex', 'i10index')
  return(cites)
}

get_cite_count <- function(url) {
  citations <- 
    xml2::read_html(url) %>% 
    rvest::html_elements("div.gs_ab_mdw") %>%   # CSS selector, several of those
    rvest::html_text2() %>% 
    keep(~ str_detect(.x, regex("\\bresults\\b", ignore_case = TRUE))) %>% # Filter results counter
    first() %>% 
    str_extract("[0-9]{1,5}") %>% # First number in there, up to 99k citations ;)
    as.numeric()
  return(glue::glue(
    '**GS citations: *{citations}***'
  ))
}



get_pubs <- function() {
    # pubs <- gsheet::gsheet2tbl(
    #     url = 'https://docs.google.com/spreadsheets/d/1BVjjPqwwBmiaMBF_aFhB3hbpAThQuz2cenXxDjshXaQ/edit?usp=sharing')
    pubs <- readxl::read_xlsx('./files/CR_Publications.xlsx')
    pubs <- make_citations(pubs)
    pubs$summary <- ifelse(is.na(pubs$summary), FALSE, pubs$summary)
    pubs$stub <- make_stubs(pubs)
    pubs$url_summary <- file.path('research', pubs$stub, "index.html")
    pubs$url_scholar <- ifelse(
      is.na(pubs$id_scholar), NA, 
      glue::glue('https://scholar.google.com/citations?view_op=view_citation&hl=en&user=kIwmmTAAAAAJ&citation_for_view=kIwmmTAAAAAJ:{pubs$id_scholar}')
    )
    return(pubs)
}

make_citations <- function(pubs) {
  pubs$citation <- unlist(lapply(split(pubs, 1:nrow(pubs)), make_citation))
  return(pubs)
}

make_citation <- function(pub) {
  if (!is.na(pub$source)) {
    pub$source <- glue::glue('_{pub$source}_, ')
  }
  if (!is.na(pub$number)) {
    pub$number <- glue::glue('{pub$number}.')
  }
  if (!is.na(pub$doi)) {
    pub$doi <- make_doi(pub$doi)
  }
  pub$year <- glue::glue("({pub$year})")
  pub$title <- glue::glue('"{pub$title}"')
  if (!is.na(pub$eds)) {
    pub$eds <- glue::glue('{pub$eds}')
  }
  pub[,which(is.na(pub))] <- ''
  return(paste(
    pub$author, pub$year, pub$title, pub$eds, pub$source, 
    pub$number, pub$reviewed
  ))
}

make_doi <- function(doi) {
  return(glue::glue('DOI: [{doi}](https://doi.org/{doi})'))
}

make_stubs <- function(pubs) {
    journal <- str_to_lower(pubs$journal)
    journal <- str_replace_all(journal, ':', '')
    journal <- str_replace_all(journal, '`', '')
    journal <- str_replace_all(journal, "'", '')
    journal <- str_replace_all(journal, "\\.", '')
    journal <- str_replace_all(journal, "&", '')
    journal <- str_replace_all(journal, ',', '')
    journal <- str_replace_all(journal, '  ', '-')
    journal <- str_replace_all(journal, ' ', '-')
    return(paste0(pubs$year, '-', journal))
}

make_pub_list <- function(pubs, category) {
    x <- pubs[which(pubs$category == category),] %>% arrange(desc(pub_date))
    pub_list <- list()
    for (i in 1:nrow(x)) {
      pub_list[[i]] <- make_pub(x[i,], index = i)
    }
    return(htmltools::HTML(paste(unlist(pub_list), collapse = "<br>")%>% 
                             str_replace_all("<ol", "<ul") %>% # I want an unordered list, the dirty way
                             str_replace_all("ol>", "ul>")))
}

make_pub <- function(pub, index = NULL) {
  header <- FALSE
  altmetric <- make_altmetric(pub)
  if (is.null(index)) {
    cite <- pub$citation
    icons <- make_icons(pub)
  } else {
    cite <- glue::glue('{index}) {pub$citation}')
    icons <- glue::glue('<ul style="list-style: none;"><li>{make_icons(pub)}</li></ul>')
    if (index == 1) { header <- TRUE }
  }
  # return(markdown_to_html(cite))
  return(htmltools::HTML(glue::glue(
    '<div class="pub">
    <div class="grid">
    <div class="g-col-11"> {markdown_to_html(cite)} </div>
    <div class="g-col-1"> {altmetric} </div>
    </div>
    {icons}'
  )))
}

make_altmetric <- function(pub) {
  altmetric <- ""
  # if (pub$category == 'Journal article') { # Basically I want it for everything that has a doi
  if (!is.na(pub$doi)) {
    altmetric <- glue::glue('<div data-badge-type="donut" data-doi="{pub$doi}" data-hide-no-mentions="true" data-badge-popover="right" class="altmetric-embed"></div>')
  }
  return(altmetric)
}

# make_haiku <- function(pub, header = FALSE) {
#   html <- ""
#   haiku <- em(
#     pub$haiku1, HTML("&#8226;"), 
#     pub$haiku2, HTML("&#8226;"), 
#     pub$haiku3
#   )
#   if (!is.na(pub$haiku1)) {
#     if (header) {
#       html <- as.character(aside_center(list(
#         HTML("<b>Haiku Summary</b>"), br(), haiku))
#       )
#     } else {
#       html <- as.character(aside_center(list(haiku)))
#     }
#   }
#   return(html)
# }

aside <- function(text) {
  return(tag("aside", list(text)))
}

center <- function(text) {
  return(tag("center", list(text)))
}

aside_center <- function(text) {
  return(aside(center(list(text))))
}

aside_center_b <- function(text) {
  return(aside(center(list(tag("b", text)))))
}

markdown_to_html <- function(text) {
  if (is.null(text)) { return(text) }
  
  # Replace the author names with underlined last names
  text <- gsub(
    pattern = "\\\\\\*([^,]+), ([^,]+)", 
    replacement = "<u>\\\\*\\1</u>, \\2", 
    text
  )
  text <- gsub(
    pattern = "\\\\\\*\\\\\\*([^,]+), ([^,]+)", 
    replacement = "<u>\\\\*\\\\*\\1</u>, \\2", 
    text
  )
  
  # Render the text as HTML
  return(HTML(markdown::renderMarkdown(text = text)))
}

make_icons <- function(pub) {
  html <- c()
  if (pub$summary) {
    html <- c(html, as.character(icon_link(
      icon = "fa fa-link",
      text = "Summary",
      url  = pub$url_summary, 
      class = "icon-link-summary", 
      target = "_self"
    )))      
  }
  if (!is.na(pub$url_pub)) {
    html <- c(html, as.character(icon_link(
      icon = "fa fa-link",
      # icon = "bi bi-link-45deg",
      text = "View",
      url  = pub$url_pub
    )))
  }
  if (!is.na(pub$url_pdf)) {
    html <- c(html, as.character(icon_link(
      icon = "fa fa-file",
      # icon = "bi bi-filetype-pdf",
      text = "PDF",
      url  = pub$url_pdf
    )))
  }
  if (!is.na(pub$url_repo)) {
    html <- c(html, as.character(icon_link(
      # icon = "fa fa-github",
      icon = "fa-brands fa-github",
      # icon = "bi bi-github",
      text = "Code & Data",
      url  = pub$url_repo
    )))
  }
  if (!is.na(pub$url_other)) {
    html <- c(html, as.character(icon_link(
      icon = "fa fa-external-link-alt",
      text = pub$other_label,
      url  = pub$url_other
    )))
  }
  if (!is.na(pub$url_rg)) {
    html <- c(html, as.character(icon_link(
      icon = "ai ai-researchgate",
      # text = "&nbsp;",
      text = "Research Gate",
      url  = pub$url_rg
    )))
  }
  if (!is.na(pub$url_scholar)) {
    html <- c(html, as.character(icon_link(
      icon = "ai ai-google-scholar",
      text = "Google Scholar",
      # text = get_cite_count(pub$url_gs), # Works in principle, but Google blocks off bc too many requests ...
      url  = pub$url_scholar
    )))
  }
  if (!is.na(pub$doi)) {
    html <- c(html, as.character(icon_link(
      icon = "ai ai-crossref",
      text = paste0("CrossRef citations: ", rcrossref::cr_citation_count(doi = pub$doi)[1,2]), # Slows down rendering, of course
      url  = paste0("https://search.crossref.org/?from_ui=yes&q=", pub$doi  %>% str_replace_all(fixed("/"), "%2F"))
    )))
  }
  return(paste(html, collapse = "&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;"))
}

# The icon_link() function is in {distilltools}, but I've modified this
# one to include  a custom class to be able to have more control over the
# CSS and an optional target argument

icon_link <- function(
  icon = NULL,
  text = NULL,
  url = NULL,
  class = "icon-link",
  target = "_blank",
  style = "text-decoration: none; color: #003399"
) {
  if (!is.null(icon)) {
    text <- make_icon_text(icon, text)
  }
  return(htmltools::a(
    href = url, text, class = class, target = target, rel = "noopener", style = style
  ))
}

make_icon_text <- function(icon, text) {
  return(HTML(paste0(make_icon(icon), " ", text)))
}

make_icon <- function(icon) {
  return(tag("i", list(class = icon)))
}

last_updated <- function() {
  return(span(
    paste0(
      'Last updated on ',
      format(Sys.Date(), format="%B %d, %Y")
    ),
    style = "font-size:0.8rem;")
  )
}




# pubs <- get_pubs()
# test <- make_pub_list(pubs, "Other")
# test <- test %>% str_replace_all("<ol", "<ul") %>% str_replace_all("ol>", "ul>")
# writeLines(test, "_testpub.html")


# cites <- get_cites("https://scholar.google.de/citations?hl=de&user=kIwmmTAAAAAJ")
# paste0("**<a href = \"https://scholar.google.de/citations?hl=en&user=kIwmmTAAAAAJ\">Google Scholar:</a>**  *Citations*: ", cites[1,1], "  -  *H-Index:* ", cites[1,2], "  -  i10-Index: ", cites[1,3])


# Crossref citations (hinges on DOI, though)
# https://docs.ropensci.org/rcrossref/articles/rcrossref.html
# library(rcrossref)
# cr_citation_count(doi = "10.1080/13501763.2024.2344849")


# GS citations
# https://scholar.google.com/scholar?oi=bibs&hl=en&cites=1236898548868579695,13525180722372907951,14135509442548774749
# get_cite_count <- function(url) {
#   citations <- 
#     xml2::read_html(url) %>% 
#     rvest::html_elements("div.gs_ab_mdw") %>%   # CSS selector, several of those
#     rvest::html_text2() %>% 
#     keep(~ str_detect(.x, regex("\\bresults\\b", ignore_case = TRUE))) %>% # Filter results counter
#     first() %>% 
#     str_extract("[0-9]{1,5}") %>% # First number in there, up to 99k citations ;)
#     as.numeric()
#   return(glue::glue(
#     '**GS citations: *{citations}***'
#   ))
# }
# 
# get_cite_count("https://scholar.google.com/scholar?oi=bibs&hl=en&cites=1236898548868579695,13525180722372907951,14135509442548774749")
