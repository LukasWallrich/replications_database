library(rvest)
library(RSelenium)

driver <- rsDriver(browser="chrome", chromever = "113.0.5672.63", port = 5004L)
remote_driver <- driver[["client"]]
# Don't forget to close the session when you're done
remote_driver$close()


get_doi <- function(url, selenium_driver) {

  response <- purrr::possibly(httr::GET)(url, httr::add_headers("User-Agent" = "Mozilla/5.0"))

    if (is.null(response) || response$status_code == 403) {

    selenium_driver$navigate(url)
    page <- selenium_driver$getPageSource() %>% .[[1]] %>% rvest::read_html()

  } else {

    page <- rvest::read_html(response)

  }

  if (!(str_detect(tolower(page %>% rvest::html_node("title") %>%
      rvest::html_text()), "just a moment") %in% c(NA, FALSE))) {
    resp <- httr::GET(paste0("https://proxy.scrapeops.io/v1/?api_key=d899d6da-8c71-4b47-86c8-f573d084f067&url=", selenium_driver$getCurrentUrl()[[1]]))
    if (resp$status_code != 200) {
      resp <- httr::GET(paste0("https://proxy.scrapeops.io/v1/?api_key=d899d6da-8c71-4b47-86c8-f573d084f067&url=", selenium_driver$getCurrentUrl()[[1]]))
      if (resp$status_code != 200) return(NA)
    }
    page <- rvest::read_html(resp)
  }

  text_body <- html_text(html_nodes(page, "body"))

  # Find the first match using the pattern
  pattern <- '10\\..*?\\/[^/]*(?=[\\s,;.\\)]|[#?&]|\\/|$)'
  first_doi <- str_extract(text_body, pattern)
  other_doi <- setdiff(str_extract_all(text_body, pattern) %>% unlist(), first_doi)

  if (length(first_doi) == 0) return(list(NA, NA))

  list(first_doi, other_doi)

}

#input <- read.csv("email_scrape/in.csv")
#output <- read.csv("email_scrape/out.csv", colClasses = "character")


#input <- input %>% filter(!DOI %in% output$DOI)

for (i in seq_len(nrow(input))) {
  doi_result <- purrr::possibly(get_doi, otherwise = NA)(input$url[i])
  output <- bind_rows(output,
                      tibble(url = input$url[i], first_doi = doi_result[[1]],
                             other_doi = doi_result[[2]]))
  message(last(output$first_doi))
}

output %>% write.csv("scraped_dois.csv")

