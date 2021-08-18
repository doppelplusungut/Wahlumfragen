library(rvest)

allensbachHtml <- read_html("https://www.wahlrecht.de/umfragen/allensbach.htm")

allensbachHtml %>%
  hmtl_nodes("tbody") %>%
  html_text()