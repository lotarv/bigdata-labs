library(rvest)

url <- "https://ru.wikipedia.org/wiki/%D0%9C%D1%83%D0%B7%D0%B5%D0%B9_%D0%9C%D0%B8%D1%80%D0%BE%D0%B2%D0%BE%D0%B3%D0%BE_%D0%BE%D0%BA%D0%B5%D0%B0%D0%BD%D0%B0"

page <- read_html(url)

museum_data <- data.frame(
  name = page %>% 
    html_node("h1.firstHeading") %>% 
    html_text() %>% 
    trimws(),
  
  address = page %>% 
    html_node('span[data-wikidata-property-id="P6375"]') %>% 
    html_text() %>%
    trimws(),
  
  link = page %>% 
    html_node('span[data-wikidata-property-id="P856"]') %>% 
    html_text() %>%
    trimws(),
  
  stringsAsFactors = FALSE
)

print(museum_data)

