library(xml2)
library(rvest)
library(stringr)
url_1 <- "https://www.worldwildlife.org/species/directory?sort=name"
url_2 <- "https://www.worldwildlife.org/species/directory?sort=name&page=2"
page_1 <- read_html(url_1)
page_2 <- read_html(url_2)
page_1 <- html_nodes(page_1, "td")
page_2 <- html_nodes(page_2, "td")
text <- html_text(page_1)
text <- c(text , html_text(page_2))
text <- matrix(text , nrow = length(text) / 3 , ncol = 3 , byrow = T)
content <- data.frame(text)

name <- read_html(url_1)
name <- html_nodes(name,"th")
name <- html_text(name)
name <- str_replace_all(name,"[^a-zA-Z 0-9]","")
names(content) <- name

levels(content$`Conservation status`) <- c("Critically Endangered" , "Endangered"  , "Vulnerable" ,"Near Threatened" ,"Least Concern","Under investigation")
content[content$`Scientific name` == "",2] <- NA
content[content$`Conservation status` == "",3] <- "Under investigation"
