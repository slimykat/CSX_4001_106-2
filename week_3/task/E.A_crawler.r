library(httr)
library(rvest)
library(stringr)
url <- "https://www.worldwildlife.org/species/directory?direction=desc&sort=extinction_status"
title <- read_html(url)
text <- html_nodes(title, "td")
text <- html_text(text)
text <- matrix(text , nrow = length(text) / 3 , ncol = 3 , byrow = T)
content <- data.frame(text)

# title_1 <- html_nodes(title,".keep")
# title_2 <- html_nodes(title,".keep+ td") 
# title_3 <- html_nodes(title,".keep~ td+ td")

name <- html_nodes(title,"th")
name <- html_text(name)
name <- str_replace_all(name,"[^a-zA-Z 0-9]","")
 
# title_1 <- html_text(title_1)
# title_2 <- html_text(title_2)
# title_3 <- html_text(title_3)

#content <- data.frame(title_1,title_2,title_3)

names(content) <- name
