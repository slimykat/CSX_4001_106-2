library(httr)
library(rvest)
url <- "https://zh.wikipedia.org/wiki/2018年冬季残奥会"
title <- read_html(url)
title <- html_nodes(title,".thumbborder+ a") 
text <- html_text(title)
#text <- list(text)
#names(text) <- "team"