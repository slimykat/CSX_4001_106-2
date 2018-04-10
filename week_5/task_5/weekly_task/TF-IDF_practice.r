library(NLP)
library(readtext)
library(tm)
library(jiebaRD)
library(jiebaR)
library(xml2)
library(rvest)
library(stringr)

#custom option===
end_index = 2360        #終止頁數#2360為2018年的開始頁數
latest_index = 2415     #最新頁數
#================程式將會從最新頁數一直抓到終止頁數
#關鍵字設定請看第25行

pre_url = "https://www.ptt.cc/bbs/NTU/index"
html_url = ".html"
#post_count = 0
#guan_count = 0
ptt_text = c()    #這是我要的文字內容，全部丟在這個vector裡面最後再做輸出

for(i in c(latest_index:end_index)){
  url <- paste0(pre_url,i,html_url)                #建立url進入看板NTU
  links_data_title <- read_html(url) %>% html_nodes(".title") %>% html_text()        #抓取標題
  links_data_url <- read_html(url) %>% html_nodes(".title a") %>% html_attr('href')  #抓取標題的連結
  #post_count = post_count + length(links_data_title)
  links_data_title <- str_replace_all(links_data_title,"\t","") %>% str_replace_all("\n","")
  
  #custom setting========
  flag = xor(grepl("管",links_data_title)|grepl("校長",links_data_title),      #包含的關鍵字 
             grepl("管理",links_data_title)|grepl("管學院",links_data_title))  #除去的關鍵字
  #======================程式會根據標題裡面的關鍵字做抓取
  #guan_count = guan_count + length(links_data_title[flag])
  
  for(j in (1:length(links_data_title))[flag]){
    links_url = paste0('https://www.ptt.cc',links_data_url[j])
    content_css = read_html(links_url) %>% html_nodes("#main-content") %>% html_text()
    ptt_text = c(ptt_text, iconv(content_css,'utf8'))
  }
}                                                                              #進入每一個貼文中抓取文字

data.corpus = Corpus(VectorSource(ptt_text))
data.corpus = tm_map(data.corpus, removePunctuation)
data.corpus = tm_map(data.corpus, removeNumbers)
data.corpus = tm_map(data.corpus, function(word){gsub("[噓推的了\n\ta-zA-Z，。、？！：～ ※→]"," ",word) })
mixseg = worker()
keywords = read.csv("keywords.csv",quote = "")
new_user_word(mixseg, as.matrix(keywords))

jieba_tokenizer = function(d){
  unlist( segment(d[[1]], mixseg) )
}
seg = lapply(data.corpus, jieba_tokenizer)

count_token = function(d){
  as.data.frame(table(d))
}
tokens = lapply(seg, count_token)

len = length(seg)
Merge_tokens = tokens[[1]]

for( id in c(2:len) )
{
  Merge_tokens = merge(Merge_tokens, tokens[[id]], by="d", all = TRUE)
  names(Merge_tokens) = c("d",as.character(1:id))
}
Merge_tokens[is.na(Merge_tokens)] = 0

sum = rowSums(Merge_tokens[,2:len+1])
Merge_tokens$Sum = sum
Result = Merge_tokens[,c("d","Sum")]

Result[order(Result$Sum,decreasing = T),]
