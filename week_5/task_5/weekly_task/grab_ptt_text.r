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

write.table(ptt_text, file = "text_content.txt", sep = " ")

