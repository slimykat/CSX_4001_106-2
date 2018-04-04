library(httr)
library(rjson)
library(httpuv)
library(Rfacebook)
library(stringr)
url = "https://www.facebook.com/zeczec.com"
my_token = "EAACEdEose0cBAMbkQj9BCKCPZAJ9n3ZCCjBv52LvhqwMZABoNjiJ9oahPXhRJNEQ7I2AVoc6klzxmQcW5PP4ZB0Ns68cSh09bxr3RQKW40iMf5ORtwerzgGsMDSgiSadHCbmZBUIeaZAJppYitwUwBXDI2kVFsZBRecDu5UldxNeh6fZC44mVs4Sx6fNl08h8ubIzQbAhhxLMwZDZD"
me = getUsers("me", my_token, private_info = T)

page_name = "嘖嘖 zeczec"
page_id = 197008103698657

target = getPage(page_id, my_token, n = 75)

flag = grepl("projects/",target$message)
num = regexec("projects/",target$message[flag])
len = nchar(target$message[flag])
project_name = substr(target$message[flag],as.numeric(num) + nchar("projects/"),len)
count = target$likes_count[flag]

flag = grepl("\n",project_name)
num = regexec("\n",project_name[flag])
project_name[flag] = substr(project_name[flag],1,as.numeric(num)-1)
project = data.frame(project_name,count)
add_up = data.frame(unique(project$project_name),0)
names(add_up) <- c("project_name" ,"count")
for(i in c(1:75)){
  flag = add_up$project_name == project[i,1]
  add_up[flag,2] = add_up[flag, 2] + project[i,2]
}

