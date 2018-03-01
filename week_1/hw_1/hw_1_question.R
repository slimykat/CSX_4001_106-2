### hw_1_question


########################################################### Task 1

# 查看內建資料集: 鳶尾花(iris)資料集
iris

# 使用dim(), 回傳iris的列數與欄數
dim(iris)

# 使用head() 回傳iris的前六列
head(iris , 6)

# 使用tail() 回傳iris的後六列
tail(iris , 6)

# 使用str() 

str(iris)
# 使用summary() 查看iris敘述性統計、類別型資料概述。
summary(iris)

########################################################### Task 2

# 使用for loop 印出九九乘法表
# Ex: (1x1=1 1x2=2...1x9=9 ~ 9x1=9 9x2=18... 9x9=81)
for( i in c(1:9) ){
  for( j in c(1:9) ){
    cat(i , "x" , j , " = " , i*j , "\n")    
  }
}

########################################################### Task 3

# 使用sample(), 產出10個介於10~100的整數，並存在變數 nums

nums <- sample(c(10:100),size = 10)

# 查看nums
nums

for(i in c(1:10)){
  if( (nums[i] > 50) & (nums[i] %% 2 == 0) )
    print(paste("偶數且大於50, 數字: " , nums[i]))
  if(nums[i] == 66)
    print("太66666666666了")  
  }



########################################################### Task 4

# 請寫一段程式碼，能判斷輸入之西元年分 year 是否為閏年
year <- sample(1000:9999 , 1)

if( year %% 400 == 0 || (year %% 4 == 0 & year %% 100 != 0) ){
	cat(year , "是閏年" , "\n")
}	else{
	cat(year , "是平年" , "\n")
}

########################################################### Task 5

# 猜數字遊戲
# 1. 請寫一個由電腦隨機產生不同數字的四位數(1A2B遊戲)
# 2. 玩家可重覆猜電腦所產生的數字，並提示猜測的結果(EX:1A2B)
# 3. 一旦猜對，系統可自動計算玩家猜測的次數

code <- sample(0:9 , 4)
code <- unlist(strsplit(code,""))

state <- 1

while(state){
	A <- 0
	B <- 0
	input <- readline(prompt = "please guess a four-digit number :")
	input <- unlist(strsplit(input,"") )
	if(length(input) != 4){
		print("error")
	}
	else if( length(c(1:4)[input == code]) == 4){
		print("correct!")
		print(paste("number of guesses :" , state))
		break
	}
	else{
		print("wrong!")
		for(i in c(1:4)){
			for(j in c(1:4)){
				if(code[i] == input[j]){
					if(i == j) A <- A+1
					else B <- B+1
				}
			}
		}
		cat(A,"A" , B, "B\n")
		state <- state + 1
	}
}






