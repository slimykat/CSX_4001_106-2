#agriculture product production analsist using Arules 
raw_data = read.csv("FAOSTAT_data.csv")    #raw_data from FAO in 2016
raw_data <- raw_data[,c(2,3,4,6)]

raw_data[,1] = as.character(raw_data[,1])
raw_data[,3] = as.character(raw_data[,3])

flag = grepl(",",raw_data[,3])
temp = regexpr(",",raw_data[,3])
attributes(temp) = NULL

for(i in which(flag)){
  raw_data[i,3] = (substr(raw_data[i,3] , start = 1, stop = temp[i]-1))
}
raw_data[grepl("paper",raw_data[,3]),3] = "Paper"
raw_data[grepl("papers",raw_data[,3]),3] = "Paper"
raw_data[grepl("wood pulp",raw_data[,3]),3] = "Wood pulp"
raw_data[grepl("roundwood",raw_data[,3]),3] = "Roundwood"
raw_data[grepl("charcoal",raw_data[,3]),3] = "Wood fuel"
raw_data[grepl("pellets",raw_data[,3]),3] = "Wood fuel"

country = read.csv(file = "countries of the world.csv")
country2 = read.csv(file = "countries of the world_2.csv")
country = country[,1:2]
flag = raw_data[,1] %in% country[,1]
country[,1] = as.character(country[,1])
country2[,1] = as.character(country2[,1])
country[,2] = as.character(country[,2])
country2[,2] = as.character(country2[,2])

raw_data[,1] = as.character(raw_data[,1])
temp = raw_data[flag,1]
for(i in c(1:length(temp))){
  temp[i] = country[temp[i] == country[,1],2]
}
raw_data[flag,1] = temp

temp2 = raw_data[!flag,1]
for(i in c(1:length(temp2))){
  temp2[i] = country2[temp2[i] == country2[,1],2]
}
raw_data[!flag,1] = temp2

Area = unique(raw_data$Area)
Element = unique(raw_data$Element)


# S = 1
# E = 1
buffer = data.frame("init","init","init",0)
names(buffer) = names(raw_data)
for(i in Area){
  for(j in Element){
    clip = raw_data[i == raw_data$Area & j == raw_data$Element,]
    n = length(clip[,1])
    for(r in c(1:n)){
      slice = (clip[r,1]==buffer[,1] & clip[r,2] == buffer[,2] & clip[r,3] == buffer[,3])
      if(length(which(slice)) != 0){
        buffer[S+slice-1,4] = buffer[S+slice-1,4] + clip[i,4]
        E = E+1
      }else{
        buffer = rbind(buffer , clip[r,])
      }
    }
  }
  # S = E
}
buffer = buffer[-c(1),]
# Bad_entry = buffer$Value < 1
# cleaned_data = buffer[!Bad_entry,c(1:3)]
# Value = buffer[!Bad_entry,4]
buffer$Value[buffer$Value < 1] = 1
cleaned_data = buffer[,c(1:3)]
Value = buffer[,4]

table(cleaned_data$Element)
plot(Value[order(Value)])
Value = round((Value^0.1))
summary(Value)
sum(Value)
plot(Value[order(Value)])

# summary(log(raw_data$Value))
# breaking = 5.919
# 
# raw_data$Value[log(raw_data$Value) <  breaking] = 0
# raw_data$Value[log(raw_data$Value) >= breaking] = 1
# table(raw_data$Value)
# raw_data$Value = as.factor(raw_data$Value)
# 
# summary(as.vector(table(raw_data$Area)))
# breaking = 80
# Area = unique(raw_data$Area)
# Acount = as.numeric(table(raw_data$Area))
# Bad_Area = Area[Acount <= breaking]
# raw_data = raw_data[!raw_data$Area %in% Bad_Area,]
# summary(raw_data$Area)

#raw_data$Area <- factor(raw_data$Area)

extend <- function(Cleaned_data, Value){
  loop = length(Value)
  cols = length(Cleaned_data)
  buffer = c()
  for (i in 1:loop){
    clip = Cleaned_data[i,]
    buffer = c(buffer, rep(as.matrix(clip),time = Value[i]))
  }
  return (matrix(buffer,ncol = cols, byrow = T))
}

cleaned_table = extend(cleaned_data,Value)

write.table(cleaned_table,file = "FAOSTAT_data_cleaned.txt", sep = ",", quote = F,
            row.names = F , col.names = F)

# require(arules)
# 
# rule <- apriori(raw_data, 
#                 # min support & confidence
#                 parameter=list( minlen=3, supp=0.1, conf=0.7),  
#                                 appearance = list(default="lhs",
#                                                   rhs=c("Value=0", "Value=1") 
#                 )
# )  
# 
# inspect(rule)
# sort.rule <- sort(rule, by="lift")
# inspect(sort.rule)
# 
# # subset.matrix <- as.matrix(is.subset(x=sort.rule, y=sort.rule))
# # subset.matrix[lower.tri(subset.matrix, diag=T)] <- NA
# # redundant <- colSums(subset.matrix, na.rm=T) >= 1
# # sort.rule <- sort.rule[!redundant]
# # inspect(sort.rule)
# 
# require(arulesViz)
# plot(sort.rule)
# plot(sort.rule, method="graph", control=list(type="items"))
# plot(sort.rule, method="grouped")

