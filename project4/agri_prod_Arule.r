rm(list=ls())
agri = read.table("FAOSTAT_data_cleaned.txt" ,sep = ",")
require(arules)
agri = data.frame(agri[,1],agri[,3],agri[2])
names(agri) = c("V1","V2","V3")
agri2 = as(agri, "transactions")
agri2
rule <- apriori( agri2, parameter = list(minlen = 2, maxlen=4, supp=0.02, conf=0.05) )

inspect(head(rule),20)
sort.rule <- head(sort(rule, by="lift"),20)
inspect(sort.rule)

# subset.matrix <- as.matrix(is.subset(x=sort.rule, y=sort.rule))
# subset.matrix[lower.tri(subset.matrix, diag=T)] <- NA
# redundant <- colSums(subset.matrix, na.rm=T) >= 1
# sort.rule <- sort.rule[!redundant]
# inspect(sort.rule)

require(arulesViz)
plot(sort.rule)
plot(sort.rule, method="graph")
plot(sort.rule, method="grouped")
