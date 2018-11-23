setwd("C:/Users/CHIRAG/Downloads/ACADgILd")
library(readr)
epi_r <- read.csv("C:/Users/Seshan/Desktop/sv R related/acadgild/assignments/session21/epi_r.csv")
View(epi_r)
data<-epi_r
View(data)
head(data, n=10)
# data sets in package
data(package="arules")
# Split data
dt <- split(data$rating, data$arizona)
dt
# Loading arules package
require(arules)
require(arulesViz)

# Convert data to transaction level
dt2 = as(dt,"transactions")
dt2
summary(dt2)
inspect(dt2)
# Most Frequent Items
itemFrequency(dt2, type = "relative")
itemFrequencyPlot(dt2,topN = 5)
# with support parameters
itemFrequency(dt2, type = "relative")
itemFrequencyPlot(dt2,support= 0.10)
# aggregated data
rules = apriori(dt2, parameter=list(support=0.005, confidence=0.8))
rules = apriori(dt2, parameter=list(support=0.005, confidence=0.8, minlen = 3))
rules = apriori(dt2, parameter=list(support=0.005, confidence=0.8, maxlen = 4))
rules
summary(rules)

inspect(rules[1:10]) # to view first 10 rules

#Convert rules into data frame
rules3 = as(rules, "data.frame")
write(rules, "C:/Users/Seshan/Desktop/PCA//rules2.csv", sep=",")

# Show only particular product rules
inspect( subset( rules, subset = rhs %pin% "0" )[1:10])

# Show the top 10 rules
options(digits=2)
inspect(rules[1:10])

# Get Summary Information

summary(rules)
plot(rules)
plot(rules, method = "graph", interactive = T)

# Sort by Lift
rules<-sort(rules, by="lift", decreasing=TRUE)

# Sort by Lift
rules<-sort(rules, by="lift", decreasing=TRUE)

# Remove Unnecessary Rules
subset.matrix <- is.subset(rules, rules)
subset.matrix[lower.tri(subset.matrix, diag=T)] <- NA
redundant <- colSums(subset.matrix, na.rm=T) >= 1
which(redundant)
rules.pruned <- rules[!redundant]
rules<-rules.pruned
rules
#Clean Rules
rules3$rules=gsub("\\{", "", rules3$rules)
rules3$rules=gsub("\\}", "", rules3$rules)
rules3$rules=gsub("\"", "", rules3$rules)

#Split the rule
library(splitstackshape)
Rules4=cSplit(rules3, "rules","=>")
names(Rules4)[names(Rules4) == 'rules_1'] <- 'LHS'
Rules5=cSplit(Rules4, "LHS",",")
Rules6=subset(Rules5, select= -c(rules_2))
names(Rules6)[names(Rules6) == 'rules_3'] <- 'RHS'

# What are customers likely to buy before they purchase "Product A"
rules<-apriori(data=dt, parameter=list(supp=0.001,conf = 0.8), 
               appearance = list(default="lhs",rhs="0"),
               control = list(verbose=F))
rules<-sort(rules, decreasing=TRUE,by="confidence")
inspect(rules[1:5])

# What are customers likely to buy if they purchased "Product A"
rules<-apriori(data=dt, parameter=list(supp=0.001,conf = 0.8),appearance = list(default="rhs",lhs="0"),control = list(verbose=F))
rules<-sort(rules, decreasing=TRUE,by="confidence")
inspect(rules[1:5])
rules
support<-seq(0.01,0.1,0.01)
support
rules_count<-c(472,128,46,26,14, 10, 10,8,8,8)
rules_count
plot(support,rules_count,type = "l",main="Number of rules at different support %",col="darkred",lwd=3)

conf<-seq(0.10,1.0,0.10)
conf

rules_count<-c(472,231,125,62,15,0,0,0,0,0)
rules_count

plot(conf,rules_count,type = "l",main="Number of rules at different confidence %",col="darkred",lwd=3)
#rules_ec <- eclat(epi_r, parameter = list(supp = 0.05))
#summary(rules_ec)
#sorting out the most relevant rules
rules<-sort(rules, by="confidence", decreasing=TRUE)
inspect(rules[1:5])

rules<-sort(rules, by="lift", decreasing=TRUE)
inspect(rules[1:5])

########################################
library(factoextra)

library("factoextra")
data1<-na.exclude(data)
na.omit(data1)
data1.active <- data1[2:100, 2:6]
na.exclude(data1.active)
View(data1.active)
head(data1.active[, 2:5])

#Compute PCA in R using prcomp()
library(factoextra)
res.pca <- prcomp(data1.active, scale = TRUE)
res.pca
summary(res.pca)
fviz_eig(res.pca)
fviz_pca_ind(res.pca, col.ind = "cos2", # Color by the quality of representation gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), repel = TRUE     # Avoid text overlapping)
fviz_pca_var(res.pca, col.var = "contrib", # Color by contributions to the PCgradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), repel = TRUE     # Avoid text overlapping)
fviz_pca_biplot(res.pca, repel = TRUE,col.var = "#2E9FDF", # Variables color col.ind = "#696969"  # Individuals color)
library(factoextra)
# Eigenvalues
eig.val <- get_eigenvalue(res.pca)

# Remove Unnecessary Rules
subset.matrix <- is.subset(rules, rules)
subset.matrix[lower.tri(subset.matrix, diag=T)] <- NA
rules.pruned <- rules[!redundant]
rules<-rules.pruned
rules
## set of 0 rules
#Clean Rules
rules3$rules=gsub("\\{", "", rules3$rules)
rules3$rules=gsub("\\}", "", rules3$rules)
rules3$rules=gsub("\"", "", rules3$rules)

#Split the rule
library(splitstackshape)
Rules4=cSplit(rules3, "rules","=>")
names(Rules4)[names(Rules4) == 'rules_1'] <- 'LHS'
Rules5=cSplit(Rules4, "LHS",",")
Rules6=subset(Rules5, select= -c(rules_2))
names(Rules6)[names(Rules6) == 'rules_3'] <- 'RHS'

# What are customers likely to buy before they purchase "Product A"
rules<-apriori(data=dt, parameter=list(supp=0.001,conf = 0.8), 
               appearance = list(default="lhs",rhs="0"),
               control = list(verbose=F))
## Warning in asMethod(object): removing duplicated items in transactions
rules<-sort(rules, decreasing=TRUE,by="confidence")
inspect(rules[1:5])

support<-seq(0.01,0.1,0.01)
support

rules_count<-c(472,128,46,26,14, 10, 10,8,8,8)
rules_count

plot(support,rules_count,type = "l",main="Number of rules at different support %",
     col="darkred",lwd=3)

conf<-seq(0.10,1.0,0.10)
conf
library(factoextra)
res.pca <- prcomp(data1.active, scale = TRUE)
res.pca

fviz_eig(res.pca)

fviz_pca_ind(res.pca,
             col.ind = "cos2", # Color by the quality of representation
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE     # Avoid text overlapping
)

fviz_pca_var(res.pca,
             col.var = "contrib", # Color by contributions to the PC
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE     # Avoid text overlapping
)

fviz_pca_biplot(res.pca, repel = TRUE,
                col.var = "#2E9FDF", # Variables color
                col.ind = "#696969"  # Individuals color
)

library(factoextra)
# Eigenvalues
eig.val <- get_eigenvalue(res.pca)
eig.val