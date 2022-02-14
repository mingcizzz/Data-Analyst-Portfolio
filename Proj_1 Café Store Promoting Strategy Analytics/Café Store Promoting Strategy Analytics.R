#Project_1
#Optimizing Cafe Store Promoting Strategy
#Mingchi Zhang

#######################################################################
#environment preparation
library(tidyverse) # data manipulation
library(arules) # mining association rules and frequent itemsets
library(arulesViz) # visualization techniques for association rules
library(knitr) # dynamic report generation
library(gridExtra) # provides a number of user-level functions to work with "grid" graphics
library(lubridate) # work with dates and times
library(rpart)
library(rpart.plot)
library(caret)     #Confusion Matrix
library(dplyr)
library(neuralnet) #Neural Network

#############################
#I.Part one_Market Basket Analysis

#1. import transaction data

transactions<-read.csv("transaction for R_Apriori.csv")
# create a binary incidence matrix
transactions.df <- transactions[, -1]
incid.transactions.df <- ifelse(transactions.df > 0, 1, 0)
incid.transactions.mat <- as.matrix(incid.transactions.df)
#  convert the binary incidence matrix into a transactions database
coffee.trans <- as(incid.transactions.mat, "transactions")
inspect(coffee.trans)   #There are 17861 transactions
# plot the frequency of each product_type
itemFrequencyPlot(coffee.trans)
#############################

#2. Choice of support and confidence
# Support and confidence values
supportLevels <- c(0.1, 0.05, 0.01, 0.005)
confidenceLevels <- c(0.9, 0.8, 0.7, 0.6, 0.5, 0.4, 0.3, 0.2, 0.1)
# Empty integers 
rules_sup10 <- integer(length=9)
rules_sup5 <- integer(length=9)
rules_sup1 <- integer(length=9)
rules_sup0.5 <- integer(length=9)
# Apriori algorithm with a support level of 10%
for (i in 1:length(confidenceLevels)) {
  rules_sup10[i] <- length(apriori(coffee.trans, parameter=list(sup=supportLevels[1], 
       conf=confidenceLevels[i], target="rules")))}
# Apriori algorithm with a support level of 5%
for (i in 1:length(confidenceLevels)){
  rules_sup5[i] <- length(apriori(coffee.trans, parameter=list(sup=supportLevels[2], 
      conf=confidenceLevels[i], target="rules")))}
# Apriori algorithm with a support level of 1%
for (i in 1:length(confidenceLevels)){
  rules_sup1[i] <- length(apriori(coffee.trans, parameter=list(sup=supportLevels[3], 
    conf=confidenceLevels[i], target="rules")))}
# Apriori algorithm with a support level of 0.5%
for (i in 1:length(confidenceLevels)){
  rules_sup0.5[i] <- length(apriori(coffee.trans, parameter=list(sup=supportLevels[4], 
        conf=confidenceLevels[i], target="rules")))}
#
# Number of rules found with a support level of 10%
plot1 <- qplot(confidenceLevels, rules_sup10, geom=c("point", "line"), 
               xlab="Confidence level", ylab="Number of rules found", 
               main="Apriori with a support level of 10%") +theme_bw()
# Number of rules found with a support level of 5%
plot2 <- qplot(confidenceLevels, rules_sup5, geom=c("point", "line"), 
               xlab="Confidence level", ylab="Number of rules found", 
               main="Apriori with a support level of 5%") + 
  scale_y_continuous(breaks=seq(0, 100, 5)) +theme_bw()
# Number of rules found with a support level of 1%
plot3 <- qplot(confidenceLevels, rules_sup1, geom=c("point", "line"), 
               xlab="Confidence level", ylab="Number of rules found", 
               main="Apriori with a support level of 1%") + 
  scale_y_continuous(breaks=seq(0, 500, 25)) +theme_bw()
# Number of rules found with a support level of 0.5%
plot4 <- qplot(confidenceLevels, rules_sup0.5, geom=c("point", "line"), 
               xlab="Confidence level", ylab="Number of rules found", 
               main="Apriori with a support level of 0.5%") + 
  scale_y_continuous(breaks=seq(0, 1000, 100)) + theme_bw()
# Subplot
grid.arrange(plot1, plot2, plot3, plot4, ncol=2)
##############################

#3. run apriori function rules
rules <- apriori(coffee.trans, 
                 parameter = list(supp= 0.03, conf = 0.3, target = "rules",minlen=2))
# inspect rules
inspect(sort(rules, by = "lift"))
###############################

#4.Visualization of valid association rules
#4.1 Graph1_Interactive graph as a html widget (using igraph layout)
plot(rules, method = "graph", engine = "htmlwidget")

#4.2 Graph2
plot(rules, method="graph", 
     control = list(
       layout = igraph::with_graphopt(),
       edges = ggnetwork::geom_edges(color = "black", 
                                     arrow = arrow(length = unit(4, "pt"), type = "open"), alpha = .5),
       nodes = ggnetwork::geom_nodes(aes(size = support, color = lift),  na.rm = TRUE),
       nodetext = ggnetwork::geom_nodetext(aes(label = label), alpha = .6)
     )) + 
  scale_color_gradient(low = "skyblue", high = "yellow") + 
  scale_size(range = c(2, 15)) 

#4.3 Graph3_Engine igraph
plot(rules, method="graph", engine = "igraph",
     layout = igraph::with_graphopt(spring.const = 5, mass = 50))

#4.4 Graph4_Show LHS and RHS
plot(rules, method = "grouped")+scale_color_gradient(low = "skyblue", high = "yellow") + 
  scale_size(range = c(2, 15)) 
##############################

#5.Arules based on three stores separately
#5.1Store3_Market Basket Analysis
trans_Store3<-read.csv("transaction_Store3.csv")
# create a binary incidence matrix
trans_Store3.df <- trans_Store3[, -1]
incid.trans_Store3.df <- ifelse(trans_Store3.df > 0, 1, 0)
incid.trans_Store3.mat <- as.matrix(incid.trans_Store3.df)
#  convert the binary incidence matrix into a transactions database
coffee.trans_Store3 <- as(incid.trans_Store3.mat, "transactions")
inspect(coffee.trans_Store3)
# plot data
itemFrequencyPlot(coffee.trans_Store3)
# run apriori function rules1
rules_store3_1 <- apriori(coffee.trans_Store3, 
                  parameter = list(supp= 0.02, conf = 0.15, target = "rules",minlen=2))
# inspect rules
inspect(sort(rules_store3_1, by = "lift"))

#5.2 Store5_Market Basket Analysis
trans_Store5<-read.csv("transaction_Store5.csv")
# create a binary incidence matrix
trans_Store5.df <- trans_Store5[, -1]
incid.trans_Store5.df <- ifelse(trans_Store5.df > 0, 1, 0)
incid.trans_Store5.mat <- as.matrix(incid.trans_Store5.df)
#  convert the binary incidence matrix into a transactions database
coffee.trans_Store5 <- as(incid.trans_Store5.mat, "transactions")
inspect(coffee.trans_Store5)
# plot data
itemFrequencyPlot(coffee.trans_Store5)
# run apriori function rules1
rules_Store5_1 <- apriori(coffee.trans_Store5, 
                          parameter = list(supp= 0.02, conf = 0.1, target = "rules",minlen=2))
# inspect rules
inspect(sort(rules_Store5_1, by = "lift"))

#5.3 Store8_ Basket Analysis
trans_Store8<-read.csv("transaction_Store8.csv")
# create a binary incidence matrix
trans_Store8.df <- trans_Store8[, -1]
incid.trans_Store8.df <- ifelse(trans_Store8.df > 0, 1, 0)
incid.trans_Store8.mat <- as.matrix(incid.trans_Store8.df)
#  convert the binary incidence matrix into a transactions database
coffee.trans_Store8 <- as(incid.trans_Store8.mat, "transactions")
inspect(coffee.trans_Store8)
# plot data
itemFrequencyPlot(coffee.trans_Store8)
# run apriori function rules1
rules_Store8_1 <- apriori(coffee.trans_Store8, 
                          parameter = list(supp= 0.02, conf = 0.1, target = "rules",minlen=2))
# inspect rules
inspect(sort(rules_Store8_1, by = "lift"))


########################################################################################
########################################################################################
#II. Part two. Clustering analysis of customer data.class

#1. Partitional clustering analysis
#1.1 import the data and normalize the variables
customer.df <- read.csv("customer_clustering.csv")
# set row names to the customer column
row.names(customer.df) <- customer.df[,1]
# remove the column of customerID
customer.df <- customer.df[,-1]
# compute Euclidean distance
d <- dist(customer.df, method = "euclidean")
# normalize input variables (since the results is scale dependency)
customer.df.norm <- sapply(customer.df, scale)
# add row names
row.names(customer.df.norm) <- row.names(customer.df) 

#1.2 Find the optimal k value for kmeans algorithm
#Run K-Means using, looping through a number of cluster centers to find the optimal number (k).
#This can be done visually, where the elbow of the plot "bends".
customer.f <- function(K) {kmeans(customer.df.norm, K)$tot.withinss}
customer.value <- vapply(1:20, customer.f, 0)
plot(customer.value, type = "l", xlab = "Number of clusters", ylab = "Total Within Clusters -
Sum of Squares")
points(customer.value)

#1.3 run kmeans algorithm with k=6
set.seed(2)
km <- kmeans(customer.df.norm, 6)
# show cluster membership
km$cluster
# centroids
km$centers

#1.4 translate the data with original values
center<-data.frame(km$centers)
center$Frequency*sd(customer.df$Frequency)+mean(customer.df$Frequency)
center$Monetary*sd(customer.df$Monetary)+mean(customer.df$Monetary)
center$recency*sd(customer.df$recency)+mean(customer.df$recency)
center$Age*sd(customer.df$Age)+mean(customer.df$Age)
center$membership*sd(customer.df$membership)+mean(customer.df$membership)

##Denorm solution 1
center<-data.frame(
  Frequency_dn=c(km$centers[,1]*sd(customer.df$Frequency)+mean(customer.df$Frequency)),
  Monetary_dn=c(km$centers[,2]*sd(customer.df$Monetary)+mean(customer.df$Monetary)),
  Recency_dn=c(km$centers[,3]*sd(customer.df$recency)+mean(customer.df$recency)),
  Age_dn=c(km$centers[,4]*sd(customer.df$Age)+mean(customer.df$Age)),
  Membership_dn=c(km$centers[,5]*sd(customer.df$membership)+mean(customer.df$membership))
)

##Denorm solution 2
center<-data.frame(km$centers)

center<-data.frame(
  Frequency_dn=c(center$Frequency*sd(customer.df$Frequency)+mean(customer.df$Frequency)),
  Monetary_dn=c(center$Monetary*sd(customer.df$Monetary)+mean(customer.df$Monetary)),
  Recency_dn=c(center$recency*sd(customer.df$recency)+mean(customer.df$recency)),
  Age_dn=c(center$Age*sd(customer.df$Age)+mean(customer.df$Age)),
  Membership_dn=c(center$membership*sd(customer.df$membership)+mean(customer.df$membership))
)

#1.5 Plot the cluster results
# plot an empty scatter plot
plot(c(0), xaxt = 'n', ylab = "", type = "l", 
     ylim = c(min(km$centers), max(km$centers)), xlim = c(0, 5))
# label x-axes
axis(1, at = c(1:5), labels = names(customer.df))
# plot centroids
for (i in c(1:6))
  lines(km$centers[i,], lty = i, lwd = 3, col = ifelse(i %in% c(1, 3, 5),
                                                       "black", "dark grey"))
# name clusters
text(x =0.5, y = km$centers[, 1], labels = paste("Cluster", c(1:6)))

#########################################################################
#2. Hierarchical clustering analysis

#2.1 import data and normalize the variables
customer.df <- read.csv("customer_clustering.csv")
# set row names to the customer column
row.names(customer.df) <- customer.df[,1]
# remove the utility column
customer.df <- customer.df[,-1]
# compute Euclidean distance
# (to compute other distance measures, change the value in method = )
d <- dist(customer.df, method = "euclidean")
# normalize input variables
customer.df.norm <- sapply(customer.df, scale)
# add row names: customer
row.names(customer.df.norm) <- row.names(customer.df) 
# compute normalized distance based on all variables
d.norm <- dist(customer.df.norm, method = "euclidean")

#2. run hierarchical clustering use ward method
# in hclust() set argument method =  ward.D
hc <- hclust(d.norm, method = "ward.D")

#3. Visualize the results
#generate dendrogram
plot(hc, hang = -1, ann = TRUE)
#define six clusters
memb <- cutree(hc, k = 6)
memb
# better grouping, dendexten
plot(hc, cex = 1.5)
rect.hclust(hc, k = 6, border = 2:5)
# set labels as cluster membership and customerID
row.names(customer.df.norm) <- paste(memb, ": ", row.names(customer.df), sep = "")
# plot heatmap 
# rev() reverses the color mapping to large = dark
heatmap(as.matrix(customer.df.norm), Colv = NA, hclustfun = hclust, 
        col=rev(paste("gray",1:99,sep="")))

#########################################################################
#3. Logistic Regression analysis
Regression.df <- read.csv("transaction_Log_Regression.csv")
Regression.df <- Regression.df[, -1]
set.seed(2)
train.index <- sample(c(1:dim(Regression.df)[1]), dim(Regression.df)[1]*0.6)  
train.df <- Regression.df[train.index, ]
valid.df <- Regression.df[-train.index, ]

# run logistic regression in the training data
logit.reg.ct <- glm(Barista.Espresso ~ ., data = train.df, family = "binomial") 
options(scipen=999)
summary(logit.reg.ct)
logit.ct.point.pred.train <- predict(logit.reg.ct,train.df[,-1], type = "response")
logit.ct.point.pred.train<-ifelse(logit.ct.point.pred.train > 0.5,1,0)
confusionMatrix(as.factor(logit.ct.point.pred.train), as.factor(train.df$Barista.Espresso))

# run logistic regression in the validation data
logit.reg.vt <- glm(Barista.Espresso ~ ., data = valid.df, family = "binomial") 
options(scipen=999)
summary(logit.reg.vt)
logit.vt.point.pred.valid <- predict(logit.reg.vt,valid.df[,-1], type = "response")
logit.vt.point.pred.valid<-ifelse(logit.vt.point.pred.valid > 0.5,1,0)
confusionMatrix(as.factor(logit.vt.point.pred.valid), as.factor(valid.df$Barista.Espresso))

#########################################################################
#4. Linear Regression analysis
# run regression analysis in the income data set
income.df <- read.csv("weather.csv")
income.df<-income.df[,-c(1)]
income.reg <- lm(Sales.Average ~ ., data = income.df) 
options(scipen=999)
summary(income.reg)

# structured training data and validation data by 60% and 40% for income data
income.df <- read.csv("weather.csv")
income.df<-income.df[,-c(1)]
set.seed(1)
train.index.in <- sample(c(1:dim(income.df)[1]), dim(income.df)[1]*0.6)  
train.df.in <- income.df[train.index.in, ]
valid.df.in <- income.df[-train.index.in, ]

# classification tree (train)
default.ct <- rpart(Sales.Average ~ ., data = train.df.in ,
                    method = "class")
# plot tree
prp(default.ct, type = 1, extra = 2, under = TRUE, split.font = 1, varlen = -10)
length(default.ct$frame$var[default.ct$frame$var == "<leaf>"])

# classify records in the training data
default.ct.point.pred.train <- predict(default.ct,train.df.in, type = "class")
# generate confusion matrix for training data
confusionMatrix(default.ct.point.pred.train, 
                as.factor(train.df.in$Sales.Average))

# classification tree (validation)
default.vt <- rpart(Sales.Average ~ ., data = valid.df.in ,method = "class")
# plot tree
prp(default.vt, type = 1, extra = 2, under = TRUE, split.font = 1, varlen = -10)
length(default.vt$frame$var[default.vt$frame$var == "<leaf>"])

# classify records in the validation data
default.vt.point.pred.train <- predict(default.vt,valid.df.in, type = "class")
# generate confusion matrix for validation data
confusionMatrix(default.vt.point.pred.train, as.factor(valid.df.in$Sales.Average))

#########################################################################
#5. Decision tree analysis
customer.df <- read.csv("customer_clustering.csv")

cus_deci_tree.df <- customer.df[ , -c(1)]  # Drop ID

mean_mone<- mean(cus_deci_tree.df$Monetary)
cus_deci_tree.df[,2] <- ifelse(cus_deci_tree.df$Monetary > mean_mone, 1, 0)
# partition 
#Here we select 60% of the data randomly for the training dataset, and the remaining will be the validation data set.
set.seed(1)  
train.index <- sample(c(1:dim(cus_deci_tree.df)[1]), dim(cus_deci_tree.df)[1]*0.6)  
train.df <- cus_deci_tree.df[train.index, ]
valid.df <- cus_deci_tree.df[-train.index, ]

############################
set.seed(1)
cv.ct <- rpart(Monetary ~ ., data = cus_deci_tree.df, method = "class", cp = 0.00001, minsplit = 1, xval = 5)  # minsplit is the minimum number of observations in a node for a split to be attempted. xval is number K of folds in a K-fold cross-validation.
printcp(cv.ct)  # Print out the cp table of cross-validation errors.

pruned.ct <- prune(cv.ct, cp = 0.0015873)
pruned.ct
printcp(pruned.ct)
prp(pruned.ct, type = 1, extra = 1, under = TRUE, split.font = 1, varlen = -10, 
    box.col=ifelse(pruned.ct$frame$var == "<leaf>", 'gray', 'white')) 
length(pruned.ct$frame$var[pruned.ct$frame$var == "<leaf>"])
###################
#Evaluate the performance of the training and validation dataset in the decision tree

# classify records in the validation data.
# set argument type = "class" in predict() to generate predicted class membership.
default.ct.point.pred.train <- predict(pruned.ct,train.df,type = "class")
# generate confusion matrix for training data
confusionMatrix(default.ct.point.pred.train, as.factor(train.df$Monetary))

### repeat the code for the validation set
default.ct.point.pred.valid <- predict(pruned.ct,valid.df,type = "class")
confusionMatrix(default.ct.point.pred.valid, as.factor(valid.df$Monetary))

#########################################################################

##6 neural network

# regression
df1<-df[,c(-1,-3,-7,-8)]
df$aim<- case_when(
  df$Monetary>=avg_mon ~ 1,
  df$Monetary<avg_mon ~ 0
)

# partition
set.seed(1)  
train.index <- sample(c(1:dim(df)[1]), dim(df)[1]*0.6)  
train.df <- df[train.index, ]
valid.df <- df[-train.index, ]

# run logistic regression.
logit.reg <- glm(aim ~ ., data = df1, family = "binomial") 
options(scipen=999)
summary(logit.reg)

## neural network
df <- read.csv("customer_clustering.csv")

avg_mon<-mean(df$Monetary)
df$target <- df$Monetary>=avg_mon
df$notarget <- df$Monetary<avg_mon

set.seed(1)
nn0 <- neuralnet(aim  ~ Frequency, data = train.df, linear.output = F, hidden = 5,stepmax = 500000)
nn <- neuralnet(aim  ~ Frequency + Monetary + recency + Age +membership, data = train.df, linear.output = F, hidden = 5,stepmax = 500000)
nn <- neuralnet(aim  ~ Frequency + Monetary + recency + Age +membership, data = train.df, linear.output = F, hidden = 5,stepmax = 500000,err.fct = "ce")

# display weights
nn$weights

# display predictions
prediction(nn)

# plot network
plot(nn, rep="best")

#predict
nn.pred.valid <- predict(nn,valid.df,type = "response")
nn.pred.valid1 <-ifelse(nn.pred.valid>.5,1,0)

# generate confusion matrix for training data
confusionMatrix(as.factor(nn.pred.valid1), as.factor(valid.df$aim))
########################################################