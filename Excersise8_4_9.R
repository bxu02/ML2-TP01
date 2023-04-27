#install.packages("ISLR2")
#install.packages("tree") 

library(ISLR2)
library(tree)
#detach(OJ)
attach(OJ)
set.seed(1)

# Inspect the data
dim(OJ) # 1070 rows, 18 columns
str(OJ) # inspect variable types 
summary(OJ) # inspect the overall data

# we see we that we are given 2 factor variables: Purchases and Store 7
# we need to change store, special MM, specialCH and storeid are being
# read as numbers instead of factors. these variables contain 
# category/group labels so we need to change them to factors


OJ$SpecialCH <- as.factor(SpecialCH)
OJ$SpecialMM <- as.factor(SpecialMM)
OJ$STORE <- as.factor(STORE)
OJ$StoreID <- as.factor(StoreID)


#a)
trainIndex<-sample(1:nrow(OJ),800)
trainset<-OJ[trainIndex,]
testset<-OJ[-trainIndex,]

#b)
tree.oj=tree(Purchase~., trainset)
summary(tree.oj)
# Variables actually used in tree construction: "LoyalCH"   "PriceDiff"
# Number of terminal nodes:  6 
# Residual mean deviance:  0.7655
# Misclassification error rate: 0.1725 

#c)
tree.oj

#d)
plot(tree.oj)
text(tree.oj,pretty=0)

#e)
tree.pred<-predict(tree.oj,testset,type="class")

# contrasts(Purchase) [ are positive class = MM]
table(testset$Purchase,tree.pred) 
# confusion matrix table results:
      # true positives = 74
      # true negatives = 138
      # false positives = 30
      # false negatives = 28

mean(testset$Purchase!=tree.pred) # accuracy rate =  0.2148148

#f)
cv.tree.oj<-cv.tree(tree.oj,FUN= prune.misclass);cv.tree.oj

#g)
plot(cv.tree.oj$size,cv.tree.oj$dev,type="b")

#h)
#7

#i)
prune.oj <- prune.misclass(tree.oj, best = 4)

#j)
unpruned.pred <- predict(tree.oj,trainset,type="class")
mean(trainset$Purchase!= unpruned.pred) # unpruned train accuracy = 0.1725

pruned.pred <- predict(prune.oj,trainset,type="class")
mean(trainset$Purchase!= pruned.pred) # pruned train accuracy = 0.17375

# We see that pruned tree's training error rate is higher 
# We see less over fitting

#k)
tree.pred2<-predict(prune.oj,testset,type="class")

mean(testset$Purchase!=tree.pred) # unpruned test accuracy = 0.2148148
mean(testset$Purchase!=tree.pred2) # pruned test accuracy = 0.2

# We see that the unpruned tree's test error rate is higher
