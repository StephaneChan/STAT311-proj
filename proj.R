d1=read.table("student-mat.csv",sep=";",header=TRUE)
d2=read.table("student-por.csv",sep=";",header=TRUE)

d3=merge(d1,d2,by=c("school","sex","age","address","famsize","Pstatus","Medu","Fedu","Mjob","Fjob","reason","nursery","internet"))
print(nrow(d3)) # 382 students

# DECISION TREES + BAGGING/RANDOM FORESTS
# Regression Tree
library(tree)
d10 <- d1[,-31:-32]
rtree <- tree(G3~., data=d10)
summary(rtree)
plot(rtree)
text(rtree, pretty=0)

# k-fold CV
set.seed(6421)
ctree <- cv.tree(rtree, K=20)
plot(ctree, type="b")
ctree
bestsize <- ctree$size[which.min(ctree$dev)]
# CV suggests 3 terminal nodes

# Pruning
p.rtree <- prune.tree(rtree, best=3)
plot(p.rtree)
text(p.rtree, pretty=0)


# Training and testing set
set.seed(763)
index <- sample(1:nrow(d10), 200)
train <- d10[index,]
test <- d10[-index,]
rtr <- tree(G3~., data = train)
plot(rtr)
text(rtr, pretty=0)

# MSE:
yhat <- predict(rtr, test[,-31])
mean((yhat-test[,31])^2)

# Bagging
library(randomForest)
bag <- randomForest(G3~., mtry=30, data=d10, importance = TRUE )
bag
varImpPlot(bag)
importance(bag)

# Random forest
rf <- randomForest(G3~., mtry=10, data=d10, importance = TRUE )
rf
varImpPlot(rf)
importance(rf)
