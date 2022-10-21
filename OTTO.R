setwd("/Users/inesalmeida/Desktop")
train <- read.csv("train.csv")
str(train)
train$target <- factor(train$target)
train$out <- relevel(train$target, ref = "Class_1")

for(f in 2:94) 
{train(f) = (train(f) - min(train(f)) / ( max(train(f)) - min(train(f)))}




setwd("/Users/inesalmeida/Desktop")
minitrain <- read.csv("minitrain.csv")

summary(minitrain)

require(rpart)
require(DMwR)
my_tree_one <- rpart(target ~ feat_82 + feat_83 + feat_84 + feat_85 + feat_86 + feat_87 + feat_88+ feat_89 + feat_90, data =minitrain, method = "class")
prettyTree(my_tree_one)
my_tree_one


my_tree_two <- rpart(target ~ feat_91 + feat_92 + feat_93, data =minitrain, method = "class")
my_tree_two

# Set seed for reproducibility
set.seed(111)

require(randomForest)

#multinomial model
library(nnet)
mymodel <- multinom(target ~ feat_2 + feat_3 + feat_8 + feat_11 + feat_14 + feat_17 + feat_27+ feat_25 + feat_24 + feat_34 + feat_36 + feat_28 + feat_40 + feat_41 + feat_43 + feat_46 + feat_50 + feat_53 + feat_62 + feat_61 + feat_60 + feat_67 + feat_69 + feat_66 + feat_80 + feat_76 + feat_78 + feat_90 + feat_86 + feat_85 + feat_92 + feat_91, data = train)
summary(mymodel)

#predict
predict(mymodel, train)

#confirm prediction accuracy
cm <- table(predict(mymodel),train$target)
print(cm)
#misclassification percentage
1-sum(diag(cm))/sum(cm)

#p-value --> confidence level (low p = high conf)
z <- summary(mymodel)$coefficient/summary(mymodel)$standard.errors
p <- (1-pnorm(abs(z),0,1)) * 2
p


#comparing data




#submit
submit.data<-data.frame("id"=1:144368)

result <- predict(mymodel, test, type="prob")
submit.data$Class_1 <- result[,<n>]
write.csv(submit.data[,c("id","Class_1","Class_2","Class_3","Class_4","Class_5","Class_6","Class_7","Class_8","Class_9”)], “submission 1.csv”)

