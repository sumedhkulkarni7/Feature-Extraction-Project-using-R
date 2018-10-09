a <- read.csv('E:/Spring 2018/Data Mining/Project Competition/Datasets/Section 1 - training.csv')
b <- read.csv('E:/Spring 2018/Data Mining/Project Competition/Datasets/Section 2 - validation.csv')

View(a)
View(b)

str(a)
str(b)

Data1 <- subset(a, X381 %in% c(0, 1, 2))
Data1
Data2 <- subset(a, X381 == 3)
Data2


library(dplyr)
#For a

a0 <- filter(a, a$X381 == '0')
View(a0)
summary(a0)
nrow(a0)

a1 <- filter(a, a$X381 == "1")
View(a1)
nrow(a1)

a2 <- filter(a, a$X381 == '2')
View(a2)
nrow(a2)

a3 <- filter(a, a$X381 == '3')
View(a3)
nrow(a3)

#For b
b0 <- filter(b, b$X381 == '0')
View(b0)
summary(b0)
nrow(b0)

b1 <- filter(b, b$X381 == "1")
View(b1)
nrow(b1)

b2 <- filter(b, b$X381 == '2')
View(b2)
nrow(b2)

b3 <- filter(b, b$X381 == '3')
View(b3)
nrow(b3)

c <- rbind(a0, a1)
View(c)

d <- rbind(a0, a2)
View(d)

e <- rbind(a0, a3)
View(e)

f <- rbind(b0, b1)
View(f)

g <- rbind(b0, b2)

h <- rbind(b0, b3)

# ensure the results are repeatable
set.seed(123)
# load the library
library(mlbench)
install.packages(caret)
# load the data
a0
# calculate correlation matrix
correlationMatrix <- cor(a0[,])
# summarize the correlation matrix
print(correlationMatrix)
# find attributes that are highly corrected (ideally >0.75)
highlyCorrelated <- findCorrelation(correlationMatrix, cutoff=0.5)
# print indexes of highly correlated attributes
print(highlyCorrelated)

dim(c)
dim(d)
dim(e)


# Build a SVM classification model 
# import SVM package
# install.packages(e1071)
library(e1071)

# Load the data
a <- read.csv('E:/Spring 2018/Data Mining/Project Competition/Datasets/Section 1 - training.csv')
b <- read.csv('E:/Spring 2018/Data Mining/Project Competition/Datasets/Section 2 - validation.csv')
str(c)
str(f)

# show some records
head(c)
head(f)

# train a SVM model
mysvm = svm(c$X381 ~ ., data = c, cost = 100, gamma = 1, 
            type = "C-classification", kernel = "linear")

# show the weights of support vectors
supportvector = cbind(mysvm$index, mysvm$coefs)
head(supportvector)
# w = sum, x_i, alpha_i 
X = as.matrix(c[,-381]) 
w = t(mysvm$coefs) %*% X[mysvm$index,-381] 

# compute accuarcies for training and testing datasets
svmTrain = predict(mysvm, c[,-381])
table(pred = svmTrain, Actual = c[,381])

svmPred = predict(mysvm, f[,-381])
table(pred = svmPred, Actual = f[,381])