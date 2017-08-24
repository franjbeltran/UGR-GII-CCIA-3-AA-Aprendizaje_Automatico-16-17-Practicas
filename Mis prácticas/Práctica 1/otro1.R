## Practical session: Linear classification
## Jean-Philippe.Vert@mines.org
##
## In this practical session to test several linear methods for classification (LDA, logistic regression)
##
## Packages needed: MASS, glmpath, e1071


set.seed(111) # random number generator

####################################
## Prepare data
####################################

# Download South African heart disease data
hr <- read.table("SAHD.data", sep=",",head=T,row.names=1)

# Pretty plot
pairs(hr[1:9],pch=21,bg=c("red","green")[factor(hr$chd)])

# Scale data and create a random train / test split
n <- nrow(hr)
p <- ncol(hr)-1
test.ratio <- .2 # ratio of test/train samples
n.test <- ceiling(n*test.ratio)
testi <- sample(1:n,n.test)
traini <- setdiff(1:n,testi)

data.train <- hr[traini,]
data.test <- hr[testi,]



####################################
## LDA
####################################
library(MASS)

# Train the model
hr.lda <- lda(chd ~ sbp + tobacco + ldl + famhist + obesity + alcohol + age , data=data.train )
hr.lda

# Evaluate performance
ypred.train <- predict(hr.lda,data.train)$class
ypred.test <- predict(hr.lda,data.test)$class

# Training error
table(ypred.train , data.train$chd)
mean(ypred.train == data.train$chd)
# Test error
table(ypred.test , data.test$chd)
mean(ypred.test == data.test$chd)
# LOO error
hr.lda.loo <- lda(chd ~ sbp + tobacco + ldl + famhist + obesity + alcohol + age , data=data.train , CV=TRUE)
table(hr.lda.loo$class , data.train$chd)
mean(hr.lda.loo$class == data.train$chd)



####################################
## QDA (not a linear method!)
####################################

hr.qda <- qda(chd ~ sbp + tobacco + ldl + famhist + obesity + alcohol + age , data=data.train )
hr.qda

# Evaluate performance
ypred.train <- predict(hr.qda,data.train)$class
ypred.test <- predict(hr.qda,data.test)$class
# Training error
table(ypred.train , data.train$chd)
mean(ypred.train == data.train$chd)
# Test error
table(ypred.test , data.test$chd)
mean(ypred.test == data.test$chd)
# LOO error
hr.qda.loo <- qda(chd ~ sbp + tobacco + ldl + famhist + obesity + alcohol + age , data=data.train , CV=TRUE)
table(hr.qda.loo$class , data.train$chd)
mean(hr.qda.loo$class == data.train$chd)

####################################
## Logistic regression
####################################

# Fit a logistic regression model on all variable to reproduce Table 4.2
hr.lr <- glm(chd ~ sbp + tobacco + ldl + famhist + obesity + alcohol + age , family = binomial , data=hr)

summary(hr.lr)

# Iteratively remove the least significant variable until all are significant (Table 4.3)
summary(glm(chd ~ sbp + tobacco + ldl + famhist + obesity + age , family = binomial , data=hr))
summary(glm(chd ~ tobacco + ldl + famhist + obesity + age , family = binomial , data=hr))
summary(glm(chd ~ tobacco + ldl + famhist + age , family = binomial , data=hr))

# Alternatively, do it automatically 
library(MASS)
hr.step <- stepAIC(hr.lr)
summary(hr.step)

# Train the full model and the reduced model on the training set, predict on test set
hr.lr.full <- glm(chd ~ sbp + tobacco + ldl + famhist + obesity + alcohol + age , family = binomial , data=data.train)
ypred <- predict(hr.lr.full,data.test)>0
table(ypred , data.test$chd)
mean(ypred == data.test$chd)

hr.lr.small <- stepAIC(hr.lr.full)
ypred <- predict(hr.lr.small,data.test)>0
table(ypred , data.test$chd)
mean(ypred != data.test$chd)



####################################
## L1 regularized logistic regression
####################################

library(glmpath)

# we need to define x and y to use glmpath
x <- hr[,c(1,2,3,5,7,8,9)]
x$famhist <- as.numeric(x$famhist)
x <- as.matrix(x)
y <- hr[,10]

# Train the model
hr.glmpath <- glmpath(x , y , family = binomial)

# Visualize the model (Fig. 4.13)
plot(hr.glmpath)

# Cross-validation
a <- cv.glmpath(x , y , family = binomial , type="response")
plot(a$fraction,a$cv.error)


r <- cv.glmpath(x[traini,] , y[traini] , family = binomial , type="response")
plot(r$fraction,r$cv.error)
bestfraction <- r$fraction[which.min(r$cv.error)]
# Retrain a model on the training set
hr.glmpath <- glmpath(x[traini,] , y[traini] , family = binomial)
# Predict (with the regularization parameter optimized on the training set)
ypred <- predict(hr.glmpath,x[testi,] , s=bestfraction , mode="norm.fraction")>0
table(ypred , data.test$chd)
mean(ypred == data.test$chd)


####################################
## SVM
####################################

library(e1071)

# Train a SVM with default parameters
hr.svm <- svm(chd ~ sbp + tobacco + ldl + famhist + obesity + alcohol + age , data=data.train , type="C-classification")
summary(hr.svm)

# Predict
ypred <- predict(hr.svm , data.test)
mean(ypred == data.test$chd)

# Linear SVM
hr.svm <- svm(chd ~ sbp + tobacco + ldl + famhist + obesity + alcohol + age , data=data.train , type="C-classification" , kernel = "linear" , cost=0.1)
ypred <- predict(hr.svm , data.test)
mean(ypred == data.test$chd)

