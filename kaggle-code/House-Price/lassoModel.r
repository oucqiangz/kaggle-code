## fit lasso regression model.
## @author : oucqiangz (qiangz2012@yeah.net)


# load train data.
train_set <- read.csv( file = "G:\\Mypro\\GitHub\\kaggle-code\\House-Price\\train_1.csv" )
train_set <- train_set[,-1]
train_set <- as.data.frame(train_set)

# train and validation set.
len <- length(train_set[,1])
train_2 <- train_set[1:1100,]
y_train <- train_2$salePrices
x_train <- subset( train_2, select = -salePrices )
valid_2 <- train_set[1101:len,]
y_valid <- valid_2$salePrices
x_valid <- subset( valid_2, select = -salePrices )

# get best lambda with cross-validation.
library(glmnet)
xMat_train <- as.matrix(x_train)
cvv <- cv.glmnet( x = xMat_train, y = y_train, nfolds = 10, type.measure = "mse", alpha = 1 )
plot(cvv)
lambdaMin <- cvv$lambda.min
lambda1se <- cvv$lambda.1se

# fit lasso regression model.
model.min <- glmnet( x = xMat_train, y = y_train, family = "gaussian", alpha = 1, lambda = lambdaMin )
#print(model.min$beta)             #model effieicients
#print(model.min$dev.ratio)        #adjust R2

model.1se <- glmnet( x = xMat_train, y = y_train, family = "gaussian", alpha = 1, lambda = lambda1se )
#print(model.1se$beta)             
#print(model.1se$dev.ratio) 

# validation.
myValid <- function(x, y, model) {
    y_fit <- predict( object = model, newx = x, s = NULL, type = "link", exact = FALSE );
    y_fit2 <- exp( y_fit ) - 1.0;
    y2 <- exp(y) - 1.0;
    rmse <- 0.0;
    len <- length(y_fit2);
    for (i in 1:len) {
        #print(y2[i]);
        #print(y_fit2[i]);
        #print("-----")
        error <- abs( y_fit2[i]-y2[i] );
        rmse <- (rmse + error^2);
    }
    rmse <- sqrt( rmse/len );
    return (rmse)
}

rmse.min <- myValid(x = as.matrix(x_valid), y = y_valid, model = model.min)
print(rmse.min)   #20535.77
rmse.1se <- myValid(x = as.matrix(x_valid), y = y_valid, model = model.1se)
print(rmse.1se)   #25597.09












