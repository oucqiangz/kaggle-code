# rebuild continuous features.
# @author : oucqiangz (qiangz2012@yeah.net)


##### load cleaned data.
train_set <- read.csv( file = "G:\\Mypro\\GitHub\\kaggle-code\\House-Price\\data\\train_clean.csv" )
train_set <- train_set[,-1]

##### continuous feature analysis.
library(ggplot2)
# point
ggplot(train_set,aes(x = PoolArea, y =salePrices)) + 
  geom_point() #+ 
#  scale_x_continuous(breaks=seq(0, 1000, 100))   

# # fit and plot.
# library(car)
# library(dplyr)
# x2 <- train_set$ScreenPorch
# y2 <- train_set$salePrices
# df <- data.frame(x2,y2)
# df <- filter(df, x2 < 400 & x2 > 0 ) # filter.
# scatterplotMatrix(df, spread = FALSE, lty.smooth = 2, main = "salePrices ~ ScreenPorch")
# 
# fit2 <- lm(y2 ~ x2, data = df)
# print(summary(fit2))
# plot( x = df$x2, y = df$y2, main = "salePrices ~ ScreenPorch", xlab = "ScreenPorch", ylab = "salePrices" )
# abline(fit2, col = "red")
# 
# # use Box-Tidwell transform to fit nonlinear y2~x2.
# x3 <- df$x2
# y3 <- df$y2
# df3 <- data.frame(x3,y3)
# fit3 <- boxTidwell(y3 ~ x3, data = df3)
# print(fit3)
# 
# # refit linear model.
# fit4 <- lm(y3 ~ x3 + I(x3^2) + I(x3^3) + I(x3^4) + I(log(x3)), data = df3)
# print(summary(fit4))
# plot( x = x3, y = y3, main = "salePrices ~ ScreenPorch", xlab = "ScreenPorch", ylab = "salePrices" )
# lines(x3,fitted(fit4), col = "red" )
# 
#   
# 
# 





























