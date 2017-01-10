# rebuild continuous features.
# @author : oucqiangz (qiangz2012@yeah.net)


##### load cleaned data.
train_set <- read.csv( file = "D:\\GitHub\\kaggle-code\\House-Price\\data\\train_clean.csv" )
train_set <- train_set[,-1]

##### continuous feature analysis.
library(ggplot2)
# point
ggplot(train_set,aes(x = LotArea, y =salePrices)) + 
  geom_point() #+ 
#  scale_x_continuous(breaks=seq(0, 1000, 100))   

# fit and plot.
library(car)
library(dplyr)
x2 <- train_set$LotArea
y2 <- train_set$salePrices
df <- data.frame(x2,y2)
df <- filter(df, x2 < 20000 & x2 > 0 ) # filter.
scatterplotMatrix(df, spread = FALSE, lty.smooth = 2, main = "salePrices ~ LotArea")

fit2 <- lm(y2 ~ x2, data = df)
print(summary(fit2))
plot( x = df$x2, y = df$y2, main = "salePrices ~ LotArea", xlab = "LotArea", ylab = "salePrices" )
abline(fit2, col = "red")

  































