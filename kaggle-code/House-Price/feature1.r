# build more feature.
# @author : oucqiangz (qiangz2012@yeah.net)

# load cleaned source train data.
cln_data <- read.csv( file = "G:\\Mypro\\GitHub\\kaggle-code\\House-Price\\train_clean.csv" )

##### category feature analysis.
library(ggplot2)
## category : MSSubClass
ggplot(cln_data, aes(x = MSSubClass, y = salePrices)) +
  # plot
  geom_point() +
  scale_x_continuous(breaks=seq(0, 200, 10))   ## 修改坐标轴的刻度间隔：X 轴每隔 10 个单位显示一个刻度

## category : MSZoning,Street,LotShape,LandContour,Utilities,LotConfig,LandSlope,Neighborhood
##            Condition1,Condition2,BldgType,HouseStyle,
library(dplyr)

# count.
ggplot(cln_data, aes(x = MasVnrType)) +
  geom_bar()   # y-axis：case count

# mean.
tbl_data <- tbl_df(cln_data)
grouped <- group_by(tbl_data,MasVnrType)
y.mean <- as.data.frame( summarise(grouped, mean(salePrices)) )
colnames(y.mean) <- c("MasVnrType","yMean")

ggplot(y.mean, aes(x = MasVnrType, y = yMean)) +
  geom_col()   # y-axis：salePrice mean




















