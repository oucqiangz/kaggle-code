# analysis and remove some useless category features.
# @author : oucqiangz (qiangz2012@yeah.net)

# load cleaned source train data.
cln_data <- read.csv( file = "D:\\GitHub\\kaggle-code\\House-Price\\data\\train_clean.csv" )

##### category feature analysis.
library(ggplot2)
## category : MSSubClass
ggplot(cln_data, aes(x = MSSubClass, y = salePrices)) +
  # plot
  geom_point() +
  scale_x_continuous(breaks=seq(0, 200, 10))   ## 修改坐标轴的刻度间隔：X 轴每隔 10 个单位显示一个刻度

## category : MSZoning,Street,LotShape,LandContour,Utilities,LotConfig,LandSlope,Neighborhood
##            Condition1,Condition2,BldgType,HouseStyle,......
library(dplyr)

# count.
ggplot(cln_data, aes(x = GarageType)) +
  geom_bar()   # y-axis：case count

# mean.
tbl_data <- tbl_df(cln_data)
grouped <- group_by(tbl_data,GarageType)
y.mean <- as.data.frame( summarise(grouped, mean(salePrices)) )
colnames(y.mean) <- c("GarageType","yMean")

ggplot(y.mean, aes(x = GarageType, y = yMean)) +
  geom_col()   # y-axis：salePrice mean

# remove some useless category features.
train_1_path <- "D:\\GitHub\\kaggle-code\\House-Price\\data\\train_1.csv"
train_set <- read.csv( file = train_1_path )
train_set <- train_set[,-1]
featureList <- c(1,2,5,6,12,14,17,18,20,21,22,23,25,27,30,31,34,37,38,42,43,47,48,
                 50,51,54,55,57,59,64,65,68,69,71,72,73,74,75,77,81,82,86,89,90,92,
                 94,95,96,99,100,101,102,103,105,106,107,108,111,112,113,115,116,117,
                 118,119,122,128,129,133,136,137,139,140,141,144,146,147,153,154,156,
                 158,162,165,168,169,170,173,174,175,177,180,181,183,184,185,188,191,
                 194,196,199,200,202,203,204,205,210,212,214,215,216,219,223,224,225,
                 230,231,232,233,234,235,236,237,238,239,240,241,244,245,247,250,251,
                 252,253,254,257,259,261,262,265,268,269,270,271,272,273,283,284,285,
                 286,287,288,289,290,291,293,294,297,298,299,300,301,304,
                 307) # 307:y
train_2_set <- train_set[,featureList]  # useless category features removed.
write.csv(train_2_set, file = "D:\\GitHub\\kaggle-code\\House-Price\\data\\train_2.csv")























