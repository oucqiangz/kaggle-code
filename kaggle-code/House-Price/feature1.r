# build more feature.
# @author : oucqiangz (qiangz2012@yeah.net)

# load cleaned source train data.
cln_data <- read.csv( file = "D:\\GitHub\\kaggle-code\\House-Price\\train_clean.csv" )

# category feature analysis.
library(dplyr)
#tbl_data <- tbl_df(cln_data)
#grouped <- group_by(tbl_data,MSSubClass)
#y.mean <- as.data.frame( summarise(grouped, mean(salePrices)) )
#print(y.mean)
library(ggplot2)
# category : MSSubClass
ggplot(cln_data, aes(x = MSSubClass, y = salePrices)) +
  # plot
  geom_point() +
  scale_x_continuous(breaks=seq(0, 200, 10))   ## 修改坐标轴的刻度间隔：X 轴每隔 10 个单位显示一个刻度

# category : MSZoning
ggplot(cln_data, aes(x = MSZoning, y = salePrices)) +
  # 条形图函数：stat表明取用样本点对应纵轴值
#  geom_bar(stat = "identity")
  geom_col()














