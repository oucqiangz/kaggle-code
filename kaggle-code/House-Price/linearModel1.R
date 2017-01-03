## fit linear model
## @author : oucqiangz (qiangz2012@yeah.net)


# load train data.
train_set <- read.csv( file = "G:\\Mypro\\GitHub\\kaggle-code\\House-Price\\train_1.csv" )
train_set <- train_set[,-1]

# fit linear regression model.
cols <- colnames(train_set)
cols <- cols[1:length(cols)-1]
fmla <- as.formula(paste("salePrices ~ ", paste(cols, collapse= "+")))
linearReg <- lm( formula = fmla, data = train_set )
print( summary(linearReg) )
plot(linearReg)





