# Clean data of train set in the House-Price competition.
# @author: oucqiangz (qiangz2012@yeah.net).
#

# 1、load data and merge train & test set.
# train size : 1460
oriential_train <- read.csv( file = "G:\\Mypro\\kaggle\\House-Price\\train.csv" )
oriential_train <- subset( oriential_train, select = -Id )
salePrices <- oriential_train$SalePrice  #y
oriential_train <- subset( oriential_train, select = -SalePrice )
# test size : 1459
oriential_test <- read.csv( file = "G:\\Mypro\\kaggle\\House-Price\\test.csv" )
oriential_test <- subset( oriential_test, select = -Id )
# merge
oriential_train <- rbind(oriential_train, oriential_test)


##### 2、process NA values.

# (1) there are a lot of NA values in features "PoolQC" "Fence" "MiscFeature" "Alley" "MiscVal",
# so remove these features from train set.
oriential_train <- subset( oriential_train, select = -PoolQC )
oriential_train <- subset( oriential_train, select = -Fence )
oriential_train <- subset( oriential_train, select = -MiscFeature )
oriential_train <- subset( oriential_train, select = -Alley )
oriential_train <- subset( oriential_train, select = -MiscVal )

# (2) for other features NA, which will be replaced with a value:
# numberial features : fill "avg".
fillNaNumByMean <- function(myData,indexArray) {
  for(j in 1:length(indexArray)) {
      colIndex <- indexArray[j];
      tempVec <- myData[,colIndex];
      meanVal <- mean(tempVec,na.rm = TRUE);   # remove NA, then calculate mean.
      for(i in 1:length(tempVec)) {
          if ( is.na(myData[i,colIndex]) ) {
            myData[i,colIndex] <- meanVal
          }
      };
  }
  return (myData)
}
numbFeatIndex <- c(3,4,25,33,35,36,37,42,43,44,45,46,47,48,49,50,51,53,55,60,61,65,66,67,68,69,70)
oriential_train <- fillNaNumByMean(myData = oriential_train, indexArray = numbFeatIndex)

# category features : fill "mode(accured more than others)".
fillNaNumByMode <- function(myData,indexArray) {
    for(j in 1:length(indexArray)) {
        colIndex <- indexArray[j];
        tempVec <- myData[,colIndex];
        # get mode
        tempDF <- as.data.frame( table(tempVec) ); # frequency of each element.
        maxV <- max(tempDF[,2]);
        index <- 0;
        for(i in 1:length(tempDF[,2])) {
            if ( tempDF[i,2] == maxV ) {
                index <- i;
                break;
            }
        }
        # fill NA.
        for(k in 1:length(tempVec)) {
          if ( is.na(myData[k,colIndex]) ) {
              myData[k,colIndex] <- tempDF[index,1]
          }
        };
    }
    return (myData)
}
catFeatIndex <- c(1,2,5,6,7,8,9,10,11,12,13,14,15,16,17,20,21,22,23,24,26,27,28,29,30,31,32,34,38,39,
                  40,41,52,54,56,57,59,62,63,64,73,74)
oriential_train <- fillNaNumByMode(oriential_train,catFeatIndex)

# time features : fill "mode".
timeFeatIndex <- c(18,19,58,71,72)
oriential_train <- fillNaNumByMode(oriential_train,timeFeatIndex)


##### 3、process noise/outlier.

# (1) check outlier by feature value dot chart.
tarCol <- 3
tarT <- 800
tempData <- oriential_train[,tarCol]
dotchart(tempData)
for(aaa in 1:length(tempData)) {
    if (tempData[aaa] > tarT) {
        sss <- paste(aaa, tempData[aaa], sep = ":")
      #  print(sss); 
    }
}

oriential_train <- oriential_train[c(-1299,-2550,-935,-250,-314,-336,-707,-298,-1786,-2607,-2504),]
salePrices <- salePrices[c(-1299,-935,-250,-314,-336,-707,-298)]
segRow <- 1453   # train:[1,1453];  test:[1454,len]

# (2) check noise by feature value distribution hist.i
tempVec <- oriential_train[,1]
featValueDist <- as.data.frame( table(tempVec) );
#hist( x = tempVec, breaks = as.vector(featValueDist[,1]) )


##### 4、data transform.
# (1) numberial feature : nomalization.
minMaxNorm <- function(x) {
  x <- (x-min(x))/(max(x)-min(x))
  return (x)
}
for(j in 1:length(numbFeatIndex)) {
  colIndex <- numbFeatIndex[j];
  oriential_train[,colIndex] <- minMaxNorm(oriential_train[,colIndex])
}

# (2) y : log1p functional transform.
#hist(salePrices)
salePrices <- log1p(salePrices)
hist(salePrices)

# (3) category feature : One-Hot Encode

# library(CatEncoders)
# oneHotEnc <- function(df,indexArray) {
#     delCount <- 0;
#     for (i in 1:length(indexArray)) {
#         # one-hot encode
#         colIndex <- (indexArray[i] - delCount);
#         colVec <- df[,colIndex];
#         tempDF <- data.frame(colVec);
#         oenc <- OneHotEncoder.fit(tempDF);
#         tempMatrix <- transform(oenc,tempDF,sparse = FALSE); 
#         tempDF <- as.data.frame(tempMatrix);
#         # new col names
#         colName <- colnames(df)[colIndex]; 
#         dummySize <- length(tempDF[1,]);
#         newColNames <- 0;
#         for (j in 1:dummySize) {
#             newColNames[j] <- paste(colName, j, sep = "_")
#         };
#         colnames(tempDF) <- newColNames;
#         # remove old column feature
#         df <- df[,-colIndex]; 
#         delCount <- (delCount + 1);
#         # add new one-hot-encode df
#         df <- cbind(df,tempDF);
#     }  
#     return (df)
# }
# oriential_train <- oneHotEnc(oriential_train,catFeatIndex)

library(dummies)
# v1 <- c("China","Japan","USA","USA","UK")
# v2 <- c(4,2,5,7,3)
# v3 <- c("asia","asia","northAmerica","northAmerica","Euop")
# v4 <- c(10.1,8.6,5.3,4.4,9.9)
# v5 <- c("baidu","toshiba","google","google","ARM")
# df <- data.frame(v1,v2,v3,v4,v5)
# colnames(df) <- c("country","gdbRatio","zhou","growth","company")
# 
# dfCols <- c(1,3,5)
# catColNames <- colnames(df)[dfCols]
# 
# subDF <- df[1:3,]
# print(subDF)
# subDF <- dummy.data.frame(data = subDF, drop = FALSE, sep = "_", names = catColNames )
# print(subDF)

catColNames <- colnames(oriential_train)[catFeatIndex]
oriential_train <- dummy.data.frame(data = oriential_train, drop = FALSE, 
                                    sep = "_", names = catColNames )

# (4) time feature : continuous or discrete.
# we will process time feature in feature engineer.


##### divide train and test,then write to csv file.
final_train <- oriential_train[1:segRow,]
final_train <- cbind(final_train,salePrices)
write.csv(final_train, file = "G:\\Mypro\\kaggle\\House-Price\\trainSet.csv")
len <- length(oriential_train[,1])
final_test <- oriential_train[(segRow+1):len,]
write.csv(final_test, file = "G:\\Mypro\\kaggle\\House-Price\\testSet.csv")
print("DONE!")












