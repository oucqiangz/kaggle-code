# Clean data of train set in the House-Price competition.
# @author: turtome (qiangz2012@yeah.net).
#

# 1、load train set.
oriential_train <- read.csv( file = "G:\\Mypro\\kaggle\\House-Price\\train.csv" )
oriential_train <- subset( oriential_train, select = -Id )
salePrices <- oriential_train$SalePrice  #y
oriential_train <- subset( oriential_train, select = -SalePrice )

##### 2、process NA values.

# (1) there are a lot of NA values in features "PoolQC" "Fence" "MiscFeature" "Alley" "MiscVal",
# so remove these features from train set.
oriential_train <- subset( oriential_train, select = -PoolQC )
oriential_train <- subset( oriential_train, select = -Fence )
oriential_train <- subset( oriential_train, select = -MiscFeature )
oriential_train <- subset( oriential_train, select = -Alley )

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
numbFeatIndex <- c(3,4,25,33,35,36,37,42,43,44,45,46,47,48,49,50,51,53,55,60,61,65,66,67,68,69,70,71,72,73)
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
                  40,41,52,54,56,57,59,62,63,64,74,75)
oriential_train <- fillNaNumByMode(oriential_train,catFeatIndex)

# time features : fill "mode".
timeFeatIndex <- c(18,19,58)
oriential_train <- fillNaNumByMode(oriential_train,timeFeatIndex)

# at last, remove featue "MiscVal($Value of feature 'MiscFeature' feature)".
oriential_train <- subset( oriential_train, select = -MiscVal )


##### 3、process noise/outlier.

# (1) method 1 : simple data analysis (feature value distribution plot).
# dot chart.
#tempData <- oriential_train[,67]
#dotchart(tempData)

#thresh <- 500
#outlier <- 0
#j <- 1
#for (i in 1:length(tempData)) {
#    if ( tempData[i] > thresh ) {
#        outlier[j] <- i;
#        j <- (j+1);
#    }
#}
#print(outlier)
oriential_train <- oriential_train[c(-1299,-935,-54,-496,-584,-1329,-198),]













