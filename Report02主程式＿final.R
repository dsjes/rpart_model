rm(list = ls())

#1.讀取資料------

x<-read.csv("/Users/jesshsieh/Desktop/庭's stuff/R/dataReport02.csv" , header = TRUE ,check.names = F, sep = "," , stringsAsFactors = FALSE,fileEncoding = "BIG-5")
dim(x)
summary(x)
head(x)
View(x)
#由於第11、10欄位太多遺失值，因此直接刪除
x <- x[,-11]
x <- x[,-10]

#2.處理遺失值------
#如果有遺失值，報告各變數的遺失值數量

for (i in 1:ncol(x)){
  index<-which(x[,i]=="$");x[,i][index]<-NA
  index<-which(x[,i]=="!");x[,i][index]<-NA
  index<-which(x[,i]=="");x[,i][index]<-NA
  index<-which(x[,i]=="*");x[,i][index]<-NA
  print(sum(is.na(x[,i])))
}

#如果有遺失值，請處理遺失值
#價格、車重、引擎功率、馬力: 用平均數替代
for (i in c(2 , 6 , 7 , 8)){
  x[,i]<-as.numeric(x[,i])
  Index<-which(is.na(x[,i]) == TRUE)
  print(Index)

  data.na.Npn<-x[-Index , i]#非遺失值
  M<-mean(data.na.Npn);M
  x[Index , i]<-M
}
View(x)

#其他變數以隨機樣本替代
for(i in c(3 , 4 , 5 , 9)){
  Index<-which(is.na(x[,i]) == TRUE)
  print(Index)

  data.na.Npn<-x[-Index , i]#非遺失值
  x[Index , i]<-sample(data.na.Npn , 
                       size = length(Index) , replace = TRUE)
}
View(x)

#報告遺失值處理結果
class(x)
dim(x)
summary(x)
head(x)
View(x)


#3.設定訓練資料與測試資料----
n <- nrow(x)
test.index<-sample(1:n,n*0.2) # 抽取20%測試樣本
traindata<-x[-test.index,] #訓練資料 
testdata<-x[test.index,]   #測試資料

#4,進行分類----
# 下載packages----
#install.packages("rpart")
#install.packages("rpart.plot")

# 載入packages----
library(rpart)
library(rpart.plot)

# 模型
tree<-rpart(分組油耗~價格+產地+可用性+類型+車重+引擎功率+馬力, 
                method="class", data=traindata)
tree
summary(tree)

# 修剪樹: minsplit----
#5.設定minsplitnum範圍
minsplitnum<-1:100

# (2) 利用迴圈找預測正確率
CR_train<-NULL
CR_test<-NULL

for (j in 1:length(minsplitnum)){
  # 給定不同的minsplitnum
  tree<-rpart(分組油耗~價格+產地+可用性+類型+車重+引擎功率+馬力, method="class", data=traindata, minsplit=minsplitnum[j])
  
  # 訓練資料預測正確率
  Y.traindata<-traindata$分組油耗
  Y.prediction<-predict(tree,traindata,type="class")
  prediction_matrix<-table(Y.traindata,Y.prediction)
  CR_train[j]<-sum(diag(prediction_matrix))/sum(prediction_matrix)
  
  # 測試資料預測正確率
  Y.testdata<-testdata$分組油耗 
  Y.prediction<-predict(tree,testdata,type="class")
  prediction_matrix<-table (Y.testdata,Y.prediction)
  CR_test[j]<-sum(diag(prediction_matrix))/sum(prediction_matrix)
}

# (3) 畫圖了解正確率
par(mfrow=c(2,1))
plot(minsplitnum,CR_train,main="CR_train VS. minsplitnum", xlab="minsplitnum", ylab="CR_train")
plot(minsplitnum,CR_test,main="CR_test VS. minsplitnum", xlab="minsplitnum", ylab="CR_test")
# (4)正確率最大值之minsplit
#印出訓練樣本最大的正確率
minsplitnum[which(CR_train==max(CR_train))]
#印出測試樣本最大的正確率
minsplitnum[which(CR_test==max(CR_test))]


#6.修剪樹: cp----

# (1) 設定cp範圍
cpvalue<-seq(from=0, to=0.1, by=0.01)

# (2) 利用迴圈找預測正確率
CR_train<-NULL
CR_test<-NULL
for (j in 1:length(cpvalue)){
  
  # 給定不同的cp
  tree<-rpart(分組油耗~價格+產地+可用性+類型+車重+引擎功率+馬力, method="class", data=traindata, cp=cpvalue[j])
  
  # 訓練資料預測正確率
  Y.traindata<-traindata$分組油耗 # 
  Y.prediction<-predict(tree,traindata,type="class")
  prediction_matrix<-table (Y.traindata,Y.prediction)
  CR_train[j]<-sum(diag(prediction_matrix))/sum(prediction_matrix)
  
  # 測試資料預測正確率
  Y.testdata<-testdata$分組油耗 # 
  Y.prediction<-predict(tree,testdata,type="class")
  prediction_matrix<-table (Y.testdata,Y.prediction)
  CR_test[j]<-sum(diag(prediction_matrix))/sum(prediction_matrix)
}

# (3) 畫圖了解正確率
par(mfrow=c(2,1))
plot(cpvalue,CR_train,main="CR_train VS. cp", xlab="cp", ylab="CR_train")
plot(cpvalue,CR_test,main="CR_test VS. cp", xlab="cp", ylab="CR_test")


# (4)正確率最大值之cp
#印出訓練樣本最大的正確率
cpvalue[which(CR_train==max(CR_train))]
#印出測試樣本最大的正確率
cpvalue[which(CR_test==max(CR_test))]

# 訓練資料預測正確率----
Y.traindata<-traindata$分組油耗 #預測變數
Y.prediction<-predict(tree,traindata,type="class") #預測函數
prediction_matrix<-table(Y.traindata,Y.prediction);prediction_matrix
# 計算正確率
CR_train<-sum(diag(prediction_matrix))/sum(prediction_matrix);CR_train
# 測試資料預測正確率----
Y.testdata<-testdata$分組油耗
Y.prediction<-predict(tree,testdata,type="class")
prediction_matrix<-table (Y.testdata,Y.prediction); prediction_matrix
# 計算正確率
CR_test<-sum(diag(prediction_matrix))/sum(prediction_matrix);CR_test

# 設定maxdepth範圍
maxdepthvalue<-1:30
# 利用迴圈找預測正確率
CR_train<-NULL
CR_test<-NULL
# 給定不同的 maxdepth
for (j in 1:length(maxdepthvalue)){
  tree<-rpart(分組油耗~價格+產地+可用性+類型+車重+引擎功率+馬力,
                  method="class", data=traindata, 
                  maxdepth=maxdepthvalue[j])
  # 訓練資料預測正確率
  Y.traindata<-traindata$分組油耗 
  Y.prediction<-predict(tree,traindata,type="class")
  prediction_matrix<-table (Y.traindata,Y.prediction)
  CR_train[j]<-sum(diag(prediction_matrix))/sum(prediction_matrix)
  
  # 測試資料預測正確率
  Y.testdata<-testdata$分組油耗 
  Y.prediction<-predict(tree,testdata,type="class")
  prediction_matrix<-table (Y.testdata,Y.prediction)
  CR_test[j]<-sum(diag(prediction_matrix))/sum(prediction_matrix)
}

# 畫圖了解正確率 
par(mfrow=c(2,1))
plot(maxdepthvalue,CR_train,main="CR_train VS. cp",
     xlab="maxdepth", ylab="CR_train")
plot(maxdepthvalue,CR_test,main="CR_test VS. cp",
     xlab="maxdepth", ylab="CR_test")

# (4)正確率最大值之 maxdepth
maxdepthvalue[which(CR_train==max(CR_train))]
maxdepthvalue[which(CR_test==max(CR_test))]