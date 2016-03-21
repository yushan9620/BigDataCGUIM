#11
x10 <- 1:3
y10 <- 10:12
cbind(x10, y10)
rbind(x10, y10)

#12
x11 <- list(1, "a", TRUE, 1 + 4i) 
x11

#13 factor有排序的字串 levels:有哪幾個值
x12<-factor(c("yes", "yes", "no", "yes", "no")) 
x12 
x12a<-factor(c("yes", "yes", "no", "yes", "no"), levels =c("yes","no"))
x12a

#14 
x13 <- data.frame(foo = 1:4, bar = c(T, T, F, F)) 
x13 #column names: foo, bar 
nrow(x13)
ncol(x13)
names(x13)<-c("fooNew","barNew")
x13

#15 libraryc後不用雙引號
install.packages("SportsAnalytics")
library(SportsAnalytics)
NBA1415<-fetch_NBAPlayerStatistics("14-15")
names(NBA1415)
head(NBA1415)#看前6筆 tail看後六筆
nrow(NBA1415)
ncol(NBA1415)

#17
x16<-as.Date("1970-01-01") #新增日期
x16
unclass(x16)#從1970開始是第一天
unclass(as.Date("1971-01-01"))
weekdays(x16)
months(x16)
seq(Sys.Date(), by="1 months", length.out=12) #現在的時間每隔一個月共要有十二個
seq(as.Date("2016-01-31"), by="1 months", length.out=12)#二月沒有31天會往後加

#18
x17<-Sys.time()
x17
LisDate<-as.POSIXlt(x17)#list
IntDate<-as.POSIXct(x17)#integer
LisDate
IntDate
unclass(LisDate)#分開
unclass(IntDate)#一長字串

#19 日期不可以*
as.Date("2012/03/01")
as.Date("2012 MAR 01")
as.Date("2012 MAR 01", "%Y %b %d") #%Y:年 %b月 %d日告訴r你的日期格式
?strptime
x18 <- as.Date("2012-03-01")
y18 <-as.Date("2012-02-28") 
x18-y18

#21 subset去除不要的資料
letters#LETTERS變大寫
LETTERS
letters [1]#第一個字
letters [1:10]#1-10個字
letters[c(1,3,5)]
letters [-1:-10]#(-)不要1-10
head(letters,5)
tail(letters,5)

islands
sort(islands)
head(sort(islands))
tail(sort(islands))

#22
iris[1,2] #(Row 1, Column 2)
iris[,"Species"] #Column name=="Species" #每個ROW都選
iris$Species #Column name=="Species" #跟上面的一樣
subset(iris, Species=="virginica") #Species == "virginica" 選row
iris[iris$Species=="virginica",]#Species == "virginica"

#23
install.packages("SportsAnalytics")
library(SportsAnalytics)
NBA1415<-fetch_NBAPlayerStatistics("14-15")
San<-subset(NBA1415,Team=='SAN') #篩選的是行
San
San[order(San$GamesPlayed,decreasing = T),"Name"] #[row,col]排序是跟row有關
San[order(San$GamesPlayed,decreasing = T),c("Name","GamesPlayed")] #顯示兩個col
#24物件的詳細資料
str(iris) #150個row 5個col
str(NBA1415)

#26只有安專要打""
available.packages()
head(available.packages())
install.packages("ggplot2")
library(ggplot2) 

#28
strsplit("Hello World"," ")
toupper("Hello World")
tolower("Hello World")
library(stringr)
str_trim("Hello World   ")#移除前後空白 也可google其他function

#29
#字串連接
paste("Hello", "World", sep='')
paste0("Hello", "World")#接起來中間不插任何東西
paste0("Hello", "it's me", "I was wondering if after all these years","You'd like to meet, to go over everything")
paste(c("Hello", "World"), sep='')#向量不會幫你接
paste(c("Hello", "World"), sep='',collapse = '')#要加collapse參數
#字串切割
substr("Hello World", start=2,stop=4)
#字串取代
gsub("o","0","Hello World")

#30 
grep("Tim",NBA1415$Name)#回傳index，NBA1415的Name裡面有tim
NBA1415[grep("Tim",NBA1415$Name),]#index塞到row，每個col都要
grepl("Tim",NBA1415$Name)#回傳TorF
subset(NBA1415,grepl("Tim",Name))

#32
which(letters>"m")#因為m是第13個

#33
a<-3
if(a>10){
  b<-10
}else if(a>5){
  b<-5
}else{
  b<-a
}
b
ifelse(a>10,b<-10,b<-a)
ifelse(a>10,b<-10,ifelse(a>5,b<-5,b<-a))


#34
ifelse(NBA1415$GamesPlayed>30,"Hardwork","Lazy")# >30是Hardwork <30lazy
NBA1415$Personality<-ifelse(NBA1415$GamesPlayed>30,"Hardwork","Lazy")#dataframe$colname

#35
#for
for(index in 1:10){
  print(index)
}
#repeat
index<-1
repeat{
  if(index>10){
    break
  }
  print(index)
  index<-index+1
}
#36
#next 
for(index in 1:10){
  if(index!=4){
    print(index)
  }
}
for(index in 1:10){
  if(index==4){
    next
  }
  print(index)
}

#37
for(i in 1:nrow(NBA1415)){
  print(NBA1415[i,"Name"])
}
for(i in 1:nrow(NBA1415)){ #從第一行到最後一行
  if(!grepl('a|A',NBA1415[i,"Name"])){
    print(NBA1415[i,"Name"])
  }
}
#38 practice
for(i in 1:nrow(NBA1415)){
  if(NBA1415[i,"GamesPlayed"]>70&
     NBA1415[i,"TotalPoints"]>1500)
  {
    print(NBA1415[i,c("Name","Team","Position")])
   }
}


#39
subset(NBA1415,GamesPlayed>70&TotalPoints>1500)[,c("Name","Team","Position")]
NBA1415[NBA1415$GamesPlayed>70&NBA1415$TotalPoints>1500,c("Name","Team","Position")]


#41
apply(NBA1415[,c("GamesPlayed","TotalMinutesPlayed","TotalPoints")],2,mean)


#43
sapply(iris, mean)
sapply(NBA1415[,c("GamesPlayed","TotalMinutesPlayed","TotalPoints")],mean)
#44
lapply(iris, mean)
lapply(NBA1415[,c("GamesPlayed","TotalMinutesPlayed","TotalPoints")],mean)
#45
tapply(NBA1415$Name,NBA1415$Team,length)
tapply(NBA1415$TotalPoints,NBA1415$Team,max)
tapply(NBA1415$TotalPoints,NBA1415$Team,mean)
tapply(NBA1415$TotalPoints,NBA1415$Team,range)

#46
split(1:30,gl(3, 10))
lapply(split(1:30,gl(3, 10)),mean)
tapply(1:30,gl(3, 10),mean)

#47
NBA1415Team<-split(NBA1415[,c("TotalPoints","GamesPlayed")],NBA1415$Team)
lapply(NBA1415Team, colMeans)
sapply(NBA1415Team, colMeans)

#48
NBA1415TP<-split(NBA1415[,c("TotalPoints","GamesPlayed")],list(NBA1415$Team,NBA1415$Position))
lapply(NBA1415TP, colMeans)
sapply(NBA1415TP, colMeans)

#50
aggregate(NBA1415$TotalPoints, by=list(NBA1415$Team,NBA1415$Position), FUN=mean, na.rm=TRUE)
aggregate(TotalPoints ~ Team+Position, data = NBA1415, mean)

#51
x<-c(1,2,3,4,5)
mean(x)
x<-c(x,NA)
mean(x)
mean(x, na.rm=T)
sum(x)
sum(x, na.rm=T)


#52
system.time({
  n <- 1000
  r <- numeric(n)
  for(i in 1:n) {
    x <- rnorm(n)
    r[i] <- mean(x)
  }
})

#53
x <- list(foo = 1:4, bar = 0.6, baz = "hello")
x[1]
x[[1]]
x[2]
x[[2]]
x$foo

#55=4
x <- c(1, 2, NA, 4, NA, 5)
x[! is.na(x)]
x[! complete.cases(x)]




##test 2



