if (!require('Rfacebook')){
    install.packages("Rfacebook")
    library(Rfacebook)
}

token <- "CAAL8BwBDYJQBANZBKTknZCCv3XTHXwgTuLeg3facfmLg7lh1tdUmZBRWdZB3ZAUsEXfZA0QZBIw0kHqwfyMxTxyczC3Us49BG4sZB2Id566s0uCa9DSZCGhdjXvAniyOcoUWfHxozL4Wm0Pg1Ye2Eq0ut8UspfkRrtFp1Y3SkCPS7xBRJxuDglth3yv3cBBSCu0TE4HY1ds1qhDGnCGefTGkP26rnPiXWIbgZD"
totalPage<-NULL ##重要
lastDate<-Sys.Date()
DateVector<-seq(as.Date("2016-01-01"),lastDate,by="5 days")
DateVectorStr<-as.character(DateVector)
for(i in 1:(length(DateVectorStr)-1)){
    tempPage<-getPage("llchu", token,since = DateVectorStr[i],
                      until = DateVectorStr[i+1])
    totalPage<-rbind(totalPage,tempPage)
}
nrow(totalPage)


totalPage$datetime <- as.POSIXct(totalPage$created_time, 
                                 format = "%Y-%m-%dT%H:%M:%S+0000", 
                                 tz = "GMT") 
totalPage$dateTPE <- format(totalPage$datetime, "%Y-%m-%d", 
                            tz = "Asia/Taipei") 
totalPage$weekdays <-weekdays(as.Date(totalPage$dateTPE))


## 每日發文數分析
PostCount<-aggregate(id~weekdays+dateTPE,totalPage,length)
library(knitr)
kable(head(PostCount[order(PostCount$id,decreasing = T),]))

## 每日likes數分析
LikesCount<-aggregate(likes_count~dateTPE,totalPage,mean)
library(knitr)
kable(LikesCount, digits=2)
kable(head(LikesCount[order(LikesCount$likes_count,decreasing = T),]))

## 每日comments數分析
CommentsCount<-aggregate(comments_count~dateTPE,totalPage,mean)
library(knitr)
kable(CommentsCount, digits=2)
kable(head(CommentsCount[order(CommentsCount$comments_count,decreasing = T),]))

## 每日shares數分析
SharesCount<-aggregate(shares_count~dateTPE,totalPage,mean)
library(knitr)
kable(SharesCount, digits=2)
kable(head(SharesCount[order(SharesCount$shares_count,decreasing = T),]))

#
totalComment<-NULL
for(i in 1:10)
    post<-getPost(totalPage$id[i],token,
                  n.comments = totalPage$comments_count)
tempComment<-cbind(post$post$id,post$comments$from_name)
totalComment<-rbind(totalComment,tempComment)

totalComment<-data.frame(totalComment)
colnames(totalComment)<-c("postID","commentName")
NameCount<-aggregate(postID~commentName,totalComment,FUN=length)
head(NameCount[order(NameCount$postID,decreasing = T),],10)
