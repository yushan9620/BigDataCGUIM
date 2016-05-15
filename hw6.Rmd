---
title: "1928-1969間，小兒麻痺在美國各州的發生率變化"
output: github_document
---

## 資料前處理

把資料讀進來
```{r}
polio<-read.csv("POLIO_Incidence.csv",stringsAsFactors=F)
head(polio)

```

將寬表格轉為長表格
```{r}
library(reshape2)
polio.m<-melt(polio,id.vars=c('YEAR','WEEK'))
head(polio.m)
```

處理缺值
```{r}
polio.m[polio.m$value=="-",]$value<-NA#處理缺值,將"-"轉為NA
polio.m$value<-as.numeric(polio.m$value)#將value欄位轉為數字
```

計算年度發生率
```{r}
polio.sumYear<-#各州各年度加總,計算該年度的總發生率
    aggregate(value~YEAR+variable,data=polio.m,FUN=sum,na.rm=F)
head(polio.sumYear)
```


## 視覺畫呈現
解釋如何選擇圖形種類

程式碼
圖形呈現
```{r}
library(ggplot2)#forggplot()
ggplot(polio.sumYear,aes(YEAR,variable))+#aes(x,y)
geom_tile(aes(fill=value),colour="white")+#geom_tile:區塊著色
scale_fill_gradient(low="white",high="red")#數值低:白色
```

解釋圖形