糖尿病 預測模型
================

資料前處理
----------

### 資料讀取

此資料來源為Pima Indians Diabetes Database。

此資料記載一些可能的因素，協助預測女性是否為糖尿病患者，一共有9個參數。另外，分類結果為二元分類，包括非糖尿病患者(neg)與糖尿病患者(pos) 。Inputs: Numeric/Output: Categorical, 2 class labels

``` r
library(mlbench)#install.packages("mlbench")
```

    ## Warning: package 'mlbench' was built under R version 3.2.5

``` r
data(PimaIndiansDiabetes)
str(PimaIndiansDiabetes) 
```

    ## 'data.frame':    768 obs. of  9 variables:
    ##  $ pregnant: num  6 1 8 1 0 5 3 10 2 8 ...
    ##  $ glucose : num  148 85 183 89 137 116 78 115 197 125 ...
    ##  $ pressure: num  72 66 64 66 40 74 50 0 70 96 ...
    ##  $ triceps : num  35 29 0 23 35 0 32 0 45 0 ...
    ##  $ insulin : num  0 0 0 94 168 0 88 0 543 0 ...
    ##  $ mass    : num  33.6 26.6 23.3 28.1 43.1 25.6 31 35.3 30.5 0 ...
    ##  $ pedigree: num  0.627 0.351 0.672 0.167 2.288 ...
    ##  $ age     : num  50 31 32 21 33 30 26 29 53 54 ...
    ##  $ diabetes: Factor w/ 2 levels "neg","pos": 2 1 2 1 2 1 2 1 2 2 ...

### 留下無缺值的資料

``` r
PimaIndiansDiabetesC<-
    PimaIndiansDiabetes[complete.cases(PimaIndiansDiabetes),]
c(nrow(PimaIndiansDiabetes),nrow(PimaIndiansDiabetesC))
```

    ## [1] 768 768

### 將資料隨機分為訓練組與測試組

隨機將2/3的資料分到訓練組（Test==F），剩下1/3為測試組（Test==T）

``` r
PimaIndiansDiabetesC$Test<-F
PimaIndiansDiabetesC[
  sample(1:nrow(PimaIndiansDiabetesC),nrow(PimaIndiansDiabetesC)/3),]$Test<-T
PimaIndiansDiabetesC$diabetes<-factor(PimaIndiansDiabetesC$diabetes,levels=c("pos","neg"))
c(sum(PimaIndiansDiabetesC$Test==F),sum(PimaIndiansDiabetesC$Test==T))
```

    ## [1] 512 256

可得訓練組案例數為512， 測試組案例數為256

預測模型建立
------------

### 模型建立

由於變數多，且多為連續變項，而輸出為二元類別變項，所以選擇邏輯迴歸演算法建立模型，並使用雙向逐步選擇最佳參數組合。

``` r
fit<-glm(diabetes~., PimaIndiansDiabetesC[PimaIndiansDiabetesC$Test==F,],family="binomial")
library(MASS)
finalFit<-stepAIC(fit,direction = "both",trace = F)
summary(finalFit)$coefficients
```

    ##                Estimate  Std. Error   z value     Pr(>|z|)
    ## (Intercept)  8.43757847 0.864147506  9.764049 1.606120e-22
    ## pregnant    -0.16281050 0.034579089 -4.708352 2.497282e-06
    ## glucose     -0.03851464 0.004512738 -8.534650 1.405800e-17
    ## pressure     0.01734055 0.005960710  2.909142 3.624220e-03
    ## mass        -0.09254997 0.018238621 -5.074395 3.887300e-07
    ## pedigree    -0.96746858 0.373478774 -2.590425 9.585764e-03

### 模型說明

由上述參數可知，使用diabetes的資料，以邏輯迴歸建立模型預測是否為糖尿病患者，經最佳化後，模型使用參數為pregnant, glucose, pressure, mass, pedigree，共5個參數，各參數代表每一個可能影響的因素

預測模型驗證
------------

``` r
MinePred<-predict(finalFit,newdata = PimaIndiansDiabetesC[PimaIndiansDiabetesC$Test==T,])
MineAns<-ifelse(MinePred<0.5,"pos","neg") 
MineAns<-factor(MineAns,levels = c("pos","neg"))
library(caret)#install.packages("caret")
sensitivity(MineAns,PimaIndiansDiabetesC[PimaIndiansDiabetesC$Test==T,]$diabetes)
```

    ## [1] 0.7159091

``` r
specificity(MineAns,PimaIndiansDiabetesC[PimaIndiansDiabetesC$Test==T,]$diabetes)
```

    ## [1] 0.8095238

``` r
posPredValue(MineAns,PimaIndiansDiabetesC[PimaIndiansDiabetesC$Test==T,]$diabetes)
```

    ## [1] 0.6631579

``` r
negPredValue(MineAns,PimaIndiansDiabetesC[PimaIndiansDiabetesC$Test==T,]$diabetes)
```

    ## [1] 0.8447205

從不同女性得到的相關資料，以邏輯回歸模型預測是否為糖尿病患者，可得：

-   敏感度 71.5909091%
-   特異性 80.952381%
-   陽性預測率 66.3157895%
-   陰性預測率 84.4720497%
