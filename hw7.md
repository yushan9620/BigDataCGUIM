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

    ##                 Estimate  Std. Error   z value     Pr(>|z|)
    ## (Intercept)  7.247258274 0.768993523  9.424342 4.328090e-21
    ## pregnant    -0.118615837 0.031833476 -3.726135 1.944381e-04
    ## glucose     -0.033461697 0.004035858 -8.291099 1.122071e-16
    ## pressure     0.009502996 0.006129619  1.550340 1.210598e-01
    ## mass        -0.071558966 0.015847874 -4.515367 6.320719e-06
    ## pedigree    -0.610194348 0.344551671 -1.770981 7.656392e-02

### 模型說明

由上述參數可知，使用diabetes的資料，以邏輯迴歸建立模型預測是否為糖尿病患者，經最佳化後，模型使用參數為pregnant, glucose, pressure, mass, pedigree，共6個參數，各參數代表每一個可能影響的因素


預測模型驗證
------------

``` r
MinePred<-predict(finalFit,newdata = PimaIndiansDiabetesC[PimaIndiansDiabetesC$Test==T,])
MineAns<-ifelse(MinePred<0.6,"pos","neg") 
MineAns<-factor(MineAns,levels = c("pos","neg"))
library(caret)#install.packages("caret")
sensitivity(MineAns,PimaIndiansDiabetesC[PimaIndiansDiabetesC$Test==T,]$diabetes)
```

    ## [1] 0.9728972

``` r
specificity(MineAns,PimaIndiansDiabetesC[PimaIndiansDiabetesC$Test==T,]$diabetes)
```

    ## [1] 0.7718121

``` r
posPredValue(MineAns,PimaIndiansDiabetesC[PimaIndiansDiabetesC$Test==T,]$diabetes)
```

    ## [1] 0.8792453

``` r
negPredValue(MineAns,PimaIndiansDiabetesC[PimaIndiansDiabetesC$Test==T,]$diabetes)
```

    ## [1] 0.9666667

從不同女性得到的相關資料，以邏輯回歸模型預測是否為糖尿病患者，可得：

-   敏感度 67.2897196%
-   特異性 77.1812081%
-   陽性預測率 67.9245283%
-   陰性預測率 76.6666667%
