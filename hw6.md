1928-1969間，小兒麻痺在美國各州的發生率變化
================

資料前處理
----------

把資料讀進來

``` r
polio<-read.csv("POLIO_Incidence.csv",stringsAsFactors=F)
head(polio)
```

    ##   YEAR WEEK ALABAMA ALASKA ARIZONA ARKANSAS CALIFORNIA COLORADO
    ## 1 1928    1       0      -       0        0       0.17     0.39
    ## 2 1928    2       0      -       0        0       0.15      0.2
    ## 3 1928    3    0.04      -       0        0       0.11        0
    ## 4 1928    4       0      -    0.24     0.11       0.07      0.2
    ## 5 1928    5       0      -    0.24        0       0.32        0
    ## 6 1928    6       0      -       0        0       0.22      0.1
    ##   CONNECTICUT DELAWARE DISTRICT.OF.COLUMBIA FLORIDA GEORGIA HAWAII IDAHO
    ## 1           0        0                    -       0    0.03      -     0
    ## 2           0        0                    -       0       0      -     0
    ## 3        0.06        0                    -       0       -      -     0
    ## 4        0.06        0                    0       0       0      -     0
    ## 5        0.13        0                    0       0       0      -     0
    ## 6           0        0                    0       0       0      -     -
    ##   ILLINOIS INDIANA IOWA KANSAS KENTUCKY LOUISIANA MAINE MARYLAND
    ## 1     0.03    0.03 0.08      0        0         0     0     0.06
    ## 2     0.01    0.03    -   0.22        0      0.05  0.13     0.06
    ## 3     0.03    0.03    -      0        0         0     0        0
    ## 4     0.05    0.12    0      0        0         0     0        0
    ## 5     0.04       0 0.04      0        0         0  0.38     0.12
    ## 6     0.03       0    0      0        0         0     0        0
    ##   MASSACHUSETTS MICHIGAN MINNESOTA MISSISSIPPI MISSOURI MONTANA NEBRASKA
    ## 1          0.14     0.04         0           0     0.03    0.18     0.07
    ## 2          0.14     0.04      0.04           0     0.06       0     0.07
    ## 3          0.07     0.02         0           0     0.03    0.18        0
    ## 4          0.02     0.02         0           0     0.06       0        0
    ## 5          0.02     0.04         0           0        0       0     0.15
    ## 6          0.05     0.06         0           0        0       0     0.07
    ##   NEVADA NEW.HAMPSHIRE NEW.JERSEY NEW.MEXICO NEW.YORK NORTH.CAROLINA
    ## 1      -             -       0.08          0     0.08              0
    ## 2      -             -       0.03          0     0.05           0.03
    ## 3      -             -          0          0     0.03              0
    ## 4      -             0       0.03          0     0.06              0
    ## 5      -             0       0.03       0.48     0.07              0
    ## 6      -             0          0          0     0.03              0
    ##   NORTH.DAKOTA OHIO OKLAHOMA OREGON PENNSYLVANIA RHODE.ISLAND
    ## 1            - 0.02        0   0.64            0            0
    ## 2         0.45    -     0.04   0.43         0.03            0
    ## 3            0 0.06        0   1.07         0.02            0
    ## 4         0.15    0     0.09   0.53         0.02            0
    ## 5            0 0.03        0   0.32            0            0
    ## 6            0 0.05     0.04   0.21         0.04            0
    ##   SOUTH.CAROLINA SOUTH.DAKOTA TENNESSEE TEXAS UTAH VERMONT VIRGINIA
    ## 1           0.06            0      0.04  0.05    0       0        -
    ## 2           0.06            0      0.04  0.04    0       0        -
    ## 3           0.35            0         0     0    0       0        -
    ## 4           0.23            0      0.04  0.05    0       0        -
    ## 5           0.17         0.15         0  0.05    0       0        -
    ## 6           0.06         0.29      0.04     0  0.2       0     0.04
    ##   WASHINGTON WEST.VIRGINIA WISCONSIN WYOMING
    ## 1       0.26          0.06      0.03       0
    ## 2       0.39          0.24      0.03       0
    ## 3       0.13          0.12      0.03       0
    ## 4       0.06          0.12         0       0
    ## 5       0.13          0.06      0.03       0
    ## 6       0.06             0      0.14       0

將寬表格轉為長表格

``` r
library(reshape2)
polio.m<-melt(polio,id.vars=c('YEAR','WEEK'))
head(polio.m)
```

    ##   YEAR WEEK variable value
    ## 1 1928    1  ALABAMA     0
    ## 2 1928    2  ALABAMA     0
    ## 3 1928    3  ALABAMA  0.04
    ## 4 1928    4  ALABAMA     0
    ## 5 1928    5  ALABAMA     0
    ## 6 1928    6  ALABAMA     0

處理缺值

``` r
polio.m[polio.m$value=="-",]$value<-NA#處理缺值,將"-"轉為NA
polio.m$value<-as.numeric(polio.m$value)#將value欄位轉為數字
```

計算年度發生率

``` r
polio.sumYear<-#各州各年度加總,計算該年度的總發生率
    aggregate(value~YEAR+variable,data=polio.m,FUN=sum,na.rm=F)
head(polio.sumYear)
```

    ##   YEAR variable value
    ## 1 1928  ALABAMA  2.39
    ## 2 1929  ALABAMA  2.25
    ## 3 1930  ALABAMA  2.57
    ## 4 1931  ALABAMA  2.07
    ## 5 1932  ALABAMA  1.38
    ## 6 1933  ALABAMA  1.12

視覺畫呈現
----------

解釋如何選擇圖形種類

我使用Heatmap圖形來畫圖，因為種共有三個參數YEAR(年)、variable(州)，以及value(小兒麻痺發生率)，x軸放YEAR，y軸放variable，可清楚看出每年每州小兒麻痺的發生率變化。而若使用點圖，因為資料值域太廣且州太多，就會壓縮圖片，無法清楚看出變化；若使用折線圖，因為折線太多，不易看懂圖形，(最下面有錯誤示範)。

程式碼

``` r
library(ggplot2)#forggplot()
ggplot(polio.sumYear,aes(YEAR,variable))+#aes(x,y)
geom_tile(aes(fill=value),colour="white")+#geom_tile:區塊著色
scale_fill_gradient(low="white",high="red")#數值低:白色
```

![](hw6_files/figure-markdown_github/unnamed-chunk-5-1.png)<!-- -->

圖形呈現 ![alt text](https://github.com/yushan9620/images/blob/master/6-1.png)

解釋圖形

以上圖表呈現1928-1969間，小兒麻痺在美國各州的發生率變化。 從1928年開始其實小兒麻痺不嚴重，直到約1945年後，小兒麻痺開始爆發。而自1955年開始有小兒麻痺疫苗,從賓州小學開始，因此賓州的發生率一職比其他地區低,其他地區也因為開始全面施打，疫苗得到效果，使小兒麻痺發生率下降，在1957年後發生率甚至趨近於0。

-------以下是錯誤示範--------------

``` r
ggplot(polio.sumYear)+
geom_line(aes(x=YEAR,y=value,color=variable))+
geom_vline(xintercept=1955,colour="black",linetype="longdash")
```

![](hw6_files/figure-markdown_github/unnamed-chunk-6-1.png)<!-- --> ![alt text](https://github.com/yushan9620/images/blob/master/6-2.png)
