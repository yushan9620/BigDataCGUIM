NBA 2014-2015球季 各隊分析
================

把資料讀進來的程式碼
--------------------

``` r
#install.packages("SportsAnalytics")
library(SportsAnalytics)
NBA1415<-fetch_NBAPlayerStatistics("14-15")
```

各隊最辛苦的球員
----------------

計算依據為出戰分數最多的球員

``` r
MaxMinutes<-aggregate(TotalMinutesPlayed~Team,NBA1415,max)
NBA1415MaxMinutes<-merge(NBA1415,MaxMinutes)
output<-NBA1415MaxMinutes[order(NBA1415MaxMinutes$TotalMinutesPlayed,decreasing = T),c("Team","Name","TotalMinutesPlayed")]
library(knitr)
kable(output,digits=2)
```

|     | Team | Name             |  TotalMinutesPlayed|
|-----|:-----|:-----------------|-------------------:|
| 11  | HOU  | James Harden     |                2979|
| 18  | MIN  | Andrew Wiggins   |                2971|
| 25  | POR  | Damian Lillard   |                2928|
| 13  | LAC  | Chris Paul       |                2860|
| 30  | WAS  | John Wall        |                2841|
| 24  | PHO  | Eric Bledsoe     |                2799|
| 3   | BRO  | Joe Johnson      |                2787|
| 6   | CLE  | Kyrie Irving     |                2735|
| 7   | DAL  | Monta Ellis      |                2698|
| 19  | NOR  | Tyreke Evans     |                2695|
| 15  | MEM  | Marc Gasol       |                2690|
| 5   | CHI  | Pau Gasol        |                2682|
| 26  | SAC  | Ben Mclemore     |                2674|
| 8   | DEN  | Ty Lawson        |                2668|
| 16  | MIA  | Goran Dragic     |                2641|
| 29  | UTA  | Gordon Hayward   |                2618|
| 10  | GSW  | Stephen Curry    |                2613|
| 9   | DET  | Ke Caldwell-pope |                2591|
| 22  | ORL  | Victor Oladipo   |                2572|
| 17  | MIL  | G Antetokounmpo  |                2542|
| 2   | BOS  | Avery Bradley    |                2427|
| 28  | TOR  | Kyle Lowry       |                2422|
| 1   | ATL  | Kyle Korver      |                2418|
| 12  | IND  | Solomon Hill     |                2380|
| 4   | CHA  | Gerald Henderson |                2323|
| 23  | PHI  | Nerlens Noel     |                2311|
| 27  | SAN  | Danny Green      |                2311|
| 21  | OKL  | Russel Westbrook |                2302|
| 14  | LAL  | Wesley Johnson   |                2244|
| 20  | NYK  | Shane Larkin     |                1864|

各隊得分王
----------

計算依據為總得分最多的球員

``` r
MaxPoint<-aggregate(TotalPoints~Team,NBA1415,max)
NBA1415MaxPoint<-merge(NBA1415,MaxPoint)
output<-NBA1415MaxPoint[order(NBA1415MaxPoint$TotalPoints,decreasing = T),c("Team","Name","TotalPoints")]
library(knitr)
kable(output,digits = 2)
```

|     | Team | Name             |  TotalPoints|
|-----|:-----|:-----------------|------------:|
| 11  | HOU  | James Harden     |         2217|
| 10  | GSW  | Stephen Curry    |         1900|
| 21  | OKL  | Russel Westbrook |         1886|
| 6   | CLE  | Lebron James     |         1740|
| 25  | POR  | Damian Lillard   |         1720|
| 19  | NOR  | Anthony Davis    |         1656|
| 13  | LAC  | Chris Paul       |         1564|
| 7   | DAL  | Monta Ellis      |         1513|
| 29  | UTA  | Gordon Hayward   |         1463|
| 5   | CHI  | Pau Gasol        |         1446|
| 26  | SAC  | Rudy Gay         |         1432|
| 22  | ORL  | Nikola Vucevic   |         1428|
| 15  | MEM  | Marc Gasol       |         1413|
| 18  | MIN  | Andrew Wiggins   |         1387|
| 30  | WAS  | John Wall        |         1385|
| 24  | PHO  | Eric Bledsoe     |         1377|
| 16  | MIA  | Dwyane Wade      |         1331|
| 28  | TOR  | Kyle Lowry       |         1244|
| 3   | BRO  | Brook Lopez      |         1236|
| 1   | ATL  | Paul Millsap     |         1218|
| 8   | DEN  | Ty Lawson        |         1143|
| 9   | DET  | Andre Drummond   |         1130|
| 2   | BOS  | Isaiah Thomas    |         1101|
| 4   | CHA  | Al Jefferson     |         1080|
| 27  | SAN  | Tim Duncan       |         1070|
| 17  | MIL  | Khris Middleton  |         1055|
| 20  | NYK  | Carmelo Anthony  |          966|
| 12  | IND  | C.j. Miles       |          942|
| 23  | PHI  | Robert Covington |          927|
| 14  | LAL  | Jordan Hill      |          841|

各隊最有效率的球員
------------------

計算依據為(總得分/出戰分鐘數)最高的球員

``` r
NBA1415$Efficiency<-NBA1415$TotalPoints/NBA1415$TotalMinutesPlayed 
MaxEfficiency<-aggregate(Efficiency~Team,NBA1415,max)
NBA1415MaxEfficiency<-merge(NBA1415,MaxEfficiency)
output<-NBA1415MaxEfficiency[order(NBA1415MaxEfficiency$Efficiency,decreasing = T),c("Team","Name","Efficiency")]
library(knitr)
kable(output,digits = 2)
```

|     | Team | Name             |  Efficiency|
|-----|:-----|:-----------------|-----------:|
| 21  | OKL  | Russel Westbrook |        0.82|
| 11  | HOU  | James Harden     |        0.74|
| 10  | GSW  | Stephen Curry    |        0.73|
| 26  | SAC  | Demarcus Cousins |        0.71|
| 6   | CLE  | Lebron James     |        0.70|
| 20  | NYK  | Carmelo Anthony  |        0.68|
| 16  | MIA  | Dwyane Wade      |        0.67|
| 19  | NOR  | Anthony Davis    |        0.67|
| 15  | MEM  | Tyrus Thomas     |        0.67|
| 25  | POR  | Lamarcu Aldridge |        0.66|
| 14  | LAL  | Kobe Bryant      |        0.65|
| 2   | BOS  | Isaiah Thomas    |        0.64|
| 13  | LAC  | Blake Griffin    |        0.62|
| 28  | TOR  | Louis Williams   |        0.62|
| 24  | PHO  | Gerald Green     |        0.61|
| 18  | MIN  | Kevin Martin     |        0.60|
| 7   | DAL  | Charl Villanueva |        0.59|
| 5   | CHI  | Derrick Rose     |        0.59|
| 3   | BRO  | Brook Lopez      |        0.59|
| 12  | IND  | Paul George      |        0.58|
| 23  | PHI  | Tony Wroten      |        0.57|
| 22  | ORL  | Nikola Vucevic   |        0.56|
| 29  | UTA  | Gordon Hayward   |        0.56|
| 4   | CHA  | Jannero Pargo    |        0.55|
| 9   | DET  | Brandon Jennings |        0.54|
| 1   | ATL  | Jeff Teague      |        0.52|
| 27  | SAN  | Kawhi Leonard    |        0.52|
| 8   | DEN  | Danilo Gallinari |        0.52|
| 17  | MIL  | Ersan Ilyasova   |        0.51|
| 30  | WAS  | John Wall        |        0.49|

各隊三分球出手最準的球員
------------------------

計算依據為(三分球進球次數/三分球出手次數)最高的球員

``` r
NBA1415$ThreesRate<-NBA1415$ThreesMade/NBA1415$ThreesAttempted 
MaxThreesRate<-aggregate(ThreesRate~Team,NBA1415,max)
NBA1415MaxThreesRate<-merge(NBA1415,MaxThreesRate)
output<-NBA1415MaxThreesRate[order(NBA1415MaxThreesRate$ThreesRate,decreasing = T),c("Team","Name","ThreesRate")]
library(knitr)
kable(output,digits = 2)
```

|     | Team | Name             |  ThreesRate|
|-----|:-----|:-----------------|-----------:|
| 4   | CHA  | Cody Zeller      |        1.00|
| 18  | MIL  | John Henson      |        1.00|
| 30  | TOR  | Bruno Caboclo    |        0.67|
| 14  | LAL  | Dwight Buycks    |        0.64|
| 27  | POR  | Victor Claver    |        0.55|
| 20  | NOR  | Luke Babbitt     |        0.51|
| 8   | DEN  | Jamaal Franklin  |        0.50|
| 11  | HOU  | Dwight Howard    |        0.50|
| 13  | LAC  | Lester Hudson    |        0.50|
| 25  | PHO  | Earl Barron      |        0.50|
| 26  | PHO  | Jerel Mcneal     |        0.50|
| 28  | SAC  | David Stockton   |        0.50|
| 1   | ATL  | Kyle Korver      |        0.49|
| 9   | DET  | Tayshaun Prince  |        0.46|
| 5   | CHI  | Pau Gasol        |        0.46|
| 2   | BOS  | Luigi Datome     |        0.45|
| 10  | GSW  | Stephen Curry    |        0.44|
| 22  | OKL  | Anthony Morrow   |        0.43|
| 17  | MIA  | Shannon Brown    |        0.43|
| 29  | SAN  | Tony Parker      |        0.43|
| 7   | DAL  | Richar Jefferson |        0.43|
| 6   | CLE  | Kyrie Irving     |        0.42|
| 21  | NYK  | Jose Calderon    |        0.42|
| 32  | WAS  | Bradley Beal     |        0.41|
| 12  | IND  | Paul George      |        0.41|
| 24  | PHI  | Hollis Thompson  |        0.40|
| 15  | MEM  | Jordan Adams     |        0.40|
| 16  | MEM  | Courtney Lee     |        0.40|
| 31  | UTA  | Jeremy Evans     |        0.40|
| 23  | ORL  | Channing Frye    |        0.39|
| 19  | MIN  | Shabazz Muhammad |        0.39|
| 3   | BRO  | Deron Williams   |        0.37|
