Melbourne Housing
================
Fabianne Monserrat

# Assignment Project - STA 4102

## Wine Data

#### Open and read .csv file

``` r
Data = read.csv("melb_data.csv", header = TRUE)
```

``` r
dim(Data)
```

    ## [1] 13580    21

``` r
sum(is.na(Data))
```

    ## [1] 11887

``` r
Data = na.omit(Data)
sum(is.na(Data))
```

    ## [1] 0

``` r
library(dplyr)
```

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

``` r
glimpse(Data)
```

    ## Rows: 6,830
    ## Columns: 21
    ## $ Suburb        <chr> "Abbotsford", "Abbotsford", "Abbotsford", "Abbotsford", …
    ## $ Address       <chr> "25 Bloomburg St", "5 Charles St", "55a Park St", "124 Y…
    ## $ Rooms         <int> 2, 3, 4, 3, 2, 2, 3, 2, 2, 3, 3, 2, 4, 2, 2, 2, 3, 2, 3,…
    ## $ Type          <chr> "h", "h", "h", "h", "h", "h", "h", "u", "h", "h", "h", "…
    ## $ Price         <dbl> 1035000, 1465000, 1600000, 1876000, 1636000, 1097000, 13…
    ## $ Method        <chr> "S", "SP", "VB", "S", "S", "S", "VB", "S", "S", "S", "S"…
    ## $ SellerG       <chr> "Biggin", "Biggin", "Nelson", "Nelson", "Nelson", "Biggi…
    ## $ Date          <chr> "4/02/2016", "4/03/2017", "4/06/2016", "7/05/2016", "8/1…
    ## $ Distance      <dbl> 2.5, 2.5, 2.5, 2.5, 2.5, 2.5, 2.5, 2.5, 2.5, 2.5, 2.5, 2…
    ## $ Postcode      <dbl> 3067, 3067, 3067, 3067, 3067, 3067, 3067, 3067, 3067, 30…
    ## $ Bedroom2      <dbl> 2, 3, 3, 4, 2, 3, 3, 2, 2, 3, 2, 2, 4, 2, 2, 2, 3, 2, 3,…
    ## $ Bathroom      <dbl> 1, 2, 1, 2, 1, 1, 2, 2, 1, 2, 1, 1, 2, 1, 2, 1, 3, 2, 1,…
    ## $ Car           <dbl> 0, 0, 2, 0, 2, 2, 2, 1, 2, 1, 1, 1, 2, 1, 1, 0, 1, 2, 1,…
    ## $ Landsize      <dbl> 156, 134, 120, 245, 256, 220, 214, 0, 238, 113, 138, 150…
    ## $ BuildingArea  <dbl> 79, 150, 142, 210, 107, 75, 190, 94, 97, 110, 105, 73, 1…
    ## $ YearBuilt     <dbl> 1900, 1900, 2014, 1910, 1890, 1900, 2005, 2009, 1890, 18…
    ## $ CouncilArea   <chr> "Yarra", "Yarra", "Yarra", "Yarra", "Yarra", "Yarra", "Y…
    ## $ Lattitude     <dbl> -37.8079, -37.8093, -37.8072, -37.8024, -37.8060, -37.80…
    ## $ Longtitude    <dbl> 144.9934, 144.9944, 144.9941, 144.9993, 144.9954, 144.99…
    ## $ Regionname    <chr> "Northern Metropolitan", "Northern Metropolitan", "North…
    ## $ Propertycount <dbl> 4019, 4019, 4019, 4019, 4019, 4019, 4019, 4019, 4019, 40…

``` r
sapply(Data, class)
```

    ##        Suburb       Address         Rooms          Type         Price 
    ##   "character"   "character"     "integer"   "character"     "numeric" 
    ##        Method       SellerG          Date      Distance      Postcode 
    ##   "character"   "character"   "character"     "numeric"     "numeric" 
    ##      Bedroom2      Bathroom           Car      Landsize  BuildingArea 
    ##     "numeric"     "numeric"     "numeric"     "numeric"     "numeric" 
    ##     YearBuilt   CouncilArea     Lattitude    Longtitude    Regionname 
    ##     "numeric"   "character"     "numeric"     "numeric"   "character" 
    ## Propertycount 
    ##     "numeric"

Feature explanation:

#### Exploratory Analysis / Descriptive Statistics

``` r
summary(Data$Price) 
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##  131000  630000  890000 1077604 1334000 9000000

``` r
pairs(select_if(Data, is.numeric))
```

![](MelbHouse_files/figure-gfm/unnamed-chunk-6-1.png)<!-- -->

``` r
library(corrplot)
```

    ## corrplot 0.92 loaded

``` r
corrplot(cor(select_if(Data, is.numeric)))
```

![](MelbHouse_files/figure-gfm/unnamed-chunk-7-1.png)<!-- -->

``` r
cor(select_if(Data, is.numeric))
```

    ##                      Rooms       Price    Distance    Postcode    Bedroom2
    ## Rooms          1.000000000  0.51771790  0.28976342  0.06867449  0.95585069
    ## Price          0.517717896  1.00000000 -0.16497466  0.10934259  0.50045197
    ## Distance       0.289763421 -0.16497466  1.00000000  0.43827430  0.29610483
    ## Postcode       0.068674489  0.10934259  0.43827430  1.00000000  0.07161117
    ## Bedroom2       0.955850691  0.50045197  0.29610483  0.07161117  1.00000000
    ## Bathroom       0.613284670  0.49248086  0.12404359  0.11277583  0.61683208
    ## Car            0.420493152  0.25091590  0.26514249  0.04922636  0.42404101
    ## Landsize       0.099030957  0.07353590  0.08244905  0.03999792  0.09806603
    ## BuildingArea   0.603149536  0.52049158  0.15514796  0.08179100  0.58846256
    ## YearBuilt     -0.049272047 -0.30734344  0.25846163  0.03681922 -0.03812612
    ## Lattitude      0.009005193 -0.21691878 -0.10109179 -0.37490363  0.01388202
    ## Longtitude     0.096665470  0.20978618  0.21559399  0.43057915  0.09553354
    ## Propertycount -0.100447456 -0.05333561 -0.06143317  0.05854245 -0.09843843
    ##                  Bathroom          Car    Landsize BuildingArea    YearBuilt
    ## Rooms          0.61328467  0.420493152  0.09903096   0.60314954 -0.049272047
    ## Price          0.49248086  0.250915900  0.07353590   0.52049158 -0.307343435
    ## Distance       0.12404359  0.265142492  0.08244905   0.15514796  0.258461626
    ## Postcode       0.11277583  0.049226356  0.03999792   0.08179100  0.036819222
    ## Bedroom2       0.61683208  0.424041014  0.09806603   0.58846256 -0.038126120
    ## Bathroom       1.00000000  0.335331245  0.08167965   0.53971687  0.166412095
    ## Car            0.33533124  1.000000000  0.11342687   0.33170216  0.114339508
    ## Landsize       0.08167965  0.113426872  1.00000000   0.08281503  0.031474094
    ## BuildingArea   0.53971687  0.331702156  0.08281503   1.00000000  0.017940312
    ## YearBuilt      0.16641210  0.114339508  0.03147409   0.01794031  1.000000000
    ## Lattitude     -0.06698307  0.003349774  0.01313278  -0.04222951  0.064333166
    ## Longtitude     0.11957345  0.061410347  0.02688183   0.10715291 -0.005064202
    ## Propertycount -0.06212654 -0.033258135 -0.01490947  -0.06331520  0.005116166
    ##                  Lattitude   Longtitude Propertycount
    ## Rooms          0.009005193  0.096665470  -0.100447456
    ## Price         -0.216918775  0.209786178  -0.053335615
    ## Distance      -0.101091792  0.215593995  -0.061433167
    ## Postcode      -0.374903626  0.430579152   0.058542450
    ## Bedroom2       0.013882020  0.095533540  -0.098438434
    ## Bathroom      -0.066983074  0.119573445  -0.062126544
    ## Car            0.003349774  0.061410347  -0.033258135
    ## Landsize       0.013132783  0.026881829  -0.014909475
    ## BuildingArea  -0.042229514  0.107152907  -0.063315199
    ## YearBuilt      0.064333166 -0.005064202   0.005116166
    ## Lattitude      1.000000000 -0.362765439   0.060939704
    ## Longtitude    -0.362765439  1.000000000   0.052395218
    ## Propertycount  0.060939704  0.052395218   1.000000000

``` r
hist(Data$Price)
```

![](MelbHouse_files/figure-gfm/unnamed-chunk-9-1.png)<!-- -->

#### Training

``` r
library(caret)
```

    ## Loading required package: ggplot2

    ## Loading required package: lattice

``` r
set.seed(123)
trainIdx = createDataPartition(Data$Price, p = 0.7, list = FALSE, times = 1)
TrainingData = Data[ trainIdx ,]
TestingData = Data[ -trainIdx ,]
dim(TrainingData)
```

    ## [1] 4783   21

``` r
dim(TestingData)
```

    ## [1] 2047   21

### Linear Regression

``` r
model_full = lm(Price ~ Rooms + Bathroom + Type + Distance + Car + Landsize + BuildingArea + YearBuilt, data = TrainingData)
summary(model_full)
```

    ## 
    ## Call:
    ## lm(formula = Price ~ Rooms + Bathroom + Type + Distance + Car + 
    ##     Landsize + BuildingArea + YearBuilt, data = TrainingData)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -3963870  -244527   -53266   172343  5298867 
    ## 
    ## Coefficients:
    ##                Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)  9172790.43  397632.17  23.069  < 2e-16 ***
    ## Rooms         116571.58   11059.19  10.541  < 2e-16 ***
    ## Bathroom      292177.52   12749.05  22.918  < 2e-16 ***
    ## Typet        -106991.26   25263.33  -4.235 2.33e-05 ***
    ## Typeu        -189500.88   21118.27  -8.973  < 2e-16 ***
    ## Distance      -30535.38    1250.06 -24.427  < 2e-16 ***
    ## Car            55042.94    7837.84   7.023 2.48e-12 ***
    ## Landsize          18.95       6.62   2.862  0.00423 ** 
    ## BuildingArea    1575.67      87.23  18.064  < 2e-16 ***
    ## YearBuilt      -4514.30     207.18 -21.789  < 2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 448100 on 4773 degrees of freedom
    ## Multiple R-squared:  0.5557, Adjusted R-squared:  0.5548 
    ## F-statistic: 663.2 on 9 and 4773 DF,  p-value: < 2.2e-16

Model R^2 = 0.5557, 55.57% of variability in data explained by this
model.

``` r
PredictionFull = predict(model_full, newdata = TestingData)
MSE_Full = mean((PredictionFull - TestingData$Price)^2)
RMSE_Full = sqrt(MSE_Full)
RMSE_Full
```

    ## [1] 478755.9

``` r
sqrt(mean(model_full$residuals^2))
```

    ## [1] 447626.4

``` r
# Model without Type, Car, Landsize
model = lm(Price ~ Rooms + Bathroom + YearBuilt + Distance + BuildingArea, data = TrainingData)
summary(model)
```

    ## 
    ## Call:
    ## lm(formula = Price ~ Rooms + Bathroom + YearBuilt + Distance + 
    ##     BuildingArea, data = TrainingData)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -4320612  -242171   -62648   168614  5383842 
    ## 
    ## Coefficients:
    ##                Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)   1.017e+07  3.680e+05   27.64   <2e-16 ***
    ## Rooms         1.736e+05  1.011e+04   17.18   <2e-16 ***
    ## Bathroom      2.923e+05  1.284e+04   22.76   <2e-16 ***
    ## YearBuilt    -5.116e+03  1.880e+02  -27.22   <2e-16 ***
    ## Distance     -2.698e+04  1.234e+03  -21.87   <2e-16 ***
    ## BuildingArea  1.714e+03  8.776e+01   19.53   <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 455100 on 4777 degrees of freedom
    ## Multiple R-squared:  0.5413, Adjusted R-squared:  0.5408 
    ## F-statistic:  1127 on 5 and 4777 DF,  p-value: < 2.2e-16

``` r
Prediction = predict(model, newdata = TestingData)
MSE = mean((Prediction - TestingData$Price)^2)
RMSE = sqrt(MSE)
RMSE
```

    ## [1] 483333.5

``` r
sqrt(mean(model$residuals^2))
```

    ## [1] 454800.9

RMSE =

Original model RMSE =

Considering “HeartDisease” takes on either the value of 0 or not. The
model RMSE for training and testing is not too far off, with the
original having RMSE = 5977.949 (a difference of 218.328) therefore the
model is pretty good. (Model is not overfit bc RMSE’s are similar, and
does perform consistently with new data)

### Decision Tree

``` r
library(tree)
Tree = tree(Price ~ Rooms + Bathroom + YearBuilt + Distance + BuildingArea, data = TrainingData)
plot(Tree)
text(Tree, cex = .75)
```

![](MelbHouse_files/figure-gfm/unnamed-chunk-17-1.png)<!-- -->

``` r
summary(Tree)
```

    ## 
    ## Regression tree:
    ## tree(formula = Price ~ Rooms + Bathroom + YearBuilt + Distance + 
    ##     BuildingArea, data = TrainingData)
    ## Variables actually used in tree construction:
    ## [1] "BuildingArea" "YearBuilt"    "Distance"     "Bathroom"    
    ## Number of terminal nodes:  9 
    ## Residual mean deviance:  2.061e+11 = 9.838e+14 / 4774 
    ## Distribution of residuals:
    ##     Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
    ## -2128000  -248000   -65000        0   172100  4937000

``` r
print("Making predictions for the following 5 houses:")
```

    ## [1] "Making predictions for the following 5 houses:"

``` r
print(head(TrainingData))
```

    ##        Suburb         Address Rooms Type   Price Method SellerG       Date
    ## 2  Abbotsford 25 Bloomburg St     2    h 1035000      S  Biggin  4/02/2016
    ## 3  Abbotsford    5 Charles St     3    h 1465000     SP  Biggin  4/03/2017
    ## 5  Abbotsford     55a Park St     4    h 1600000     VB  Nelson  4/06/2016
    ## 7  Abbotsford    124 Yarra St     3    h 1876000      S  Nelson  7/05/2016
    ## 10 Abbotsford   10 Valiant St     2    h 1097000      S  Biggin  8/10/2016
    ## 12 Abbotsford 40 Nicholson St     3    h 1350000     VB  Nelson 12/11/2016
    ##    Distance Postcode Bedroom2 Bathroom Car Landsize BuildingArea YearBuilt
    ## 2       2.5     3067        2        1   0      156           79      1900
    ## 3       2.5     3067        3        2   0      134          150      1900
    ## 5       2.5     3067        3        1   2      120          142      2014
    ## 7       2.5     3067        4        2   0      245          210      1910
    ## 10      2.5     3067        3        1   2      220           75      1900
    ## 12      2.5     3067        3        2   2      214          190      2005
    ##    CouncilArea Lattitude Longtitude            Regionname Propertycount
    ## 2        Yarra  -37.8079   144.9934 Northern Metropolitan          4019
    ## 3        Yarra  -37.8093   144.9944 Northern Metropolitan          4019
    ## 5        Yarra  -37.8072   144.9941 Northern Metropolitan          4019
    ## 7        Yarra  -37.8024   144.9993 Northern Metropolitan          4019
    ## 10       Yarra  -37.8010   144.9989 Northern Metropolitan          4019
    ## 12       Yarra  -37.8085   144.9964 Northern Metropolitan          4019

``` r
print("The predictions are")
```

    ## [1] "The predictions are"

``` r
print(predict(Tree, head(TrainingData)))
```

    ##         2         3         5         7        10        12 
    ## 1065207.4 1535892.3  898513.5 1744996.1 1065207.4  898513.5

``` r
print("Actual Price")
```

    ## [1] "Actual Price"

``` r
print(head(TrainingData$Price))
```

    ## [1] 1035000 1465000 1600000 1876000 1097000 1350000

``` r
PredictionTree = predict(Tree, newdata = TrainingData)
MSE = mean((PredictionTree - TestingData$Price)^2)
```

    ## Warning in PredictionTree - TestingData$Price: longer object length is not a
    ## multiple of shorter object length

``` r
RMSE = sqrt(MSE)
RMSE
```

    ## [1] 843977.3

``` r
library(rpart)
library(rpart.plot)
tree2 = rpart(Price ~ Rooms + Bathroom + YearBuilt + Distance + BuildingArea, data = TrainingData, control = rpart.control(minsplit = 2, cp = 0.005, maxdepth = 30))
# summary(tree2)
best2 = tree2$cptable[which.min(tree2$cptable[,'xerror']),'CP']
prunetree2 = prune(tree2, cp = best2)
prp(prunetree2)
```

![](MelbHouse_files/figure-gfm/unnamed-chunk-20-1.png)<!-- -->

``` r
print("Making predictions for the last 5 houses:")
```

    ## [1] "Making predictions for the last 5 houses:"

``` r
print(head(TrainingData))
```

    ##        Suburb         Address Rooms Type   Price Method SellerG       Date
    ## 2  Abbotsford 25 Bloomburg St     2    h 1035000      S  Biggin  4/02/2016
    ## 3  Abbotsford    5 Charles St     3    h 1465000     SP  Biggin  4/03/2017
    ## 5  Abbotsford     55a Park St     4    h 1600000     VB  Nelson  4/06/2016
    ## 7  Abbotsford    124 Yarra St     3    h 1876000      S  Nelson  7/05/2016
    ## 10 Abbotsford   10 Valiant St     2    h 1097000      S  Biggin  8/10/2016
    ## 12 Abbotsford 40 Nicholson St     3    h 1350000     VB  Nelson 12/11/2016
    ##    Distance Postcode Bedroom2 Bathroom Car Landsize BuildingArea YearBuilt
    ## 2       2.5     3067        2        1   0      156           79      1900
    ## 3       2.5     3067        3        2   0      134          150      1900
    ## 5       2.5     3067        3        1   2      120          142      2014
    ## 7       2.5     3067        4        2   0      245          210      1910
    ## 10      2.5     3067        3        1   2      220           75      1900
    ## 12      2.5     3067        3        2   2      214          190      2005
    ##    CouncilArea Lattitude Longtitude            Regionname Propertycount
    ## 2        Yarra  -37.8079   144.9934 Northern Metropolitan          4019
    ## 3        Yarra  -37.8093   144.9944 Northern Metropolitan          4019
    ## 5        Yarra  -37.8072   144.9941 Northern Metropolitan          4019
    ## 7        Yarra  -37.8024   144.9993 Northern Metropolitan          4019
    ## 10       Yarra  -37.8010   144.9989 Northern Metropolitan          4019
    ## 12       Yarra  -37.8085   144.9964 Northern Metropolitan          4019

``` r
print("Predicted Price:")
```

    ## [1] "Predicted Price:"

``` r
print(predict(tree2, tail(TrainingData)))
```

    ##     13564     13565     13572     13573     13574     13580 
    ## 1279066.8  827898.2  827898.2  552938.1  827898.2 1065207.4

``` r
print("Actual Price:")
```

    ## [1] "Actual Price:"

``` r
print(tail(TrainingData$Price))
```

    ## [1] 1271000  540000 1330000  650000  635000 1285000

``` r
PredictionTree2 = predict(tree2, newdata = TrainingData)
MSE = mean((PredictionTree2 - TestingData$Price)^2)
```

    ## Warning in PredictionTree2 - TestingData$Price: longer object length is not a
    ## multiple of shorter object length

``` r
RMSE = sqrt(MSE)
RMSE
```

    ## [1] 863224.1

RMSE = 0.4835247 (tree 1)

RMSE = 0.4938585 (tree 2)

The weighted average error between predictions and actual values is
0.940, which is only decent considering the range of quality in the data
set is from 3 - 8.

### Random Forest

``` r
library(randomForest)
```

    ## randomForest 4.7-1.1

    ## Type rfNews() to see new features/changes/bug fixes.

    ## 
    ## Attaching package: 'randomForest'

    ## The following object is masked from 'package:ggplot2':
    ## 
    ##     margin

    ## The following object is masked from 'package:dplyr':
    ## 
    ##     combine

``` r
rf1 = train(Price ~ Rooms + Bathroom + YearBuilt + Distance + BuildingArea, method = "rf", data = Data, trControl = trainControl(method = 'cv', number = 3))
summary(rf1)
```

    ##                 Length Class      Mode     
    ## call               4   -none-     call     
    ## type               1   -none-     character
    ## predicted       6830   -none-     numeric  
    ## mse              500   -none-     numeric  
    ## rsq              500   -none-     numeric  
    ## oob.times       6830   -none-     numeric  
    ## importance         5   -none-     numeric  
    ## importanceSD       0   -none-     NULL     
    ## localImportance    0   -none-     NULL     
    ## proximity          0   -none-     NULL     
    ## ntree              1   -none-     numeric  
    ## mtry               1   -none-     numeric  
    ## forest            11   -none-     list     
    ## coefs              0   -none-     NULL     
    ## y               6830   -none-     numeric  
    ## test               0   -none-     NULL     
    ## inbag              0   -none-     NULL     
    ## xNames             5   -none-     character
    ## problemType        1   -none-     character
    ## tuneValue          1   data.frame list     
    ## obsLevels          1   -none-     logical  
    ## param              0   -none-     list

``` r
library(ROCR)
prediction1 = predict(rf1, newdata = TestingData)
```

``` r
rf = randomForest(Price ~ Rooms + Bathroom + YearBuilt + Distance + BuildingArea, data = Data, importance = TRUE, mtry = 2)
vi = varImpPlot(rf)
```

![](MelbHouse_files/figure-gfm/unnamed-chunk-25-1.png)<!-- -->

``` r
rf
```

    ## 
    ## Call:
    ##  randomForest(formula = Price ~ Rooms + Bathroom + YearBuilt +      Distance + BuildingArea, data = Data, importance = TRUE,      mtry = 2) 
    ##                Type of random forest: regression
    ##                      Number of trees: 500
    ## No. of variables tried at each split: 2
    ## 
    ##           Mean of squared residuals: 148509331304
    ##                     % Var explained: 67.24

``` r
summary(rf)
```

    ##                 Length Class  Mode     
    ## call               5   -none- call     
    ## type               1   -none- character
    ## predicted       6830   -none- numeric  
    ## mse              500   -none- numeric  
    ## rsq              500   -none- numeric  
    ## oob.times       6830   -none- numeric  
    ## importance        10   -none- numeric  
    ## importanceSD       5   -none- numeric  
    ## localImportance    0   -none- NULL     
    ## proximity          0   -none- NULL     
    ## ntree              1   -none- numeric  
    ## mtry               1   -none- numeric  
    ## forest            11   -none- list     
    ## coefs              0   -none- NULL     
    ## y               6830   -none- numeric  
    ## test               0   -none- NULL     
    ## inbag              0   -none- NULL     
    ## terms              3   terms  call
