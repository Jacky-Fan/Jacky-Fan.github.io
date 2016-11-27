---
layout: post
title: "Iris Data Analysis - quantify the morphologic variation of three related species"
subtitle:   " \"Iris,R实战\""
date:   2016-08-17 21:14:54
categories: R数据分析
header-img: "img/post-bg-2015.jpg"
author: Jacky Fan
tags: 
     -R实战 
---

* content
{:toc}

# Descriptive Statistics - Exploration of data


```r
library(ggplot2)
library(gridExtra)
library(ggthemes)
library(caTools)
library(corrgram)
```

```r
# importing the iris dataset
iris.kag <- read.csv('iris.csv')
```

```
## Warning in file(file, "rt"): cannot open file 'iris.csv': No such file or
## directory
```

```
## Error in file(file, "rt"): cannot open the connection
```

```r
# getting data summary
summary(iris.kag)
```

```
##        Id         SepalLengthCm    SepalWidthCm   PetalLengthCm  
##  Min.   :  1.00   Min.   :4.300   Min.   :2.000   Min.   :1.000  
##  1st Qu.: 38.25   1st Qu.:5.100   1st Qu.:2.800   1st Qu.:1.600  
##  Median : 75.50   Median :5.800   Median :3.000   Median :4.350  
##  Mean   : 75.50   Mean   :5.843   Mean   :3.054   Mean   :3.759  
##  3rd Qu.:112.75   3rd Qu.:6.400   3rd Qu.:3.300   3rd Qu.:5.100  
##  Max.   :150.00   Max.   :7.900   Max.   :4.400   Max.   :6.900  
##   PetalWidthCm              Species  
##  Min.   :0.100   Iris-setosa    :50  
##  1st Qu.:0.300   Iris-versicolor:50  
##  Median :1.300   Iris-virginica :50  
##  Mean   :1.199                       
##  3rd Qu.:1.800                       
##  Max.   :2.500
```

```r
# understanding internal structure of data
str(iris.kag)
```

```
## 'data.frame':	150 obs. of  6 variables:
##  $ Id           : int  1 2 3 4 5 6 7 8 9 10 ...
##  $ SepalLengthCm: num  5.1 4.9 4.7 4.6 5 5.4 4.6 5 4.4 4.9 ...
##  $ SepalWidthCm : num  3.5 3 3.2 3.1 3.6 3.9 3.4 3.4 2.9 3.1 ...
##  $ PetalLengthCm: num  1.4 1.4 1.3 1.5 1.4 1.7 1.4 1.5 1.4 1.5 ...
##  $ PetalWidthCm : num  0.2 0.2 0.2 0.2 0.2 0.4 0.3 0.2 0.2 0.1 ...
##  $ Species      : Factor w/ 3 levels "Iris-setosa",..: 1 1 1 1 1 1 1 1 1 1 ...
```

```r
# understanding the type of all features
sapply(iris.kag,class)
```

```
##            Id SepalLengthCm  SepalWidthCm PetalLengthCm  PetalWidthCm 
##     "integer"     "numeric"     "numeric"     "numeric"     "numeric" 
##       Species 
##      "factor"
```

```r
# checking missing value
sapply(iris.kag,function(x) sum(is.na(x)))
```

```
##            Id SepalLengthCm  SepalWidthCm PetalLengthCm  PetalWidthCm 
##             0             0             0             0             0 
##       Species 
##             0
```

```r
# checking the proportions of species
prop.table(table(iris.kag$Species))
```

```
## 
##     Iris-setosa Iris-versicolor  Iris-virginica 
##       0.3333333       0.3333333       0.3333333
```



```r
# boxplot
par(mfrow = c(2,2))
for(i in 2:5) boxplot(iris.kag[,i] ~ Species,data=iris.kag, main=names(iris.kag[i]))
```

![plot of chunk unnamed-chunk-7](figure/unnamed-chunk-7-1.png)


```r
# histgoram density
ggplot(iris.kag,aes(x=SepalLengthCm))+
        geom_histogram(aes(y=..density..),binwidth = 0.1, 
                       fill = "skyblue", colour = "black") +
        stat_density(geom = 'line', color='black',
                     linetype = 2, size=1, adjust = 2)+
         theme_economist() + facet_wrap(~Species)
```

![plot of chunk unnamed-chunk-8](figure/unnamed-chunk-8-1.png)

```r
# histgoram
ggplot(iris.kag,aes(x=SepalLengthCm))+
        geom_histogram(aes(fill=..count..), bins = 60)+
        scale_fill_gradient("Count", low="green", high="red")
```

![plot of chunk unnamed-chunk-9](figure/unnamed-chunk-9-1.png)

```r
# histgoram count by species
ggplot(iris.kag, aes(x=PetalLengthCm, fill = factor(Species)))+
        geom_bar()+
        scale_fill_discrete(name="Species")
```

![plot of chunk unnamed-chunk-9](figure/unnamed-chunk-9-2.png)


```r
# kernel density estimation
ggplot(data = iris.kag,aes(x = SepalLengthCm,color = Species,linetype=Species))+
        stat_density(geom = 'line',size=1,position = 'identity',adjust = 1)+
        scale_color_economist() + theme_economist()
```

![plot of chunk unnamed-chunk-10](figure/unnamed-chunk-10-1.png)


```r
# boxplot
ggplot(iris.kag,aes(x=Species, y=SepalLengthCm, fill=Species))+
        geom_boxplot(outlier.size = 4)+ggtitle("Petal Length by Species")+
        scale_fill_discrete(name="Species")
```

![plot of chunk unnamed-chunk-11](figure/unnamed-chunk-11-1.png)


```r
# Violin plot
ggplot(iris.kag,aes(x=Species, y=SepalLengthCm,
                    color=Species))+geom_violin(size = 1)+
        geom_point(alpha=0.5, position = position_jitter(0.1))+
        scale_color_brewer(palette="Set1")+theme_economist()
```

![plot of chunk unnamed-chunk-12](figure/unnamed-chunk-12-1.png)

# statistical model - classfication

```r
# covariance between feature
cor(iris.kag[, 2:5])
```

```
##               SepalLengthCm SepalWidthCm PetalLengthCm PetalWidthCm
## SepalLengthCm     1.0000000   -0.1093692     0.8717542    0.8179536
## SepalWidthCm     -0.1093692    1.0000000    -0.4205161   -0.3565441
## PetalLengthCm     0.8717542   -0.4205161     1.0000000    0.9627571
## PetalWidthCm      0.8179536   -0.3565441     0.9627571    1.0000000
```

```r
cov(iris.kag[, 2:5])
```

```
##               SepalLengthCm SepalWidthCm PetalLengthCm PetalWidthCm
## SepalLengthCm    0.68569351  -0.03926846     1.2736823    0.5169038
## SepalWidthCm    -0.03926846   0.18800403    -0.3217128   -0.1179812
## PetalLengthCm    1.27368233  -0.32171275     3.1131794    1.2963875
## PetalWidthCm     0.51690380  -0.11798121     1.2963875    0.5824143
```

```r
corrgram(iris.kag[,2:5])
```

![plot of chunk unnamed-chunk-13](figure/unnamed-chunk-13-1.png)
longer petals generally mean wider petals,but longer Sepal length means longer and wider petals!

```r
# plot - covariance between feature
plot(iris.kag, col=iris.kag$Species)
```

![plot of chunk unnamed-chunk-14](figure/unnamed-chunk-14-1.png)


```r
ggplot(iris.kag, aes(x=SepalWidthCm, y=SepalLengthCm))+
        geom_point(aes(colour=Species))+
        ggtitle("Sepal Length Vs Sepal Width")
```

![plot of chunk unnamed-chunk-15](figure/unnamed-chunk-15-1.png)

```r
set.seed(88) # is used to set for the pseudo random
spl <- sample.split(iris.kag$Species, SplitRatio = 0.5)
irisTrain <- subset(iris.kag,spl==T)
irisTest <- subset(iris.kag,spl==F)
# Making a linear regression model
model1 <- lm(PetalLengthCm~Species, data=irisTrain)
# predict with model
predict1 <- predict(model1, newdata = irisTest)
predictdata <- data.frame(PetalLengthCm=predict1, Species=irisTest$Species)
# comparing out test set against prediction
ggplot(irisTest, aes(x=Species, y=PetalLengthCm))+
        geom_point(aes(color='Test Set'))+
        geom_point(data = predictdata, aes(color="Prediction"))+
        ggtitle("Petal Length Vs Species")
```

![plot of chunk unnamed-chunk-16](figure/unnamed-chunk-16-1.png)

```r
summary(model1)
```

```
## 
## Call:
## lm(formula = PetalLengthCm ~ Species, data = irisTrain)
## 
## Residuals:
##    Min     1Q Median     3Q    Max 
## -1.324 -0.220  0.052  0.264  1.184 
## 
## Coefficients:
##                        Estimate Std. Error t value Pr(>|t|)    
## (Intercept)             1.44800    0.08982   16.12   <2e-16 ***
## SpeciesIris-versicolor  2.87600    0.12702   22.64   <2e-16 ***
## SpeciesIris-virginica   4.06800    0.12702   32.02   <2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.4491 on 72 degrees of freedom
## Multiple R-squared:  0.9377,	Adjusted R-squared:  0.936 
## F-statistic: 542.1 on 2 and 72 DF,  p-value: < 2.2e-16
```
可以看到截距估计为1.45，versicolor的斜率为2.88，virginica的斜率为4.07.
判定系数为0.94，说明此模型解释力很强。

```r
# Other stats
# SSR: Sum Squared Regression 回归平方和
# SSE: Sum Squared Error 残差平方和
# SST: Sum Squared Total 总平方和
# SST = SSR + SSE
# R^2=SSR/SST=1-SSE/SST = 0.94
SSE=sum((irisTest$PetalLengthCm-predict1)^2)
SST=sum((irisTest$PetalLengthCm-mean(irisTrain$PetalLengthCm))^2)
SSE/SST
```

```
## [1] 0.05627417
```


```r
model2 <- lm(PetalLengthCm~.-Id, data = irisTrain)
# prediction
prediction2 <- predict(model2, newdata = irisTest)
predictdata2 <- data.frame(PetalLengthCm=prediction2,
                           SepalWidthCm=irisTest$SepalWidthCm)
# Comparing out test set against prediction
# Jitter included to allow us to see data points that are stacked
ggplot(irisTest,aes(x=SepalWidthCm, y=PetalLengthCm))+
        geom_point(aes(color="Test Set"))+
        geom_point(data=predictdata2,aes(color="Actual Prediction"))+
        ggtitle("Petal Length Vs Sepal Width")
```

![plot of chunk unnamed-chunk-19](figure/unnamed-chunk-19-1.png)

```r
summary(model2)
```

```
## 
## Call:
## lm(formula = PetalLengthCm ~ . - Id, data = irisTrain)
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -0.80395 -0.13971  0.04525  0.20239  0.52274 
## 
## Coefficients:
##                        Estimate Std. Error t value Pr(>|t|)    
## (Intercept)            -1.06655    0.36097  -2.955 0.004278 ** 
## SepalLengthCm           0.57337    0.07822   7.330 3.34e-10 ***
## SepalWidthCm           -0.15921    0.11998  -1.327 0.188877    
## PetalWidthCm            0.73939    0.19439   3.804 0.000305 ***
## SpeciesIris-versicolor  1.39721    0.28007   4.989 4.36e-06 ***
## SpeciesIris-virginica   1.80718    0.39822   4.538 2.33e-05 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.2866 on 69 degrees of freedom
## Multiple R-squared:  0.9757,	Adjusted R-squared:  0.9739 
## F-statistic: 553.9 on 5 and 69 DF,  p-value: < 2.2e-16
```

判定系数为0.975，说明此模型解释力比model1强。

```r
AIC(model1,model2)
```

```
##        df      AIC
## model1  4 97.70196
## model2  7 33.14883
```





