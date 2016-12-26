---
layout: post
title: "Iris Data Analysis - quantify the morphologic variation of three related species"
subtitle:   " \"Iris,R实战\""
date:   2016-08-17 21:14:54
author: "Jacky"
header-img: "img/post-bg-2015.jpg"
tags: 
     R实战 
---

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

![plot of chunk unnamed-chunk-7](http://oiqvvwjga.bkt.clouddn.com/Iris-Data-Analysis/unnamed-chunk-7-1.png)


```r
# histgoram density
ggplot(iris.kag,aes(x=SepalLengthCm))+
        geom_histogram(aes(y=..density..),binwidth = 0.1, 
                       fill = "skyblue", colour = "black") +
        stat_density(geom = 'line', color='black',
                     linetype = 2, size=1, adjust = 2)+
         theme_economist() + facet_wrap(~Species)
```

![plot of chunk unnamed-chunk-8](http://oiqvvwjga.bkt.clouddn.com/Iris-Data-Analysis/unnamed-chunk-8-1.png)

```r
# histgoram
ggplot(iris.kag,aes(x=SepalLengthCm))+
        geom_histogram(aes(fill=..count..), bins = 60)+
        scale_fill_gradient("Count", low="green", high="red")
```

![plot of chunk unnamed-chunk-9](http://oiqvvwjga.bkt.clouddn.com/Iris-Data-Analysis/unnamed-chunk-9-1.png)

```r
# histgoram count by species
ggplot(iris.kag, aes(x=PetalLengthCm, fill = factor(Species)))+
        geom_bar()+
        scale_fill_discrete(name="Species")
```

![plot of chunk unnamed-chunk-9](http://oiqvvwjga.bkt.clouddn.com/Iris-Data-Analysis/unnamed-chunk-9-2.png)


```r
# kernel density estimation
ggplot(data = iris.kag,aes(x = SepalLengthCm,color = Species,linetype=Species))+
        stat_density(geom = 'line',size=1,position = 'identity',adjust = 1)+
        scale_color_economist() + theme_economist()
```

![plot of chunk unnamed-chunk-10](http://oiqvvwjga.bkt.clouddn.com/Iris-Data-Analysis/unnamed-chunk-10-1.png)


```r
# boxplot
ggplot(iris.kag,aes(x=Species, y=SepalLengthCm, fill=Species))+
        geom_boxplot(outlier.size = 4)+ggtitle("Petal Length by Species")+
        scale_fill_discrete(name="Species")
```

![plot of chunk unnamed-chunk-11](http://oiqvvwjga.bkt.clouddn.com/Iris-Data-Analysis/unnamed-chunk-11-1.png)


```r
# Violin plot
ggplot(iris.kag,aes(x=Species, y=SepalLengthCm,
                    color=Species))+geom_violin(size = 1)+
        geom_point(alpha=0.5, position = position_jitter(0.1))+
        scale_color_brewer(palette="Set1")+theme_economist()
```

![plot of chunk unnamed-chunk-12](http://oiqvvwjga.bkt.clouddn.com/Iris-Data-Analysis/unnamed-chunk-12-1.png)

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

![plot of chunk unnamed-chunk-13](http://oiqvvwjga.bkt.clouddn.com/Iris-Data-Analysis/unnamed-chunk-13-1.png)
longer petals generally mean wider petals,but longer Sepal length means longer and wider petals!

```r
# plot - covariance between feature
plot(iris.kag, col=iris.kag$Species)
```

![plot of chunk unnamed-chunk-14](http://oiqvvwjga.bkt.clouddn.com/Iris-Data-Analysis/unnamed-chunk-14-1.png)


```r
ggplot(iris.kag, aes(x=SepalWidthCm, y=SepalLengthCm))+
        geom_point(aes(colour=Species))+
        ggtitle("Sepal Length Vs Sepal Width")
```

![plot of chunk unnamed-chunk-15](http://oiqvvwjga.bkt.clouddn.com/Iris-Data-Analysis/unnamed-chunk-15-1.png)

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

![plot of chunk unnamed-chunk-16](http://oiqvvwjga.bkt.clouddn.com/Iris-Data-Analysis/unnamed-chunk-16-1.png)

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
