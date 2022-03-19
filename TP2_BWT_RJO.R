
library(plotly)
library(ggplot2)
library(corrplot)
library(datasets)
library(tidyverse)
library(psych)
library(tidyr)
c<-read.csv("imports-85.data")
names(c)[1]<-"symboling"
names(c)[2]<-"normalized_losses"
names(c)[3]<-"make"
names(c)[4]<-"fuel_type"
names(c)[5]<-"aspiration"
names(c)[6]<-"num_of_doors"
names(c)[7]<-"body_style"
names(c)[8]<-"drive_wheels"
names(c)[9]<-"engine_location"
names(c)[10]<-"wheel_base"
names(c)[11]<-"length"
names(c)[12]<-"width"
names(c)[13]<-"height"
names(c)[14]<-"curb_weight"
names(c)[15]<-"engine_type"
names(c)[16]<-"num_of_cylinders"
names(c)[17]<-"engine_size"
names(c)[18]<-"fuel_system"
names(c)[19]<-"bore"
names(c)[20]<-"stroke"
names(c)[21]<-"compression_ratio"
names(c)[22]<-"horsepower"
names(c)[23]<-"peak_rpm"
names(c)[24]<-"city_mpg"
names(c)[25]<-"highway_mpg"
names(c)[26]<-"price"
describe(c,na.rm=TRUE,skew=FALSE)
sapply(c,class)
cc<-subset(c, select = c("normalized_losses","wheel_base","horsepower","length","width","height","curb_weight","engine_size","bore","stroke","compression_ratio","peak_rpm","city_mpg","highway_mpg","price")) 
car<-na.omit(c)
car
cor(cc,cc)
is.na(cc)
dc<-c[rowSums(is.na(c))==0]
cc%>%
  na.omit(cc)
c%>%
  na.omit(c)
sapply(cc,class)
cc[]<-lapply(cc, function(x) as.numeric(as.character(x)))
sapply(cc,class)
na.pass(cc)
cor(cc, y = NULL, use = "na.or.complete")
corr<- cor(cc, y = NULL, use = "na.or.complete")
heatmap(corr, symm=TRUE,)
as.data.frame(cc)
categ<-subset(c, select = c("symboling","make","fuel_type","aspiration","num_of_doors","body_style","drive_wheels","engine_location","engine_type","num_of_cylinders","fuel_system"))
#Scatter Plot
ggplot(data=cc, mapping=aes(x=width, y=length, color=height)) +
  geom_point() + ggtitle("Width vs. Length (by height)")

#Continuous Variables
ggplot(data=cc, mapping=aes(x=normalized_losses)) + geom_histogram(fill="blue", color='black') + ggtitle('Distribution of Normalized Losses')
ggplot(data=cc, mapping=aes(x=wheel_base)) + geom_histogram(fill="blue", color='black') + ggtitle('Distribution of Wheel Base')
ggplot(data=cc, mapping=aes(x=horsepower)) + geom_histogram(fill="blue", color='black') + ggtitle('Distribution of Horsepower')
ggplot(data=cc, mapping=aes(x=length)) + geom_histogram(fill="blue", color='black') + ggtitle('Distribution of Length')
ggplot(data=cc, mapping=aes(x=width)) + geom_histogram(fill="blue", color='black') + ggtitle('Distribution of Width')
ggplot(data=cc, mapping=aes(x=height)) + geom_histogram(fill="blue", color='black') + ggtitle('Distribution of Height')
ggplot(data=cc, mapping=aes(x=curb_weight)) + geom_histogram(fill="blue", color='black') + ggtitle('Distribution of Curb Weight')
ggplot(data=cc, mapping=aes(x=engine_size)) + geom_histogram(fill="blue", color='black') + ggtitle('Distribution of Engine Size')
ggplot(data=cc, mapping=aes(x=bore)) + geom_histogram(fill="blue", color='black') + ggtitle('Distribution of Bore')
ggplot(data=cc, mapping=aes(x=stroke)) + geom_histogram(fill="blue", color='black') + ggtitle('Distribution of Stroke')
ggplot(data=cc, mapping=aes(x=compression_ratio)) + geom_histogram(fill="blue", color='black') + ggtitle('Distribution of compression Ration')
ggplot(data=cc, mapping=aes(x=peak_rpm)) + geom_histogram(fill="blue", color='black') + ggtitle('Distribution of Peak RPM')
ggplot(data=cc, mapping=aes(x=city_mpg)) + geom_histogram(fill="blue", color='black') + ggtitle('Distribution of City MPG')
ggplot(data=cc, mapping=aes(x=highway_mpg)) + geom_histogram(fill="blue", color='black') + ggtitle('Distribution of Highway MPG')
ggplot(data=cc, mapping=aes(x=price)) + geom_histogram(fill="blue", color='black') + ggtitle('Distribution of Price')

#Categorical Distributions
ggplot(data=categ, mapping=aes(x=symboling)) + geom_histogram(fill="blue", color='black') + ggtitle('Distribution of Symboling')
ggplot(data=categ, mapping=aes(x=make)) + geom_histogram(fill="blue", color='black') + ggtitle('Distribution of Make')
ggplot(data=categ, mapping=aes(x=fuel_type)) + geom_histogram(fill="blue", color='black',stat="count") + ggtitle('Distribution of Fuel Type')
ggplot(data=categ, mapping=aes(x=aspiration)) + geom_histogram(fill="blue", color='black',stat="count") + ggtitle('Distribution of Aspiration')
ggplot(data=categ, mapping=aes(x=num_of_doors)) + geom_histogram(fill="blue", color='black',stat="count") + ggtitle('Distribution of Number of Doors')
ggplot(data=categ, mapping=aes(x=body_style)) + geom_histogram(fill="blue", color='black',stat="count") + ggtitle('Distribution of Body Style')       
ggplot(data=categ, mapping=aes(x=drive_wheels)) + geom_histogram(fill="blue", color='black',stat="count") + ggtitle('Distribution of Drive Wheels')
ggplot(data=categ, mapping=aes(x=engine_location)) + geom_histogram(fill="blue", color='black',stat="count") + ggtitle('Distribution of Enigne Location')
ggplot(data=categ, mapping=aes(x=engine_type)) + geom_histogram(fill="blue", color='black',stat="count") + ggtitle('Distribution of Engine Type')
ggplot(data=categ, mapping=aes(x=num_of_cylinders)) + geom_histogram(fill="blue", color='black',stat="count") + ggtitle('Distribution of Number of Cylinders')
ggplot(data=categ, mapping=aes(x=fuel_system)) + geom_histogram(fill="blue", color='black',stat="count") + ggtitle('Distribution of Fuel System')

#PAIRPLOT

pairs(cc)

