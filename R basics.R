library(dplyr)
library(hflights)
View(hflights)

#Select function
flight1<-select(hflights,FlightNum,ArrTime,DepTime)
View(flight1)
flight1<-select(hflights,5,6,8)
View(flight1)     
flight1<-select(hflights,contains("Time"))
View(flight1)
flight1<-select(hflights,Year:ArrTime)
View(flight1)
flight1<-select(hflights,1:6)
View(flight1)
flight1<-select(hflights,starts_with("Day"),ends_with("Time"))
View(flight1)

#Mutate Function
flight1<-mutate(hflights,ActualGroundTime=ActualElapsedTime-AirTime)
View(flight1)
flight1<-mutate(hflights,AverageSpeed=Distance/AirTime *60)
View(flight1)
flight1<-mutate(hflights,TotalTaxi=TaxiIn+TaxiOut)
View(flight1)
flight1<-mutate(hflights,TimeLoss=ArrDelay+DepDelay)
View(flight1)

#filter
flight1<-filter(hflights,Distance>3000)
View(flight1)
range(flight1$Distance)
flight1<-filter(hflights,UniqueCarrier %in% c("OO","US","AA"))
View(flight1)
table(flight1$UniqueCarrier)
flight1<-filter(hflights,TaxiIn+TaxiOut>AirTime)
View(flight1)
flight1<-filter(hflights,DepTime<500|ArrTime>2200)
View(flight1)
flight1<-filter(hflights,Dest=="JFK" & Cancelled==1)
View(flight1)

#Arrange
flight1<-arrange(hflights,DepDelay)
View(flight1)
flight1<-arrange(hflights,DepDelay+ArrDelay)
View(flight1)

#Summarise
flight1<-summarise(hflights,min_dist=min(Distance),max_dist=max(Distance))
View(flight1)
flight1<-summarise(hflights,earliest=min(ArrDelay,na.rm = T),
                   average=mean(ArrDelay,na.rm=T),
                   latest=max(ArrDelay,na.rm = T),
                   sd=sd(ArrDelay,na.rm = T))
View(flight1)

#Pipe
flight1<-hflights %>% select(contains("Time")) %>% filter(AirTime>60)
View(flight1)
range(flight1$AirTime)
flight1<-hflights %>% filter(UniqueCarrier=="WN") %>% 
  summarise(Min_Time=min(AirTime,na.rm = T))
View(flight1)

#Data visualization
house<-read.csv("D:/Data Science/houses.csv")
View(house)
house<-house %>% select(c(-1,-2))
View(house)
house<-house%>% mutate(waterfront=ifelse(waterfront==0,"No","Yes"))
house<-house%>% mutate(construction=ifelse(construction==0,"No","Yes"))
house<-house%>% mutate(air_cond=ifelse(air_cond==0,"No","Yes"))
View(house)
library(ggplot2)
ggplot(data = house,aes(x=price))+geom_histogram()
ggplot(data = house,aes(x=price))+geom_histogram(fill="palegreen4",col="green")
ggplot(data = house,aes(x=air_cond))+geom_bar(fill="orange")
ggplot(data = house,aes(y=price,x=living_area,col=factor(rooms)))+geom_point()
ggplot(data = house,aes(y=price,x=factor(rooms),fill=factor(rooms)))+geom_boxplot()

#Supervised learning

#Linear regression
View(mtcars)
library(caTools)
mysplit<-sample.split(mtcars$mpg,SplitRatio = 0.65)
train<-subset(mtcars,mysplit==T)
test<-subset(mtcars,mysplit==F)
nrow(train)
View(train)
nrow(test)
mod1=lm(mpg~.,data = train)
result<-predict(mod1,test)
View(result)
final<-cbind(actual=test$mpg,predicted=result)
View(final)
final<-as.data.frame(final)
View(final)
final<-cbind(final,error=final$actual-final$predicted)
View(final)
