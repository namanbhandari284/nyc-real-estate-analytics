library(tidyverse)
library(data.table)
library(lubridate)


nycdt <- fread("NYC_TRANSACTION_DATA.csv",header=TRUE)
nycnbrhd <- fread("NEIGHBORHOOD.csv",header = TRUE)
nycbldcls <- fread("BUILDING_CLASS.csv",header = TRUE)
nycbrgh <- fread("BOROUGH.csv",header = TRUE)


DJ <- left_join(nycdt, nycnbrhd, by= "NEIGHBORHOOD_ID")%>%
  left_join(y=nycbrgh, by="BOROUGH_ID")%>%
  inner_join(y=nycbldcls, by=c("BUILDING_CLASS_FINAL_ROLL"="BUILDING_CODE_ID")) 


DJF <- DJ%>%
  filter(TYPE=="RESIDENTIAL" , SALE_PRICE>=25000 & BOROUGH_ID== 2, GROSS_SQUARE_FEET != 0)

DJFD <- DJF%>%
  mutate(SALE_YEAR = as.integer(substr(SALE_DATE, 1, 4))
  )


DJFD2 <- DJFD %>%
  mutate(SALE_PRICE = as.double(SALE_PRICE),
         GROSS_SQUARE_FEET = as.double(GROSS_SQUARE_FEET),
         PRICEPERSF = SALE_PRICE / GROSS_SQUARE_FEET) %>%
  mutate(REVENUE = SALE_PRICE * 0.15 * 0.05)


K1<- DJFD2%>%
  group_by(SALE_YEAR,NEIGHBORHOOD_NAME)%>%
  summarise(median(SALE_PRICE),sum(REVENUE), )


K2<- DJFD2%>%
  group_by(SALE_YEAR,NEIGHBORHOOD_NAME)%>%
  summarise(SALES_COUNT=n())

K3 <- DJFD2%>%
  group_by(SALE_YEAR,NEIGHBORHOOD_NAME)%>%
  summarise(sd(SALE_PRICE))

K4 <- DJFD2%>%
  group_by(SALE_YEAR,NEIGHBORHOOD_NAME)%>%
  summarise(sd(PRICEPERSF))

K5 <- DJFD2%>%
  group_by(SALE_YEAR,NEIGHBORHOOD_NAME)%>%
  summarise(mean(SALE_PRICE))

K6<- K5%>%
  group_by(NEIGHBORHOOD_NAME)%>%
  arrange(SALE_YEAR,NEIGHBORHOOD_NAME)%>%
  mutate(SALE_GROWTH=(`mean(SALE_PRICE)`-lag(`mean(SALE_PRICE)`))/lag(`mean(SALE_PRICE)`))


GROWTHBYYEAR_NHBRHD <- K6 %>%
  filter(NEIGHBORHOOD_NAME == "BATHGATE" | NEIGHBORHOOD_NAME == "BAYCHESTER")

ggplot(GROWTHBYYEAR_NHBRHD,aes(x=SALE_YEAR,y=`SALE_GROWTH`,color=NEIGHBORHOOD_NAME))+
  geom_bar(stat="identity")

df<-merge(K1,K2,by=c("NEIGHBORHOOD_NAME","SALE_YEAR"))

df1<-merge(df,K3,by=c("NEIGHBORHOOD_NAME","SALE_YEAR"))

df2<-merge(df1,K4,by=c("NEIGHBORHOOD_NAME","SALE_YEAR"))

df3<-merge(df2,K5,by=c("NEIGHBORHOOD_NAME","SALE_YEAR"))

df4<-merge(df3,K6,by=c("NEIGHBORHOOD_NAME","SALE_YEAR"))



Final<- na.omit(df4)

Finalscaled<-scale(Final[,-(1:2)])

set.seed(1112023)

library(cluster)
kmeansscaled<-kmeans(Finalscaled,center=4)

final_cluster<-Final%>%
  mutate(cluster=kmeansscaled$cluster)



install.packages("corrplot")
library(corrplot)


correlation_matrix<-cor(final_cluster[,-(1:2)])

corrplot(correlation_matrix,method="color",type="upper")

ggplot(final_cluster,aes(x=`sd(SALE_PRICE)`,y=`mean(SALE_PRICE).x`,color=cluster))+
  geom_point(size=3)