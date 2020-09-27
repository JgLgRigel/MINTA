library(dplyr)
library(ggplot2)

df<-read.csv("Downloads/sample_05.csv")
df2<-read.csv("Downloads/sample_05_SOM.csv")
tots<-c()
set.seed(1234)
for(i in 1:10){
  cluster<-select(df2,contains('SOM')) %>%
    kmeans(i)
  tots<-c(tots,cluster$tot.withinss)
}
plot(tots,type="l")

df3<-read.csv("Downloads/sample_05_cluster.csv")
df4<-group_by(SOM_0,SOM_1,cluster) %>%
  summarize(freq=n())

ggplot(df3,aes(x=SOM_0,y=SOM_1,color=cluster,size=freq)) + geom_point(alpha=0.7)

df5<-merge(df,df3,by='row_id',sort=F) #%>%
  group_by(cluster) %>%
  summarize(viaje=mean(viaje))
       