-------------------------------------------------------------------------------
  # Master of Science in Artificial Intelligence
  # Authors: Guillermo López
  # Date: 2020, September 27th
------------------------------------------------------------------------------

library(dplyr)
library(ggplot2)
library(caret)

# reading file from feature_engineering script
db<-read.csv("sample_100.csv")
# reading files from RapidMiner analysis
db2<-read.csv("sample_100_SOM.csv")
db3<-read.csv("sample_100_cluster.csv")
# empty vector
tots<-c()
# ensuring reproducibility
set.seed(1234)
for(i in 1:10){
  cluster<-select(db2,contains('SOM')) %>%
    kmeans(i)
  tots<-c(tots,cluster$tot.withinss)
}
# elbow method plot to choose cluster's number
plot(tots,type="b",xlab='Number of Clusters',
     ylab='Total Sum of Squares Within Clusters',
     main='Distance Evolution Within Clusters')
abline(h=tots[5],lty=2)
# clusters visualization tool
db3 %>%
  group_by(SOM_0,SOM_1,cluster) %>%
  summarize(freq=n()) %>% 
  ggplot(aes(x=SOM_0,y=SOM_1,color=cluster,size=freq)) + geom_point(alpha=0.7)
# clusters characterization
db4<-merge(db,db3,by='row_id',sort=F)
db4 %>%
  group_by(cluster) %>%
  summarize(pro_viaje=mean(viaje),valor_promedio=mean(valor),total_valor=sum(valor),
            tiempo_segundos_m1=mean(tiempo_segundos_m1),
            misma_parada_m1=mean(misma_parada_m1),
            misma_linea_m1=mean(misma_linea_m1),
            mismo_vehiculo_m1=mean(mismo_vehiculo_m1),
            distancia_kilometros_m1=mean(distancia_kilometros_m1,na.rm=T),
            freq=n()) %>%
  data.frame()
# ensuring reproducibility
set.seed(1234)
expert_sample<-createDataPartition(db4$cluster,p=100/dim(df4)[1],list=F)
#creating file to expert validation
db4[expert_sample,] %>%
  write.table("Downloads/expert_sample.csv",sep=",",row.names=F)
