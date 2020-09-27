-------------------------------------------------------------------------------
  # Master of Science in Artificial Intelligence
  # Authors: Guillermo López
  # Date: 2020, September 27th
------------------------------------------------------------------------------

# required libraries
library(RCurl)
library(jsonlite)
library(dplyr)

# downloading information from TransMilenio open data repository
paraderos_request<-"https://opendata.arcgis.com/datasets/70b111e96b514bdfb36a7eb532d0eb4f_1.geojson"
paraderos<-fromJSON(getURL(paraderos_request))$feature$properties %>%
  select(c("cenefa_paradero","longitud_paradero","latitud_paradero","localidad_paradero")) %>%
  mutate(longitud_paradero=longitud_paradero*10) %>%
  mutate(latitud_paradero=latitud_paradero/1000)
paraderos$latitud_paradero[paraderos$latitud_paradero<=0]<-4.5974502
plot(paraderos$latitud_paradero,paraderos$longitud_paradero,col=paraderos$localidad_paradero)
# we will use the dates from February 10th to February 16th
dates<-seq(10,16)
url_part1<-"https://storage.googleapis.com/validaciones_tmsa/validacionZonal/validacionZonal202002"
url_part2<-".csv"
db<-c()
for(i in dates){
  url_request<-paste0(url_part1,i,url_part2)
  tmp<-read.csv2(url_request) %>%
    # analyzed card will be those with any benefit to the fee
    filter(nombreperfil=="(006) Apoyo Ciudadano" |
              nombreperfil=="(002) Adulto Mayor" |
              nombreperfil=="(005) Discapacidad" |
              nombreperfil=="(101) Adulto PV")
  db<-rbind(db,tmp)
}
# removing objects that we will not use further
rm(tmp,dates,i,paraderos_request,url_part1,url_part2,url_request)
# reformatting dates variables
db2<-mutate(db,timestamp=paste0(fechatransaccion,horatransaccion)) %>%
  mutate(fecha=strptime(fechatransaccion,"%Y%m%d")) %>%
  mutate(fecha_hora=strptime(timestamp,"%Y%m%d%T")) %>%
  select(c("fecha","fecha_hora","nombrelinea","parada","recaudovehiculo",
           "numerotarjeta","valor"))
# merging travels information with bus station
db3<-merge(db2,paraderos,by.x="parada",by.y="cenefa_paradero",all.x=T,sort=F)
db4<-db3[order(db3$fecha_hora),] %>%
  group_by(numerotarjeta,fecha) %>%
  mutate(viaje=row_number()) %>%
  # last dates when card was used
  mutate(fecha_hora_m1=lag(fecha_hora,order_by=fecha_hora)) %>%
  mutate(fecha_hora_m2=lag(fecha_hora,2L,order_by=fecha_hora)) %>%
  mutate(fecha_hora_m3=lag(fecha_hora,3L,order_by=fecha_hora)) %>%
  mutate(fecha_hora_m4=lag(fecha_hora,4L,order_by=fecha_hora)) %>%
  mutate(fecha_hora_m5=lag(fecha_hora,5L,order_by=fecha_hora)) %>%
  # last bus stations where card was used
  mutate(parada_m1=lag(parada,order_by=fecha_hora)) %>%
  mutate(parada_m2=lag(parada,2L,order_by=fecha_hora)) %>%
  mutate(parada_m3=lag(parada,3L,order_by=fecha_hora)) %>%
  mutate(parada_m4=lag(parada,4L,order_by=fecha_hora)) %>%
  mutate(parada_m5=lag(parada,5L,order_by=fecha_hora)) %>%
  # last vehicles (bus) where card was used
  mutate(recaudovehiculo_m1=lag(recaudovehiculo,order_by=fecha_hora)) %>%
  mutate(recaudovehiculo_m2=lag(recaudovehiculo,2L,order_by=fecha_hora)) %>%
  mutate(recaudovehiculo_m3=lag(recaudovehiculo,3L,order_by=fecha_hora)) %>%
  mutate(recaudovehiculo_m4=lag(recaudovehiculo,4L,order_by=fecha_hora)) %>%
  mutate(recaudovehiculo_m5=lag(recaudovehiculo,5L,order_by=fecha_hora)) %>%
  # coordinates of last bus stations where card was used
  mutate(longitud_paradero_m1=lag(longitud_paradero,order_by=fecha_hora)) %>%
  mutate(longitud_paradero_m2=lag(longitud_paradero,2L,order_by=fecha_hora)) %>%
  mutate(longitud_paradero_m3=lag(longitud_paradero,3L,order_by=fecha_hora)) %>%
  mutate(longitud_paradero_m4=lag(longitud_paradero,4L,order_by=fecha_hora)) %>%
  mutate(longitud_paradero_m5=lag(longitud_paradero,5L,order_by=fecha_hora)) %>%
  mutate(latitud_paradero_m1=lag(latitud_paradero,order_by=fecha_hora)) %>%
  mutate(latitud_paradero_m2=lag(latitud_paradero,2L,order_by=fecha_hora)) %>%
  mutate(latitud_paradero_m3=lag(latitud_paradero,3L,order_by=fecha_hora)) %>%
  mutate(latitud_paradero_m4=lag(latitud_paradero,4L,order_by=fecha_hora)) %>%
  mutate(latitud_paradero_m5=lag(latitud_paradero,5L,order_by=fecha_hora)) %>%
  # lane information of last card usages
  mutate(linea_m1=lag(nombrelinea,order_by=fecha_hora)) %>%
  mutate(linea_m2=lag(nombrelinea,2L,order_by=fecha_hora)) %>%
  mutate(linea_m3=lag(nombrelinea,3L,order_by=fecha_hora)) %>%
  mutate(linea_m4=lag(nombrelinea,4L,order_by=fecha_hora)) %>%
  mutate(linea_m5=lag(nombrelinea,5L,order_by=fecha_hora)) %>%
  data.frame()
# how long did it take between each use?
db5<-mutate(db4,tiempo_segundos_m1=fecha_hora-fecha_hora_m1,
            tiempo_segundos_m2=fecha_hora_m1-fecha_hora_m2,
            tiempo_segundos_m3=fecha_hora_m2-fecha_hora_m3,
            tiempo_segundos_m4=fecha_hora_m3-fecha_hora_m4,
            tiempo_segundos_m5=fecha_hora_m4-fecha_hora_m5,
            # is the card being used in the same bus station?
            misma_parada_m1=parada==parada_m1,
            misma_parada_m2=parada_m1==parada_m2,
            misma_parada_m3=parada_m2==parada_m3,
            misma_parada_m4=parada_m3==parada_m4,
            misma_parada_m5=parada_m4==parada_m5,
            # is the card being used in the same lane?
            misma_linea_m1=nombrelinea==linea_m1,
            misma_linea_m2=linea_m1==linea_m2,
            misma_linea_m3=linea_m2==linea_m3,
            misma_linea_m4=linea_m3==linea_m4,
            misma_linea_m5=linea_m4==linea_m5,
            # is the card being used in the same vehicle?
            mismo_vehiculo_m1=recaudovehiculo==recaudovehiculo_m1,
            mismo_vehiculo_m2=recaudovehiculo_m1==recaudovehiculo_m2,
            mismo_vehiculo_m3=recaudovehiculo_m2==recaudovehiculo_m3,
            mismo_vehiculo_m4=recaudovehiculo_m3==recaudovehiculo_m4,
            mismo_vehiculo_m5=recaudovehiculo_m4==recaudovehiculo_m5
            )

# function to calculate distance between bus stations due the coordinates
harvesian_distance<-function(lat1,lon1,lat2,lon2){
  r<-6.371e3
  lat1<-lat1*pi/180
  lat2<-lat2*pi/180
  delta_lat<-lat2-lat1
  delta_lon<-(lon2-lon1)*pi/180
  harv<-sin(delta_lat/2)^2+cos(lat1)*cos(lat2)*sin(delta_lon/2)^2
  harv<-2*r*asin(sqrt(harv))
  return(harv)
}

# distance between bus stations
db5$distancia_kilometros_m1<-harvesian_distance(db5$latitud_paradero_m1,
  db5$longitud_paradero_m1,db5$latitud_paradero,db5$longitud_paradero)
db5$distancia_kilometros_m2<-harvesian_distance(db5$latitud_paradero_m2,
    db5$longitud_paradero_m2,db5$latitud_paradero_m1,db5$longitud_paradero_m1)
db5$distancia_kilometros_m3<-harvesian_distance(db5$latitud_paradero_m3,
    db5$longitud_paradero_m3,db5$latitud_paradero_m2,db5$longitud_paradero_m2)
db5$distancia_kilometros_m4<-harvesian_distance(db5$latitud_paradero_m4,
    db5$longitud_paradero_m4,db5$latitud_paradero_m3,db5$longitud_paradero_m3)
db5$distancia_kilometros_m5<-harvesian_distance(db5$latitud_paradero_m5,
    db5$longitud_paradero_m5,db5$latitud_paradero_m4,db5$longitud_paradero_m4)
db5<-mutate(db5,row_id=row_number())
# removing variables
db6<-db5 %>%
  select(starts_with("viaje") | starts_with("valor") | starts_with("row_id") |
           starts_with("tiempo_") | starts_with("mism") |
           starts_with("distancia_")) %>%
  filter(viaje>1)
# ensuring reproducibility
set.seed(1234)
sample_vector<-runif(dim(db6)[1])
# output file to be use in RapidMiner
write.table(db6,"sample_100.csv",na="",sep=",",row.names=F)
# smaller sample to quick analysis if requested
db6[sample_vector>=0.995,] %>%
  write.table("sample_05.csv",na="",sep=",",row.names=F)
