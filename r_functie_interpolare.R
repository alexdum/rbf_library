setwd("~/Documents/clima/2014/rbf_library")
d<-read.csv("annual_all_2013.csv")
sub<-subset(d,Parameter.Name %in% c("PM2.5 - Local Conditions", "Ozone")
            & Pollutant.Standard %in% c("Ozone 8-Hour 2008", "PM25 Annual 2006"),
            c(Longitude, Latitude,Parameter.Name,Arithmetic.Mean))
pollavg<-aggregate(sub[,"Arithmetic.Mean"],sub[, c("Longitude","Latitude","Parameter.Name")],mean,na.rm=T)
head(pollavg)

names(pollavg)[4]<-"level"
pollavg<-transform(pollavg,Parameter.Name=factor(Parameter.Name))
rm(d,sub)

monitors<-data.matrix(pollavg[,c("Longitude","Latitude")])

library(fields)

pollutant <- function(df){
  x <- data.matrix(df[,c("lon","lat")])
  r <- df$radius
  d <- rdist.earth(monitors,x)
  use<- lapply(seq_len(ncol(d)), function(i) {
    which(d[,i]<r[i])
  })
  
  levels<-sapply(use, function(idx){
    #with(pollavg[idx,], tapply(level,Parameter.Name,mean))
    with(pollavg[idx,], {
      mm<-mean(level)
      ss<-sum(level)
      rbind(mm,ss)
    })
  })
  
  dlevels<-as.data.frame(t(levels))
  data.frame(df,dlevels)  
  }

dd<-pollutant(data.frame(lon=monitors[,"Longitude"],lat=monitors[,"Latitude"],radius=60))

df<-data.frame(lon=c(-76.6,-76),lat=c(39.28,40),radius=40)
