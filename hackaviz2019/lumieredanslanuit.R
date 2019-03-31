library(plyr)
library(ggplot2)
library(maptools)
library(gganimate)
library(geosphere)

options(stringsAsFactors = F)
com <- read.csv("par_commune.csv",h=T,sep=",",na.strings = "")
traj <- read.csv("par_trajet.csv",h=T,sep=",",na.strings = "")
com <- com[com$insee%in%traj$insee | com$insee%in%traj$travail_insee,]
dest.xy <- traj[,names(traj)%in%c("commune","travail_commune","insee",
                            
                                  "X2009_extra_travail_commune","insee","latitude","longitude",
                                  
                                  "travail_insee","travail_latitude","travail_longitude",
                                  
                                  "duree_auto_minutes","distance_auto_km")]

names(dest.xy)<- c("o_name","origin","d_name", "destination","oY", "oX", "dY", "dX","dist","duree","trips")
dest.xy <- dest.xy[dest.xy$d_name!=0,]
dest.xy <- dest.xy[dest.xy$dY<46,]
dest.xy <- dest.xy[dest.xy$trips!=0,]
dest.xy <- dest.xy[order(-dest.xy$duree),]
dest.xy <- dest.xy[-1,]

## Bordeau  pas en Occitanie!
dest.xy$quand <-max(dest.xy$duree)-dest.xy$duree
dest.xy$heure <- as.POSIXct("9:00:00",format="%H:%M:%S")
dest.xy$heure <- dest.xy$heure-dest.xy$duree*60
xquiet<- scale_x_continuous("", breaks=NULL)
yquiet<-scale_y_continuous("", breaks=NULL)
quiet<-list(xquiet, yquiet)
p <- ggplot(dest.xy, aes(oX, oY))+
    #The next line tells ggplot that we wish to plot line segments. The "alpha=" is line transparency and used below
    geom_segment(aes(x=oX, y=oY,xend=dX, yend=dY, alpha=trips), col="white")+
    #Here is the magic bit that sets line transparency - essential to make the plot readable
    scale_alpha_continuous(range = c(0.3, 0.6))+ ## A voir!
    #Set black background, ditch axes and fix aspect ratio
    theme(panel.background = element_rect(fill='000033',colour='black'),legend.position = "none")+quiet

dest.xy <- traj[,names(traj)%in%c("commune","travail_commune","insee",
                                  
                                  "X2014_extra_travail_commune","insee","latitude","longitude",
                                  
                                  "travail_insee","travail_latitude","travail_longitude",
                                  
                                  "duree_auto_minutes","distance_auto_km")]

names(dest.xy)<- c("o_name","origin","d_name", "destination","oY", "oX", "dY", "dX","dist","duree","trips")
dest.xy <- dest.xy[dest.xy$d_name!=0,]
dest.xy <- dest.xy[dest.xy$dY<46,]
dest.xy <- dest.xy[dest.xy$trips!=0,]
dest.xy <- dest.xy[order(-dest.xy$duree),]
dest.xy <- dest.xy[-c(2:3),]

## Bordeau pas en Occitanie!
dest.xy$quand <-max(dest.xy$duree)-dest.xy$duree
dest.xy$heure <- as.POSIXct("9:00:00",format="%H:%M:%S")
dest.xy$heure <- dest.xy$heure-dest.xy$duree*60
xquiet<- scale_x_continuous("", breaks=NULL)
yquiet<-scale_y_continuous("", breaks=NULL)
quiet<-list(xquiet, yquiet)

save(quiet, dest.xy, file = "lumiere_dans_la_nuit.RData")

p1 <- ggplot(dest.xy, aes(oX, oY))+
    geom_segment(aes(x=oX, y=oY,xend=dX, yend=dY, alpha=trips), col="white")+
    scale_alpha_continuous(range = c(0.1, 0.4))+ ## A voir!
    theme(panel.background = element_rect(fill='#0b0b31',colour='#0b0b31'),legend.position = "none") + quiet
