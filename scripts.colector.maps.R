# Script.colector.map
# Make Mat&Met Lines Maps  (Lineas_MAP)
# And show recruit bubble plot on satellite map (tasas_totales_por_linea_MAPS)

#Depend Packages
library(ggmap)
library(ggrepel)

tasas.lineas <- print(summarySE(na.omit(long.data), measurevar="TASA", 
                                groupvars=c("LINEA"), 
                                na.rm=TRUE))


#Methods maps 
#Lines Coords lat and long
tasas.lineas$lat <-
  ifelse(tasas.lineas$LINEA == "LN", -26.513996,
         ifelse(tasas.lineas$LINEA == "L3", -26.531829,
                ifelse(tasas.lineas$LINEA == "L2", -26.535019,
                       ifelse(tasas.lineas$LINEA == "LT", -26.537381,
                              ifelse(tasas.lineas$LINEA == "L1", -26.539032, "NA")))))

tasas.lineas$lon <-
  ifelse(tasas.lineas$LINEA == "LN", -70.708479,
         ifelse(tasas.lineas$LINEA == "L3", -70.707268,
                ifelse(tasas.lineas$LINEA == "L2", -70.700170,
                       ifelse(tasas.lineas$LINEA == "LT", -70.701179,
                              ifelse(tasas.lineas$LINEA == "L1", -70.702076, "NA")))))
tasas.lineas$lat<-as.numeric(tasas.lineas$lat)
tasas.lineas$lon<-as.numeric(tasas.lineas$lon)


anglomap <- get_map(location = c(lon = -70.698903, lat = -26.528201), zoom = 14, maptype = "satellite")


tasas.lineas$TYPE<-c("ControlN", "AMERBS", "ACCABIER", "ACCABIER", "AMERBS")
bahia<-data.frame(lon = -70.711927, lat = -26.5325)
flamenco<-data.frame(lon=-70.708006, lat=-26.528320)
rbaja<-data.frame(lon=-70.700724, lat=-26.542542)
control<-data.frame(lon=-70.703928, lat=-26.513833)

matmet_mapa<-ggmap(anglomap, legend = "topright") +
  geom_point(aes(x=lon,y=lat, color=LINEA), data=tasas.lineas, alpha = 1/1)+
  scale_size(range=c(5,10))+
  geom_text_repel(aes(x=lon,y=lat, label=LINEA), color="white",
                  data=tasas.lineas, fontface = "bold", segment.size=0, nudge_x = -.0015)+
  geom_text_repel(aes(x=lon,y=lat, label="Ba.Corral de los Chanchos"), color="black",
                  data=bahia, fontface = "italic", segment.size=0, nudge_x = .027)+
  geom_text_repel(aes(x=lon,y=lat, label="AMERB Pta. Flamenco"), color="black",
                  data=flamenco, fontface = "italic", segment.size=0, nudge_x = .010)+
  geom_text_repel(aes(x=lon,y=lat, label="AMERB Pta. Roca Baja"), color="black",
                  data=rbaja, fontface = "italic", segment.size=0, nudge_x = .009)+
  geom_text_repel(aes(x=lon,y=lat, label="Línea Control Norte"), color="black",
                  data=control, fontface = "italic", segment.size=0, nudge_x = .009)+
  scale_color_discrete(name = "TYPE") +
  
  labs(x = "Longitud (O)", y = "Latitud (S)")


tiff(file.path(campaña.IMG.dir,
               paste("Lineas_MAP", ".tiff", sep = "")),
     width = 1300, height = 1300, res = 220, compression = "lzw")
matmet_mapa
dev.off()

#Result Maps
lineas_maps <-
ggmap(anglomap, legend = "topright") +
  geom_point(aes(x=lon,y=lat, size=TASA, color=LINEA), data=tasas.lineas, alpha = 1/2)+
#   geom_errorbarh(data=tasas.lineas, aes(x=lon,y=lat, xmin=TASA, xmax=TASA), 
#                 color="white")+
  scale_size(range=c(5,10))+
  geom_text_repel(aes(x=lon,y=lat, label=LINEA), color="white",
                  data=tasas.lineas, fontface = "bold", segment.size=0)+
  labs(x = "Longitud (O)", y = "Latitud (S)")


tiff(file.path(campaña.IMG.dir,
               paste("tasas_totales_por_linea_MAPS", ".tiff", sep = "")),
     width = 1300, height = 1300, res = 200, compression = "lzw")
lineas_maps
dev.off()

