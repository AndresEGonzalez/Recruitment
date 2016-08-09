# script.Anglo.total.recruit.R
# Calcula el reclutamiento total

#Tasas de Reclutamiento Totales marca estacionalidad====

# Tasas de Reclutamiento Totales por Campaña de Monitoreo # Tasas totales
d_total <- print(summarySE((long.data),
                           measurevar="TASA", groupvars=c("MES"),
                           na.rm=TRUE))

estacionalidad <- data.frame(ESTACION = c("Primavera", "Verano", "Otoño", "Invierno", "Primavera", "Verano", "Otoño", "Invierno"),
                             INICIO = as.Date(c("21/09/2014", "21/12/2014", "21/03/2015",
                                                "21/06/2015", "21/09/2015", "21/12/2015", "21/03/2016", "21/06/2016"), "%d/%m/%Y"),
                             TERMINO =as.Date(c("20/12/2014", "20/03/2015", "20/06/2015",
                                                "20/09/2015", "20/12/2015", "20/03/2016", "20/06/2016", "20/09/2016"), "%d/%m/%Y"))

estacionalidad$ESTACION <- factor(estacionalidad$ESTACION, 
                                  levels = c("Primavera", "Verano", "Otoño", "Invierno"),
                                  labels = c("Primavera", "Verano", "Otoño", "Invierno"))

fecha_mean <- (as.Date(long.data$INSTALADO, "%d/%m/%Y") + long.data$PERMANENCIA/2)
# is.na(fecha_mean)

unique(long.data$INSTALADO); unique(fecha_mean)


# Tasas de Reclutamiento Totales por Campaña de Monitoreo # Tasas totales
d_total <- print(summarySE((long.data),
                           measurevar="TASA", groupvars=c("MES"),
                           na.rm=TRUE))
d_total$fecha_mean <-unique(fecha_mean)

library(scales)
plot.tasas.campañas <-
  ggplot() +
  # rectangulos de colores para las estaciones del año
  geom_rect(data = estacionalidad, aes(xmin = INICIO, xmax = TERMINO, 
                                       ymin = -Inf, ymax = Inf, 
                                       fill = ESTACION), alpha = 0.2)+
  # gradiente de color para las estaciones
  scale_fill_manual(values=c("#66CC99", "#CC6666", "#FFCC66", "#3333FF"))+
  # plor barras con las tasas de reclutamiento
  geom_bar(data = d_total, aes(x = fecha_mean, y= TASA, ymax=TASA), 
           position=position_dodge(.9), colour="black", stat="identity")+
  # plot barras de error en base al ES
  geom_errorbar(data = d_total, aes(x =fecha_mean, ymin = TASA, ymax= TASA + se),
                position=position_dodge(.9), width=.25)+
  # punto medio blanco de las barras
  geom_point(data = d_total, aes(x = fecha_mean, y= TASA),
             position=pd, size=2, shape=21, fill="white") +
  # borra el background pero mantiene la grilla
  theme_bw() +
  labs(x = "", y = "Tasa reclutamiento"~(ind.col^-1~dia^-1))+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  # suprime el titulo de la leyenda
  theme(legend.title=element_blank())+
  # cambia labels x axis usa B para el nombre completo
  # scale_x_date(breaks = "2 month", labels=date_format("%b-%y"))
  scale_x_date("Campañas", breaks = d_total$fecha_mean, labels=date_format("%b-%y"))

# #   scale_x_continuous(labels=c("Campaña 1", "Campaña 2", "Campaña 3", 
# #                             "Campaña 4", "Campaña 5", "Campaña 6"))  
# #   
# scale_x_continuous(breaks=c(d_total$fecha_mean), 
#                    limits
#                    labels = c("Campaña 1", "Campaña 2", "Campaña 3", 
#                               "Campaña 4", "Campaña 5", "Campaña 6"))
# 
# scale_x_continuous(breaks= pretty_breaks())



tiff(file.path(campaña.IMG.dir,
               paste("tasas_totales_por_campaña_2", ".tiff", sep = "")),
     width = 1300, height = 1000, res = 280, compression = "lzw")
plot.tasas.campañas
dev.off()

# plot.tasas.campañas <-
#   ggplot(d_total, aes(x = MES, y= TASA, ymax=(TASA)))+
#   geom_bar(position=position_dodge(.9), colour="black", stat="identity")+
#   geom_errorbar(position=position_dodge(.9), width=.25, aes(ymin = TASA, ymax= TASA + se))+
#   geom_point(position=pd, size=2, shape=21, fill="white") +
#   theme_bw() +
#   labs(x = "", y = "Tasa reclutamiento"~(ind.col^-1~dia^-1))+
#   theme(axis.text.x = element_text(angle = 45, hjust = 1))+
#   theme(strip.text.x = element_text(size = 12, face="italic"))+
#   theme(strip.text = element_text(face = "italic"))
# 
# tiff(file.path(campaña.IMG.dir,
#                paste("tasas_totales_por_campaña", ".tiff", sep = "")),
#      width = 1300, height = 1000, res = 280, compression = "lzw")
# plot.tasas.campañas
# dev.off()



# Tasas de Reclutamiento por Línea de Monitoreo ====

# Tasas promedio total lineas todas las campañas
arrange(aggregate(long.data$TASA, list(long.data$LINEA), mean, na.rm = TRUE), desc(x))

L.mean <-aggregate(long.data$TASA, list(long.data$LINEA), mean, na.rm = TRUE)
L.sd <-aggregate(long.data$TASA, list(long.data$LINEA), sd, na.rm = TRUE)
data.frame("mean" = L.mean, "SD"=L.sd[,2])

d_total_linea <- print( summarySE(long.data,
                                  measurevar="TASA", 
                                  groupvars=c("MES","LINEA"), 
                                  na.rm=TRUE))
plot.linea <-
  ggplot(d_total_linea, aes(x = MES, y= (TASA), ymax=max(TASA), colour= LINEA,  group= LINEA))+ 
  geom_errorbar(aes(ymin=TASA-se, ymax=TASA+se), colour="black", width=.1, position=pd) +
  geom_line(position=pd) +
  geom_point(position=pd, size=2, shape=21, fill="white") +  
  # facet_wrap(~ LINEA, scales="fixed") +
  theme_bw() +
  labs(x = "", y = "Tasa reclutamiento"~(ind.col^-1~dia^-1))+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  theme(strip.text.x = element_text(size = 12, face="bold"))+
  theme(strip.text = element_text(face = "bold"))#+
# theme(legend.position="none")

tiff(file.path(campaña.IMG.dir,
               paste("tasas_totales_por_linea", ".tiff", sep = "")),
     width = 1300, height = 1000, res = 280, compression = "lzw")
plot.linea 
dev.off()

# #  Tasa lineas Taxa
# d_tasa_taxa_linea <- 
#   print(arrange( summarySE(long.data,
#                            measurevar="TASA", 
#                            groupvars=c("MES","TAXA", "LINEA"), 
#                            na.rm=TRUE), MES, desc(TASA))  )
# 
# plot.tasa.linea.taxa <-
#   ggplot(d_tasa_taxa_linea, aes(x = MES, y= TASA, colour= LINEA,  group= LINEA))+ 
#   geom_errorbar(aes(ymin=TASA - se, ymax=TASA + se), colour="black", width=.1, position=pd) +
#   geom_line(position=pd) +
#   geom_point(position=pd, size=3, shape=21, fill="white") +  
#   theme_bw() +
#   facet_wrap(~ TAXA, scales="free") +
#   labs(x = "CAMPAÑA", y = "Tasa reclutamiento"~(ind.col^-1~dia^-1))+
#   theme(axis.text.x = element_text(angle = 45, hjust = 1))+
#   theme(strip.text.x = element_text(size = 12, face="italic"))+
#   theme(strip.text = element_text(face = "italic"))
# 
# 
# tiff("tasas_totales_por_linea_taxa.tiff", 
#      width = 1900, height = 1900, res = 300, compression = "lzw")
# plot.tasa.linea.taxa 
# dev.off()

# TASAS TOTALES POR ESTRATO DE PROFUNDIDAD======

# Tasas promedio total campañas
arrange(aggregate(long.data$TASA, list(long.data$PROFUNDIDAD), mean, na.rm = TRUE), desc(x))

P.mean <-aggregate(long.data$TASA, list(long.data$PROFUNDIDAD), mean, na.rm = TRUE)
P.sd <-aggregate(long.data$TASA, list(long.data$PROFUNDIDAD), sd, na.rm = TRUE)
data.frame("mean" = P.mean, "SD"=P.sd[,2])

# tasas profundidad por campaña
d_total_estrato_profund <-print( summarySE(long.data, 
                                           measurevar="TASA", groupvars=c("MES","PROFUNDIDAD"), 
                                           na.rm=TRUE))


plot.prof <-
  # ggplot(long.data, aes(x = MES, y= TASA, fill=PROFUNDIDAD))+ 
ggplot(d_total_estrato_profund, aes(x = MES, y= TASA, colour= PROFUNDIDAD,  group= PROFUNDIDAD))+ 
geom_errorbar(aes(ymin= TASA-se, ymax= TASA+se), colour="black", width=.1, position=pd) +
  geom_line(position=pd) +
  geom_point(position=pd, size=2, shape=21, fill="white") +  
  # geom_boxplot()+
  theme_bw() +
  labs(x = "CAMPAÑAS", y = "Tasa reclutamiento"~(ind.col^-1~dia^-1))+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
#   theme(strip.text.x = element_text(size = 12, face="italic"))+
#   theme(strip.text = element_text(face = "italic"))

tiff(file.path(campaña.IMG.dir,
               paste("tasas_totales_por_profundidad", ".tiff", sep = "")),
     width = 1300, height = 1000, res = 280, compression = "lzw")
plot.prof 
dev.off()

# TASA TOTAL POR COLECTOR ====

# Tasas promedio total campañas por colector
arrange(aggregate(long.data$TASA, list(long.data$COLECTOR), mean, na.rm = TRUE), desc(x))

C.mean <-aggregate(long.data$TASA, list(long.data$COLECTOR), mean, na.rm = TRUE)
C.sd <-aggregate(long.data$TASA, list(long.data$COLECTOR), sd, na.rm = TRUE)
data.frame("mean" = C.mean, "SD"=C.sd[,2])



# Total colector
d_total_colector <- print(summarySE(long.data, 
                                    measurevar="TASA", groupvars=c("MES","COLECTOR"), 
                                    na.rm=TRUE))

plot.colector.total <-
  ggplot(d_total_colector, aes(x = MES, y= (TASA), colour= COLECTOR,  group= COLECTOR))+ 
  geom_errorbar(aes(ymin=TASA-se, ymax=TASA+se), colour="black", width=.1, position=pd) +
  geom_line(position=pd) +
  geom_point(position=pd, size=2, shape=21, fill="white") +  
  theme_bw() +
  labs(x = "CAMPAÑAS", y = "Tasa reclutamiento"~(ind.col^-1~dia^-1))+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  theme(strip.text.x = element_text(size = 12, face="italic"))+
  theme(strip.text = element_text(face = "italic"))


tiff(file.path(campaña.IMG.dir,
               paste("tasas_totales_por_colector", ".tiff", sep = "")),
     width = 1300, height = 1000, res = 280, compression = "lzw")
plot.colector.total 
dev.off()

# Tasas de Reclutamiento Total por Grupo Taxonómico ====
# Tasas promedio total campañas por TAXA
arrange(aggregate(long.data$TASA, list(long.data$TAXA), mean, na.rm = TRUE), desc(x))

Tax.mean <-aggregate(long.data$TASA, list(long.data$TAXA), mean, na.rm = TRUE)
Tax.sd <-aggregate(long.data$TASA, list(long.data$TAXA), sd, na.rm = TRUE)
data.frame("mean" = Tax.mean, "SD"=Tax.sd[,2])



total_taxa <-print(summarySE(long.data, measurevar="TASA", 
                             groupvars=c("MES","TAXA"), 
                             na.rm=TRUE))

arrange(total_taxa, TAXA)

# PLOT 
plot.total.taxa <-
#   ggplot(total_taxa, aes(x=TAXA, y=TASA, ymax=max(TASA), group=MES, fill=TAXA))+
#   geom_bar(position=position_dodge(.9), colour="black", stat="identity")+
#   geom_errorbar(position=position_dodge(.9), width=.25, aes(ymin = TASA , ymax= TASA + se))+
#   geom_point(position=position_dodge(.9), size=2, shape=21, fill="white")+
#   facet_wrap(~ MES, scales="fixed", ncol=2) +
#   theme_bw() +
#   theme(axis.text.x = element_text(angle = 45, hjust = 1))+
#   labs(x = "Grupo Taxonómico", y = "Tasa reclutamiento"~(ind.col^-1~dia^-1))+
#   theme(strip.text.x = element_text(size = 12, face = "bold"))

#   plot.total.taxa + annotate("text", label = c("*","","*","*"), x = c(1:4), y = 5, size = 8)  
#   
#   geom_text(data = NULL, x=c(1:4), y=7, label=c("*","","*","*"), size =6)+
#   geom_text(data = NULL, x=c(1:4), y=2, label=c("*","","*","*"), size =6)


ggplot(total_taxa, aes(x = MES, y= TASA, ymax=max(TASA), colour= TAXA,  group= TAXA))+ 
  geom_errorbar(aes(ymin=TASA - se, ymax=TASA + se), colour="black", width=.1, position=pd) +
  geom_line(position=pd) +
  geom_point(position=pd, size=2, shape=21, fill="white") +  
  theme_bw() +
  # facet_wrap(~ TAXA, scales="fixed") +
  labs(x = "CAMPAÑAS", y = "Tasa reclutamiento"~(ind.col^-1~dia^-1))+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  theme(strip.text.x = element_text(size = 12, face="italic"))+
  theme(strip.text = element_text(face = "italic"))

tiff(file.path(campaña.IMG.dir,
               paste("tasa_total_taxa", ".tiff", sep = "")),
     width = 1500, height = 1200, res = 280, compression = "lzw")
plot.total.taxa
dev.off()