# script.Anglo.multiple.plot.R
# Hace todos los graficos para los grupos taxonomicos analizados


# GRUPOS TAXONOMICOS====
# plots tasas de reclutamiento total por especie y grupo taxonomico ====

# resumen con error estandar (se) de las tasas de reclutamiento por grupo taxonomico 
tasas <- print(summarySE(na.omit(long.data), measurevar="TASA", 
                         groupvars=c("MES", "ESPECIES", "TAXA"), 
                         na.rm=TRUE))

# factores para ordenar las especies en los plot's
tasas$ESPECIES <-
  factor(tasas$ESPECIES,
         levels=c(row.names(biv_arrange), row.names(gas_arrange),row.names(crus_arrange),row.names(equino_arrange)),
         labels=c(row.names(biv_arrange), row.names(gas_arrange),row.names(crus_arrange),row.names(equino_arrange)))


# plot para la tasa de reclutamiento por grupo y especies
plots.tasas.taxa <- lapply(split(tasas, tasas$TAXA), 
                           function(x) 
                             ggplot(x, aes(x = MES, y= TASA, ymax=max(TASA), colour= ESPECIES,  group= ESPECIES))+ 
                             geom_errorbar(aes(ymin=TASA-se, ymax=TASA+se), colour="black", width=.1, position=pd) +
                             geom_line(position=pd) +
                             geom_point(position=pd, size=2, shape=21, fill="white") +  
                             facet_wrap(~ ESPECIES, scales="fixed") +
                             theme_bw() +
                             labs(x = "CAMPAÑAS", y = "Tasa reclutamiento"~(ind.col^-1~dia^-1))+
                             theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8))+
                             theme(strip.text.x = element_text(size = 11, face="italic"))+
                             theme(strip.text = element_text(face = "italic"))+
                             theme(legend.position="none"))

#loop para graficar un archivo por plot
nombres <- c("BIVALVIA", "GASTROPODA", "CRUSTACEA", "EQUINODERMATA")
# dput(unique(tasas$TAXA))
for(i in 1:4){
  
  print(mypath <- file.path(campaña.IMG.dir,
                            paste("tasas_totales_", nombres[i], ".tiff", sep = "")))
  
  tiff(file=mypath, width = 1600, height = 1600, res = 280, compression = "lzw")
  print(plots.tasas.taxa[i])
  dev.off()
}

# tabla PROMEDIO, sd Y ABUNDANCIA:  CAMPAÑA ~ grupo taxonomico====
arrange((ddply(long.data, .(MES, TAXA), summarize, 
               mean = round(mean(TASA, na.rm=TRUE), 2),
               sd = round(sd(TASA, na.rm=TRUE),1),
               n = round(sum(ABUNDANCIA, na.rm=TRUE), 0))),MES, desc(mean))

# PROMEDIO CAMPAÑA ~  taxa (Tabla resumen)
arrange((ddply(na.omit(long.data), .(CAMPAÑA, ESPECIES), summarize, 
               mean = round(mean(TASA, na.rm=TRUE), 2),
               sd = round(sd(TASA, na.rm=TRUE),1),
               n = round(sum(ABUNDANCIA, na.rm=TRUE), 0))), CAMPAÑA, desc(mean))


# plots tasas de reclutamiento por lineas de monitoreo ====
# resumen con error estandar (se) de las tasas de reclutamiento por linea de monitoreo 
tasas.lineas <- print(summarySE(na.omit(long.data), measurevar="TASA", 
                                groupvars=c("LINEA", "TAXA","ESPECIES"), 
                                na.rm=TRUE))
# tasas.lineas <-print(summarySE(long.data, measurevar="TASA", 
#                 groupvars=c("LINEA", "TAXA","ESPECIES")))


# factores para ordenar las especies en los plot's
tasas.lineas$ESPECIES <-
  factor(tasas.lineas$ESPECIES,
         levels=c(row.names(biv_arrange), row.names(gas_arrange),row.names(crus_arrange),row.names(equino_arrange)),
         labels=c(row.names(biv_arrange), row.names(gas_arrange),row.names(crus_arrange),row.names(equino_arrange)))

# plot para la tasa de reclutamiento promedio por grupo, lineas y especies
plots.tasas.lineas <- lapply(split(tasas.lineas, tasas.lineas$TAXA), function(x) 
  ggplot(x, aes(x = LINEA, y= TASA))+
    geom_bar(position=position_dodge(.9), colour="black", stat="identity")+
    geom_errorbar(position=position_dodge(.9), width=.25, aes(ymin = TASA, ymax= TASA + se))+
    geom_point(position=pd, size=2, shape=21, fill="white")+
    facet_wrap(~ ESPECIES, scales="fixed")+
    theme_bw()+
    labs(x = "Líneas de monitoreo", y = "Tasas de reclutamiento"~(ind.col^-1~dia^-1))+
    theme(strip.text.x = element_text(size = 10, face="italic"))+
    theme(strip.text = element_text(face = "italic")))

#nombres para usar en los archivos del plot
nombres <- unique(tasas.lineas$TAXA)
#loop para graficar un archivo por plot
for(i in 1:4){
  
  print(mypath <- file.path(campaña.IMG.dir,
                            paste("tasas_promedio_lineas_", nombres[i], ".tiff", sep = "")))
  
  tiff(file=mypath, width = 1600, height = 1600, res = 280, compression = "lzw")
  print(plots.tasas.lineas[i])
  dev.off()
}



# profundidad promedio ====
# resumen con error estandar (se) de las tasas de reclutamiento por profundidad del colector 
tasas.profundidad <- print(summarySE(na.omit(long.data), measurevar="TASA", 
                                     groupvars=c("PROFUNDIDAD", "TAXA","ESPECIES"), 
                                     na.rm=TRUE))

# factores para ordenar las especies en los plot's
tasas.profundidad$ESPECIES <-
  factor(tasas.profundidad$ESPECIES,
         levels=c(row.names(biv_arrange), row.names(gas_arrange),row.names(crus_arrange),row.names(equino_arrange)),
         labels=c(row.names(biv_arrange), row.names(gas_arrange),row.names(crus_arrange),row.names(equino_arrange)))

# plot para la tasa de reclutamiento promedio por grupo, lineas y especies
plots.tasas.profundidad <- lapply(split(tasas.profundidad, tasas.profundidad$TAXA), function(x) 
  ggplot(x, aes(x = PROFUNDIDAD, y= TASA))+
    geom_bar(position=position_dodge(.9), colour="black", stat="identity")+
    geom_errorbar(position=position_dodge(.9), width=.25, aes(ymin = TASA, ymax= TASA + se))+
    geom_point(position=pd, size=2, shape=21, fill="white") +
    facet_wrap(~ ESPECIES, scales="fixed")+
    theme_bw() +
    labs(x = "Profundidad del colector", y = "Tasa reclutamiento"~(ind.col^-1~dia^-1))+
    theme(strip.text.x = element_text(size = 10, face="italic"))+
    theme(strip.text = element_text(face = "italic")))

#nombres para usar en los archivos del plot
nombres <- unique(tasas.profundidad$TAXA)
#loop para graficar un archivo por plot
for(i in 1:4){
  
  print(mypath <- file.path(campaña.IMG.dir,
                            paste("tasas_promedio_profundidad_", nombres[i], ".tiff", sep = "")))
  
  tiff(file=mypath, width = 1600, height = 1600, res = 280, compression = "lzw")
  print(plots.tasas.profundidad[i])
  dev.off()
}


# profundidad temporal ====
# resumen con error estandar (se) de las tasas de reclutamiento por profundidad del colector 
tasas.profundidad.mes <- print(summarySE(na.omit(long.data), measurevar="TASA", 
                                         groupvars=c("MES","PROFUNDIDAD", "TAXA","ESPECIES"), 
                                         na.rm=TRUE))

# factores para ordenar las especies en los plot's
tasas.profundidad.mes$ESPECIES <-
  factor(tasas.profundidad.mes$ESPECIES,
         levels=c(row.names(biv_arrange), row.names(gas_arrange),row.names(crus_arrange),row.names(equino_arrange)),
         labels=c(row.names(biv_arrange), row.names(gas_arrange),row.names(crus_arrange),row.names(equino_arrange)))

# plot para la tasa de reclutamiento promedio por grupo, profundidad y especies
plots.tasas.profundidad.mes <- lapply(split(tasas.profundidad.mes, tasas.profundidad.mes$TAXA), function(x) 
  ggplot(x, aes(x = MES, y= TASA, colour= PROFUNDIDAD,  group= PROFUNDIDAD))+ 
    geom_errorbar(aes(ymax = TASA + se, ymin = TASA - se), colour="black", width=.1, position=pd) +
    geom_line(position=pd) +
    geom_point(position=pd, size=3, shape=21, fill="white") +  
    facet_wrap(~ ESPECIES, scales="fixed", ncol = 3) +
    theme_bw() +
    labs(x = "CAMPAÑA", y = "Tasa reclutamiento"~(ind.col^-1~dia^-1))+
    theme(axis.text.x = element_text(angle = 45, hjust = 1))+
    theme(strip.text.x = element_text(size = 12, face="italic"))+
    theme(strip.text = element_text(face = "italic")))

#nombres para usar en los archivos del plot
nombres <- unique(tasas.profundidad.mes$TAXA)
#loop para graficar un archivo por plot
for(i in 1:4){
  
  print(mypath <- file.path(campaña.IMG.dir,
                            paste("tasas_profundidad_temporal_", nombres[i], ".tiff", sep = "")))
  
  tiff(file=mypath, width = 1600, height = 1600, res = 280, compression = "lzw")
  print(plots.tasas.profundidad.mes[i])
  dev.off()
}

# # colectores promedio ====
# # resumen con error estandar (se) de las tasas de reclutamiento por profundidad del colector 
# tasas.colector <- print(summarySE(na.omit(long.data), measurevar="TASA", 
#                                   groupvars=c("COLECTOR", "TAXA","ESPECIES"), 
#                                   na.rm=TRUE))
# 
# # factores para ordenar las especies en los plot's
# tasas.colector$ESPECIES <-
#   factor(tasas.colector$ESPECIES,
#          levels=c(row.names(biv_arrange), row.names(gas_arrange),row.names(crus_arrange),row.names(equino_arrange)),
#          labels=c(row.names(biv_arrange), row.names(gas_arrange),row.names(crus_arrange),row.names(equino_arrange)))
# 
# # plot para la tasa de reclutamiento promedio por grupo, lineas y especies
# plots.tasas.colector <- lapply(split(tasas.colector, tasas.colector$TAXA), function(x) 
#   ggplot(x, aes(x = COLECTOR, y= TASA))+
#     geom_bar(position=position_dodge(.9), colour="black", stat="identity")+
#     geom_errorbar(position=position_dodge(.9), width=.25, aes(ymin = TASA, ymax= TASA + se))+
#     geom_point(position=pd, size=2, shape=21, fill="white") +
#     facet_wrap(~ ESPECIES, scales="fixed")+
#     theme_bw() +
#     labs(x = "Tipo del colector", y = "Tasa reclutamiento"~(ind.col^-1~dia^-1))+
#     theme(strip.text.x = element_text(size = 10, face="italic"))+
#     theme(strip.text = element_text(face = "italic"))+
#     theme(axis.text.x = element_text(angle = 45, hjust = 1)))
# 
# #nombres para usar en los archivos del plot
# nombres <- unique(tasas.colector$TAXA)
# #loop para graficar un archivo por plot
# for(i in 1:4){
#   
#   print(mypath <- file.path(campaña.IMG.dir,
#                             paste("tasas_colector_promedio_", nombres[i], ".tiff", sep = "")))
#   
#   tiff(file=mypath, width = 1600, height = 1600, res = 280, compression = "lzw")
#   print(plots.tasas.colector[i])
#   dev.off()
# }
# 
# 
# # colector temporal ====
# # resumen con error estandar (se) de las tasas de reclutamiento por colector del colector 
# tasas.colector.mes <- print(summarySE(na.omit(long.data), measurevar="TASA", 
#                                       groupvars=c("MES","COLECTOR", "TAXA","ESPECIES"), 
#                                       na.rm=TRUE))
# 
# # factores para ordenar las especies en los plot's
# tasas.colector.mes$ESPECIES <-
#   factor(tasas.colector.mes$ESPECIES,
#          levels=c(row.names(biv_arrange), row.names(gas_arrange),row.names(crus_arrange),row.names(equino_arrange)),
#          labels=c(row.names(biv_arrange), row.names(gas_arrange),row.names(crus_arrange),row.names(equino_arrange)))
# 
# # plot para la tasa de reclutamiento promedio por grupo, colector y especies
# plots.tasas.colector.mes <- lapply(split(tasas.colector.mes, tasas.colector.mes$TAXA), function(x) 
#   ggplot(x, aes(x = MES, y= TASA, colour= COLECTOR,  group= COLECTOR))+ 
#     geom_errorbar(aes(ymax = TASA + se, ymin = TASA - se), colour="black", width=.1, position=pd) +
#     geom_line(position=pd) +
#     geom_point(position=pd, size=3, shape=21, fill="white") +  
#     facet_wrap(~ ESPECIES, scales="fixed", ncol = 3) +
#     theme_bw() +
#     labs(x = "CAMPAÑA", y = "Tasa reclutamiento"~(ind.col^-1~dia^-1))+
#     theme(axis.text.x = element_text(angle = 45, hjust = 1))+
#     theme(strip.text.x = element_text(size = 12, face="italic"))+
#     theme(strip.text = element_text(face = "italic")))
# 
# #nombres para usar en los archivos del plot
# nombres <- unique(tasas.colector.mes$TAXA)
# #loop para graficar un archivo por plot
# for(i in 1:4){
#   
#   print(mypath <- file.path(campaña.IMG.dir,
#                             paste("tasas_colector_temporal_", nombres[i], ".tiff", sep = "")))
#   
#   tiff(file=mypath, width = 1600, height = 1600, res = 280, compression = "lzw")
#   print(plots.tasas.colector.mes[i])
#   dev.off()
# }

