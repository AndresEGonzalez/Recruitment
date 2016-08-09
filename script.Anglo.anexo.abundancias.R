# script.Anglo.anexo.abundancias.R
# Confecciona anexo abundancias

# Abundancias anexo 



# Tablas anexo ====

abundancia.lineas <-
  print(dcast(arrange(ddply(na.omit(long.data), .(CAMPANA, LINEA, ESPECIES), summarize, 
                            mean.A = round(sum(ABUNDANCIA, na.rm=TRUE), 2)),
                      LINEA), ESPECIES ~ CAMPANA+LINEA))

abundancia.profundidad <-
  print(dcast(arrange(ddply(na.omit(long.data), .(CAMPANA, PROFUNDIDAD, ESPECIES), summarize, 
                            mean.A = round(sum(ABUNDANCIA, na.rm=TRUE), 2)),
                      PROFUNDIDAD), ESPECIES ~ CAMPANA+PROFUNDIDAD))




## Escribe tabla TASAS DE RECLUTAMIENTO a archivo excel 
frame.6 <- c("abundancia.lineas", "abundancia.profundidad")
name.pestaña <- c("lineas", "profundidad")
WriteXLS(frame.6, 
         ExcelFileName = 
           paste("/home/dell/Documents/DOCS_PERS/CONSULTORIA/2015_ANGLOAMERICA/CAMPAÑA_8/Numero_individuos_especie_ANGLO8", format(Sys.time(), "%Y_%m_%d"), 
                 "xls", sep = "."), 
         SheetNames = name.pestaña, verbose=TRUE, AdjWidth = TRUE, BoldHeaderRow = TRUE,
         Encoding = "latin1")


# Abundancia Total por Campaña de Monitoreo ====
d_AB_total <- print(summarySE((long.data),
                              measurevar="ABUNDANCIA", groupvars=c("MES"),
                              na.rm=TRUE))
plot.abund.campañas <-
  ggplot(d_AB_total, aes(x = MES, y= ABUNDANCIA))+
  # ggplot(d_AB_total, aes(x = MES, y= ab_total))+
  geom_bar(position=position_dodge(.9), colour="black", stat="identity")+
  geom_errorbar(position=position_dodge(.9), width=.25, aes(ymin = ABUNDANCIA, ymax= ABUNDANCIA + se))+
  geom_point(position=pd, size=2, shape=21, fill="white") +
  theme_bw() +
  labs(x = "", y = "Abundancia (Nº de Individuos)")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  theme(strip.text.x = element_text(size = 12, face="italic"))+
  theme(strip.text = element_text(face = "italic"))


tiff(file.path(campaña.IMG.dir,
               paste("abundancia_total_por_campaña", ".tiff", sep = "")),
     width = 1300, height = 1000, res = 280, compression = "lzw")
plot.abund.campañas
dev.off()


# Abundancias_totales_por_linea ====
d_abund_total_linea <- print(summarySE(long.data, 
                                       measurevar="ABUNDANCIA", groupvars=c("MES","LINEA"), 
                                       na.rm=TRUE))
plot.abund.linea <-
  ggplot(d_abund_total_linea, aes(x = MES, y= ABUNDANCIA, colour= LINEA,  group= LINEA))+ 
  geom_errorbar(aes(ymin=ABUNDANCIA-se, ymax=ABUNDANCIA+se), colour="black", width=.1, position=pd) +
  geom_line(position=pd) +
  geom_point(position=pd, size=3, shape=21, fill="white") +  
  theme_bw() +
  labs(x = "CAMPAÑAS", y = "Abundancia (Nº de Individuos)")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  theme(strip.text.x = element_text(size = 12, face="italic"))+
  theme(strip.text = element_text(face = "italic"))


tiff(file.path(campaña.IMG.dir,
               paste("abundancia_total_por_linea", ".tiff", sep = "")),
     width = 1300, height = 1000, res = 280, compression = "lzw")
plot.abund.linea
dev.off()

# Abundancia total por linea y por TAXA ====
d_abund_taxa_linea <- 
  print(arrange( summarySE(long.data,
                           measurevar="ABUNDANCIA", 
                           groupvars=c("MES", "LINEA", "TAXA"), 
                           na.rm=TRUE), MES, TAXA, LINEA, desc(ab_total)))

plot.abund.linea.taxa <-
  ggplot(d_abund_taxa_linea, aes(x = MES, y= (ABUNDANCIA), colour= LINEA,  group= LINEA))+ 
  geom_errorbar(aes(ymin=ABUNDANCIA - se, ymax=ABUNDANCIA + se), colour="black", width=.1, position=pd) +
  geom_line(position=pd) +
  geom_point(position=pd, size=3, shape=21, fill="white") +  
  theme_bw() +
  facet_wrap(~ TAXA, scales="fixed") +
  labs(x = "CAMPAÑA", y = "Número de individuos")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  theme(strip.text.x = element_text(size = 10, face="italic"))+
  theme(strip.text = element_text(face = "italic"))


tiff(file.path(campaña.IMG.dir,
               paste("abundancia_total_por_linea_taxa", ".tiff", sep = "")),
     width = 1300, height = 1500, res = 280, compression = "lzw")
plot.abund.linea.taxa 
dev.off()


# ABUNDANCIA TOTAL POR ESTRATOS ====

d_abund_estrato_profund <-print( summarySE(long.data, 
                                           measurevar="ABUNDANCIA", groupvars=c("MES","PROFUNDIDAD"), 
                                           na.rm=TRUE))
plot.abund.prof <-
  ggplot(d_abund_estrato_profund, aes(x = MES, y= ABUNDANCIA, colour= PROFUNDIDAD,  group= PROFUNDIDAD))+ 
  geom_errorbar(aes(ymin= ABUNDANCIA-se, ymax= ABUNDANCIA+se), colour="black", width=.1, position=pd) +
  geom_line(position=pd) +
  geom_point(position=pd, size=3, shape=21, fill="white") +  
  theme_bw() +
  labs(x = "CAMPAÑA", y = "Abundancia (Nº de Individuos)")+  
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  theme(strip.text.x = element_text(size = 12, face="italic"))+
  theme(strip.text = element_text(face = "italic"))


tiff(file.path(campaña.IMG.dir,
               paste("abundancia_total_por_profundidad", ".tiff", sep = "")),
     width = 1300, height = 1000, res = 280, compression = "lzw")
plot.abund.prof 
dev.off()



# ABUNDANCIA TAXA POR ESTRATO DE PROFUNDIDAD
d_abund_estrato <-print(summarySE(long.data, 
                                  measurevar="ABUNDANCIA", groupvars=c("MES","PROFUNDIDAD","TAXA"), 
                                  na.rm=TRUE))

plot.abund.profund.taxa <-
  ggplot(d_abund_estrato, aes(x = MES, y= (ABUNDANCIA), colour= PROFUNDIDAD,  group= PROFUNDIDAD))+ 
  geom_errorbar(aes(ymin=ABUNDANCIA-se, ymax=ABUNDANCIA+se), colour="black", width=.1, position=pd) +
  geom_line(position=pd) +
  geom_point(position=pd, size=3, shape=21, fill="white") +  
  theme_bw() +
  facet_wrap(~ TAXA, scales="fixed") +
  labs(x = "CAMPAÑAS", y = "Número de individuos")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  theme(strip.text.x = element_text(size = 12, face="italic"))+
  theme(strip.text = element_text(face = "italic"))


tiff("abundancias_totales_por_profundidad_taxa.tiff", 
     width = 1900, height = 1900, res = 300, compression = "lzw")
plot.abund.profund.taxa 

dev.off()


# abundancia por grupo====

print(summarySE((long.data),
                measurevar="ABUNDANCIA", groupvars=c("MES", "TAXA"),
                na.rm=TRUE))


tot <- (870+940+195+2)
fx  <- 2
porc <- print((fx * 100)/tot) 


## TABLAS FINALES=============================
# Tasa de reclutamiento por especies

reclutam.species <-print(dcast(arrange(ddply(na.omit(long.data), .(CAMPANA,LINEA, PROFUNDIDAD,COLECTOR, REPLICA, ESPECIES), summarize, 
                                             mean = round(mean(TASA, na.rm=TRUE), 2)),
                                       LINEA), ESPECIES ~ CAMPANA+LINEA+PROFUNDIDAD+COLECTOR+REPLICA))


## Escribe tabla TASAS DE RECLUTAMIENTO a archivo excel 
frame.5 <- c("reclutam.species")
name.pestaña <- c("Tasa")
WriteXLS(frame.5, 
         ExcelFileName = file.path(campaña.IMG.dir,
                                   paste("Tasas_recluta_campañas_Anglo_", format(Sys.time(), "%Y_%m_%d"), 
                                         ".xls", sep = "")), 
         SheetNames = name.pestaña, verbose=TRUE, AdjWidth = TRUE, BoldHeaderRow = TRUE,
         Encoding = "latin1")

# ABUNDANCIAS POR ESPECIE=============

abundancia.species <-print(dcast(arrange(ddply(na.omit(long.data), .(CAMPANA, LINEA, PROFUNDIDAD, COLECTOR, REPLICA, ESPECIES), summarize, 
                                               mean.A = round(sum(ABUNDANCIA, na.rm=TRUE), 2)),
                                         LINEA), ESPECIES ~ CAMPANA+LINEA+PROFUNDIDAD+COLECTOR+REPLICA))

## Escribe tabla TASAS DE RECLUTAMIENTO a archivo excel 
frame.6 <- c("abundancia.species")
name.pestaña <- c("Abundancia")
WriteXLS(frame.6, 
         ExcelFileName = file.path(campaña.IMG.dir,
           paste("Numero_de_individuos_campañas_Anglo_", format(Sys.time(), "%Y_%m_%d"), 
                 ".xls", sep = "")), 
         SheetNames = name.pestaña, verbose=TRUE, AdjWidth = TRUE, BoldHeaderRow = TRUE,
         Encoding = "latin1")



# tabla taxonomico
library(taxize)
dput(unique(long.data$ESPECIES))
species <- c("Pilumnoides perlatus", "Pachycheles grossimanus", "Halicarcinus planatus", 
             "Taliepus dentatus", "Romaleon polyodon", "Betaeus truncatus", "Pisoides edwarsii", "Rhynchocinetes typus", 
             "Choromytilus chorus", "Semimytilus algosus", "Perumytilus purpuratus", "Brachidontes granulata", "Entodesma cuneata", 
             "Ensis macha", "Eurhomalea", "Hiatella solida", "Cyclocardia compressa", 
             "Carditella tegulata", "Aulacomya atra", "Argopecten purpuratus", "Salitra radwini", "Tricolia umbilicata", 
             "Crassilabrum crassilabrum", "Mitrella unifasciata", "Turritella cingulata", "Caecum chilense", 
             "Nodilittorina araucana", "Crepidula dilatata", "Nassarius gayii", "Liotia cancellata", "Prisogaster niger", 
             "Tegula", "Fissurella", "Concholepas concholepas", "Cerithidae", 
             "Siphonaria lessoni", "Triphoridae", "Xanthochorus cassidiformis", "Stichaster striatus", 
             "Tetrapygus niger", "Loxechinus albus", "Ophioplus")



family<-tax_name(query = species, get = c("family"), db = "both")
na.omit(taxon)
famnames <- sapply(species, tax_name, get = "family", USE.NAMES = F)

data.frame(species = species, family = famnames)
### resuelve nombres taxonomicos
resolv.tx<-gnr_resolve(species)
str(resolv.tx)
resolv.tx[ , -c(1,4)]

tax.name<-tnrs(query = species, source = "NCBI,MSW3", getpost = "POST")
tax.name[, -c(5:7)]

#==================================================================

clatx<-classification(species, db = 'gbif')#extract classification for species names from gbif data base
# cbind.classification(clatx)


# tx<-sapply(clatx, "[", i = 1)
# rrrrr<-ldply (tx, data.frame)
# do.call(rbind, lapply(clatx, data.frame, stringsAsFactors=FALSE))
# do.call("rbind", lapply(clatx, as.data.frame))
# dcast(l, L1 ~ L2)
taxi<-do.call(rbind.data.frame, clatx)
taxi$sp<-rownames(taxi)

dcast(taxi, sp ~ rank)

taxi$sp.group <-substr(taxi$sp, 1, nchar(taxi$sp)-2)# create column sp.group para agrupar 
# FACTOR LEVELS for rank variable
taxi$rank <- factor(taxi$rank,
                          levels=c("kingdom",
                                   "phylum",
                                   "class", 
                                   "order",
                                   "family", 
                                   "genus",
                                   "species"),
                          labels=c("kingdom",
                                   "phylum",
                                   "class", 
                                   "order",
                                   "family", 
                                   "genus",
                                   "species"))
taxonomy<-arrange(dcast(taxi, sp.group ~ rank, value.var = "name"), phylum, class, family)

## Escribe tabla taxonomia a archivo excel 
frame.6 <- c("taxonomy")
name.pestaña <- c("structure")
WriteXLS(frame.6, 
         ExcelFileName = file.path(campaña.IMG.dir,
                                   paste("Taxonomy_Anglo_", format(Sys.time(), "%Y_%m_%d"), 
                                         ".xls", sep = "")), 
         SheetNames = name.pestaña, verbose=TRUE, AdjWidth = TRUE, BoldHeaderRow = TRUE,
         Encoding = "latin1")
