#**************ejercicio para las lineas de monitoreo********************

nn<- c("P.perlatus", "P.grossimanus")#rownames(ww[ which(ww$p < .05), ])

#Test colectores superficie
LCSup<-data.frame.tasa[data.frame.tasa$PROFUNDIDAD == "SUP", c(nn, "LINEA")]

lapply((LCSup),
       function(x) posthoc.kruskal.nemenyi.test(x ~ LINEA, na.action(na.omit(data)), data=LCSup))

#Test colectores superficie
LCFon<-data.frame.tasa[data.frame.tasa$PROFUNDIDAD == "FON", c(nn, "LINEA")]

lapply((LCFon),
       function(x) posthoc.kruskal.nemenyi.test(x ~ LINEA, na.action(na.omit(data)), data=LCFon))

#PLOT

t.lineas <- print(summarySE(na.omit(long.data), measurevar="TASA", 
                                groupvars=c("LINEA", "TAXA","ESPECIES", "PROFUNDIDAD"), 
                                na.rm=TRUE))

t.lin.crust <-rbind.data.frame(
t.lineas[t.lineas$ESPECIES == c("P.perlatus"),],
t.lineas[t.lineas$ESPECIES == c("P.grossimanus"),])
  # t.lineas[t.lineas$ESPECIES == c("P.perlatus","P.grossimanus"),]

crusta<-ggplot(t.lin.crust, aes(x = LINEA, y= TASA))+
  geom_bar(position=position_dodge(.9), colour="black", stat="identity")+
  geom_errorbar(position=position_dodge(.9), width=.25, aes(ymin = TASA, ymax= TASA + se))+
  geom_point(position=pd, size=2, shape=21, fill="white")+
  # facet_wrap(~ PROFUNDIDAD+ESPECIE, scales="fixed")+
  facet_grid(PROFUNDIDAD ~ ESPECIES)+
  theme_bw()+
  labs(x = "Líneas de monitoreo", y = "Tasas de reclutamiento"~(ind.col^-1~dia^-1))+
  theme(strip.text.x = element_text(size = 10, face="italic"))+
  theme(strip.text = element_text(face = "italic"))


tiff(file.path(campaña.IMG.dir,
               paste("exercice_crustace_lineas_profund", ".tiff", sep = "")),
     width = 1300, height = 1000, res = 280, compression = "lzw")
crusta
dev.off()



#Segundo exercice................

# d1<-(long.data)
# names(d1)


#filtra solo sp más representativas
sprep<- c("P.purpuratus", "S.algosus", "E.cuneata",
       "M.unifasciata", "P.niger", "T.cingulata",
  "P.perlatus", "P.grossimanus",
  "T.niger", "L.albus")

d1<-data.frame.tasa[data.frame.tasa$PROFUNDIDAD == "SUP" | 
                    data.frame.tasa$PROFUNDIDAD == "FON", 
                    c(sprep, "PROFUNDIDAD","LINEA")]

d1$CODE <-ifelse(d1$LINEA %in% c("LN"), "CONTROLN",
                 ifelse(d1$LINEA %in% c("L3", "L1"), "AMERBS",
                        ifelse(d1$LINEA %in% c("L2", "LT"),"ACCABIER", "NA")
                 ))

d1$CODE<-as.factor(d1$CODE)

# 
# subset(d1, PROFUNDIDAD == "SUP" | PROFUNDIDAD == "SUP" & ESPECIE == sprep,
#        drop = TRUE)

#Test integrado
lapply((d1),
       function(x) posthoc.kruskal.nemenyi.test(x ~ CODE, na.action(na.omit(data)), 
                                                data=d1))

#Test SUP
lapply((d1[d1$PROFUNDIDAD == "SUP",]),
       function(x) posthoc.kruskal.nemenyi.test(x ~ CODE, na.action(na.omit(data)), 
                                                data=d1[d1$PROFUNDIDAD == "SUP",]))

#Test FON
lapply((d1[d1$PROFUNDIDAD == "FON",]),
       function(x) posthoc.kruskal.nemenyi.test(x ~ CODE, na.action(na.omit(data)), 
                                                data=d1[d1$PROFUNDIDAD == "FON",]))



#plot*******************sp representativas

# d2<-subset(long.data, ESPECIES == sprep,
#          drop = TRUE)
unique(long.data$ESPECIES)
C <-subset (Crustacea, ESPECIES == c("P.perlatus", "P.grossimanus"), drop=T)
C$CODE <-ifelse(C$LINEA == c("LN"), "CONTROLN",
                 ifelse(C$LINEA == c("L3"), "AMERBS",
                        ifelse(C$LINEA == c("L1"), "AMERBS",
                               ifelse(C$LINEA == c("L2"),"ACCABIER", 
                                      ifelse(C$LINEA == c("LT"),"ACCABIER",NA)))))

C$CODE<-as.factor(C$CODE)
B <-subset (Bivalvia, ESPECIES == c("P.purpuratus", "S.algosus", "E.cuneata"), drop=T)
B$CODE <-ifelse(B$LINEA == c("LN"), "CONTROLN",
                 ifelse(B$LINEA == c("L3"), "AMERBS",
                        ifelse(B$LINEA == c("L1"), "AMERBS",
                               ifelse(B$LINEA == c("L2"),"ACCABIER", 
                                      ifelse(B$LINEA == c("LT"),"ACCABIER",NA)))))

B$CODE<-as.factor(B$CODE)
G <-subset (Gastropoda, ESPECIES == c("M.unifasciata", "P.niger", "T.cingulata"),drop=T)
G$CODE <-ifelse(G$LINEA == c("LN"), "CONTROLN",
                 ifelse(G$LINEA == c("L3"), "AMERBS",
                        ifelse(G$LINEA == c("L1"), "AMERBS",
                               ifelse(G$LINEA == c("L2"),"ACCABIER", 
                                      ifelse(G$LINEA == c("LT"),"ACCABIER",NA)))))

G$CODE<-as.factor(G$CODE)
E <-subset (Equinodermata, ESPECIES == c("T.niger", "L.albus"), drop=T)
E$CODE <-ifelse(E$LINEA == c("LN"), "CONTROLN",
                 ifelse(E$LINEA == c("L3"), "AMERBS",
                        ifelse(E$LINEA == c("L1"), "AMERBS",
                               ifelse(E$LINEA == c("L2"),"ACCABIER", 
                                      ifelse(E$LINEA == c("LT"),"ACCABIER",NA)))))

E$CODE<-as.factor(E$CODE)

# todo a un data frame
d2 <-rbind.data.frame(B,G,C,E)

str(d2)

d2$CODE <-ifelse(d2$LINEA == c("LN"), "CONTROLN",
                ifelse(d2$LINEA == c("L3"), "AMERBS",
                ifelse(d2$LINEA == c("L1"), "AMERBS",
                        ifelse(d2$LINEA == c("L2"),"ACCABIER", 
                        ifelse(d2$LINEA == c("LT"),"ACCABIER",NA)))))

d2$CODE<-as.factor(d2$CODE)
d2$ESPECIES <- factor(d2$ESPECIES,
                      levels=c("P.purpuratus", "S.algosus", "E.cuneata",
                               "M.unifasciata", "P.niger", "T.cingulata",
                               "P.perlatus", "P.grossimanus",
                               "T.niger", "L.albus"),
                      labels=c("P.purpuratus", "S.algosus", "E.cuneata",
                               "M.unifasciata", "P.niger", "T.cingulata",
                               "P.perlatus", "P.grossimanus",
                               "T.niger", "L.albus"))

str(d2)
# d2<-d2[complete.cases(d2),]

boxplot_code<-ggplot(E, aes(x = CODE, y= TASA))+
  #   geom_bar(position=position_dodge(.9), colour="black", stat="identity")+
  #   geom_errorbar(position=position_dodge(.9), width=.25, aes(ymin = TASA, ymax= TASA + sd))+
  #   geom_point(position=pd, size=2, shape=21, fill="white")+
    geom_boxplot()+
  facet_grid( PROFUNDIDAD ~ ESPECIES,scales="free")+
  # facet_wrap(PROFUNDIDAD ~ ESPECIE, scales="free_y")+
  theme_bw()+
  labs(x = "Líneas de monitoreo", y = "Tasas de reclutamiento"~(ind.col^-1~dia^-1))+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  theme(strip.text.x = element_text(size = 8, face="italic"))+
  theme(strip.text = element_text(face = "italic"))


tiff(file.path(campaña.IMG.dir,
               paste("EQUIN_boxplot_sprepresent_code_line_free", ".tiff", sep = "")),
     width = 2500, height = 1200, res = 300, compression = "lzw")
boxplot_code
dev.off()




d3<-print(summarySE((d2), measurevar="TASA", 
                    groupvars=c("ESPECIES", "CODE","PROFUNDIDAD"), 
                    na.rm=TRUE))
# FACTOR LEVELS LINEA
d3$ESPECIES <- factor(d3$ESPECIES,
                          levels=c("P.purpuratus", "S.algosus", "E.cuneata",
                                   "M.unifasciata", "P.niger", "T.cingulata",
                                   "P.perlatus", "P.grossimanus",
                                   "T.niger", "L.albus"),
                          labels=c("P.purpuratus", "S.algosus", "E.cuneata",
                                   "M.unifasciata", "P.niger", "T.cingulata",
                                   "P.perlatus", "P.grossimanus",
                                   "T.niger", "L.albus"))

code_line<-ggplot(d3, aes(x = CODE, y= TASA))+
  geom_bar(position=position_dodge(.9), colour="black", stat="identity")+
  geom_errorbar(position=position_dodge(.9), width=.25, aes(ymin = TASA, ymax= TASA + sd))+
  geom_point(position=pd, size=2, shape=21, fill="white")+
  # facet_wrap( ~ ESPECIE, scales="fixed")+
  facet_grid(PROFUNDIDAD ~ ESPECIES)+
  theme_bw()+
  labs(x = "Líneas de monitoreo", y = "Tasas de reclutamiento"~(ind.col^-1~dia^-1))+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  theme(strip.text.x = element_text(size = 8, face="italic"))+
  theme(strip.text = element_text(face = "italic"))

tiff(file.path(campaña.IMG.dir,
               paste("exercice_sprepresent_code_line", ".tiff", sep = "")),
     width = 2500, height = 1200, res = 300, compression = "lzw")
code_line
dev.off()


#un boxplot
# ggplot(d2, aes(x = LINEA, y= TASA))+
# #   geom_bar(position=position_dodge(.9), colour="black", stat="identity")+
# #   geom_errorbar(position=position_dodge(.9), width=.25, aes(ymin = TASA, ymax= TASA + sd))+
# #   geom_point(position=pd, size=2, shape=21, fill="white")+
#   # facet_wrap( ~ ESPECIE, scales="fixed")+
#   geom_boxplot()+
#   # facet_grid(PROFUNDIDAD ~ ESPECIES)+
#   facet_grid( ~ ESPECIES)+
#   theme_bw()+
#   labs(x = "Líneas de monitoreo", y = "Tasas de reclutamiento"~(ind.col^-1~dia^-1))+
#   theme(axis.text.x = element_text(angle = 45, hjust = 1))+
#   theme(strip.text.x = element_text(size = 8, face="italic"))+
#   theme(strip.text = element_text(face = "italic"))



#por lineas separado por profundidad

ggplot(d2, aes(x = LINEA, y= TASA))+
  #   geom_bar(position=position_dodge(.9), colour="black", stat="identity")+
  #   geom_errorbar(position=position_dodge(.9), width=.25, aes(ymin = TASA, ymax= TASA + sd))+
  #   geom_point(position=pd, size=2, shape=21, fill="white")+
  # facet_wrap( ~ ESPECIE, scales="fixed")+
  geom_boxplot()+
  facet_grid(PROFUNDIDAD ~ ESPECIES)+
  theme_bw()+
  labs(x = "Líneas de monitoreo", y = "Tasas de reclutamiento"~(ind.col^-1~dia^-1))+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  theme(strip.text.x = element_text(size = 8, face="italic"))+
  theme(strip.text = element_text(face = "italic"))

ggplot(d2, aes(x = LINEA, y= TASA))+
  #   geom_bar(position=position_dodge(.9), colour="black", stat="identity")+
  #   geom_errorbar(position=position_dodge(.9), width=.25, aes(ymin = TASA, ymax= TASA + sd))+
  #   geom_point(position=pd, size=2, shape=21, fill="white")+
  # facet_wrap( ~ ESPECIE, scales="fixed")+
  geom_boxplot()+
  facet_grid(~ ESPECIES)+
  theme_bw()+
  labs(x = "Líneas de monitoreo", y = "Tasas de reclutamiento"~(ind.col^-1~dia^-1))+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  theme(strip.text.x = element_text(size = 8, face="italic"))+
  theme(strip.text = element_text(face = "italic"))



# solo la linea LT succion de agua.....

suck<-subset(long.data, LINEA == "LT", drop = TRUE)
summary(suck)


ggplot(suck, aes(x = MES, y= TASA))+
  #   geom_bar(position=position_dodge(.9), colour="black", stat="identity")+
  #   geom_errorbar(position=position_dodge(.9), width=.25, aes(ymin = TASA, ymax= TASA + sd))+
  #   geom_point(position=pd, size=2, shape=21, fill="white")+
  # facet_wrap( ~ ESPECIE, scales="fixed")+
  geom_boxplot()+
  # facet_grid(~ ESPECIES)+
  theme_bw()+
  labs(x = "Líneas de monitoreo", y = "Tasas de reclutamiento"~(ind.col^-1~dia^-1))+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  theme(strip.text.x = element_text(size = 8, face="italic"))+
  theme(strip.text = element_text(face = "italic"))

suck1<-print(summarySE((suck), measurevar="TASA", 
                groupvars=c("MES"), 
                na.rm=TRUE))

ggplot(suck1, aes(x = MES, y= TASA))+
  geom_bar(position=position_dodge(.9), colour="black", stat="identity")+
  geom_errorbar(position=position_dodge(.9), width=.25, aes(ymin = TASA, ymax= TASA + sd))+
  geom_point(position=pd, size=2, shape=21, fill="white")+
  # facet_wrap( ~ ESPECIE, scales="fixed")+
  # facet_grid(PROFUNDIDAD ~ ESPECIES)+
  theme_bw()+
  labs(x = "", y = "Tasas de reclutamiento"~(ind.col^-1~dia^-1))+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  theme(strip.text.x = element_text(size = 8, face="italic"))+
  theme(strip.text = element_text(face = "italic"))

tiff(file.path(campaña.IMG.dir,
               paste("exercice_sprepresent_code_line", ".tiff", sep = "")),
     width = 2500, height = 1200, res = 300, compression = "lzw")
code_line
dev.off()


ggplot(suck1, aes(x = MES, y= TASA))+
  geom_errorbar(aes(ymax = TASA + sd, ymin = TASA - sd), colour="black", width=.1, position=pd) +
  geom_line(position=pd, colour="black") +
  geom_point(position=pd, size=3, shape=21, fill="white") +
  # facet_wrap( ~ ESPECIE, scales="fixed")+
  # facet_grid(PROFUNDIDAD ~ ESPECIES)+
  theme_bw()+
  labs(x = "", y = "Tasas de reclutamiento"~(ind.col^-1~dia^-1))+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  theme(strip.text.x = element_text(size = 8, face="italic"))+
  theme(strip.text = element_text(face = "italic"))

## eSPECIES QUE RECLUTAN POR LÍNEA

L2 <-long.data

L2$CODE <-ifelse(L2$LINEA == c("LN"), "CONTROLN",
                      ifelse(L2$LINEA == c("L3"), "AMERBS",
                             ifelse(L2$LINEA == c("L1"), "AMERBS",
                                    ifelse(L2$LINEA == c("L2"),"ACCABIER", 
                                           ifelse(L2$LINEA == c("LT"),"ACCABIER",NA)))))

df<-print(summarySE(L2, measurevar="TASA", 
                groupvars=c("CODE", "ESPECIES"), 
                na.rm=TRUE))
library(reshape2)

df1<-dcast(df, CODE ~ ESPECIES, value.var="ab_total")
row.names(df1) <- df1$CODE

str(df1)


dcast(L2, CODE ~ ESPECIES, value.var="ABUNDANCIA")
#************************
library(vegan)
library(MASS) ## isoMDS
# NMDS
sol <- metaMDS(df1[,2:ncol(df1)], distance = "bray")
sol
stressplot(sol)
#site(line) priority
priSite <- diversity(df1[,2:ncol(df1)], index = "invsimpson", MARGIN = 1)
gof <-goodness(sol)
#sp priority
stem <-colSums(df1[,2:ncol(df1)])
priSpp <- diversity(df1[,2:ncol(df1)], index = "invsimpson", MARGIN = 2)

#plot
plot(sol, type="t")

# plot(sol, type="t", display="sites", ylim=c(-1,1), xlim=c(-1,1), scaling=3)#, main=paste("nMDS/Bray-Curtis. Stress =", round(f.nmds$stress, 3))
# points(sol, display="sites", cex=gof, col="red")
# abline(v=c(0,0), h=c(0,0), lty=3)
# text(x=.85, y=.8, labels = paste("Stress =", round(sol$stress, 3)))
# orditorp(sol, "sp", label = names(df1[,2:ncol(df1)]), pch="+", priority=stem,
#          pcol="grey", col = "blue")

plot(sol, type="n", scaling=3)#, main=paste("nMDS/Bray-Curtis. Stress =", round(f.nmds$stress, 3))
# points(sol, display="sites", cex=gof, col="red")
abline(v=c(0,0), h=c(0,0), lty=3)
# text(x=.85, y=.8, labels = paste("Stress =", round(sol$stress, 3)))
orditorp(sol, display = "species", priority = priSpp, scaling = scl,
         col = "forestgreen", pch = 2, cex = 1)
orditorp(sol, display = "sites", priority = priSite, scaling = 3,
         col = "blue", cex = 1, pch = 19)


#cluster

dist.df <- vegdist(df1[,2:length(df1)], "bray", binary =T)
tree.col <- hclust(-dist.df+1)

plot(tree.col, main="", xlab="Líneas de monitoreo", 
                      ylab = "Similitud Bray-Curtis", sub="")


#join plot
tiff(file.path(campaña.IMG.dir,
               paste("ordination.plots", ".tiff", sep = "")),
     width = 4000, height = 2000, res = 330, compression = "lzw")

layout(matrix(1:2, ncol = 2), widths=c(2.5,1))
op <- par(mar = c(4,4,1,1) + 0.1)

plot(sol, type="n")
abline(v=c(0,0), h=c(0,0), lty=3)
# text(x=.85, y=.8, labels = paste("Stress =", round(sol$stress, 3)))
orditorp(sol, display = "species", priority = priSpp, scaling = 3,
         col = "forestgreen", pch = 2, cex = 1, air = 2)
orditorp(sol, display = "sites", priority = priSite, scaling = 3,
         col = "blue", cex = 1, pch = 19)
mtext(letters[1], side = 1, line = 0, padj =1, adj=0,cex = 1.5, font = 2, las=1)

plot(tree.col, main="", xlab="Líneas de monitoreo", 
     ylab = "Similitud Bray-Curtis", sub="")
mtext(letters[2], side = 1, line = 0, padj =1 , adj=0,cex = 1.5, font = 2, las=1)
par(op)
layout(1)

dev.off()

# SIMPER
# Analisis SIMPER (Porcentaje de similitud) se basa en la descomposición del indice de disimilitud de Bray-Curtis.
# Realiza comparaciones entre pares de grupos de muestreo y encuentra la contribución promedio de cada especie 
# al promedio total del indice de didimilitud de Bray-Curtis. 
# Como resultado indica cuales son las especies que contribuyen al 70% de las diferencias entre grupos. 

contrib<-simper(df1[,2:ncol(df1)], factor(rownames(df1)), permutations = 100)
summary(contrib, digits = max(3,getOption("digits") - 3))
