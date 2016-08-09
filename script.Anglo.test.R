# script.Anglo.test.R
# Hace analisis estadisticos


# test Tasas de Reclutamiento Totales  ====
print(summarySE(na.omit(long.data),
                measurevar="TASA", groupvars=c("MES"),
                na.rm=TRUE))

kruskal.test(TASA ~ MES, data = na.omit(long.data))

x<-posthoc.kruskal.nemenyi.test(TASA ~ CAMPANA, long.data, method="Chisquare",
                             na.action(na.omit(data)))
format(round(x$p.value, digits = 6), scientific = FALSE)
  
# TEST por lineas de monitoreo ====
a<-print(summarySE(na.omit(long.data),
                measurevar="TASA", groupvars=c("LINEA"),
                na.rm=TRUE))
plot(a$LINEA,a$TASA)

# Diferencias entre lineas 
kruskal.test(TASA ~ LINEA, data = long.data)

# para los totales
posthoc.kruskal.nemenyi.test(TASA ~ LINEA, long.data, method="Chisquare",p.adj="bonferroni",exact=F,
                             na.action(na.omit(data)))

#VAR TEMP: TEST por linea ENTRE CAMPAÑAS 

# Comparación de las tasas de reclutamiento por líneas de monitoreo (LN, L3, L2, LT y L1) entre campañas realizadas
# lapply(split(long.data, long.data$LINEA), function(x)
#   kruskal.test(TASA ~ MES, data = x))

lin <-as.data.frame(matrix(unlist(lapply(split(long.data, long.data$LINEA), function(x)
  kruskal.test(TASA ~ MES, data = x))), nrow=5, byrow=T, 
  dimnames = list(c("LN", "L3", "L2", "LT", "L1"),
                  c("X2", "df", "p", "test", "variable"))), 
  stringsAsFactors=FALSE)

data.frame(Líneas = dimnames(lin)[[1]],Test = lin[,"test"], X2 = round(as.numeric(lin[,"X2"]),3),
           df = lin[,"df"],p = format(as.numeric(lin[,"p"]),scientific = TRUE, digits = 4), stringsAsFactors=FALSE) 


#Comparaciones múltiples de las tasas de reclutamiento promedio por cada línea de monitoreo.


lin.mult<-lapply(split(long.data, long.data$LINEA), function(x)
  posthoc.kruskal.nemenyi.test(TASA ~ MES, data = x))

lin.mult2<-sapply(lin.mult, "[", i = 3)


ddd<-ldply (lin.mult2, data.frame)
# ldply (lin.mult2, rbind)

data.frame("Especies" = ddd[,1], round(ddd[,-1], 5))



# Comparación de las tasas de reclutamiento entre líneas de monitoreo  para cada campaña realizada. 
# lapply(split(long.data, long.data$MES), function(x)
#   kruskal.test(TASA ~ LINEA, data = x))

lin2 <-as.data.frame(matrix(unlist(lapply(split(long.data, long.data$MES), function(x)
  kruskal.test(TASA ~ LINEA, data = x))), nrow=8, byrow=T, 
  dimnames = list(c("Campaña 1", "Campaña 2", "Campaña 3", "Campaña 4", "Campaña 5", "Campaña 6", "Campaña 7", "Campaña 8"),
                  c("X2", "df", "p", "test", "variable"))), 
  stringsAsFactors=FALSE)

data.frame(Campañas = dimnames(lin2)[[1]],Test = lin2[,"test"], X2 = round(as.numeric(lin2[,"X2"]),3),
           df = lin2[,"df"],p = format(as.numeric(lin2[,"p"]),scientific = FALSE, digits = 4), stringsAsFactors=FALSE) 

#Comparaciones múltiples de las tasas de reclutamiento promedio por campaña
# lapply(split(long.data, long.data$MES), function(x)
#   posthoc.kruskal.nemenyi.test(TASA ~ LINEA, data = x))

lin.multC<-lapply(split(long.data, long.data$MES), function(x)
  posthoc.kruskal.nemenyi.test(TASA ~ LINEA, data = x))

lin.multC2<-sapply(lin.multC, "[", i = 3)


ldply (lin.multC2, data.frame)


# test por estrato de profundidad =====
print(summarySE(na.omit(long.data),
                measurevar="TASA", groupvars=c("PROFUNDIDAD"),
                na.rm=TRUE))

# Diferencias entre profundidades (agrupado poara todas las campañas)
wilcox.test(TASA ~ PROFUNDIDAD, data = long.data)

# Test para cada profundidad entre campañas (Temporal)
# Comparación de las tasas de reclutamiento por profundidad del colector (SUP y FON).
prof<-as.data.frame(matrix(unlist(lapply(split(long.data, long.data$PROFUNDIDAD), function(x)
  kruskal.test(TASA ~ MES, data = x))), nrow=2, byrow=T, 
  dimnames = list(c("Superficie", "Fondo"),
                  c("X2", "df", "p", "test", "variable"))), 
  stringsAsFactors=FALSE)

data.frame(Profundidad = dimnames(prof)[[1]],Test = prof[,"test"], X2 = round(as.numeric(prof[,"X2"]),3),
           df = prof[,"df"],p = format(as.numeric(prof[,"p"]),scientific = TRUE, digits = 4), stringsAsFactors=FALSE)

# Comparciones multiples
# Comparaciones múltiples de las tasas de reclutamiento promedio entre campañas por estrato de profundidad (SUP y FON)
posthoc.kruskal.nemenyi.test(TASA ~ CAMPANA, long.data,  subset = long.data$PROFUNDIDAD == "FON",
                             method="Chisquare", na.action(na.omit(data)))

PM <-lapply(split(long.data, long.data$PROFUNDIDAD), function(x)
  posthoc.kruskal.nemenyi.test(TASA ~ MES, data = x))

PM2<-sapply(PM, "[", i = 3)


ldply(PM2, data.frame)


# Por campaña
# Comparación de las tasas de reclutamiento entre estratos de profundidad (SUP y FON) para cada campaña 
xxx <-lapply(split(long.data, long.data$MES), function(x)
  # wilcox.test(TASA ~ PROFUNDIDAD, data = x))
  t.test(TASA ~ PROFUNDIDAD, data = x))#wilcox.test

P1<-as.data.frame(matrix(unlist(xxx), nrow=length(xxx), byrow=T, 
  dimnames = list(c(names(xxx)),
                  c("U", "p", "df","alternat","test", "variable"))), 
  stringsAsFactors=FALSE)

data.frame(Campañas = dimnames(P1)[[1]],
           Test = P1[,"test"], 
           U = round(as.numeric(P1[,"U"]),3),
           p = format(as.numeric(P1[,"p"]),scientific = TRUE, digits = 3), 
           stringsAsFactors=FALSE)


# TEST TAXA ====
arrange(summarySE(na.omit(long.data),
                measurevar="TASA", groupvars=c("TAXA"),
                na.rm=TRUE), desc(TASA))

# Diferencias entre taxa (agrupado poara todas las campañas)
kruskal.test(TASA ~ TAXA, data = long.data)

# Comparaciones múltiples de las tasas de reclutamiento entre grupos taxonómicos. 
posthoc.kruskal.nemenyi.test(TASA ~ TAXA, data = long.data,
                             method="Chisquare", na.action(na.omit(data)))
# tabla 3-6
arrange(summarySE(na.omit(long.data),
                  measurevar="TASA", groupvars=c("TAXA", "CAMPANA"),
                  na.rm=TRUE), (CAMPANA))

#Comparación de las tasas de reclutamiento entre grupos taxonómicos para cada campaña
# Diferencias entre taxa para cada campaña TEMPORAL

tax <-lapply(split(long.data, long.data$MES), function(x)
  kruskal.test(TASA ~ TAXA, data = x))

T1<-as.data.frame(matrix(unlist(tax), nrow=length(tax), byrow=T, 
                         dimnames = list(c(names(tax)),
                                         c("X2", "df", "p", "test", "variable"))), 
                  stringsAsFactors=FALSE)

data.frame(Campañas = dimnames(T1)[[1]],
           Test = T1[,"test"], 
           X2 = round(as.numeric(T1[,"X2"]),3),
           df = T1[,"df"],
           p = format(as.numeric(T1[,"p"]),scientific = TRUE, digits = 3), 
           stringsAsFactors=FALSE)
# Comparaciones múltiples de las tasas de reclutamiento promedio entre grupos taxonómicos por campaña

TM <-lapply(split(long.data, long.data$MES), function(x)
  posthoc.kruskal.nemenyi.test(TASA ~ TAXA, data = x))

TM2<-sapply(TM, "[", i = 3)


ldply(TM2, data.frame)

# # todos los grupos ====
# 
# # Agrupadas al total de campañas
# 
# lapply(split(long.data, long.data$TAXA), function(x)
#   kruskal.test(TASA ~ ESPECIES, data = x))
# # 
# lapply(split(long.data, long.data$TAXA), function(x)
# posthoc.kruskal.nemenyi.test(TASA ~ MES, data = x,
#                              method="Chisquare", na.action(na.omit(data))))
# 
# # con el obj tasa que tiene el promedio hasta la campaña x
# 
# test_promedio <-print(summarySE(na.omit(long.data), measurevar="TASA", 
#                                 groupvars=c("ESPECIES", "TAXA"), 
#                                 na.rm=TRUE))
# 
# test_promedio$ESPECIES <-
#   factor(test_promedio$ESPECIES,
#          levels=c(row.names(biv_arrange), row.names(gas_arrange),row.names(crus_arrange),row.names(equino_arrange)),
#          labels=c(row.names(biv_arrange), row.names(gas_arrange),row.names(crus_arrange),row.names(equino_arrange)))
# 
# 
# lapply(split(test_promedio, test_promedio$TAXA), function(x)
#   kruskal.test(TASA ~ ESPECIES, data = x))
# 
# lapply(split(test_promedio, test_promedio$TAXA), function(x)
#   posthoc.kruskal.nemenyi.test(TASA ~ ESPECIES, data = x,
#                                method="Chisquare", na.action(na.omit(data))))




# Comparación de las tasas de reclutamiento para cada grupo taxonómico entre campañas realizadas
ww<-as.data.frame(matrix(unlist(lapply(split(long.data, long.data$TAXA), function(x)
  kruskal.test(TASA ~ CAMPANA, data = x))), nrow=4, byrow=T, 
  dimnames = list(c("Bivalvia", "Gastropoda", "Crustacea", "Echinodermata"),
                  c("X2", "df", "p", "test", "variable"))), 
  stringsAsFactors=FALSE)

data.frame(Grupo = dimnames(ww)[[1]],
           Test = ww[,"test"], 
           X2 = round(as.numeric(ww[,"X2"]),3),
           df = ww[,"df"],
           p = format(as.numeric(ww[,"p"]),scientific = TRUE, digits = 4), 
           stringsAsFactors=FALSE)


# Comparaciones múltiples de las tasas de reclutamiento promedio entre grupos taxonómicos para cada una de las campañas
lapply(split(long.data, long.data$TAXA), function(x)
  posthoc.kruskal.nemenyi.test(TASA ~ MES, data = x,
                               method="Chisquare", na.action(na.omit(data))))

### entre especies de cada grupo!!
#Comparación de las tasas de reclutamiento promedio entre especies en cada grupo taxonómico
yy<-as.data.frame(matrix(unlist(lapply(split(long.data, long.data$TAXA), function(x)
  kruskal.test(TASA ~ ESPECIES, data = x))), nrow=4, byrow=T, 
  dimnames = list(c("Bivalvia", "Gastropoda", "Crustacea", "Echinodermata"),
                  c("X2", "df", "p", "test", "variable"))), 
  stringsAsFactors=FALSE)

data.frame(Grupo = dimnames(yy)[[1]],
           Test = yy[,"test"], 
           X2 = round(as.numeric(yy[,"X2"]),3),
           df = yy[,"df"],
           p = format(as.numeric(yy[,"p"]),scientific = TRUE, digits = 4), 
           stringsAsFactors=FALSE)

#BIVALVIA =====
# tasas de reclutamiento total para todas las especies del grupo Bivalvia 
arrange((ddply((Bivalvia), .(ESPECIES), summarize, 
               n = round(sum(ABUNDANCIA, na.rm=TRUE), 0),
               mean = round(mean(TASA, na.rm=TRUE), 2),
               sd = round(sd(TASA, na.rm=TRUE),1))), desc(mean))

# Número de reclutas totales y tasas de reclutamiento por especies para el grupo Bivalvia para cada campaña
arrange((ddply((Bivalvia), .(CAMPANA, ESPECIES), summarize, 
               n = round(sum(ABUNDANCIA, na.rm=TRUE), 0),
               mean = round(mean(TASA, na.rm=TRUE), 3),
               sd = round(sd(TASA, na.rm=TRUE),2))), CAMPANA, desc(n))

#Comparaciones múltiples de las tasas de reclutamiento promedio de las especies del grupo Bivalvia.
# lapply(list(Bivalvia), function (x) posthoc.kruskal.nemenyi.test(TASA ~ ESPECIES, data=x))
posthoc.kruskal.nemenyi.test(TASA ~ ESPECIES, Bivalvia,  
                             method="Chisquare", na.action(na.omit(data)))

# test entre campañas por especies
dput(unique (Bivalvia$ESPECIES))
spnames_test<-c("P.purpuratus", "E.cuneata", "A.atra", "B.granulata", 
                "Eurhomalea.sp", "Bivalvo.sp1", "H.solida", "S.algosus", "A.purpuratus", 
                "C.compressa", "E.macha", "C.tegulata", "C.chorus")
#   c("P.purpuratus", "E.cuneata", "A.atra", "B.granulata", 
#                 "Bivalvo.sp1", "H.solida", "Eurhomalea.sp", "S.algosus", "E.macha", 
#                 "C.compressa", "C.tegulata")
temp_test <-lapply((data.frame.tasa[spnames_test]),
                   function(x) kruskal.test(x ~ CAMPANA, na.action(na.omit(data)), data=data.frame.tasa))

df_tem<-as.data.frame(matrix(unlist(temp_test), nrow=length(temp_test), byrow=T, 
                             dimnames = list(c(spnames_test),
                                             c("W", "df", "p", "test", "variable"))), 
                      stringsAsFactors=FALSE)
# redondea decimales
tabla_test <-data.frame(rep("Kruskal-Wallis",length(temp_test)),
                        round(as.numeric(as.character(df_tem[,1])),4),
                        round(as.numeric(as.character(df_tem[,2])),0),
                        round(as.numeric(as.character(df_tem[,3])),5))

dimnames (tabla_test) <- list(c(spnames_test),c("Test","W", "df", "p"))

tabla_test


# # comparaciones multiples entre campañas
lapply((data.frame.tasa[spnames_test]),
       function(x) posthoc.kruskal.nemenyi.test(x ~ CAMPANA, na.action(na.omit(data)), data=data.frame.tasa))

# lapply(list(Bivalvia), function (x) posthoc.kruskal.nemenyi.test(TASA ~ MES, data=x, subset = x$ESPECIES))

# BIVALVIA LINEA 
# Comparación de las tasas de reclutamiento para cada grupo taxonómico entre lineas de monitoreo
as.data.frame(matrix(unlist(lapply(split(long.data, long.data$TAXA), function(x)
  kruskal.test(TASA ~ LINEA, data = x))), nrow=4, byrow=T, 
  dimnames = list(c("Bivalvia", "Gastropda", "Crustacea", "Equinodermata"),
                  c("X2", "df", "p", "test", "variable"))), 
  stringsAsFactors=FALSE)


# Bivalvos tabla especies y lineas 
arrange((ddply((Bivalvia), .(LINEA, ESPECIES), summarize, 
               n = round(sum(ABUNDANCIA, na.rm=TRUE), 0),
               mean = round(mean(TASA, na.rm=TRUE), 1),
               sd = round(sd(TASA, na.rm=TRUE),1))), LINEA, desc(mean))

# Bivalvos test por especies entre lineas (5)
# Comparación de las tasas de reclutamiento entre líneas de monitoreo para los Bivalvos 
bl<-as.data.frame(
  matrix(
    unlist(
      lapply(split(Bivalvia, Bivalvia$ESPECIES), function(x)
  kruskal.test(TASA ~ LINEA, data = x))
  ), 
  nrow=length(unique(Bivalvia$ESPECIES)), byrow=T, 
  dimnames = list(c("P.purpuratus", "E.cuneata", "A.atra", "B.granulata", 
                    "Bivalvo.sp1", "H.solida", "Eurhomalea.sp", "S.algosus", "E.macha", 
                    "C.compressa", "A.purpuratus", "C.tegulata", "C.chorus"),
                  c("X2", "df", "p", "test", "variable"))), 
  stringsAsFactors=FALSE)

data.frame(Grupo = dimnames(bl)[[1]],
           Test = bl[,"test"], 
           X2 = round(as.numeric(bl[,"X2"]),3),
           df = bl[,"df"],
           p = format(as.numeric(bl[,"p"]),scientific = TRUE, digits = 4), 
           stringsAsFactors=FALSE)

# comparacion multiple para el que tiene dif sig
posthoc.kruskal.nemenyi.test(TASA ~ LINEA, Bivalvia,  
                             subset = Bivalvia$ESPECIES == c("P.purpuratus"),
                             method="Chisquare", na.action(na.omit(data)))
posthoc.kruskal.nemenyi.test(TASA ~ LINEA, Bivalvia,  
                             subset = Bivalvia$ESPECIES == c("A.atra"),
                             method="Chisquare", na.action(na.omit(data))) 
posthoc.kruskal.nemenyi.test(TASA ~ LINEA, Bivalvia,  
                             subset = Bivalvia$ESPECIES == c("B.granulata"),
                             method="Chisquare", na.action(na.omit(data)))

# BIVALVIA PROFUNDIDAD 
 
# tabla especies y profundidad 
arrange((ddply((Bivalvia), .(PROFUNDIDAD, ESPECIES), summarize, 
               n = round(sum(ABUNDANCIA, na.rm=TRUE), 0),
               mean = round(mean(TASA, na.rm=TRUE), 3),
               sd = round(sd(TASA, na.rm=TRUE),2))), PROFUNDIDAD, desc(mean))

# Test dif entre estratos por especie
# lapply(split(Bivalvia, Bivalvia$ESPECIES), function(x)
#   wilcox.test(TASA ~ PROFUNDIDAD, data = na.omit(x)))

bp<-as.data.frame(
  matrix(
    unlist(
      lapply(split(Bivalvia, Bivalvia$ESPECIES), function(x)
        wilcox.test(TASA ~ PROFUNDIDAD, data = x))
    ), 
    nrow=length(unique(Bivalvia$ESPECIES)), byrow=T, 
    dimnames = list(spnames_test, c("U","p","alt","met","corr", "by"))), 
  stringsAsFactors=FALSE)

data.frame(Grupo = dimnames(bp)[[1]],
           Test = bp[,"corr"], 
           U = round(as.numeric(bp[,"U"]),3),
           p = format(as.numeric(bp[,"p"]),scientific = TRUE, digits = 4), 
           stringsAsFactors=FALSE)

 

#GASTROPODA====

# tasas de reclutamiento total para todas las especies del grupo gastropoda 
arrange((ddply((Gastropoda), .(ESPECIES), summarize, 
               n = round(sum(ABUNDANCIA, na.rm=TRUE), 0),
               mean = round(mean(TASA, na.rm=TRUE), 3),
               sd = round(sd(TASA, na.rm=TRUE),2))), desc(n))

# Número de reclutas totales y tasas de reclutamiento por especies para el grupo gastropoda para cada campaña
arrange((ddply((Gastropoda), .(CAMPANA, ESPECIES), summarize, 
               n = round(sum(ABUNDANCIA, na.rm=TRUE), 0),
               mean = round(mean(TASA, na.rm=TRUE), 3),
               sd = round(sd(TASA, na.rm=TRUE),2))), CAMPANA, desc(n))

#Comparaciones múltiples de las tasas de reclutamiento promedio de las especies del grupo Gastropoda
lapply(list(Gastropoda), function (x) posthoc.kruskal.nemenyi.test(TASA ~ ESPECIES, data=x))
posthoc.kruskal.nemenyi.test(TASA ~ ESPECIES, Gastropoda,  method="Chisquare", na.action(na.omit(data)))


#Comparación de las tasas de reclutamiento entre campañas de monitoreo para las especies de Gastrópodos
dput(row.names(gas_arrange))
spnames_test<-c("M.unifasciata", "P.niger", "T.cingulata", "C.dilatata", "S.lessoni", 
                "T.umbilicata", "Tegula.spp", "C.chilensis", "N.araucana", "C.crassilabrum", 
                "Cerithidae", "Triphoridae", "Fissurella.spp", 
                "L.cancellata", "N.gayii", "C.concholepas", "S.radwini")


temp_test <-lapply((data.frame.tasa[spnames_test]),
                   function(x) kruskal.test(x ~ CAMPANA, na.action(na.omit(data)), data=data.frame.tasa))

df_tem<-as.data.frame(matrix(unlist(temp_test), nrow=length(temp_test), byrow=T, 
                             dimnames = list(c(spnames_test),
                                             c("W", "df", "p", "test", "variable"))), 
                      stringsAsFactors=FALSE)
# redondea decimales
tabla_test <-data.frame(rep("Kruskal-Wallis",length(temp_test)),
                        round(as.numeric(as.character(df_tem[,1])),4),
                        round(as.numeric(as.character(df_tem[,2])),0),
                        round(as.numeric(as.character(df_tem[,3])),5))

dimnames (tabla_test) <- list(c(spnames_test),c("Test","W", "df", "p"))

tabla_test


# # comparaciones multiples entre campañas
#Comparaciones múltiples de las tasas de reclutamiento entre campañas de monitoreo para las especie de Gastrópodos.
temp_test_G<-lapply((data.frame.tasa[spnames_test]),
       function(x) posthoc.kruskal.nemenyi.test(x ~ CAMPANA, na.action(na.omit(data)), data=data.frame.tasa))

xxx<-sapply(temp_test_G, "[", i = 3)

# do.call(rbind.data.frame, xxx)
ccc<-ldply (xxx, data.frame)
# ldply (xxx, data.frame, .id = )
# xxx[[1]][[2]][[1]]

data.frame("Especies" = ccc[,1], round(ccc[,-1], 5))

# Gastropoda LINEA 
# Gastrop tabla especies y lineas es muy larga para el word!!
arrange((ddply((Gastropoda), .(LINEA, ESPECIES), summarize, 
               n = round(sum(ABUNDANCIA, na.rm=TRUE), 0),
               mean = round(mean(TASA, na.rm=TRUE), 3),
               sd = round(sd(TASA, na.rm=TRUE),2))), LINEA, desc(mean))

# Comparación de las tasas de reclutamiento entre líneas de monitoreo para las especies de Gastrópodos

ww<-as.data.frame(
  matrix(
    unlist(
      lapply(split(Gastropoda, Gastropoda$ESPECIES), function(x)
        kruskal.test(TASA ~ LINEA, data = x))
    ), 
    nrow=18, byrow=T, 
    dimnames = list(c("M.unifasciata", "P.niger", 
                      "T.cingulata", "C.dilatata", "T.umbilicata", "S.lessoni", "C.chilensis", 
                      "Cerithidae", "N.araucana", "Tegula.spp", "X.cassidiformis", 
                      "L.cancellata", "C.crassilabrum", "Triphoridae", "N.gayii", "C.concholepas", 
                      "Fissurella.spp", "S.radwini"),
                    c("X2", "df", "p", "test", "variable"))), 
  stringsAsFactors=FALSE)

data.frame("Test" = rep("Kruskal-Wallis", n=nrow(ww)), "W"= round(as.numeric(unlist(ww["X2"])), 3), ww["df"],"p"= round(as.numeric(unlist(ww["p"])), 5), stringsAsFactors = FALSE)

# comparacion multiple para el que tiene dif sig
nn<- rownames(ww[ which(ww$p < .05), ])
#   rownames(ww["p" < 0.05])
# posthoc.kruskal.nemenyi.test(TASA ~ LINEA, Gastropoda,  subset = Gastropoda$ESPECIES == nn,
#                              method="Chisquare", na.action(na.omit(data)))

G<-lapply((data.frame.tasa[nn]),
                    function(x) posthoc.kruskal.nemenyi.test(x ~ LINEA, na.action(na.omit(data)), 
                                                             data=data.frame.tasa))

g<-sapply(G, "[", i = 3)

# do.call(rbind.data.frame, xxx)
ddd<-ldply (g, data.frame)


data.frame("Especies" = ddd[,1], round(ddd[,-1], 5))

# Gastropoda PROFUNDIDAD

# tabla especies y profundidad 
arrange((ddply((Gastropoda), .(PROFUNDIDAD, ESPECIES), summarize, 
               n = round(sum(ABUNDANCIA, na.rm=TRUE), 0),
               mean = round(mean(TASA, na.rm=TRUE), 3),
               sd = round(sd(TASA, na.rm=TRUE),1))), PROFUNDIDAD, desc(mean))

# Comparación de las tasas de reclutamiento entre profundidad del colector (SUP y FON) 
# para cada una de la especies del grupo Gastropoda. 

pop<-as.data.frame(
  matrix(
    unlist(
      lapply(split(Gastropoda, Gastropoda$ESPECIES), function(x)
        wilcox.test(TASA ~ PROFUNDIDAD, data = x))
    ), 
    nrow=length(unique(Gastropoda$ESPECIES)), byrow=T, 
    dimnames = list(c("M.unifasciata", "P.niger", 
                      "T.cingulata", "C.dilatata", "T.umbilicata", "S.lessoni", "C.chilensis", 
                      "Cerithidae", "N.araucana", "Tegula.spp", "X.cassidiformis", 
                      "L.cancellata", "C.crassilabrum", "Triphoridae", "N.gayii", "C.concholepas", 
                      "Fissurella.spp", "S.radwini"))), 
  stringsAsFactors=FALSE)

data.frame("Especies"=rownames(pop), "Test"=rep("Mann-Whitney",nrow(pop)),
                                    "U"= round(as.numeric(pop[,1]),1),
                                    "p"= round(as.numeric(pop[,2]),5))

#CRUSTACEA====
dput(row.names(crus_arrange))

# tasas de reclutamiento total para todas las especies del grupo 
arrange((ddply((Crustacea), .(ESPECIES), summarize, 
               n = round(sum(ABUNDANCIA, na.rm=TRUE), 0),
               mean = round(mean(TASA, na.rm=TRUE), 3),
               sd = round(sd(TASA, na.rm=TRUE),2))), desc(mean))

# Número de reclutas totales y tasas de reclutamiento por especies para el grupo gastropoda para cada campaña
arrange((ddply((Crustacea), .(CAMPANA, ESPECIES), summarize, 
               n = round(sum(ABUNDANCIA, na.rm=TRUE), 0),
               mean = round(mean(TASA, na.rm=TRUE), 3),
               sd = round(sd(TASA, na.rm=TRUE),2))), CAMPANA, desc(n))

# test comparaciones multiples especies promedio
lapply(list(Crustacea), function (x) posthoc.kruskal.nemenyi.test(TASA ~ ESPECIES, data=x))
posthoc.kruskal.nemenyi.test(TASA ~ ESPECIES, Crustacea,  method="Chisquare", na.action(na.omit(data)))

# test entre campañas por especies
dput(unique (Crustacea$ESPECIES))
spnames_test<-c("P.perlatus", "P.grossimanus", "R.polyodon", "H.planatus","T.dentatus", "P.edwarsii")
temp_test <-lapply((data.frame.tasa[spnames_test]),
                   function(x) kruskal.test(x ~ CAMPANA, na.action(na.omit(data)), data=data.frame.tasa))

df_tem<-as.data.frame(matrix(unlist(temp_test), nrow=length(temp_test), byrow=T, 
                             dimnames = list(c(spnames_test),
                                             c("W", "df", "p", "test", "variable"))), 
                      stringsAsFactors=FALSE)
# redondea decimales
tabla_test <-data.frame(rep("Kruskal-Wallis",nrow(df_tem)),
                        round(as.numeric(as.character(df_tem[,1])),4),
                        round(as.numeric(as.character(df_tem[,2])),0),
                        round(as.numeric(as.character(df_tem[,3])),5))

dimnames (tabla_test) <- list(c(spnames_test),c("Test","X2", "df", "p"))

tabla_test


#Comparaciones múltiples de las tasas de reclutamiento entre campañas de monitoreo para las especie de Gastrópodos.
temp_test_G<-lapply((data.frame.tasa[spnames_test]),
                    function(x) posthoc.kruskal.nemenyi.test(x ~ CAMPANA, na.action(na.omit(data)), data=data.frame.tasa))

xxx<-sapply(temp_test_G, "[", i = 3)

# do.call(rbind.data.frame, xxx)
ccc<-ldply (xxx, data.frame)
# ldply (xxx, data.frame, .id = )
# xxx[[1]][[2]][[1]]

data.frame("Especies" = ccc[,1], round(ccc[,-1], 5))


# Crustacea LINEA 
# tabla especies y lineas es muy larga para el word!!
arrange((ddply((Crustacea), .(LINEA, ESPECIES,PROFUNDIDAD), summarize, 
               n = round(sum(ABUNDANCIA, na.rm=TRUE), 0),
               mean = round(mean(TASA, na.rm=TRUE), 1),
               sd = round(sd(TASA, na.rm=TRUE),1))), LINEA, PROFUNDIDAD,desc(mean))

# Comparación de las tasas de reclutamiento entre líneas de monitoreo para las especies de Crustáceos

ww<-as.data.frame(
  matrix(
    unlist(
      lapply(split(Crustacea, Crustacea$ESPECIES), function(x)
        kruskal.test(TASA ~ LINEA, data = x))
    ), 
    nrow=length(unique(Crustacea$ESPECIES)), byrow=T, 
    dimnames = list(c("P.perlatus", 
                      "P.grossimanus", "R.polyodon", "B.truncatus", "R.typus", "H.planatus", 
                      "T.dentatus", "P.edwarsii"),
                    c("X2", "df", "p", "test", "variable"))), 
  stringsAsFactors=FALSE)

data.frame("Test" = rep("Kruskal-Wallis", n=nrow(ww)), "X2"= round(as.numeric(unlist(ww["X2"])), 3), ww["df"],"p"= round(as.numeric(unlist(ww["p"])), 5), stringsAsFactors = FALSE)

# Comparaciones múltiples de las tasas de reclutamiento promedio entre líneas de monitoreo
# para el que tiene dif sig nn
nn<- c("P.perlatus", "P.grossimanus")#rownames(ww[ which(ww$p < .05), ])
# rownames(ww["p" < 0.05])
# posthoc.kruskal.nemenyi.test(TASA ~ LINEA, Crustacea,  subset = Crustacea$ESPECIES == nn,
#                              method="Chisquare", na.action(na.omit(data)))

G<-lapply((data.frame.tasa[nn]),
          function(x) posthoc.kruskal.nemenyi.test(x ~ LINEA, na.action(na.omit(data)), data=data.frame.tasa))

g<-sapply(G, "[", i = 3)

# do.call(rbind.data.frame, xxx)
ddd<-ldply (g, data.frame)


data.frame("Especies" = ddd[,1], round(ddd[,-1], 5))

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


# Crustacea PROFUNDIDAD 

# tabla especies y profundidad 
arrange((ddply((Crustacea), .(PROFUNDIDAD, ESPECIES), summarize, 
               n = round(sum(ABUNDANCIA, na.rm=TRUE), 0),
               mean = round(mean(TASA, na.rm=TRUE), 3),
               sd = round(sd(TASA, na.rm=TRUE),2))), PROFUNDIDAD, desc(mean))

# Comparación de las tasas de reclutamiento entre profundidad del colector (SUP y FON) para cada una de la especies del grupo Crustacea

pop<-as.data.frame(
  matrix(
    unlist(
      lapply(split(Crustacea, Crustacea$ESPECIES), function(x)
        wilcox.test(TASA ~ PROFUNDIDAD, data = x))
    ), 
    nrow=length(unique(Crustacea$ESPECIES)), byrow=T, 
    dimnames = list(c("P.perlatus", 
                      "P.grossimanus", "R.polyodon", "B.truncatus", "R.typus", "H.planatus", 
                      "T.dentatus", "P.edwarsii"))), 
  stringsAsFactors=FALSE)

data.frame("Especies"=rownames(pop), 
           "Test"=rep("Mann-Whitney",nrow(pop)),
           "U"= round(as.numeric(pop[,1]),1),
           "p"= round(as.numeric(pop[,2]),5))

#EQUINODERMATA====
dput(unique (Equinodermata$ESPECIES))

# tasas de reclutamiento total para todas las especies del grupo 
arrange((ddply((Equinodermata), .(ESPECIES), summarize, 
               n = round(sum(ABUNDANCIA, na.rm=TRUE), 0),
               mean = round(mean(TASA, na.rm=TRUE), 3),
               sd = round(sd(TASA, na.rm=TRUE),2))), desc(n))

# Número de reclutas totales y tasas de reclutamiento por especies para el grupo gastropoda para cada campaña
arrange((ddply((Equinodermata), .(CAMPANA, ESPECIES), summarize, 
               n = round(sum(ABUNDANCIA, na.rm=TRUE), 0),
               mean = round(mean(TASA, na.rm=TRUE), 3),
               sd = round(sd(TASA, na.rm=TRUE),2))), CAMPANA, desc(n))

# test comparaciones multiples especies promedio
lapply(list(Equinodermata), function (x) posthoc.kruskal.nemenyi.test(TASA ~ ESPECIES, data=x))
# posthoc.kruskal.nemenyi.test(TASA ~ ESPECIES, Equinodermata,  method="Chisquare", na.action(na.omit(data)))

# Comparación de las tasas de reclutamiento entre campañas de monitoreo para todas las especies de Equinodermos. 
dput(unique (Equinodermata$ESPECIES))
spnames_test<-c("T.niger", "L.albus", 
                 "Ophioplus.sp")
temp_test <-lapply((data.frame.tasa[spnames_test]),
                   function(x) kruskal.test(x ~ CAMPANA, na.action(na.omit(data)), data=data.frame.tasa))

df_tem<-as.data.frame(matrix(unlist(temp_test), nrow=length(temp_test), byrow=T, 
                             dimnames = list(c(spnames_test),
                                             c("W", "df", "p", "test", "variable"))), 
                      stringsAsFactors=FALSE)
data.frame("Especies"=rownames(df_tem),
           "test"=df_tem[,4],
           "W"= round(as.numeric(as.character(df_tem[,1])),4),
          "df"=df_tem[,2],
          "p"=round(as.numeric(as.character(df_tem[,3])),5)
          )



# # comparaciones multiples entre campañas
#Comparaciones múltiples de las tasas de reclutamiento entre campañas de monitoreo para las especie de Gastrópodos.
temp_test_G<-lapply((data.frame.tasa[spnames_test]),
                    function(x) posthoc.kruskal.nemenyi.test(x ~ CAMPANA, na.action(na.omit(data)), 
                                                             data=data.frame.tasa))

xxx<-sapply(temp_test_G, "[", i = 3)

ccc<-ldply (xxx, data.frame)#convierte a data frame las matrices de probabilidad

data.frame("Especies" = ccc[,1], round(ccc[,-1], 5))


# tabla especies y lineas es muy larga para el word!!
arrange((ddply((Equinodermata), .(LINEA, ESPECIES), summarize, 
               n = round(sum(ABUNDANCIA, na.rm=TRUE), 0),
               mean = round(mean(TASA, na.rm=TRUE), 3),
               sd = round(sd(TASA, na.rm=TRUE),2))), LINEA, desc(n))

# Comparación de las tasas de reclutamiento entre líneas de monitoreo para todas las especies del grupo Echinodermata

ww<-as.data.frame(
  matrix(
    unlist(
      lapply(split(Equinodermata, Equinodermata$ESPECIES), function(x)
        kruskal.test(TASA ~ LINEA, data = x))
    ), 
    nrow=length(unique(Equinodermata$ESPECIES)), byrow=T, 
    dimnames = list(c("T.niger", "L.albus", 
                      "S.striatus", "Ophioplus.sp"),
                    c("X2", "df", "p", "test", "variable"))), 
  stringsAsFactors=FALSE)

data.frame("Test" = rep("Kruskal-Wallis", n=nrow(ww)), 
           "W"= round(as.numeric(unlist(ww["X2"])), 3), 
           ww["df"],"p"= round(as.numeric(unlist(ww["p"])), 5), stringsAsFactors = FALSE)

# comparacion multiple para el que tiene dif sig
nn<- rownames(ww[ which(ww$p < .05), ])
rownames(ww["p" < 0.05])
posthoc.kruskal.nemenyi.test(TASA ~ LINEA, Equinodermata,  subset = Equinodermata$ESPECIES == nn,
                             method="Chisquare", na.action(na.omit(data)))

G<-lapply((data.frame.tasa[nn]),
          function(x) posthoc.kruskal.nemenyi.test(x ~ LINEA, na.action(na.omit(data)), data=data.frame.tasa))

g<-sapply(G, "[", i = 3)

# do.call(rbind.data.frame, xxx)
ddd<-ldply (g, data.frame)


data.frame("Especies" = ddd[,1], round(ddd[,-1], 5))

# Equinodermata PROFUNDIDAD 

# tabla especies y profundidad 
arrange((ddply((Equinodermata), .(PROFUNDIDAD, ESPECIES), summarize, 
               n = round(sum(ABUNDANCIA, na.rm=TRUE), 0),
               mean = round(mean(TASA, na.rm=TRUE), 3),
               sd = round(sd(TASA, na.rm=TRUE),2))), PROFUNDIDAD, desc(mean))

# Comparación de las tasas de reclutamiento entre profundidad del colector (SUP y FON) para cada una de la especies del grupo Echinodermata

pop<-as.data.frame(
  matrix(
    unlist(
      lapply(split(Equinodermata, Equinodermata$ESPECIES), function(x)
        wilcox.test(TASA ~ PROFUNDIDAD, data = x))
    ), 
    nrow=length(unique(Equinodermata$ESPECIES)), byrow=T, 
    dimnames = list(c("T.niger", "L.albus", 
                      "S.striatus", "Ophioplus.sp"))), 
  stringsAsFactors=FALSE)

data.frame("Especies"=rownames(pop), "Test"=rep("Mann-Whitney",nrow(pop)),
           "U"= round(as.numeric(pop[,1]),1),
           "p"= round(as.numeric(pop[,2]),5))



# Ejercicio


