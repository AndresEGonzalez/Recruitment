######################################################################################
# Script.colectores_Angloamerican_2015.R 
# Confecciona figuras, tablas, archivos y analisis del
# informe de reclutamiento de invertebrados Marinos
# Tasas de reclutamiento en colectores pasivos Tuffy
# Cinco lineas de monitoreo :L1, L2, L3, LT (Torre) y LN (Norte). 
# 30 muestras
# Dos estratos de superficie: SUP (5m) y FON (L1,2 y 3 a 16m; LT y LN a 20m)
# Agrega temporalidad campaña 1, 2....etc (MES).
# Creado: 04/2015
# Update: 09/08/2016
# Author: AEGL
######################################################################################

# CALL PKGS ==========================================================================
rm(list=ls())
library(reshape2)
library(ggplot2)
library(PMCMR)
library(grid)
library(vegan)
library(MASS)
library(WriteXLS)
library(dataframes2xls)#package ‘dataframe2xls’ is not available (for R version 3.3.1)
library(plyr)
library(scales)
library(ReporteRs)#depend on Rjava 
# install Java ubuntu 16.04 https://www.digitalocean.com/community/tutorials/how-to-install-java-with-apt-get-on-ubuntu-16-04
# configure Java Path sudo R CMD javareconf
source("~/Documents/1_WORKING/SCRIPTS/R_CRAN_script_files/summarySE.R")

# PREPARA DATOS ======================================================================
setwd("~/Documents/1_WORKING/DATA/COLECTORES/2015/ANGLOAMERICAN/ENE_FEB_2015_ANGLO")
d <- read.csv("colectores_ANGLO_data.csv",
              header=TRUE, sep = ",", dec = ".")
# # Tiempo de permanencia en días
# date1 <- as.Date("2015-09-02")
# date2 <- as.Date("2015-11-03")
# 
# date2-date1

# Escribe el archivo excel Costa_Sur_Master_Angloamerican_fecha.xls =========
# Excel file == Cuatro pestañas una para cada data frame:
# a) Abundancia, b) Tasas, c) Binaria y c) Longdata format tasas y abundancia
# HOJA 1 ABUNDANCIA
data.frame.abundancia <-
  data.frame( "CIUDAD"= rep("CHAÑARAL", nrow(d)),
              "SITIO"= rep("ANGLO", nrow(d)),
              d[,6:9, drop=T], #"CAMPANA""INSTALADO""RETIRO""PERMANENCIA"
              d[,2:5, drop=T], #"LINEA""PROFUNDIDAD"COLECTOR""REPLICA"
              d[,11:ncol(d), drop=T], #sp's
              check.names = FALSE)
# HOJA 2 TASAS
# Tasa reclutamiento por colector en (permanencia) X dias
data.frame.tasa <- data.frame( data.frame.abundancia[,1:10],
                               data.frame.abundancia[,11:ncol(d)]/(data.frame.abundancia[,6]),
                               check.names = FALSE)
# HOJA 3 BINARIA: presencia ausencia
binaria <- data.frame(data.frame.tasa[,1:10, drop=T],
                      ifelse(data.frame.tasa[,11:ncol(data.frame.tasa),
                                             drop=T] > 0,1,0))
# HOJA 4 LONG FORMAT
long.data <- melt(data.frame.tasa,
                  id.vars = c("CIUDAD","SITIO","CAMPANA","INSTALADO","RETIRO",
                              "PERMANENCIA","LINEA","PROFUNDIDAD","COLECTOR","REPLICA"),
                  measure.vars=names(data.frame.tasa[11:ncol(data.frame.tasa)]),
                  variable.name="ESPECIES", value.name="TASA")
tail(long.data)
# Agrega column abundancia a long.format.data
abundancia<- melt(data.frame.abundancia,
                  id.vars = c("CIUDAD","SITIO","CAMPANA","INSTALADO","RETIRO",
                              "PERMANENCIA","LINEA","PROFUNDIDAD","COLECTOR","REPLICA"),
                  measure.vars=names(data.frame.abundancia[,11:ncol(data.frame.abundancia)]),
                  variable.name="ESPECIES", value.name="ABUNDANCIA")
long.data$ABUNDANCIA <-abundancia[,12]

# AGREGA COLUMNA GRUPO TAXONOMICO
#Lista especies
# dput(names(data.frame.abundancia[,11:ncol(data.frame.abundancia)]))
long.data$TAXA <-
  ifelse(long.data$ESPECIES %in% c(
    "P.perlatus", "P.grossimanus", "H.planatus", "T.dentatus", "R.polyodon",
    "B.truncatus", "P.edwarsii", "R.typus"), "CRUSTACEA",
    ifelse(long.data$ESPECIES %in% c(
      "C.chorus",
      "S.algosus", "P.purpuratus", "B.granulata", "E.cuneata", "E.macha",
      "Bivalvo.sp1", "Eurhomalea.sp", "H.solida", "C.compressa", "C.tegulata",
      "A.atra", "A.purpuratus"), "BIVALVIA",
      ifelse(long.data$ESPECIES %in% c(
        "S.radwini", "T.umbilicata", "C.crassilabrum", "M.unifasciata",
        "I.cingulata", "C.chilensis", "N.araucana", "C.dilatata", "N.gayii",
        "L.cancellata", "P.niger", "Tegula.spp", "Fissurella.spp", "C.concholepas",
        "Cerithidae","S.lessoni", "Triphoridae", "X.cassidiformis"), "GASTROPODA",
        ifelse(long.data$ESPECIES %in%
                 c("S.striatus", "T.niger", "L.albus", "Ophioplus.sp"),"EQUINODERMATA", "NA")
      )))
# Agrega columna mes para graficar serie de fechas, agregar fechas para cada campaña
long.data$MES <-
  ifelse(long.data$CAMPANA == 1, "Campaña 1", 
         ifelse(long.data$CAMPANA == 2, "Campaña 2", 
                ifelse(long.data$CAMPANA == 3, "Campaña 3",
                       ifelse(long.data$CAMPANA == 4,"Campaña 4", 
                              ifelse(long.data$CAMPANA == 5,"Campaña 5", 
                                     ifelse(long.data$CAMPANA == 6,"Campaña 6",
                                            ifelse(long.data$CAMPANA == 7,"Campaña 7",
                                                   ifelse(long.data$CAMPANA == 8,"Campaña 8",
                                            ifelse(long.data$CAMPANA == 9,"Campaña 9","")))))))))
## SORT FACTORS LEVELS ====
# FACTOR LEVELS MES (CAMPAÑA)
long.data$MES<-
  factor(long.data$MES,
         levels=c("Campaña 1", "Campaña 2", "Campaña 3","Campaña 4","Campaña 5", "Campaña 6", "Campaña 7", "Campaña 8", "Campaña 9"),
         labels=c("Campaña 1", "Campaña 2", "Campaña 3","Campaña 4", "Campaña 5", "Campaña 6", "Campaña 7","Campaña 8", "Campaña 9"))

# FACTOR LEVELS LINEA
long.data$LINEA <- factor(long.data$LINEA,
                          levels=c("LN","L3", "L2", "LT","L1"),
                          labels=c("LN","L3", "L2", "LT","L1"))
# FACTOR LEVELS ESTRATO DE PROFUNDIDAD
long.data$PROFUNDIDAD<- factor(long.data$PROFUNDIDAD,
                               levels=c("SUP", "FON"),
                               labels=c("SUP", "FON"))
# FACTOR LEVELS TAXA
long.data$TAXA <-   factor(long.data$TAXA,
                           levels=c("BIVALVIA", "GASTROPODA","CRUSTACEA", "EQUINODERMATA"),
                           labels=c("BIVALVIA", "GASTROPODA","CRUSTACEA", "EQUINODERMATA"))

## TAXONOMIC SUBSET ====
Bivalvia          <-subset (na.omit(long.data), TAXA == "BIVALVIA",
                            select= CIUDAD:ncol(long.data), drop=T)
#Sort Bivalvia by mean TASA desc Total
biv_arrange <-arrange(aggregate(Bivalvia$TASA, list(Bivalvia$ESPECIES), mean), desc(x))
arrange(aggregate(Bivalvia$TASA, list(Bivalvia$ESPECIES), sd), desc(x))
row.names(biv_arrange) <- biv_arrange$Group.1
Bivalvia$ESPECIES <-
  factor(Bivalvia$ESPECIES,
         levels=c(row.names(biv_arrange)),
         labels=c(row.names(biv_arrange)))

#Sort Gastropoda by mean TASA desc Total
Gastropoda        <-subset (na.omit(long.data), TAXA == "GASTROPODA",
                            select= CIUDAD:ncol(long.data), drop=T)

gas_arrange <-arrange(aggregate(Gastropoda$TASA, list(Gastropoda$ESPECIES), mean), desc(x))
arrange(aggregate(Gastropoda$TASA, list(Gastropoda$ESPECIES), sd), desc(x))
row.names(gas_arrange) <- gas_arrange$Group.1
Gastropoda$ESPECIES <-
  factor(Gastropoda$ESPECIES,
         levels=c(row.names(gas_arrange)),
         labels=c(row.names(gas_arrange)))

#Sort Crustacea by mean TASA desc Total
Crustacea         <-subset (na.omit(long.data), TAXA == "CRUSTACEA",
                            select= CIUDAD:ncol(long.data), drop=T)
# Crustacea         <-subset (long.data, TAXA == "CRUSTACEA",
#                             select= CIUDAD:ncol(long.data), drop=T)

crus_arrange <-arrange(aggregate(Crustacea$TASA, list(Crustacea$ESPECIES), mean), desc(x))
arrange(aggregate(Crustacea$TASA, list(Crustacea$ESPECIES), sd), desc(x))
row.names(crus_arrange) <- crus_arrange$Group.1
Crustacea$ESPECIES <-
  factor(Crustacea$ESPECIES,
         levels=c(row.names(crus_arrange)),
         labels=c(row.names(crus_arrange)))

#Sort Equinodermata by mean TASA desc Total
Equinodermata     <-subset (na.omit(long.data), TAXA == "EQUINODERMATA",
                            select= CIUDAD:ncol(long.data), drop=T)

equino_arrange <-arrange(aggregate(Equinodermata$TASA, list(Equinodermata$ESPECIES), mean), desc(x))
arrange(aggregate(Equinodermata$TASA, list(Equinodermata$ESPECIES), sd), desc(x))
row.names(equino_arrange) <- equino_arrange$Group.1
Equinodermata$ESPECIES <-
  factor(Equinodermata$ESPECIES,
         levels=c(row.names(equino_arrange)),
         labels=c(row.names(equino_arrange)))


# VARIABLES====!!!!!!!!!!!!! cambiar IMG.dir cada nueva campaña
#PLOT's
# The errorbars overlapped, so use position_dodge to move them horizontally
pd <- position_dodge(.1) # move them .05 to the left and right
#IMG folder
# dir.img <-setwd("~/Documents/DOCS_PERS/CONSULTORIA/2015_ANGLOAMERICA/CAMPAÑA_4/IMG")
campaña.IMG.dir <-paste("/home/dell/Documents/DOCS_PERS/CONSULTORIA/2015_ANGLOAMERICA/CAMPAÑA_9/IMG")

# save.image("/home/dell/Documents/DOCS_PERS/CONSULTORIA/2015_ANGLOAMERICA/Anglo_data.RData")


## Escribe planilla excel y pestañas para cada data frame

# setwd("~/Documents/DOCS_PERS/CONSULTORIA/2015_ANGLOAMERICA/CAMPAÑA_9")
# frames <- c("data.frame.abundancia", "data.frame.tasa", "binaria", "long.data")
# name.pestaña <- c("Abundancia", "Tasa", "Presencia", "Formato extendido")
# WriteXLS(frames,
#          ExcelFileName =
#            paste("Costa_Sur_Master_Angloamerican", format(Sys.Date(),"%d_%B_%Y"),
#                  ".xlsx", sep="_"),
#          SheetNames = name.pestaña, verbose=TRUE, AdjWidth = TRUE, BoldHeaderRow = TRUE,
#          Encoding = "UTF-8" )#"latin1")