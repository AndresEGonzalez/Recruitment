# script.Anglo.tablas.tax.grup.R
# Confecciona tablas para cada grupo taxonomico

# tabla :Número total de reclutas (n) y tasas de reclutamiento promedios por campaña 
  arrange(summarySE(long.data, measurevar="TASA", 
                groupvars=c("MES", "TAXA"), 
                na.rm=TRUE), MES, desc(TASA))



# grupos taxonomicos=====
# Tasas de Reclutamiento Total por Especie grupo Bivalvia ====

# tabla promedio ~ especies grupo
lapply(split(long.data, long.data$TAXA), function(x)
  as.data.frame (arrange((ddply(x, .(ESPECIES), summarize, 
                                n = round(sum(ABUNDANCIA, na.rm=TRUE), 0),
                                mean = round(mean(TASA, na.rm=TRUE), 2),
                                sd = round(sd(TASA, na.rm=TRUE),1))), desc(mean))))

# tabla campañas ~ especies grupo
lapply(split(long.data, long.data$TAXA), function(x)
  as.data.frame(arrange((ddply(na.omit(x), .(CAMPANA, ESPECIES), summarize,
                               n = round(sum(ABUNDANCIA, na.rm=TRUE), 0),
                               mean = round(mean(TASA, na.rm=TRUE), 2),
                               sd = round(sd(TASA, na.rm=TRUE),1))), CAMPANA, desc(mean))))

print(summarySE(Bivalvia, measurevar="TASA", 
                groupvars=c("MES"), 
                na.rm=TRUE))

# Por ESPECIES
d_colectores_bivalvos <- print(summarySE(Bivalvia, measurevar="TASA", 
                                         groupvars=c("MES", "ESPECIES"), 
                                         na.rm=TRUE))
# PROMEDIO, sd Y ABUNDANCIA:  CAMPAÑA ~ ESPECIES BIVALVOS
arrange((ddply(Bivalvia, .(CAMPANA, TAXA), summarize, 
               mean = round(mean(TASA, na.rm=TRUE), 2),
               sd = round(sd(TASA, na.rm=TRUE),1),
               n = round(sum(ABUNDANCIA, na.rm=TRUE), 0))),CAMPANA, desc(mean))