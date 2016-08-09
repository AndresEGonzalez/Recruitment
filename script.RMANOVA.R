# RMANOVA
# Analisis de medidas repetidas


setwd("~/Documents/1_WORKING/DATA/")
dir()

d<-read.delim("Rmanova.txt")

str(d)

dotchart(d$Eggs)
interaction.plot(d$Time, factor(d$Group), d$Eggs)
 plot(d, outer=T)
 
 
 #Mixed-effect models
 
 library(nlme)
 
 model <- lme(
   (Eggs) ~ factor(Time),
   random = ~1|ID,
   data=d
 )
 
 plot(model)#homogeneidad de varianzas
 
 qqnorm(model, ~resid(.)|ID)# normalidad de las replicas
 
 summary(model)
 
 anova(model)
 
 #analizando con mixed.model
 
 model2 <-lme(
   (Eggs) ~ factor(Group)+factor(Time),
   random = ~1|ID, data=d
 )
 
 anova(model2)
 plot(model2)
 
 ########################################################
 #datos colectores
 
 mod_colector<-lme(
   (TASA)+(MES) ~ (PROFUNDIDAD),
   # (TASA) ~ (PROFUNDIDAD)+(MES),
   # (TASA) ~ (PROFUNDIDAD),
   # (TASA) ~ (MES),
   random = ~1|REPLICA, data=long.data,
   na.action = na.exclude
 )
 summary(mod_colector)
 anova(mod_colector)
 plot(mod_colector)
 
 
 # post-Hoc testing http://stats.stackexchange.com/questions/14078/post-hoc-test-after-anova-with-repeated-measures-using-r
 install.packages("multcomp")
 library(multcomp)
 
#  lme_velocity = lme(Velocity ~ Material, data=scrd, random = ~1|Subject)
#  anova(lme_velocity)
#  
#  require(multcomp)
#  summary(glht(lme_velocity, linfct=mcp(Material = "Tukey")), test = adjusted(type = "bonferroni"))

 #entre campañas
 summary(
   glht(
     mod_colector, linfct=mcp(MES = "Tukey")), test = adjusted(type = "bonferroni")
   )
 
 #entre profundidad
 summary(
   glht(
     mod_colector, linfct=mcp(PROFUNDIDAD = "Tukey")), 
   test = adjusted(type = "bonferroni")
 )
 
 #ambas variables
 summary(
   glht(
     mod_colector, linfct=mcp(MES="Tukey", PROFUNDIDAD = "Tukey")), 
     test = adjusted(type = "bonferroni")
 )