# Heat map script Anglo
# hace heat map de reclutamiento espacio temporal

library(data.table)  # faster fread() and better weekdays()
library(dplyr)       # consistent data.frame operations
library(purrr)       # consistent & safe list/vector munging
library(tidyr)       # consistent data.frame cleaning
library(lubridate)   # date manipulation
library(countrycode) # turn country codes into pretty names
library(ggplot2)     # base plots are for Coursera professors
library(scales)      # pairs nicely with ggplot2 for plot label formatting
library(gridExtra)   # a helper for arranging individual ggplot objects
library(ggthemes)    # has a clean theme for ggplot2
library(viridis)     # best. color. palette. evar.
library(knitr)       # kable : prettier data.frame output
library(matlab)
library(colorRamps)
library(zoo)        # rollaplay
library(ggthemes)
library(RColorBrewer)






tasas.lineas <- print(summarySE(na.omit(long.data), measurevar="TASA", 
                                groupvars=c("LINEA", "MES"), 
                                na.rm=TRUE))
tasas.lineas$LINEA <- factor(tasas.lineas$LINEA,
                          levels=c("L1","LT", "L2", "L3","LN"),
                          labels=c("L1","LT", "L2", "L3","LN"))
tasas.lineas$date <-
  ifelse(tasas.lineas$MES == "Campaña 1", "2015-02-14",
         ifelse(tasas.lineas$MES ==  "Campaña 2", "2015-04-13",
                ifelse(tasas.lineas$MES == "Campaña 3", "2015-06-13",
                       ifelse(tasas.lineas$MES == "Campaña 4", "2015-08-07", 
                              ifelse(tasas.lineas$MES == "Campaña 5", "2015-10-03", 
                                     ifelse(tasas.lineas$MES == "Campaña 6","2015-11-26",""
                                            ))))))

tasas.lineas$date<- as.Date(tasas.lineas$date, "%Y-%m-%d")


quantile_range <- quantile(tasas.lineas$TASA, probs = seq(0, 1, 0.05))


# color_palette <- colorRampPalette(matlab.like2(length(tasas.lineas$TASA)))(length(quantile_range) - 1)
color_palette <- jet.colors(length(tasas.lineas$TASA))

# colorRampPalette(matlab.like2(length(tasas.lineas$TASA)))
label_text <- round(sort(tasas.lineas$TASA), 2)
  
#   rollapply(round(quantile_range, 2), width = 2, by = 1, 
#                         FUN = function(i) paste(i, collapse = " : "))



  ## remove background and axis from plot
  theme_change <- theme(
    plot.background = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
    panel.background = element_blank(),
    panel.border = element_blank(),
    axis.line = element_blank(),
    axis.ticks = element_blank(),
    axis.text.x = element_text(angle = 45, vjust = 1, 
size = 12, hjust = 1),
    axis.text.y = element_text(vjust = 1, 
size = 12, hjust = 1, face = "bold"),
    axis.title.x = element_text(size = 10),
    axis.title.y = element_text(size = 10),
  legend.justification = c(1, 0))
  # legend.position = c(0.6, 0.7),
  # legend.direction = "horizontal")
  

  

gg <- ggplot(tasas.lineas, aes(x=date, y=LINEA, fill=factor(TASA)))
gg <- gg + geom_tile(color="black") #geom_tile(color="white", size=0.1)
# gg <- gg + scale_fill_viridis(name="Tasa",option="inferno")
# gg + scale_fill_brewer(palette = "RdYlBu", guide= "legend", direction = -1)

gg <- gg + theme_tufte(base_family="Helvetica")
gg <-gg + theme_change 
gg <- gg + theme(plot.title=element_text(hjust=0))
# gg <- gg + coord_equal()
gg <- gg + labs(y="Líneas de monitoreo",  title="Tasa de reclutamiento"~(ind.col^-1~dia^-1))
gg <- gg + scale_x_date("Campañas", breaks = tasas.lineas$date, labels=date_format("%b-%y"))
gg




ggplot(tasas.lineas, aes(x = date, y = LINEA)) +
  geom_tile(aes(fill = TASA)) +
  scale_fill_continuous(guide = "colourbar") + #labels = label_text
  # scale_fill_manual(values = color_palette, name = "") +
  scale_x_date("Campañas", breaks = tasas.lineas$date, labels=date_format("%b-%y"))+
  theme_change
