# Script for Taxonomic table from sp's list

# install.packages("taxize")
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


#retrieve family data by list
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
