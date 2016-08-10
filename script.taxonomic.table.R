# Script for Taxonomic table from sp's list

# install.packages("taxize")
library(taxize)
library(plyr)
library(WriteXLS)

rm(list=ls())

dput(unique(long.data$ESPECIES))
species <- c("Pilumnoides perlatus", "Pachycheles grossimanus", "Halicarcinus planatus", 
             "Taliepus dentatus", "Romaleon polyodon", "Betaeus truncatus", "Pisoides edwarsii", "Rhynchocinetes typus", 
             "Choromytilus chorus", "Semimytilus algosus", "Perumytilus purpuratus", "Brachidontes granulata", "Entodesma cuneata", 
             "Ensis macha", "Eurhomalea", "Hiatella solida", "Cyclocardia compressa", 
             "Carditella tegulata", "Aulacomya atra", "Argopecten purpuratus", "Salitra radwini", "Tricolia umbilicata", 
             "Crassilabrum crassilabrum", "Mitrella unifasciata", "Incatella cingulata", "Caecum chilense", 
             "Nodilittorina araucana", "Crepidula dilatata", "Nassarius gayii", "Liotia cancellata", "Prisogaster niger", 
             "Tegula", "Fissurella", "Concholepas concholepas", "Cerithidae", 
             "Siphonaria lessoni", "Triphoridae", "Xanthochorus cassidiformis", "Stichaster striatus", 
             "Tetrapygus niger", "Loxechinus albus", "Ophioplus")
length(species)

species <-sort(species)#sort by alphabetical names

# Resolve sp names
gnr_resolve(names=species)# see all chances and choose data sources ids
gnr_datasources()#see data sources for resolve 

#retrieve sp names (by data source 8 & 168)
temp <- gnr_resolve(names=species, best_match_only=TRUE, data_source_ids = c(8))#8 == Interim Register of Marine and Nonmarine data base
# match(species, temp$submitted_name)
mtch<-species %in% temp$submitted_name
sp_lost<-species[-which(mtch)]

# Retrieve sp lost from another data source id
temp1 <- gnr_resolve(names=sp_lost, best_match_only=TRUE, data_source_ids = c(168))


#Combine both temp & temp1 df
sp_list<-arrange(rbind(temp, temp1), user_supplied_name)


#Retrieve a list classification for species names 
# Use gbif interactive data base
# clatx<-classification(species, db = 'gbif')
clatx<-classification(sp_list$submitted_name, db = 'gbif')
str(clatx)

# classification(species, db = 'ncbi')
# classification(species, db = 'itis')
# classification(species, db = 'eol')
# classification(species, db = 'col')
# classification(species, db = 'tropicos')
# classification(species, db = 'nbn')

taxi<-do.call(rbind.data.frame, clatx)#manipulate sp taxonomy convert to data frame from rows
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

taxonomy<-arrange(dcast(taxi, sp.group ~ rank, value.var = "name"), sp.group)#phylum, class, family

#Combine by sp_list authority
tax_table <- cbind(taxonomy, "matched_name"=sp_list$matched_name)

#Select columns
tax <- arrange(tax_table[,c(3,4,6,7,9)],phylum, class, family)

## Escribe tabla taxonomia a archivo excel 
frame.6 <- c("tax")
name.pestaña <- c("structure")
WriteXLS(frame.6, 
         ExcelFileName = file.path(campaña.IMG.dir,
                                   paste("Taxonomy_Anglo_", format(Sys.time(), "%Y_%m_%d"), 
                                         ".xls", sep = "")), 
         SheetNames = name.pestaña, verbose=TRUE, AdjWidth = TRUE, BoldHeaderRow = TRUE,
         Encoding = "latin1")



# *** Dont Run *****
# another proofs

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
