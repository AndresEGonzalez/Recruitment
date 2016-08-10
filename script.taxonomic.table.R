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
             "Tegula", "Fissurella", "Concholepas concholepas", "Cerithiidae", 
             "Siphonaria lessoni", "Triphoridae", "Xanthochorus cassidiformis", "Stichaster striatus", 
             "Tetrapygus niger", "Loxechinus albus", "Ophioplus")
length(species)

species <-sort(species)#sort by alphabetical names

# Resolve sp names
gnr_resolve(names=species)# see all chances and choose data sources ids
gnr_datasources()#see data sources for resolve 

#retrieve sp names (by data source 8 & 168) A. purpuratus [Cnidaria???!!!]
temp <- gnr_resolve(names=species, best_match_only=TRUE, data_source_ids = c(8))#8 == Interim Register of Marine and Nonmarine data base
# match(species, temp$submitted_name)
mtch<-species %in% temp$submitted_name
sp_lost<-species[-which(mtch)]

# Retrieve sp lost from another data source id
temp1 <- gnr_resolve(names=sp_lost, best_match_only=TRUE, data_source_ids = c(168))


#Combine both temp & temp1 df
sp_list<-arrange(rbind(temp, temp1), user_supplied_name)


# Retrieve a list of high classification for species names 
# Use gbif & eol interactive data base
# clatx<-classification(species, db = 'gbif')
clatx <- classification(sp_list$submitted_name[-1], db = 'gbif')

#Workaround for miss classified A. purpuratus on gbif db (show as antozoa!!)
clatx1 <-classification(sp_list$submitted_name[1], db = 'eol')#retrieve classification from eol db choose 3

#set to 9 rownames object
# clatx1 <- list(`Argopecten purpuratus`=na.omit(clatx1$`Argopecten purpuratus`))#omit NA
# clatx1 <- list(`Argopecten purpuratus`=clatx1$`Argopecten purpuratus`[-1,])#delete superkingdom, subclass, superfamily
# row.names(clatx1$`Argopecten purpuratus`) <- seq(1,9,1)#reset rownames number

clatx2 <- c(clatx, clatx1)

#manipulate sp taxonomy convert to data frame from rows
taxi<-do.call(rbind.data.frame, clatx2)
taxi$sp<-rownames(taxi)
taxi
# dcast(taxi, sp ~ rank)

# make column sp.group for aggregate & delette last two characters
taxi$sp.group <-substr(taxi$sp, 1, nchar(taxi$sp)-2)
taxi
# FACTOR LEVELS for rank variable
taxi$rank <- factor(taxi$rank,
                    levels=c("kingdom",
                             "phylum",
                             "class", 
                             "order", "superfamily",
                             "family", 
                             "genus",
                             "species"),
                    labels=c("kingdom",
                             "phylum",
                             "class", 
                             "order","superfamily",
                             "family", 
                             "genus",
                             "species"))

taxi

# taxonomy<-arrange(dcast(taxi, sp.group ~ rank, value.var = "name"), phylum, class, family)


taxonomy<-arrange(dcast(taxi, sp.group ~ rank, value.var = "name"), sp.group)#phylum, class, family

#Combine by sp_list authority
tax_table <- cbind(taxonomy, "matched_name"=sp_list$matched_name)
names(tax_table)

#Select columns
tax <- arrange(tax_table[,c(3,4,5,7,8,10)],phylum, class, family)

## Escribe tabla taxonomia a archivo excel 
frame.6 <- c("tax")
name.pestaña <- c("structure")
WriteXLS(frame.6, 
         ExcelFileName = file.path(campaña.FILE.dir,
                                   paste("Taxonomy_Anglo_", format(Sys.time(), "%Y_%m_%d"), 
                                         ".xls", sep = "")), 
         SheetNames = name.pestaña, verbose=TRUE, AdjWidth = TRUE, BoldHeaderRow = TRUE,
         Encoding = "UTF-8")



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
