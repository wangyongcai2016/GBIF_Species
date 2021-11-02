################################################################################################################
# Obtains current occurrence data in GBIF for a taxonomic list of spiders and two .csv files
# containing literature based records for spiders of the Iberian Penninsula and elsewhere.
# Calculates extent of occurrence (an IUCN metric for Red Lists) using these records and compares the putative
# IUCN Red List designations of the litereture, GBIF, and combined datasets.
#Article:Current GBIF occurrence data demonstrates both promise and limitations for potential red listing of spiders
# Author: Vaughn M. Shirey
################################################################################################################

# load required libraries
library(red); library(reshape2); library(tidyverse)
'%!in%' <- Negate('%in%')

# load Iberian dataset & endemic taxon list
idat <- read.csv("iberian_occur.txt", sep = "\t", header = TRUE)
itax <- read.csv("iberian_endem.csv", sep = ",", header = FALSE)

# load global dataset
gdat.1 <- read.csv("reclaimedSRLIspecies_mergedwithSRLI.csv", sep = ",", header = TRUE)

# load GBIF data
gbif <- read.csv("gbif_occur.csv", sep = "\t", header = TRUE, stringsAsFactors = FALSE) %>%
  filter(datasetKey != "8655c292-f762-11e1-a439-00145eb45e9a" & datasetKey !="c47e7a5b-692d-4e26-a32f-74b0188eb594" &
           datasetKey != "8655c292-f762-11e1-a439-00145eb45e9a") %>%
  select(species, decimalLongitude, decimalLatitude) # remove contributed literature data from gbif occurrences

# reformat both Iberian and global literature data to match Red-package GBIF output, grab unique taxa
idat <- (data.frame(long = idat[, 17], lat = idat[, 16], V1 = paste(idat$genus, idat$specificEpithet)))
gdat.1 <- (data.frame(long = gdat.1[, 23], lat = gdat.1[, 22], V1 = gdat.1$species))  %>%
  filter(V1 %!in% c("Gea spinipes", "Megaphobema robustum", "Pamphobeteus ferox", "Pamphobeteus fortis", "Spelungula cavernicola", "Xenesthis immanis"))

length(unique(gdat.1$V1)) # check number of taxa

idat <- idat[!is.na(idat$long),]
gdat <- gdat.1[!is.na(gdat.1$long),]

idat <- idat[(idat$V1 %in% itax$V1),] # use only Iberian endemics

itaxa <- as.vector(unique(idat$V1))
gtaxa <- as.vector(unique(gdat.1$V1))
gtaxa <- c(gtaxa, c("Cataxia bolganupensis",
                    "Cyclosa bianchoria",
                    "Cyrtarachne hubeiensis",
                    "Oonops tectulus",
                    "Orodrassus coloradensis",
                    "Poecilotheria subfusca",
                    "Stasimopus nanus",
                    "Austrarchaea platnikorum"))
length(gtaxa)

# grab GBIF records for all unique taxa in the Iberian dataset, remove records from contributed literature dataset
idat_gbif <- gbif %>% filter(species %in% itaxa) %>%
  mutate(long = as.numeric(decimalLongitude), lat = round(as.numeric(decimalLatitude), 5), V1 = species) %>%
  select(long, lat, V1)

# grab GBIF records for all unique taxa in the global dataset, remove records from contributed literature dataset
gdat_gbif <- gbif %>% filter(species %in% gtaxa) %>%
  mutate(long = as.numeric(decimalLongitude), lat = round(as.numeric(decimalLatitude), 5), V1 = species) %>%
  select(long, lat, V1)

# calculate EOO from literature only data set for both Iberian and global lists
n <- length(itaxa)
idat_eoo <- data.frame()

for(i in 1:n){
  taxon <- itaxa[i]
  
  print(paste("Calculating: ", i, " of ", n, "."))
  
  idat_eoo[i, 1] <- taxon
  idat_eoo[i, 2] <- eoo(idat[which(idat[, 3]==taxon), c(1,2)])
}

n <- length(as.vector(unique(gdat$V1)))
gdat_eoo <- data.frame()

for(i in 1:n){
  taxon <- gtaxa[i]
  
  print(paste("Calculating: ", i, " of ", n, "."))
  
  gdat_eoo[i, 1] <- taxon
  gdat_eoo[i, 2] <- eoo(gdat[which(gdat[, 3]==taxon), c(1,2)])
}

# calculate EOO from GBIF only dataset for both Iberian and global lists
n <- length(itaxa)
idat_gbif_eoo <- data.frame()

for(i in 1:n){
  taxon <- itaxa[i]
  
  print(paste("Calculating: ", i, " of ", n, "."))
  
  idat_gbif_eoo[i, 1] <- taxon
  idat_gbif_eoo[i, 2] <- eoo(na.omit(idat_gbif[which(idat_gbif[, 3]==taxon), c(1,2)]))
}

n <- length(gtaxa)
gdat_gbif_eoo <- data.frame()

for(i in 1:n){
  taxon <- gtaxa[i]
  
  print(paste("Calculating: ", i, " of ", n, "."))
  
  gdat_gbif_eoo[i, 1] <- taxon
  gdat_gbif_eoo[i, 2] <- eoo(na.omit(gdat_gbif[which(gdat_gbif[, 3]==taxon), c(1,2)]))
}

# calculate EOO for the combined dataset for both Iberian and global lists
idat_merge <- rbind(idat, idat_gbif)
gdat_merge <- rbind(gdat.1, gdat_gbif)

n <- length(itaxa)
idat_merge_eoo <- data.frame()

for(i in 1:n){
  taxon <- itaxa[i]
  
  print(paste("Calculating: ", i, " of ", n, "."))
  
  idat_merge_eoo[i, 1] <- taxon
  idat_merge_eoo[i, 2] <- eoo(na.omit(idat_merge[which(idat_merge[, 3]==taxon), c(1,2)]))
}

n <- length(gtaxa)
gdat_merge_eoo <- data.frame()

for(i in 1:n){
  taxon <- gtaxa[i]
  
  print(paste("Calculating: ", i, " of ", n, "."))
  
  gdat_merge_eoo[i, 1] <- taxon
  gdat_merge_eoo[i, 2] <- eoo(na.omit(gdat_merge[which(gdat_merge[, 3]==taxon), c(1,2)]))
}

# cbind and write results to .csv files
idat_all_eoo <- cbind(idat_eoo, idat_gbif_eoo, idat_merge_eoo)
idat_all_eoo <- idat_all_eoo[, c(1,2,4,6)]
colnames(idat_all_eoo) <- c("ScientificName", "Literature", "GBIF", "Combined")

# add the species from literature with no coordinate data
gdat_eoo <- gdat_eoo %>%
  add_row(V1="Cataxia bolganupensis", V2=0)  %>%
  add_row(V1="Cyclosa bianchoria", V2=0)  %>%
  add_row(V1="Cyrtarachne hubeiensis", V2=0)  %>%
  add_row(V1="Oonops tectulus", V2=0)  %>%
  add_row(V1="Orodrassus coloradensis", V2=0)  %>%
  add_row(V1="Poecilotheria subfusca", V2=0)  %>%
  add_row(V1="Stasimopus nanus", V2=0)  %>%
  add_row(V1="Austrarchaea platnickorum", V2=0) %>%
  add_row(V1="Cardiopelma mascatum", V2=0) %>%
  add_row(V1="Zenonina fusca", V2=0) %>%
  add_row(V1="Urozelotes mysticus", V2=0) %>%
  add_row(V1="Theuma aprica", V2=0) %>%
  arrange(V1)
gdat_all_eoo.1 <- merge(gdat_eoo, gdat_gbif_eoo, by="V1", all=TRUE)
gdat_all_eoo <- merge(gdat_all_eoo.1, gdat_merge_eoo, by="V1", all=TRUE)
colnames(gdat_all_eoo) <- c("ScientificName", "Literature", "GBIF", "Combined")

write.csv(idat_all_eoo, "Iberian_results.csv")
write.csv(gdat_all_eoo, "Global_results.csv")