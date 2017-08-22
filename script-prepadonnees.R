setwd("D:/Switchdrive/Mémoire/DATA")

list.files()


library(stringr)




###############################################################################################
###############################################################################################


#    1. Fichier des applicants par ville via subset et correspondance NUTS3-FUA


###############################################################################################
###############################################################################################


# charger REGPAT-Inventeurs

inventeurs <- read.csv("201602_EPO_Inv_reg.txt", sep= "|")
inventeursEPO_RA <- subset(inventeurs, Reg_code=="FR711"| Reg_code=="FR712" | Reg_code=="FR713"| Reg_code=="FR714" | Reg_code=="FR715" | Reg_code=="FR716")
write.csv(file="inventeursEPO_RA.csv", inventeursEPO_RA)
patipc <- read.csv("201307_Patents_IPC.txt", sep= "|")


# API Google ne gÃ¨re pas les adresses avec des accents, il faut donc les enlever
# fonction qui supprime les accents

inventeursEPO_RA$Address <- gsub("Ã©","e",inventeursEPO_RA$Address)
inventeursEPO_RA$Address <- gsub("Ã¨","e",inventeursEPO_RA$Address)
inventeursEPO_RA$Address <- gsub("Ã¢","a",inventeursEPO_RA$Address)
inventeursEPO_RA$Address <- gsub("Ã´","o",inventeursEPO_RA$Address)
inventeursEPO_RA$Address <- gsub("Ãª","e",inventeursEPO_RA$Address)
inventeursEPO_RA$Address <- gsub("Ã»","u",inventeursEPO_RA$Address)
inventeursEPO_RA$Address <- gsub("Ã¯","i",inventeursEPO_RA$Address)
inventeursEPO_RA$Address <- gsub("Ã®","i",inventeursEPO_RA$Address)


Unaccent <- function(text) {
  text <- gsub("['`^~\"]", " ", text)
  text <- iconv(text, to="ASCII//TRANSLIT//IGNORE")
  text <- gsub("['`^~\"]", "", text)
  return(text)
}

# execution de la fonction sur la colonne address

inventeursEPO_RA$adress2 <- Unaccent(inventeursEPO_RA$Address)

# on enleve l'identifiant du pays du code postal qui gene souvent

inventeursEPO_RA$adress2 <- gsub("F-", "", inventeursEPO_RA$adress2)




# fonction de geolocalisation par l'adresse via API Google

geocodeAdddress <- function(address) {
  require(RJSONIO)
  url <- "http://maps.google.com/maps/api/geocode/json?address="
  url <- URLencode(paste(url, address, sep = "", key="AIzaSyCsjDAVV5C2cVMbGHlOVLT1JXcMI029g30"))
  x <- fromJSON(url, simplify = FALSE)
  if (x$status == "OK") {
    out <- c(x$results[[1]]$geometry$location$lng,
             x$results[[1]]$geometry$location$lat)
  } else {
    out <- NA
  }
  Sys.sleep(0.20)  # API ne peut gÃ©rer que 5 requetes par seconde
  out
}

# pour i in 1:nombre d'observations, on applique la fonction pour la longitude

for (i in 1001:1500) {
  inventeursEPO_RA$lng[i] <- (as.matrix(geocodeAdddress(inventeursEPO_RA$Address[i])))[1]
}  


# puis pour la latitude

for (i in 1000:1005){
  inventeursEPO_RA$lat[i] <- (as.matrix(geocodeAdddress(inventeursEPO_RA$Address[i])))[2]
}

write.csv(file="inventeursEPO_RA.csv", inventeursEPO_RA)



# ne garder que les adresses europÃ©ennes
regeuroapp <- subset(regapplicants, Ctry_code == "FI"|Ctry_code == "CH"|Ctry_code == "ES"|
                       Ctry_code == "IT"|Ctry_code == "FR"|Ctry_code == "GB"|
                       Ctry_code == "DE"|Ctry_code == "BE"|Ctry_code == "PT"|
                       Ctry_code == "SE"|Ctry_code == "NO"|Ctry_code == "IE"|
                       Ctry_code == "GR"|Ctry_code == "DK"|Ctry_code == "AT"|
                       Ctry_code == "BG"|Ctry_code == "CZ"|Ctry_code == "EE"|
                       Ctry_code == "HR"|Ctry_code == "HU"|Ctry_code == "LT"|
                       Ctry_code == "NL"|Ctry_code == "PL"|Ctry_code == "RO"|
                       Ctry_code == "SI"|Ctry_code == "SK")

# charger correspondance FUA-NUTS3

nutsaero <- read.csv("final_nuts_aero_3.csv", sep = ",")


# renomme la deuxiÃ¨me colonne pour jointure

colnames(nutsaero)[2] <- "Reg_code"

# fusionner sur l'attribut Reg_code

regeuroappvilles <- merge(regeuroapp, nutsaero, by="Reg_code")

# supprimer les colonnes inutiles

regeuroappvilles <- regeuroappvilles[,- c(9:10)]

# statistiques descriptives

summary(regeuroappvilles, maxsum = 11)

# test sur une seule ville (Lyon, code=LYS)

brevetslyon <- subset(regeuroappvilles, CAERO == "LYS")
summary(brevetslyon, maxsum = 11)

###############################################################################################
###############################################################################################


#    2. Fichier des inventeurs par ville via subset et correspondance NUTS3-FUA


###############################################################################################
###############################################################################################

# charger REGPAT-Inventeurs

reginv<- read.csv("201602_EPO_Inv_reg.txt", sep= "|")


# ne garder que les adresses europÃ©ennes
regeuroinv <- subset(reginv, Ctry_code == "FI"|Ctry_code == "CH"|Ctry_code == "ES"|
                       Ctry_code == "IT"|Ctry_code == "FR"|Ctry_code == "GB"|
                       Ctry_code == "DE"|Ctry_code == "BE"|Ctry_code == "PT"|
                       Ctry_code == "SE"|Ctry_code == "NO"|Ctry_code == "IE"|
                       Ctry_code == "GR"|Ctry_code == "DK"|Ctry_code == "AT"|
                       Ctry_code == "BG"|Ctry_code == "CZ"|Ctry_code == "EE"|
                       Ctry_code == "HR"|Ctry_code == "HU"|Ctry_code == "LT"|
                       Ctry_code == "NL"|Ctry_code == "PL"|Ctry_code == "RO"|
                       Ctry_code == "SI"|Ctry_code == "SK")

# charger correspondance FUA-NUTS3

nutsaero <- read.csv("final_nuts_aero_3.csv", sep = ";")


# renomme la deuxiÃ¨me colonne pour jointure

colnames(nutsaero)[2] <- "Reg_code"

# fusionner sur l'attribut Reg_code

regeuroinvvilles <- merge(regeuroinv, nutsaero, by="Reg_code")

# supprimer les colonnes inutiles

regeuroinvvilles <- regeuroinvvilles[,- c(9:10)]

# statistiques descriptives

summary(regeuroinvvilles, maxsum = 11)

# test sur une seule ville (Lyon, code=LYS)

invlyon <- subset(regeuroinvvilles, CAERO == "LYS")
summary(brevetslyon, maxsum = 11)

###############################################################################################
###############################################################################################


#    3. Fichier des localisations des inventeurs via gÃ©olocalisation API Google
#                     test sur la rÃ©gion RhÃ´ne-Alpes


###############################################################################################
###############################################################################################

regfranceinv <- subset(reginv, Ctry_code == "FR")
regRAinv <- subset(regfranceinv, Reg_code == "FR711"|Reg_code == "FR712"|Reg_code == "FR713"|Reg_code == "FR714"
                   |Reg_code == "FR715"|Reg_code == "FR716"|Reg_code == "FR717"|Reg_code == "FR718")

# si besoin autre fichier

# inventeurgeo <- subset(reginv.....)

# API Google ne gÃ¨re pas les adresses avec des accents, il faut donc les enlever
# fonction qui supprime les accents

Unaccent <- function(text) {
  text <- gsub("['`^~\"]", " ", text)
  text <- iconv(text, to="ASCII//TRANSLIT//IGNORE")
  text <- gsub("['`^~\"]", "", text)
  return(text)
}

# exÃ©cution de la fonction sur la colonne address

regRAinv$adress2 <- Unaccent(regRAinv$Address)

# on enlÃ¨ve l'identifiant du pays du code postal qui gene souvent

regRAinv$adress2 <- gsub("F-", "", regRAinv$adress2)

# fonction de gÃ©olocalisation par l'adresse via API Google

geocodeAdddress <- function(address) {
  require(RJSONIO)
  url <- "http://maps.google.com/maps/api/geocode/json?address="
  url <- URLencode(paste(url, address, sep = ""))
  x <- fromJSON(url, simplify = FALSE)
  if (x$status == "OK") {
    out <- c(x$results[[1]]$geometry$location$lng,
             x$results[[1]]$geometry$location$lat)
  } else {
    out <- NA
  }
  Sys.sleep(0.20)  # API ne peut gÃ©rer que 5 requetes par seconde
  out
}

# pour i in 1:nombre d'observations, on applique la fonction pour la longitude

for (i in 1:20) {
  regRAinv$lng[i] <- (as.matrix(geocodeAdddress(regRAinv$adress2[i])))[1]
}  


# puis pour la latitude

for (i in 1:77384){
  regRAinv$lat[i] <- (as.matrix(geocodeAdddress(regRAinv$adress2[i])))[2]
}


####################################################################################
# travail sur les citations


citationsEPO <- read.csv("201602_EP_Citations.txt", sep= "|")

regeuroapp <- read.csv ("regeuroapp.csv")

nutsaero <- read.csv("final_nuts_aero_3.csv", sep = ",")


colnames(nutsaero)[2] <- "Reg_code"


regeuroappvilles <- merge(regeuroapp, nutsaero, by="Reg_code")


regeuroappvilles <- regeuroappvilles[,- c(9:10)]

write.csv(regeuroappvilles,"regeuroappvilles.csv")

colnames (regeuroappvilles)[3] <- "Cited_Appln_id"

citedreg <- merge(citationsEPO, regeuroappvilles, by="Cited_Appln_id", suffixes = TRUE)

colnames (regeuroappvilles)[3] <- "Citing_appln_id"

citreg <- merge(citedreg, regeuroappvilles, by="Citing_appln_id", suffixes = TRUE)

colnames (citreg)[25] <- "Target"
colnames (citreg)[37] <- "Source"

citreg <- citreg[,c("Citing_appln_id","Cited_Appln_id","Source","Target")]

write.csv(citreg,"citreg.csv")

citreg <- read.csv("citreg.csv")

links <- citreg[,c(4:5)] 

links <- count(links,c("Source","Target"))

net <- graph_from_data_frame(d=links, directed=T)

plot(net)
l <- layout_with_fr(net)

linksAalborg <- subset(citreg, Source=="Aalborg")
linksAarhus <- subset(citreg, Source=="Aarhus")


linksHelsinki <- subset(citreg, Source=="Helsinki")

c <- unique(links$Source)

# fusion data techfield

list.files()

Qualityp <- read.csv("201602_OECD_PATENT_QUALITY_EPO.txt", sep="|")
colnames(Qualityp)[1] <- "Citing_appln_id"

citregtf <- merge(citreg, Qualityp, by="Citing_appln_id")
citregtf <- citregtf[,-c(9:28)]
citregtf <- citregtf[,-2]
citregtf <- citregtf[,-5]

# crÃ©ation de chaque fichier de liens totaux, non filtrÃ©s



for (i in c){
  x <- subset(citregtf, Source== i)
  write.csv(x, file = paste("citstf",i,".csv", sep=""))
  
}


# pour chaque fichier, liens totaux

for (i in c) {
  x <-read.csv(file = paste("citstf",i,".csv", sep=""))
  x <- x[,c(4:5)]
  x <- count(x,c("Source","Target"))
  write.csv(x, file=paste("linksTot",i,".csv", sep=""))
}

# creation de chaque fichier gÃ©nÃ©ral pour les techfield

d <- unique(citregtf$tech_field)

for (i in 1:35) {
  x <- subset(citregtf, tech_field==i)
  write.csv(x, paste("citregtf", i,".csv", sep = ""))
}

# creation des links gÃ©nÃ©raux pour les techfields

for (i in 1:35){
  x<- read.csv(paste("citregtf", i,".csv", sep = ""))
  x <- count(x,c("Source","Target"))
  write.csv(x,file=paste("linksTot",i,".csv", sep=""))
}

# creation pour techfield du fichier pour histogramme
for (i in 1:35){
  x <- read.csv(paste("citregtf", i,".csv", sep = ""))
  y <- count(x, "Source")
  y <- y[order(-y$freq),]
  colnames(y)[2]<-"number"
  colnames(y)[1]<-"letter"
  y$frequency <- (y$number/sum(y$number))
  y$frequency <- round(y$frequency, digits=4)
  y$name <- y$letter
  y <- y[1:15,]
  write.table(y, file=paste("tffreq", i,".tsv", sep = ""), quote=FALSE, sep='\t', row.names = FALSE)
  
}

# creation pour techfield du fichier pour histogramme / ville + spÃ©cialisation
library("plyr")
library("car")

for (i in c){
  x <-read.csv(file = paste("citstf",i,".csv", sep=""))
  y <- count(x,"tech_field")
  colnames (y)[1]<-"letter"
  y$name=y$letter
  y$letter<-recode(y$letter,"1='A1'")
  y$letter<-recode(y$letter,"2='A2'")
  y$letter<-recode(y$letter,"3='A3'")
  y$letter<-recode(y$letter,"4='A4'")
  y$letter<-recode(y$letter,"5='A5'")
  y$letter<-recode(y$letter,"6='A6'")
  y$letter<-recode(y$letter,"7='A7'")
  y$letter<-recode(y$letter,"8='A8'")
  y$letter<-recode(y$letter,"9='B1'")
  y$letter<-recode(y$letter,"10='B2'")
  y$letter<-recode(y$letter,"11='B3'")
  y$letter<-recode(y$letter,"12='B4'")
  y$letter<-recode(y$letter,"13='B5'")
  y$letter<-recode(y$letter,"14='C1'")
  y$letter<-recode(y$letter,"15='C2'")
  y$letter<-recode(y$letter,"16='C3'")
  y$letter<-recode(y$letter,"17='C4'")
  y$letter<-recode(y$letter,"18='C5'")
  y$letter<-recode(y$letter,"19='C6'")
  y$letter<-recode(y$letter,"20='C7'")
  y$letter<-recode(y$letter,"21='C8'")
  y$letter<-recode(y$letter,"22='C9'")
  y$letter<-recode(y$letter,"23='C10'")
  y$letter<-recode(y$letter,"24='C11'")
  y$letter<-recode(y$letter,"25='D1'")
  y$letter<-recode(y$letter,"26='D2'")
  y$letter<-recode(y$letter,"27='D3'")
  y$letter<-recode(y$letter,"28='D4'")
  y$letter<-recode(y$letter,"29='D5'")
  y$letter<-recode(y$letter,"30='D6'")
  y$letter<-recode(y$letter,"31='D7'")
  y$letter<-recode(y$letter,"32='D8'")
  y$letter<-recode(y$letter,"33='E1'")
  y$letter<-recode(y$letter,"34='E2'")
  y$letter<-recode(y$letter,"35='E3'")
  
  y$name<-recode(y$name,"1='Electrical Machinery, apparatus, energy'")
  y$name<-recode(y$name,"2='Audio-Visual Technology'")
  y$name<-recode(y$name,"3='Telecommunications'")
  y$name<-recode(y$name,"4='Digital Communication'")
  y$name<-recode(y$name,"5='Basic Communication Processes'")
  y$name<-recode(y$name,"6='Computer Technology'")
  y$name<-recode(y$name,"7='IT Methods for Management'")
  y$name<-recode(y$name,"8='Semiconductors'")
  y$name<-recode(y$name,"9='Optics'")
  y$name<-recode(y$name,"10='Measurement'")
  y$name<-recode(y$name,"11='Analysis of Biological Materials'")
  y$name<-recode(y$name,"12='Control'")
  y$name<-recode(y$name,"13='Medical Technology'")
  y$name<-recode(y$name,"14='Organic Fine Chemistery'")
  y$name<-recode(y$name,"15='Biotechnology'")
  y$name<-recode(y$name,"16='Pharmaceuticals'")
  y$name<-recode(y$name,"17='Macromolecular chemistery, polymers'")
  y$name<-recode(y$name,"18='Food chemistery'")
  y$name<-recode(y$name,"19='Basic materials chemistery'")
  y$name<-recode(y$name,"20='Materials, metallurgy'")
  y$name<-recode(y$name,"21='Surface technology, coating'")
  y$name<-recode(y$name,"22='Micro-structural and nano-technology'")
  y$name<-recode(y$name,"23='Chemical Engineering'")
  y$name<-recode(y$name,"24='Environmental Technology'")
  y$name<-recode(y$name,"25='Handling'")
  y$name<-recode(y$name,"26='Machine Tools'")
  y$name<-recode(y$name,"27='Engines, Pumps, Turbines'")
  y$name<-recode(y$name,"28='Textile and Paper Machines'")
  y$name<-recode(y$name,"29='Other Special Machines'")
  y$name<-recode(y$name,"30='Thermal Processes and Apparatus'")
  y$name<-recode(y$name,"31='Mechanical Elements'")
  y$name<-recode(y$name,"32='Transport'")
  y$name<-recode(y$name,"33='Furniture, Games'")
  y$name<-recode(y$name,"34='Other Consumer Goods'")
  y$name<-recode(y$name,"35='Civil Engineering'")
  y <- y[order(-y$freq),]
  colnames (y)[2]<-"number"
  y$frequency <- (y$number/sum(y$number))
  y$frequency <- round(y$frequency, digits=4)
  
  y$specialisation <- y$frequency
  y <- y[order(y$letter),]
  y$specialisation[y$letter=='A1']=y$specialisation[y$letter=='A1']/0.0601
  y$specialisation[y$letter=='A2']=y$specialisation[y$letter=='A2']/0.0265
  y$specialisation[y$letter=='A3']=y$specialisation[y$letter=='A3']/0.0258
  y$specialisation[y$letter=='A4']=y$specialisation[y$letter=='A4']/0.0351
  y$specialisation[y$letter=='A5']=y$specialisation[y$letter=='A5']/0.0098
  y$specialisation[y$letter=='A6']=y$specialisation[y$letter=='A6']/0.0324
  y$specialisation[y$letter=='A7']=y$specialisation[y$letter=='A7']/0.0042
  y$specialisation[y$letter=='A8']=y$specialisation[y$letter=='A8']/0.0151
  y$specialisation[y$letter=='B1']=y$specialisation[y$letter=='B1']/0.0189
  y$specialisation[y$letter=='B2']=y$specialisation[y$letter=='B2']/0.0497
  y$specialisation[y$letter=='B3']=y$specialisation[y$letter=='B3']/0.0079
  y$specialisation[y$letter=='B4']=y$specialisation[y$letter=='B4']/0.0158
  y$specialisation[y$letter=='B5']=y$specialisation[y$letter=='B5']/0.0462
  y$specialisation[y$letter=='C1']=y$specialisation[y$letter=='C1']/0.0459
  y$specialisation[y$letter=='C2']=y$specialisation[y$letter=='C2']/0.0283
  y$specialisation[y$letter=='C3']=y$specialisation[y$letter=='C3']/0.0431
  y$specialisation[y$letter=='C4']=y$specialisation[y$letter=='C4']/0.0263
  y$specialisation[y$letter=='C5']=y$specialisation[y$letter=='C5']/0.0115
  y$specialisation[y$letter=='C6']=y$specialisation[y$letter=='C6']/0.0283
  y$specialisation[y$letter=='C7']=y$specialisation[y$letter=='C7']/0.0204
  y$specialisation[y$letter=='C8']=y$specialisation[y$letter=='C8']/0.0147
  y$specialisation[y$letter=='C9']=y$specialisation[y$letter=='C9']/0.0006
  y$specialisation[y$letter=='C10']=y$specialisation[y$letter=='C10']/0.0306
  y$specialisation[y$letter=='C11']=y$specialisation[y$letter=='C11']/0.0152
  y$specialisation[y$letter=='D1']=y$specialisation[y$letter=='D1']/0.0426
  y$specialisation[y$letter=='D2']=y$specialisation[y$letter=='D2']/0.0324
  y$specialisation[y$letter=='D3']=y$specialisation[y$letter=='D3']/0.0337
  y$specialisation[y$letter=='D4']=y$specialisation[y$letter=='D4']/0.0268
  y$specialisation[y$letter=='D5']=y$specialisation[y$letter=='D5']/0.0399
  y$specialisation[y$letter=='D6']=y$specialisation[y$letter=='D6']/0.0204
  y$specialisation[y$letter=='D7']=y$specialisation[y$letter=='D7']/0.0409
  y$specialisation[y$letter=='D8']=y$specialisation[y$letter=='D8']/0.0575
  y$specialisation[y$letter=='E1']=y$specialisation[y$letter=='E1']/0.0239
  y$specialisation[y$letter=='E2']=y$specialisation[y$letter=='E2']/0.0217
  y$specialisation[y$letter=='E3']=y$specialisation[y$letter=='E3']/0.0476
  y$specialisation <- round(y$specialisation, digits=4)
  
  write.table(y, file=paste("tffreq",i,".tsv", sep=""), quote=FALSE, sep='\t', row.names = FALSE)
}



############## Memoire ############
setwd("C:/Users/rlx/Desktop/memoire_tmp")
list.files()

inventeurs <- read.csv("inventeursEPO_AML.csv", sep=";")

annees <- as.data.frame(table(inventeurs$Année))
colnames(annees)[1]<-"Année"
colnames(annees)[2]<-"Brevets"
annees$Année <- as.numeric(as.character(annees$Année))
plot(annees)
lines(annees)


codes_ipc <- read.csv("201602_EPO_IPC.txt", sep="|")

# jointure des codes IPC pour chaque brevet dont un inventeur est localisé dans AML

brevets_ipc <- merge(inventeurs, codes_ipc, by="Appln_id", all.x = TRUE)


plot(brevets_ipc$IPC, ylim=c(0,1400))


write.csv(brevets_ipc, file="brevets_ipc.csv",row.names = FALSE)


brevets_ipc <- read.csv("brevets_ipc_AML.csv", sep=";", quote="")

ipc <- count(brevets_ipc$IPC)

# on ne garde que les paires id-IPC uniques (pour éviter les doublons dues au co-brevetage)

graph_brevets_ipc <- unique(brevets_ipc[c("Appln_id", "IPC")])

write.csv(graph_brevets_ipc, file="graph_brevets_ipc.csv",row.names = FALSE)

graph_brevets_ipc <-  read.csv("graph_brevets_ipc.csv", sep=";")

graph_brevets_ipc$Appln_id <- as.factor(graph_brevets_ipc$Appln_id)


# réseau full-digit

library(igraph)
bip <- graph.data.frame(graph_brevets_ipc)
V(bip)$type <- V(bip)$name %in% graph_brevets_ipc[,2]

## sparse=TRUE is a good idea if you have a large matrix here
v <- get.adjacency(bipartite.projection(bip)[[2]], attr="weight", sparse=FALSE)

## Need to reorder if you want it alphabetically
v[order(rownames(v)), order(colnames(v))]

g <- graph_from_adjacency_matrix(v, mode="undirected", weighted = TRUE, diag = TRUE)
e <- cbind( get.edgelist(g) , round( E(g)$weight, 3 ))
e <- as.data.frame(e)


summary(g)

transitivity(g)

average.path.length(g)

graph.density(g)

summary(degree(g))

summary(graph.strength(g))

edge.betweenness.community(g)
multilevel.community(g)
leading.eigenvector.community(g)
optimal.community(g)


colnames(e)[1]<-"Source"
colnames(e)[2]<-"Target"
colnames(e)[3]<-"Weight"

write.csv(e, "cooc_pat_fulldigits.csv")

# réseau 6 digits

graph_brevets_ipc$IPC <- substr(graph_brevets_ipc$IPC, 0, 6)
graph_brevets_ipc$Appln_id <- as.factor(graph_brevets_ipc$Appln_id)

library(igraph)
bip <- graph.data.frame(graph_brevets_ipc)
V(bip)$type <- V(bip)$name %in% graph_brevets_ipc[,2]

## sparse=TRUE is a good idea if you have a large matrix here
v <- get.adjacency(bipartite.projection(bip)[[2]], attr="weight", sparse=FALSE)

## Need to reorder if you want it alphabetically
v[order(rownames(v)), order(colnames(v))]

g <- graph_from_adjacency_matrix(v, mode="undirected", weighted = TRUE, diag = TRUE)
e <- cbind( get.edgelist(g) , round( E(g)$weight, 3 ))
e <- as.data.frame(e)

colnames(e)[1]<-"Source"
colnames(e)[2]<-"Target"
colnames(e)[3]<-"Weight"



summary(g)

transitivity(g)

average.path.length(g)

graph.density(g)

summary(degree(g))

summary(graph.strength(g))
diameter(g)

summary(clusters(g))

edge.betweenness.community(g)
multilevel.community(g)
leading.eigenvector.community(g)
optimal.community(g)


write.csv(e, "cooc_pat_6digits.csv")

# réseau 4 digits

graph_brevets_ipc$IPC <- substr(graph_brevets_ipc$IPC, 0, 4)
graph_brevets_ipc$Appln_id <- as.factor(graph_brevets_ipc$Appln_id)

library(igraph)
bip <- graph.data.frame(graph_brevets_ipc)
V(bip)$type <- V(bip)$name %in% graph_brevets_ipc[,2]

## sparse=TRUE is a good idea if you have a large matrix here
v <- get.adjacency(bipartite.projection(bip)[[2]], attr="weight", sparse=FALSE)

## Need to reorder if you want it alphabetically
v[order(rownames(v)), order(colnames(v))]

g <- graph_from_adjacency_matrix(v, mode="undirected", weighted = TRUE, diag = TRUE)
e <- cbind( get.edgelist(g) , round( E(g)$weight, 3 ))
e <- as.data.frame(e)

colnames(e)[1]<-"Source"
colnames(e)[2]<-"Target"
colnames(e)[3]<-"Weight"


write.csv(e, "cooc_pat_4digits.csv")


# réseau 3 digits

graph_brevets_ipc$IPC <- substr(graph_brevets_ipc$IPC, 0, 3)
graph_brevets_ipc$Appln_id <- as.factor(graph_brevets_ipc$Appln_id)

library(igraph)
bip <- graph.data.frame(graph_brevets_ipc)
V(bip)$type <- V(bip)$name %in% graph_brevets_ipc[,2]

## sparse=TRUE is a good idea if you have a large matrix here
v <- get.adjacency(bipartite.projection(bip)[[2]], attr="weight", sparse=FALSE)

## Need to reorder if you want it alphabetically
v[order(rownames(v)), order(colnames(v))]

g <- graph_from_adjacency_matrix(v, mode="undirected", weighted = TRUE, diag = TRUE)
e <- cbind( get.edgelist(g) , round( E(g)$weight, 3 ))
e <- as.data.frame(e)

colnames(e)[1]<-"Source"
colnames(e)[2]<-"Target"
colnames(e)[3]<-"Weight"

library(EconGeo)

relatedness.cosine <- relatedness(v, method="Jaccard")
summary(relatedness.cosine)
relatedness.density <- (relatedness.density.int.avg(v, relatedness.cosine))
summary(relatedness.density)

transitivity(g)

average.path.length(g)

graph.density(g)

summary(degree(g))

summary(graph.strength(g))

diameter(g)

clusters(g)

dist <- distance_table(g)

barplot(dist$res, names.arg = seq_along(dist$res))

edge.betweenness.community(g)

multilevel.community(g)

leading.eigenvector.community(g)

optimal.community(g)

write.csv(e, "cooc_pat_3digits.csv")

# to clear now unused memory
gc() 

# to remove all files in memory

rm(list = ls(all = TRUE))
