###### settings ######

rm(list=ls())

setwd("/memoire_tmp")
setwd("D:/memoire_tmp")

library(igraph)
library(intergraph)
library(rgdal)
library(spdep)
library(cartography)
library(sp)
library(SpatialPosition)
library(geospacom)


read.table



### mesures generales

brevets_ipc <- read.table(file = "brevets_ipc_applicants.csv", sep = ";", header = TRUE, encoding = "UTF-8")

brevets_unique <- as.data.frame(unique(brevets_ipc$Appln_id)) #nombre de brevets 
inventeurs_unique <- as.data.frame(unique(brevets_ipc$Inv_name)) #nombre d'inventeurs
applicants_unique <- as.data.frame(unique(brevets_ipc$App_name)) #nombre d'applicants
insee_unique <-  as.data.frame(unique(brevets_ipc$INSEE_Code)) #nombre de communes
ipc_subgroup_unique <- as.data.frame(unique(brevets_ipc$IPC))
colnames(ipc_subgroup_unique)[1]<-"IPC_subgroup"
ipc_group_unique <- as.data.frame(unique(substr(ipc_subgroup_unique$IPC_subgroup, 0, 7)))
colnames(ipc_group_unique)[1]<-"IPC_group"
ipc_subclass_unique <- as.data.frame(unique(substr(ipc_group_unique$IPC_group, 0, 4)))
colnames(ipc_subclass_unique)[1]<-"IPC_subclass"
ipc_class_unique <- as.data.frame(unique(substr(ipc_subclass_unique$IPC_subclass, 0, 3)))
colnames(ipc_class_unique)[1]<-"IPC_class"

#nombre d'inventeurs par brevet

breveinv <- unique(brevets_ipc[c("Appln_id", "Inv_name")])
breveinv$Appln_id <- as.factor(breveinv$Appln_id)
breveinv <- as.data.frame(table(breveinv$Appln_id))
breveinv <- as.data.frame(table(breveinv$Freq))
breveinv$Var1<-as.numeric(breveinv$Var1)
par(pty="s")
plot(breveinv, type="h",log="y",xlab = "Nombre d'inventeurs par brevet", ylab = "Fréquence",col = "red", lwd=7)
axis(1, at=c(1,2,3,4,5,6,7,8,9,10,11,12))
y1 <- floor(log10(range(breveinv$Freq)))
pow <- seq(y1[1], y1[2]+1)
ticksat <- as.vector(sapply(pow, function(p) (1:10)*10^p))
axis(2, ticksat, labels=NA, tcl=-0.25, lwd=0, lwd.ticks=1)

#nombre d'applicants par brevet

brevapp <- unique(brevets_ipc[c("Appln_id", "App_name")])
brevapp$Appln_id <- as.factor(brevapp$Appln_id)
brevapp <- as.data.frame(table(brevapp$Appln_id))
brevapp <- as.data.frame(table(brevapp$Freq))
brevapp$Var1<-as.numeric(brevapp$Var1)
par(pty="s")
plot(brevapp, type="h",log="y",xlab = "Nombre d'applicants par brevet", ylab = "Fréquence",col = "red", lwd=7)
axis(1, at=c(1,2,3,4,5,6,7,8,9,10,11,12,13,14))
y1 <- floor(log10(range(brevapp$Freq)))
pow <- seq(y1[1], y1[2]+1)
ticksat <- as.vector(sapply(pow, function(p) (1:10)*10^p))
axis(2, ticksat, labels=NA, tcl=-0.25, lwd=0, lwd.ticks=1)

#nombre de code ipc par brevet

brevipc <- unique(brevets_ipc[c("Appln_id", "IPC")])
brevipc$Appln_id <- as.factor(brevipc$Appln_id)
brevipc <- as.data.frame(table(brevipc$Appln_id))
brevipc <- as.data.frame(table(brevipc$Freq))
brevipc$Var1<-as.numeric(brevipc$Var1)
par(pty="s")
plot(brevipc, type="h",log="y",xlab = "Nombre de codes IPC par brevet", ylab = "Fréquence",col = "red", lwd=2)
y1 <- floor(log10(range(brevipc$Freq)))
pow <- seq(y1[1], y1[2]+1)
ticksat <- as.vector(sapply(pow, function(p) (1:10)*10^p))
axis(2, ticksat, labels=NA, tcl=-0.25, lwd=0, lwd.ticks=1)

#nombre de brevets par inventeur

breveinv <- unique(brevets_ipc[c("Appln_id", "Inv_name")])
breveinv$Appln_id <- as.factor(breveinv$Appln_id)
breveinv <- as.data.frame(table(breveinv$Inv_name))
summary(breveinv)
breveinv <- as.data.frame(table(breveinv$Freq))
breveinv$Var1<-as.numeric(breveinv$Var1)
par(pty="s")
plot(breveinv, type="h",log="y",xlab = "Nombre de brevets par inventeur", ylab = "Fréquence",col = "red", lwd=2)
y1 <- floor(log10(range(breveinv$Freq)))
pow <- seq(y1[1], y1[2]+1)
ticksat <- as.vector(sapply(pow, function(p) (1:10)*10^p))
axis(2, ticksat, labels=NA, tcl=-0.25, lwd=0, lwd.ticks=1)


breveinvapp <- unique(brevets_ipc[c("App_name", "Inv_name")])
breveinvapp <- as.data.frame(table(breveinvapp$Inv_name))
summary(breveinvapp)
breveinv <- as.data.frame(table(breveinv$Freq))


breveinvipc <- unique(brevets_ipc[c("IPC", "Inv_name")])
breveinvipc <- as.data.frame(table(breveinvipc$Inv_name))
summary(breveinvipc)


breveappinv <- unique(brevets_ipc[c("App_name", "Inv_name")])
breveappinv <- as.data.frame(table(breveappinv$App_name))
summary(breveappinv)

breveappipc <- unique(brevets_ipc[c("IPC", "App_name")])
breveappipc <- as.data.frame(table(breveappipc$App_name))
summary(breveappipc)

apphorsaml <- unique(brevets_ipc[c("HORS.AML.NUTS", "App_name")])
summary(apphorsaml)


# brevets par inventeur

brevets_par_inv <- as.data.frame(unique(brevets_ipc[c("Appln_id", "Inv_name")]))
brevets_par_inv <- as.data.frame(table(brevets_par_inv$Inv_name))
summary(brevets_par_inv$Freq)
brevets_par_inv$rank = rank(-brevets_par_inv$Freq)
reg <- lm(formula = log(brevets_par_inv$Freq)~log(brevets_par_inv$rank))
coeff <- coefficients(reg)
R2 <- summary(reg)[8]$r.squared
R2 <- round(R2, 4)
par(pty="s")
plot(x=log(brevets_par_inv$rank), y=log((brevets_par_inv$Freq)), bg="black",pch=21,xlab = "Log (Rang)", ylab = "Log (Nombre de brevets par inventeur)")
abline(lm(formula = log(brevets_par_inv$Freq)~log(brevets_par_inv$rank)), col = "red",lwd=3)
legend("topright", legend = c(paste("Beta = ", round(-coeff[2],4)), paste("R2 = ", R2)))

dev.off()

# brevets par applicant

brevets_par_app <- as.data.frame(unique(brevets_ipc[c("Appln_id", "App_name")]))
brevets_par_app <- as.data.frame(table(brevets_par_app$App_name))
summary(brevets_par_app$Freq)
brevets_par_app$rank = rank(-brevets_par_app$Freq)
reg <- lm(formula = log(brevets_par_app$Freq)~log(brevets_par_app$rank))
coeff <- coefficients(reg)
R2 <- summary(reg)[8]$r.squared
R2 <- round(R2, 4)
par(pty="s")
plot(x=log(brevets_par_app$rank), y=log((brevets_par_app$Freq)), bg="black",pch=21,xlab = "Log (Rang)", ylab = "Log (Nombre de brevets par applicant)")
abline(lm(formula = log(brevets_par_app$Freq)~log(brevets_par_app$rank)), col = "red",lwd=3)
legend("topright", legend = c(paste("Beta = ", round(-coeff[2],4)), paste("R2 = ", R2)))


# brevets par commune de résidence inventeur

brevets_par_geo <- as.data.frame(unique(brevets_ipc[c("Appln_id", "INSEE_Code")]))
brevets_par_geo <- as.data.frame(table(brevets_par_geo$INSEE_Code))
summary(brevets_par_geo$Freq)
brevets_par_geo$rank = rank(-brevets_par_geo$Freq)
reg <- lm(formula = log(brevets_par_geo$Freq)~log(brevets_par_geo$rank))
coeff <- coefficients(reg)
R2 <- summary(reg)[8]$r.squared
R2 <- round(R2, 4)
par(pty="s")
plot(x=log(brevets_par_geo$rank), y=log((brevets_par_geo$Freq)), bg="black",pch=21,xlab = "Log (Rang)", ylab = "Log (Nombre de brevets par commune)")
abline(lm(formula = log(brevets_par_geo$Freq)~log(brevets_par_geo$rank)), col = "red",lwd=3)
legend("topright", legend = c(paste("Beta = ", round(-coeff[2],4)), paste("R2 = ", R2)))





##### 4.1.1 Analyses prox tech #####

brevets_ipc <- read.table(file = "brevets_ipc_applicants.csv", sep = ";", header = TRUE, encoding = "UTF-8")


# IPC niveau brevet

brevets_par_IPCsubgroup <- as.data.frame(unique(brevets_ipc[c("Appln_id", "IPC")]))

bip <- graph.data.frame(brevets_par_IPCsubgroup)
V(bip)$type <- V(bip)$name %in% brevets_par_IPCsubgroup[,2]
vpat <- get.adjacency(bipartite.projection(bip)[[2]], attr="weight", sparse=TRUE)
vpat[order(rownames(vpat)), order(colnames(vpat))]
g <- graph_from_adjacency_matrix(vpat, mode="undirected", weighted = TRUE, diag = TRUE)

summary(g) # nb noeuds et liens
graph.density(g) # densité
summary(degree(g)) # degré moyen
summary(strength(g)) # degré pondéré moyen
transitivity(g, type= "localaverageundirected") # coef clustering
cluster_louvain(g) # communautés et modularité
c <- components(g) 
summary (c$csize) # plus grande composante
average.path.length(g) # plus courts chemins moyens


brevets_par_IPCsubgroup <- as.data.frame(table(brevets_par_IPCsubgroup$IPC))
summary(brevets_par_IPCsubgroup$Freq)
brevets_par_IPCsubgroup$rank = rank(-brevets_par_IPCsubgroup$Freq)
brevets_par_IPCsubgroup <- as.data.frame(unique(brevets_par_IPCsubgroup[c("rank", "Freq")]))
reg <- lm(formula = log(brevets_par_IPCsubgroup$Freq)~log(brevets_par_IPCsubgroup$rank))
coeff <- coefficients(reg)
R2 <- summary(reg)[8]$r.squared
R2 <- round(R2, 4)
par(pty="s")
plot(x=log(brevets_par_IPCsubgroup$rank), y=log((brevets_par_IPCsubgroup$Freq)), bg="black",pch=21,xlab = "Log (Rang)", ylab = "Log (Nombre de brevets par IPC (sous-groupes))")
abline(lm(formula = log(brevets_par_IPCsubgroup$Freq)~log(brevets_par_IPCsubgroup$rank)), col = "red",lwd=3)
legend("topright", legend = c(paste("Beta = ", round(-coeff[2],4)), paste("R2 = ", R2)))


# IPC niveau inventeur

inventeurs_par_IPCsubgroup <- as.data.frame(unique(brevets_ipc[c("Inv_name", "IPC")]))

bip <- graph.data.frame(inventeurs_par_IPCsubgroup)
V(bip)$type <- V(bip)$name %in% inventeurs_par_IPCsubgroup[,2]
vpat <- get.adjacency(bipartite.projection(bip)[[2]], attr="weight", sparse=TRUE)
vpat[order(rownames(vpat)), order(colnames(vpat))]
g <- graph_from_adjacency_matrix(vpat, mode="undirected", weighted = TRUE, diag = TRUE)

summary(g) # nb noeuds et liens
graph.density(g) # densité
summary(degree(g)) # degré moyen
summary(strength(g)) # degré pondéré moyen
transitivity(g, type= "localaverageundirected") # coef clustering
cluster_louvain(g) # communautés et modularité
c <- components(g) 
summary (c$csize) # plus grande composante
c$no # nombre de composantes
average.path.length(g) # plus courts chemins moyens


inventeurs_par_IPCsubgroup <- as.data.frame(table(inventeurs_par_IPCsubgroup$IPC))
summary(inventeurs_par_IPCsubgroup$Freq)
inventeurs_par_IPCsubgroup$rank = rank(-inventeurs_par_IPCsubgroup$Freq)
inventeurs_par_IPCsubgroup <- as.data.frame(unique(inventeurs_par_IPCsubgroup[c("rank", "Freq")]))
reg <- lm(formula = log(inventeurs_par_IPCsubgroup$Freq)~log(inventeurs_par_IPCsubgroup$rank))
coeff <- coefficients(reg)
R2 <- summary(reg)[8]$r.squared
R2 <- round(R2, 4)
par(pty="s")
plot(x=log(inventeurs_par_IPCsubgroup$rank), y=log((inventeurs_par_IPCsubgroup$Freq)), bg="black",pch=21,xlab = "Log (Rang)", ylab = "Log (Nombre de brevets par IPC (sous-groupes))")
abline(lm(formula = log(inventeurs_par_IPCsubgroup$Freq)~log(inventeurs_par_IPCsubgroup$rank)), col = "red",lwd=3)
legend("topright", legend = c(paste("Beta = ", round(-coeff[2],4)), paste("R2 = ", R2)))


# IPC niveau applicant

applicant_par_IPCsubgroup <- as.data.frame(unique(brevets_ipc[c("App_name", "IPC")]))

bip <- graph.data.frame(applicant_par_IPCsubgroup)
V(bip)$type <- V(bip)$name %in% applicant_par_IPCsubgroup[,2]
vpat <- get.adjacency(bipartite.projection(bip)[[2]], attr="weight", sparse=TRUE)
vpat[order(rownames(vpat)), order(colnames(vpat))]
g <- graph_from_adjacency_matrix(vpat, mode="undirected", weighted = TRUE, diag = TRUE)

summary(g) # nb noeuds et liens
graph.density(g) # densité
summary(degree(g)) # degré moyen
summary(strength(g)) # degré pondéré moyen
transitivity(g, type= "localaverageundirected") # coef clustering
cluster_louvain(g) # communautés et modularité
c <- components(g) 
summary (c$csize) # plus grande composante
c$no # nombre de composantes
average.path.length(g) # plus courts chemins moyens


applicant_par_IPCsubgroup <- as.data.frame(table(applicant_par_IPCsubgroup$IPC))
summary(applicant_par_IPCsubgroup$Freq)
applicant_par_IPCsubgroup$rank = rank(-applicant_par_IPCsubgroup$Freq)
applicant_par_IPCsubgroup <- as.data.frame(unique(applicant_par_IPCsubgroup[c("rank", "Freq")]))
reg <- lm(formula = log(applicant_par_IPCsubgroup$Freq)~log(applicant_par_IPCsubgroup$rank))
coeff <- coefficients(reg)
R2 <- summary(reg)[8]$r.squared
R2 <- round(R2, 4)
par(pty="s")
plot(x=log(applicant_par_IPCsubgroup$rank), y=log((applicant_par_IPCsubgroup$Freq)), bg="black",pch=21,xlab = "Log (Rang)", ylab = "Log (Nombre de brevets par IPC (sous-groupes))")
abline(lm(formula = log(applicant_par_IPCsubgroup$Freq)~log(applicant_par_IPCsubgroup$rank)), col = "red",lwd=3)
legend("topright", legend = c(paste("Beta = ", round(-coeff[2],4)), paste("R2 = ", R2)))


# brevets par IPC subgroup

brevets_par_IPCsubgroup <- as.data.frame(unique(brevets_ipc[c("Appln_id", "IPC")]))

bip <- graph.data.frame(brevets_par_IPCsubgroup)
V(bip)$type <- V(bip)$name %in% brevets_par_IPCsubgroup[,2]
vpat <- get.adjacency(bipartite.projection(bip)[[2]], attr="weight", sparse=TRUE)
vpat[order(rownames(vpat)), order(colnames(vpat))]
g <- graph_from_adjacency_matrix(vpat, mode="undirected", weighted = TRUE, diag = TRUE)

summary(g) # nb noeuds et liens
graph.density(g) # densité
summary(degree(g)) # degré moyen
summary(strength(g)) # degré pondéré moyen
transitivity(g, type= "localaverageundirected") # coef clustering
cluster_louvain(g) # communautés et modularité
c <- components(g) 
summary (c$csize) # plus grande composante
average.path.length(g) # plus courts chemins moyens


brevets_par_IPCsubgroup <- as.data.frame(table(brevets_par_IPCsubgroup$IPC))
summary(brevets_par_IPCsubgroup$Freq)
brevets_par_IPCsubgroup$rank = rank(-brevets_par_IPCsubgroup$Freq)
brevets_par_IPCsubgroup <- as.data.frame(unique(brevets_par_IPCsubgroup[c("rank", "Freq")]))
reg <- lm(formula = log(brevets_par_IPCsubgroup$Freq)~log(brevets_par_IPCsubgroup$rank))
coeff <- coefficients(reg)
R2 <- summary(reg)[8]$r.squared
R2 <- round(R2, 4)
par(pty="s")
plot(x=log(brevets_par_IPCsubgroup$rank), y=log((brevets_par_IPCsubgroup$Freq)), bg="black",pch=21,xlab = "Log (Rang)", ylab = "Log (Nombre de brevets par IPC (sous-groupes))")
abline(lm(formula = log(brevets_par_IPCsubgroup$Freq)~log(brevets_par_IPCsubgroup$rank)), col = "red",lwd=3)
legend("topright", legend = c(paste("Beta = ", round(-coeff[2],4)), paste("R2 = ", R2)))


# brevets par IPC group

brevets_par_IPCgroup <- as.data.frame(unique(brevets_ipc[c("Appln_id", "IPC")]))
brevets_par_IPCgroup$IPC <- substr(brevets_par_IPCgroup$IPC, 0, 7)

bip <- graph.data.frame(brevets_par_IPCgroup)
V(bip)$type <- V(bip)$name %in% brevets_par_IPCgroup[,2]
vpat <- get.adjacency(bipartite.projection(bip)[[2]], attr="weight", sparse=TRUE)
vpat[order(rownames(vpat)), order(colnames(vpat))]
g <- graph_from_adjacency_matrix(vpat, mode="undirected", weighted = TRUE, diag = TRUE)

summary(g) # nb noeuds et liens
graph.density(g) # densité
summary(degree(g)) # degré moyen
summary(strength(g)) # degré pondéré moyen
transitivity(g, type= "localaverageundirected") # coef clustering
cluster_louvain(g) # communautés et modularité
c <- components(g) 
summary (c$csize) # plus grande composante
average.path.length(g) # plus courts chemins moyens

brevets_par_IPCgroup <- as.data.frame(table(brevets_par_IPCgroup$IPC))
summary(brevets_par_IPCgroup$Freq)
brevets_par_IPCgroup$rank = rank(-brevets_par_IPCgroup$Freq)
brevets_par_IPCgroup <- as.data.frame(unique(brevets_par_IPCgroup[c("rank", "Freq")]))

reg <- lm(formula = log(brevets_par_IPCgroup$Freq)~log(brevets_par_IPCgroup$rank))
coeff <- coefficients(reg)
R2 <- summary(reg)[8]$r.squared
R2 <- round(R2, 4)
par(pty="s")
plot(x=log(brevets_par_IPCgroup$rank), y=log((brevets_par_IPCgroup$Freq)), bg="black",pch=21,xlab = "Log (Rang)", ylab = "Log (Nombre de brevets par IPC (groupes))")
abline(lm(formula = log(brevets_par_IPCgroup$Freq)~log(brevets_par_IPCgroup$rank)), col = "red",lwd=3)
legend("topright", legend = c(paste("Beta = ", round(-coeff[2],4)), paste("R2 = ", R2)))
totalipc <- sum(brevets_par_IPCgroup$Freq)
brevets_par_IPCgroup$perc <- brevets_par_IPCgroup$Freq/totalipc

# brevets par IPC subclass


brevets_par_IPCsubclass <- as.data.frame(unique(brevets_ipc[c("Appln_id", "IPC")]))
brevets_par_IPCsubclass$IPC <- substr(brevets_par_IPCsubclass$IPC, 0, 4)
bip <- graph.data.frame(brevets_par_IPCsubclass)
V(bip)$type <- V(bip)$name %in% brevets_par_IPCsubclass[,2]
vpat <- get.adjacency(bipartite.projection(bip)[[2]], attr="weight", sparse=TRUE)
vpat[order(rownames(vpat)), order(colnames(vpat))]
g <- graph_from_adjacency_matrix(vpat, mode="undirected", weighted = TRUE, diag = TRUE)

summary(g) # nb noeuds et liens
graph.density(g) # densité
summary(degree(g)) # degré moyen
summary(strength(g)) # degré pondéré moyen
transitivity(g, type= "localaverageundirected") # coef clustering
cluster_louvain(g) # communautés et modularité
c <- components(g) 
summary (c$csize) # plus grande composante
average.path.length(g) # plus courts chemins moyens


brevets_par_IPCsubclass <- as.data.frame(table(brevets_par_IPCsubclass$IPC))
summary(brevets_par_IPCsubclass$Freq)
brevets_par_IPCsubclass$rank = rank(-brevets_par_IPCsubclass$Freq)
brevets_par_IPCsubclass <- as.data.frame(unique(brevets_par_IPCsubclass[c("rank", "Freq")]))

reg <- lm(formula = log(brevets_par_IPCsubclass$Freq)~log(brevets_par_IPCsubclass$rank))
coeff <- coefficients(reg)
R2 <- summary(reg)[8]$r.squared
R2 <- round(R2, 4)
par(pty="s")
plot(x=log(brevets_par_IPCsubclass$rank), y=log((brevets_par_IPCsubclass$Freq)), bg="black",pch=21,xlab = "Log (Rang)", ylab = "Log (Nombre de brevets par IPC (sous-classes))")
abline(lm(formula = log(brevets_par_IPCsubclass$Freq)~log(brevets_par_IPCsubclass$rank)), col = "red",lwd=3)
legend("topright", legend = c(paste("Beta = ", round(-coeff[2],4)), paste("R2 = ", R2)))
totalipcclass <- sum(brevets_par_IPCsubclass$Freq)
brevets_par_IPCsubclass$perc <- brevets_par_IPCsubclass$Freq/totalipcclass


# brevets par IPC class

brevets_par_IPCclass <- as.data.frame(unique(brevets_ipc[c("Appln_id", "IPC")]))
brevets_par_IPCclass$IPC <- substr(brevets_par_IPCclass$IPC, 0, 3)

bip <- graph.data.frame(brevets_par_IPCclass)
V(bip)$type <- V(bip)$name %in% brevets_par_IPCclass[,2]
vpat <- get.adjacency(bipartite.projection(bip)[[2]], attr="weight", sparse=TRUE)
vpat[order(rownames(vpat)), order(colnames(vpat))]
g <- graph_from_adjacency_matrix(vpat, mode="undirected", weighted = TRUE, diag = TRUE)

summary(g) # nb noeuds et liens
graph.density(g) # densité
summary(degree(g)) # degré moyen
summary(strength(g)) # degré pondéré moyen
transitivity(g, type= "localaverageundirected") # coef clustering
cluster_louvain(g) # communautés et modularité
c <- components(g) 
summary (c$csize) # plus grande composante
average.path.length(g) # plus courts chemins moyens


brevets_par_IPCclass <- as.data.frame(table(brevets_par_IPCclass$IPC))
summary(brevets_par_IPCclass$Freq)
brevets_par_IPCclass$rank = rank(-brevets_par_IPCclass$Freq)
brevets_par_IPCclass <- as.data.frame(unique(brevets_par_IPCclass[c("rank", "Freq")]))

reg <- lm(formula = log(brevets_par_IPCclass$Freq)~log(brevets_par_IPCclass$rank))
coeff <- coefficients(reg)
R2 <- summary(reg)[8]$r.squared
R2 <- round(R2, 4)
par(pty="s")
plot(x=log(brevets_par_IPCclass$rank), y=log((brevets_par_IPCclass$Freq)), bg="black",pch=21,xlab = "Log (Rang)", ylab = "Log (Nombre de brevets par IPC (classes))")
abline(lm(formula = log(brevets_par_IPCclass$Freq)~log(brevets_par_IPCclass$rank)), col = "red",lwd=3)
legend("topright", legend = c(paste("Beta = ", round(-coeff[2],4)), paste("R2 = ", R2)))

# Distribution hiérarchique des liens de proximité technologique

brevets_par_IPCgroup <- as.data.frame(unique(brevets_ipc[c("Appln_id", "IPC")]))
brevets_par_IPCgroup$IPC <- substr(brevets_par_IPCgroup$IPC, 0, 7)

bip <- graph.data.frame(brevets_par_IPCgroup)
V(bip)$type <- V(bip)$name %in% brevets_par_IPCgroup[,2]
vpat <- get.adjacency(bipartite.projection(bip)[[2]], attr="weight", sparse=TRUE)
vpat[order(rownames(vpat)), order(colnames(vpat))]
g <- graph_from_adjacency_matrix(vpat, mode="undirected", weighted = TRUE, diag = TRUE)

e <- cbind( get.edgelist(g) , round( E(g)$weight, 3 ))
e <- as.data.frame(e)
colnames(e)[1]<-"Source"
colnames(e)[2]<-"Target"
colnames(e)[3]<-"Weight"
e$Weight <- as.numeric(as.character(sub("," , ".", e$Weight)))
e$Type <- "Undirected"


e$rank = rank(-e$Weight)
e <- as.data.frame(unique(e[c("rank", "Weight")]))

reg <- lm(formula = log(e$Weight)~log(e$rank))
coeff <- coefficients(reg)
R2 <- summary(reg)[8]$r.squared
R2 <- round(R2, 4)
par(pty="s")
plot(x=log(e$rank), y=log((e$Weight)), bg="black",pch=21, xlab = "Log (Rang)", ylab = "Log (Valeur des liens)")
abline(lm(formula = log(e$Weight)~log(e$rank)), col = "red",lwd=3)
legend("topright", legend = c(paste("Beta = ", round(-coeff[2],4)), paste("R2 = ", R2)))

# Distribution hiérarchique des degrés du réseau technologique

deg <- as.data.frame(strength(g))

colnames(deg)[1]<- "Degree"

deg$rank = rank(-deg$Degree)
deg <- subset(deg, deg$Degree>0)
deg <- as.data.frame(unique(deg[c("rank", "Degree")]))

reg <- lm(formula = log(deg$Degree)~log(deg$rank))
coeff <- coefficients(reg)
R2 <- summary(reg)[8]$r.squared
R2 <- round(R2, 4)
par(pty="s")
plot(x=log(deg$rank), y=log((deg$Degree)), bg="black",pch=21, xlab = "Log (Rang)", ylab = "Log (Degré pondéré)")
abline(lm(formula = log(deg$Degree)~log(deg$rank)), col = "red",lwd=3)
legend("topright", legend = c(paste("Beta = ", round(-coeff[2],4)), paste("R2 = ", R2)))

# graphes aléatoires pour test small world

set.seed(100)
gs <- list()
apl <- vector()
t <- vector()
for (x in seq_len(100L)) {
  gs[[x]] <- erdos.renyi.game(3867, 33870 /(3867*(3867-1)/2), directed = FALSE)
  apl[x] <- round(average.path.length(gs[[x]]),4)
  t[x] <- transitivity(gs[[x]], type= "localaverageundirected")
  
}

summary(apl)
summary(t)
summary(d)

# test scale free network

fit_power_law = function(graph) {
  # calcul degré
  d = degree(graph, mode = "all")
  dd = degree.distribution(graph, mode = "all", cumulative = FALSE)
  degree = 1:max(d)
  probability = dd[-1]
  # enlève valeur nulle
  nonzero.position = which(probability != 0)
  probability = probability[nonzero.position]
  degree = degree[nonzero.position]
  reg = lm(log(probability) ~ log(degree))
  cozf = coef(reg)
  power.law.fit = function(x) exp(cozf[[1]] + cozf[[2]] * log(x))
  alpha = -cozf[[2]]
  R.square = summary(reg)$r.squared
  print(paste("Gamma =", round(alpha, 3)))
  print(paste("R2 =", round(R.square, 3)))
  # plot
  par(pty="s")
  plot(probability ~ degree, log = "xy", xlab = "Degré (log)", ylab = "Fréquence (log)", bg="black",pch=21,
       col = 1)
  curve(power.law.fit, col = "red", add = T, n = length(d))
  legend("topright", legend = c(paste("Gamma = ", round(alpha,4)), paste("R2 = ", round(R.square,2))))
  
}

brevets_par_IPCgroup <- as.data.frame(unique(brevets_ipc[c("Appln_id", "IPC")]))
brevets_par_IPCgroup$IPC <- substr(brevets_par_IPCgroup$IPC, 0, 7)

bip <- graph.data.frame(brevets_par_IPCgroup)
V(bip)$type <- V(bip)$name %in% brevets_par_IPCgroup[,2]
vpat <- get.adjacency(bipartite.projection(bip)[[2]], attr="weight", sparse=TRUE)
vpat[order(rownames(vpat)), order(colnames(vpat))]
g <- graph_from_adjacency_matrix(vpat, mode="undirected", weighted = TRUE, diag = TRUE)

fit_power_law(g)



##### 4.1.2 Proxi organisationnelle #####

###### org prox ######

orgprox <- unique(brevets_ipc[c("Inv_name","App_name")])

orgprox$Inv_name <- as.character(orgprox$Inv_name)
orgprox$App_name <- as.character(orgprox$App_name)


bip <- graph.data.frame(orgprox)
V(bip)$type <- V(bip)$name %in% orgprox[,2]
mat <- get.adjacency(bipartite.projection(bip)[[2]], attr="weight", sparse=TRUE)
mat[order(rownames(mat)), order(colnames(mat))]
mat <- as.matrix(mat)
g <- graph_from_adjacency_matrix(mat, mode="undirected", weighted = TRUE, diag = TRUE)
e <- cbind( get.edgelist(bip))
e <- as.data.frame(e)
colnames(e)[1]<-"Source"
colnames(e)[2]<-"Target"
colnames(e)[3]<-"Weight"
e$Weight <- as.numeric(as.character(sub("," , ".", e$Weight)))
e$Type <- "Undirected"
e$Weight <- 1

write.csv(e, "organizproximity.csv", row.names = FALSE)

fit_power_law(bip)



deg <- as.data.frame(degree(bip, mode= c("all")))
colnames(deg)[1]<- "Degree"
deg$rank = rank(-deg$Degree)
deg <- subset(deg, deg$Degree>0)
deg <- unique(deg[c("Degree","rank")])
reg <- lm(formula = log(deg$Degree)~log(deg$rank))
coeff <- coefficients(reg)
R2 <- summary(reg)[8]$r.squared
R2 <- round(R2, 4)
par(pty="s")
plot(x=log(deg$rank), y=log((deg$Degree)), bg="black",pch=21, xlab = "Log (Rang)", ylab = "Log (Degré)")
abline(lm(formula = log(deg$Degree)~log(deg$rank)), col = "red",lwd=3)
legend("topright", legend = c(paste("Beta = ", round(-coeff[2],4)), paste("R2 = ", R2)))



transitivity(bip, type= "localaverageundirected")


write.csv(e, file = "organiz.csv")

###### mobi inv ######

mobiinv <- unique(brevets_ipc[c("Person_id.x","App_name")])


bip <- graph.data.frame(mobiinv)
V(bip)$type <- V(bip)$name %in% mobiinv[,2]
mat <- get.adjacency(bipartite.projection(bip)[[2]], attr="weight", sparse=TRUE)
mat[order(rownames(mat)), order(colnames(mat))]
mat <- as.matrix(mat)
g <- graph_from_adjacency_matrix(mat, mode="undirected", weighted = TRUE, diag = TRUE)
e <- cbind( get.edgelist(g) , round( E(g)$weight, 3 ))
e <- as.data.frame(e)
colnames(e)[1]<-"Source"
colnames(e)[2]<-"Target"
colnames(e)[3]<-"Weight"
e$Weight <- as.numeric(as.character(sub("," , ".", e$Weight)))
e$Type <- "Undirected"

write.csv(e, file = "mobiinv.csv", row.names = FALSE)

freqmobiinv <- as.data.frame(table(e$Source)) # fréquences de mobilité pro
sum(e$Weight)

fit_power_law(g)

##### 4.1.3 Prox sociale #####


cobrevinv <- unique(brevets_ipc[c("Appln_id","Inv_name")])
bip <- graph.data.frame(cobrevinv)
V(bip)$type <- V(bip)$name %in% cobrevinv[,2]
mat <- get.adjacency(bipartite.projection(bip)[[2]], attr="weight", sparse=TRUE)
mat[order(rownames(mat)), order(colnames(mat))]
mat <- as.matrix(mat)
g <- graph_from_adjacency_matrix(mat, mode="undirected", weighted = TRUE, diag = TRUE)
einv <- cbind( get.edgelist(g) , round( E(g)$weight, 3 ))
einv <- as.data.frame(einv)
colnames(einv)[1]<-"Source"
colnames(einv)[2]<-"Target"
colnames(einv)[3]<-"Weight"
einv$Weight <- as.numeric(as.character(sub("," , ".", einv$Weight)))
einv$Type <- "Undirected"

fit_power_law(g)


e$rank = rank(-e$Weight)
e <- as.data.frame(unique(e[c("rank", "Weight")]))

reg <- lm(formula = log(e$Weight)~log(e$rank))
coeff <- coefficients(reg)
R2 <- summary(reg)[8]$r.squared
R2 <- round(R2, 4)
par(pty="s")
plot(x=log(e$rank), y=log((e$Weight)), bg="black",pch=21, xlab = "Log (Rang)", ylab = "Log (Valeur des liens)")
abline(lm(formula = log(e$Weight)~log(e$rank)), col = "red",lwd=3)
legend("topright", legend = c(paste("Beta = ", round(-coeff[2],4)), paste("R2 = ", R2)))

summary(g) # nb noeuds et liens
graph.density(g) # densité
summary(degree(g)) # degré moyen
summary(strength(g)) # degré pondéré moyen
transitivity(g, type= "localaverageundirected") # coef clustering
cluster_louvain(g) # communautés et modularité
c <- components(g) 
summary (c$csize) # plus grande composante
average.path.length(g) # plus courts chemins moyens

bc <- as.data.frame(betweenness(g, directed = FALSE))
colnames(bc)[1]<- "BC"
bc$rank = rank(-bc$BC)
bc <- subset(bc, bc$BC>0)
bc <- unique(bc[c("BC","rank")])
reg <- lm(formula = log(bc$BC)~log(bc$rank))
coeff <- coefficients(reg)
R2 <- summary(reg)[8]$r.squared
R2 <- round(R2, 4)
par(pty="s")
plot(x=log(bc$rank), y=log((bc$BC)), bg="black",pch=21, xlab = "Log (Rang)", ylab = "Log (BC)")
abline(lm(formula = log(bc$BC)~log(bc$rank)), col = "red",lwd=3)
legend("topright", legend = c(paste("Beta = ", round(-coeff[2],4)), paste("R2 = ", R2)))




##### 4.1.4 prox geo #####

## creation figure voisinage

dpt = readOGR(dsn = "D:/memoire_tmp/l_commune_scot_bdt_r82", layer="Aire_metro_Lyon", stringsAsFactors=FALSE)


# informations relatives au shapefile

str(dpt@data, 2)

# affichage de la couche

plot(dpt)

# création des voisins

dpt_queen_nb = poly2nb(dpt, row.names=dpt@data$NOM, queen=TRUE)

# affichage des voisins
coords = coordinates(dpt)
plot(dpt, border="grey") + plot(dpt_queen_nb, coords, add=TRUE, col= "red")

# Nombre de liens par département - graphe
qq=table(card(dpt_queen_nb))
barplot(qq,col=2,main="Répartition du nombre de voisins par commune")

# transformation en matrice d'adjacence

com <- nb2mat(dpt_queen_nb)   
row.names(com) = dpt$insee_comm
colnames(com) = dpt$insee_comm

com[com > 0] <- 1

setwd("D:/memoire_tmp")
write.csv(com, file = "geomat.csv", row.names = FALSE)

g <- graph_from_adjacency_matrix(com, mode = "undirected", weighted = NULL, diag = FALSE)


## creation de la matrice de distances

points_brevets = readOGR(dsn = "C:/Users/rlx/Desktop/memoire_tmp/l_commune_scot_bdt_r82", layer="points_brevets", stringsAsFactors=FALSE)
points_brevets$Nombre.de <- as.numeric(as.character(sub("," , ".", points_brevets$Nombre.de)))

dm <- DistanceMatrix(points_brevets, unit = 1000, "insee_comm")


g <- graph_from_adjacency_matrix(dm, mode="undirected", weighted = TRUE, diag = TRUE)
dm <- cbind( get.edgelist(g) , round( E(g)$weight, 3 ))
dm <- as.data.frame(dm)
colnames(dm)[1]<-"Source"
colnames(dm)[2]<-"Target"
colnames(dm)[3]<-"Weight"
dm$Weight <- as.numeric(as.character(sub("," , ".", dm$Weight)))
dm$Type <- "Undirected"

dm[dm   < 0] = 0

write.csv(dm, file = "dist_matrix.csv", row.names = FALSE)


median(dm$Weight)


deg <- as.data.frame(degree(bip, mode= c("all")))
colnames(deg)[1]<- "Degree"
deg$rank = rank(-deg$Degree)
deg <- subset(deg, deg$Degree>0)
deg <- unique(deg[c("Degree","rank")])
reg <- lm(formula = log(deg$Degree)~log(deg$rank))
coeff <- coefficients(reg)
R2 <- summary(reg)[8]$r.squared
R2 <- round(R2, 4)
par(pty="s")
plot(x=log(deg$rank), y=log((deg$Degree)), bg="black",pch=21, xlab = "Log (Rang)", ylab = "Log (Degré)")
abline(lm(formula = log(deg$Degree)~log(deg$rank)), col = "red",lwd=3)
legend("topright", legend = c(paste("Beta = ", round(-coeff[2],4)), paste("R2 = ", R2)))


####### 4.2 Positions et collaborations #######

#### 4.2.1 Collaborations -> analyse des liens

#### 4.2.1.a liens entre technologies


citations <- read.csv(file = "201703_EP_Citations.txt", sep="|", header = TRUE)
brevets_ipc <- read.table(file = "brevets_ipc_applicants.csv", sep = ";", header = TRUE, encoding = "UTF-8")

citations <- citations[c("Citing_appln_id","Cited_Appln_id")]

citationsAML <- subset(citations, citations$Citing_appln_id %in% brevets_ipc$Appln_id)

reg <- read.csv(file="201602_EPO_Inv_reg.txt", sep="|", header = TRUE)
reg <- reg[c("Appln_id","Reg_code")]


colnames(citations)[1]<- "Source"
colnames(citations)[2]<- "Target"
colnames(reg)[1]<-"Source"
citationsregsource <- merge(citations, reg[c("Source","Reg_code")], by="Source" )
colnames(reg)[1]<-"Target"
colnames(reg)[2]<-"Reg_code"
citationsregsourcetarget <- merge(citationsregsource, reg[c("Target","Reg_code")], by="Target" )
colnames(citationsregsourcetarget)[3]<-"Source_reg"
colnames(citationsregsourcetarget)[4]<-"Target_reg"


citdeAML <- subset(citationsregsourcetarget, citationsregsourcetarget$Source %in% brevets_ipc$Appln_id)
citversAML <- subset(citationsregsourcetarget, citationsregsourcetarget$Target %in% brevets_ipc$Appln_id)
citdeAMLversAML <- subset(citdeAML, citdeAML$Target %in% brevets_ipc$Appln_id)


ipctot <- read.csv("201602_EPO_IPC.txt",header = TRUE, sep = "|")
ipctot <- ipctot[,c(1,4)]

colnames(ipctot)[1]<-"Source"

citdeAMLversAML <- merge(citdeAMLversAML, ipctot, by="Source")
colnames(citdeAMLversAML)[5]<-"IPC_Source"
colnames(ipctot)[1]<-"Target"

citdeAMLversAML <- merge(citdeAMLversAML, ipctot, by="Target")
colnames(citdeAMLversAML)[6]<-"IPC_Target"

citdeAMLversAML$IPC_Source <- substr(citdeAMLversAML$IPC_Source, 0, 7)
citdeAMLversAML$IPC_Target <- substr(citdeAMLversAML$IPC_Target, 0, 7)
citdeAMLversAML <- unique(citdeAMLversAML[c("Source", "Target", "IPC_Source","IPC_Target")])

write.csv(citdeAMLversAML, file="citdeAMLversAML.csv", row.names = FALSE)

citdeAMLversAML_unique <- unique(citdeAMLversAML[c("Source", "Target")])  

colnames(ipctot)[1]<-"Source"

citdeAML <- merge(citdeAML, ipctot, by="Source")
colnames(citdeAML)[5]<-"IPC_Source"
colnames(ipctot)[1]<-"Target"
citdeAML <- merge(citdeAML, ipctot, by="Target")
colnames(citdeAML)[6]<-"IPC_Target"
citdeAML$IPC_Source <- substr(citdeAML$IPC_Source, 0, 7)
citdeAML$IPC_Target <- substr(citdeAML$IPC_Target, 0, 7)
citdeAML_unique <- unique(citdeAML[c("Source", "Target")])  
excel1 <- citdeAML[1:1000000,]  
write.csv2(excel1, file = "excel1.csv", row.names = FALSE)
excel2 <- citdeAML[1000001:2000000,]  
write.csv2(excel2, file = "excel2.csv", row.names = FALSE)
excel3 <- citdeAML[2000001:3000000,]  
write.csv2(excel3, file = "excel3.csv", row.names = FALSE)
excel4 <- citdeAML[3000001:4000000,]  
write.csv2(excel4, file = "excel4.csv", row.names = FALSE)
excel5 <- citdeAML[4000001:5000000,]  
write.csv2(excel5, file = "excel5.csv", row.names = FALSE)
excel6 <- citdeAML[5000001:6000000,]  
write.csv2(excel6, file = "excel6.csv", row.names = FALSE)
excel7 <- citdeAML[6000001:7000000,]  
write.csv2(excel7, file = "excel7.csv", row.names = FALSE)
excel8 <- citdeAML[7000001:8000000,]  
write.csv2(excel8, file = "excel8.csv", row.names = FALSE)
excel9 <- citdeAML[8000001:9000000,]  
write.csv2(excel9, file = "excel9.csv", row.names = FALSE)


### positions citations ####


citations <- read.csv("201703_EP_Citations.txt",header = TRUE, sep = "|")


citedAML <- subset(citations, citations$Cited_Appln_id %in% brevets_ipc$Appln_id)

citedAML <- citedAML[,c(4,9)]
colnames(citedAML)[1] <- "Appln_id"

brevetsAMLIPC <- unique(brevets_ipc[ , c("Appln_id", "IPC")])
citedAML <- merge(citedAML,brevetsAMLIPC, by = "Appln_id")
colnames(citedAML)[1] <- "Cited_Appln_id"
citedAML$IPC <- substr(citedAML$IPC, 0, 7)

freq_cited_ipc <- as.data.frame(table(citedAML$IPC))

colnames(freq_cited_ipc)[1]<-"IPC"
freq_cited_ipc$FreqTotReg <- freq_cited_ipc$Freq/(sum(freq_cited_ipc$Freq))



ipctot <- read.csv("201602_EPO_IPC.txt",header = TRUE, sep = "|")

colnames(citations)[9] <- "Appln_id"
citation_ipc <- merge(citations[,c("Citing_appln_id","Appln_id")],ipctot[,c("Appln_id","IPC")],by="Appln_id")
citation_ipc$IPC <- substr(citation_ipc$IPC, 0, 7)



freq_cited_ipctot <- as.data.frame(table(citation_ipc$IPC))
freq_cited_ipctot <- freq_cited_ipctot[-1,]

freq_cited_ipctot <- subset(freq_cited_ipctot, freq_cited_ipctot$Var1 %in% freq_cited_ipc$IPC)

freq_cited_ipctot$FreqTot <- freq_cited_ipctot$Freq/(sum(freq_cited_ipctot$Freq))
colnames(freq_cited_ipctot)[1]<- "IPC"


LQ_citations_IPC <- merge(freq_cited_ipc, freq_cited_ipctot, by="IPC")
LQ_citations_IPC$LQ <- LQ_citations_IPC$FreqTotReg/LQ_citations_IPC$FreqTot
LQ_citations_IPC <- subset(LQ_citations_IPC, LQ_citations_IPC$Freq.x > 19)

write.csv(file="LQ_Citations_recues_AML.csv", LQ_citations_IPC, row.names = FALSE)


### 4.2.1.b co-brevets app ###

cobrev <- unique(brevets_ipc[c("Appln_id","App_name")])
bip <- graph.data.frame(cobrev)
V(bip)$type <- V(bip)$name %in% cobrev[,2]
mat <- get.adjacency(bipartite.projection(bip)[[2]], attr="weight", sparse=TRUE)
mat[order(rownames(mat)), order(colnames(mat))]
mat <- as.matrix(mat)
g <- graph_from_adjacency_matrix(mat, mode="undirected", weighted = TRUE, diag = TRUE)
e <- cbind( get.edgelist(g) , round( E(g)$weight, 3 ))
e <- as.data.frame(e)
colnames(e)[1]<-"Source"
colnames(e)[2]<-"Target"
colnames(e)[3]<-"Weight"
e$Weight <- as.numeric(as.character(sub("," , ".", e$Weight)))
e$Type <- "Undirected"


### 4.2.1.c Collaborations dans l'espace social ###


cobrevinv <- unique(brevets_ipc[c("Appln_id","Inv_name")])
bip <- graph.data.frame(cobrevinv)
V(bip)$type <- V(bip)$name %in% cobrevinv[,2]
mat <- get.adjacency(bipartite.projection(bip)[[2]], attr="weight", sparse=TRUE)
mat[order(rownames(mat)), order(colnames(mat))]
mat <- as.matrix(mat)
g <- graph_from_adjacency_matrix(mat, mode="undirected", weighted = TRUE, diag = TRUE)
e <- cbind( get.edgelist(g) , round( E(g)$weight, 3 ))
e <- as.data.frame(e)
colnames(e)[1]<-"Source"
colnames(e)[2]<-"Target"
colnames(e)[3]<-"Weight"
e$Weight <- as.numeric(as.character(sub("," , ".", e$Weight)))
e$Type <- "Undirected"
sum(e$Weight)

summary(g) # nb noeuds et liens
graph.density(g) # densité
summary(degree(g)) # degré moyen
summary(strength(g)) # degré pondéré moyen
transitivity(g, type= "localaverageundirected") # coef clustering
cc<- cluster_louvain(g) # communautés et modularité
c <- components(g) 
summary (c$csize) # plus grande composante
average.path.length(g) # plus courts chemins moyens
summary(as.data.frame(sizes(cc)))

# graphes aléatoires pour test small world

set.seed(100)
gs <- list()
apl <- vector()
t <- vector()
for (x in seq_len(100L)) {
  gs[[x]] <- erdos.renyi.game(11852, 15513 /(11852*(11852-1)/2), directed = FALSE)
  apl[x] <- round(average.path.length(gs[[x]]),4)
  t[x] <- transitivity(gs[[x]], type= "localaverageundirected")
  
}

summary(apl)
summary(t)



e$rank = rank(-e$Weight)
e <- as.data.frame(unique(e[c("rank", "Weight")]))

reg <- lm(formula = log(e$Weight)~log(e$rank))
coeff <- coefficients(reg)
R2 <- summary(reg)[8]$r.squared
R2 <- round(R2, 4)
par(pty="s")
plot(x=log(e$rank), y=log((e$Weight)), bg="black",pch=21, xlab = "Log (Rang)", ylab = "Log (Valeur des liens)")
abline(lm(formula = log(e$Weight)~log(e$rank)), col = "red",lwd=3)
legend("topright", legend = c(paste("Beta = ", round(-coeff[2],4)), paste("R2 = ", R2)))


deg <- as.data.frame(degree(g))
colnames(deg)[1]<- "Degree"

deg$rank = rank(-deg$Degree)
deg <- as.data.frame(unique(deg[c("rank", "Degree")]))

deg <- subset(deg, deg$Degree>0)
reg <- lm(formula = log(deg$Degree)~log(deg$rank))
coeff <- coefficients(reg)
R2 <- summary(reg)[8]$r.squared
R2 <- round(R2, 4)
par(pty="s")
plot(x=log(deg$rank), y=log((deg$Degree)), bg="black",pch=21, xlab = "Log (Rang)", ylab = "Log (Degré)")
abline(lm(formula = log(deg$Degree)~log(deg$rank)), col = "red",lwd=3)
legend("topright", legend = c(paste("Beta = ", round(-coeff[2],4)), paste("R2 = ", R2)))


fit_power_law(g)

#### 4.1.2.d collaborations géographiques ####

points_brevets = readOGR(dsn = "C:/Users/rlx/Desktop/memoire_tmp/l_commune_scot_bdt_r82", layer="points_brevets", stringsAsFactors=FALSE)
points_brevets$Nombre.de <- as.numeric(as.character(sub("," , ".", points_brevets$Nombre.de)))

dm <- DistanceMatrix(points_brevets, unit = 1000, "insee_comm")


g <- graph_from_adjacency_matrix(dm, mode="undirected", weighted = TRUE, diag = TRUE)
dm <- cbind( get.edgelist(g) , round( E(g)$weight, 3 ))
dm <- as.data.frame(dm)
colnames(dm)[1]<-"Source"
colnames(dm)[2]<-"Target"
colnames(dm)[3]<-"Weight"
dm$Weight <- as.numeric(as.character(sub("," , ".", dm$Weight)))
dm$Type <- "Undirected"

dm[dm   < 0] = 0

write.csv(dm, file = "dist_matrix.csv", row.names = FALSE)


median(dm$Weight)

cobrevinv <- unique(brevets_ipc[c("Appln_id","Inv_name")])
bip <- graph.data.frame(cobrevinv)
V(bip)$type <- V(bip)$name %in% cobrevinv[,2]
mat <- get.adjacency(bipartite.projection(bip)[[2]], attr="weight", sparse=TRUE)
mat[order(rownames(mat)), order(colnames(mat))]
mat <- as.matrix(mat)
g <- graph_from_adjacency_matrix(mat, mode="undirected", weighted = TRUE, diag = TRUE)
e <- cbind( get.edgelist(g) , round( E(g)$weight, 3 ))
e <- as.data.frame(e)
colnames(e)[1]<-"Source"
colnames(e)[2]<-"Target"
colnames(e)[3]<-"Weight"
e$Weight <- as.numeric(as.character(sub("," , ".", e$Weight)))
e$Type <- "Undirected"



colnames(brevets_ipc)[7] <- "Source"
inv_geo <- merge(e, brevets_ipc[c("Source", "INSEE_Code")], by="Source")
inv_geo <- unique(inv_geo)
colnames(brevets_ipc)[7] <- "Target"
inv_geod <- merge(inv_geo, brevets_ipc[c("Target", "INSEE_Code")], by="Target")
inv_geod <- unique(inv_geod)
inv_geod$merg <- paste(inv_geod$INSEE_Code.x,inv_geod$INSEE_Code.y)
dm$merg <- paste(dm$Source, dm$Target)
inv_geod <- merge(inv_geod, dm[c("merg", "Weight")], by="merg")
colnames(inv_geod)[8]<- "Distance"
inv_geod$Distance <- as.numeric(as.character(sub("," , ".", inv_geod$Distance)))

summary(inv_geod$Distance)
plot(inv_geod$Distance, type="h")
d <- density(inv_geod$Distance)
plot(d, main = "Distribution des distances entre inventeurs", ylab="Densité", xlab="Distance (km)", col = "black",lwd=3)

# citations

citAMLAML <- read.csv(file="citdeAMLversAML.csv")
dm <- read.csv(file = "dist_matrix.csv")

colnames(brevets_ipc)[1] <- "Source"
cit_geo <- merge(citAMLAML, brevets_ipc[c("Source", "INSEE_Code")], by="Source")
cit_geo <- unique(cit_geo)
colnames(brevets_ipc)[1] <- "Target"
cit_geod <- merge(cit_geo, brevets_ipc[c("Target", "INSEE_Code")], by="Target")
cit_geod <- unique(cit_geod)

cit_geod$merg <- paste(cit_geod$INSEE_Code.x,cit_geod$INSEE_Code.y)
dm$merg <- paste(dm$Source, dm$Target)
cit_geod <- merge(cit_geod, dm[c("merg", "Weight")], by="merg")
colnames(cit_geod)[8]<- "Distance"
cit_geod$Distance <- as.numeric(as.character(sub("," , ".", cit_geod$Distance)))

summary(cit_geod$Distance)
plot(cit_geod$Distance, type="h")
d <- density(cit_geod$Distance)
plot(d, main = "Distribution des distances entre inventeurs se citant", ylab="Densité", xlab="Distance (km)", col = "black",lwd=3)



e <- cit_geod[c("INSEE_Code.x","INSEE_Code.y")]
bip <- graph.data.frame(e)
V(bip)$type <- V(bip)$name %in% cobrevAML[,2]
mat <- get.adjacency(bipartite.projection(bip)[[2]], attr="weight", sparse=TRUE)
mat[order(rownames(mat)), order(colnames(mat))]
mat <- as.matrix(mat)
g <- graph_from_adjacency_matrix(mat, mode="undirected", weighted = TRUE, diag = TRUE)
e <- cbind( get.edgelist(g) , round( E(g)$weight, 3 ))
e <- as.data.frame(e)
colnames(e)[1]<-"Source"
colnames(e)[2]<-"Target"
colnames(e)[3]<-"Weight"
e$Weight <- as.numeric(as.character(sub("," , ".", e$Weight)))
e$Type <- "Directed"


write.csv(e, file = "citationsAMLgeo.csv", row.names = FALSE)

citations <- read.csv(file = "201703_EP_Citations.txt", sep="|", header = TRUE)
citations <- citations[c("Citing_appln_id","Cited_Appln_id")]
citationsAML <- subset(citations, citations$Citing_appln_id %in% brevets_ipc$Appln_id)
reg <- read.csv(file="201602_EPO_Inv_reg.txt", sep="|", header = TRUE)

colnames(citations)[1]<- "Source"
colnames(citations)[2]<- "Target"


colnames(reg)[2]<-"Source"
citationsregsource <- merge(citations, reg[c("Source","Reg_code")], by="Source" )
colnames(reg)[2]<-"Target"
citationsregsourcetarget <- merge(citationsregsource, reg[c("Target","Reg_code")], by="Target" )
colnames(citationsregsourcetarget)[3]<-"Source_reg"
colnames(citationsregsourcetarget)[4]<-"Target_reg"

citationdeAML <- subset(citationsregsourcetarget, citationsregsourcetarget$Source %in% brevets_ipc$Appln_id)
citationversAML <- subset(citationsregsourcetarget, citationsregsourcetarget$Target %in% brevets_ipc$Appln_id)

freqcitversAML <- as.data.frame(table(citationversAML$Source_reg))
freqcitdeAML <- as.data.frame(table(citationdeAML$Target_reg))
sum(freqcitdeAML$Freq)
freqcitdeAML$perc <- freqcitdeAML$Freq/153945

write.csv(freqcitdeAML,file="freqcitdeAML.csv", row.names = FALSE)
write.csv(freqcitversAML,file="freqcitversAML.csv", row.names = FALSE)


#### 4.2.2 Positions -> analyse des noeuds


####### GEO relatedness ######

brevets_ipc <- read.table(file = "brevets_ipc_applicants.csv", sep = ";", header = TRUE, encoding = "UTF-8")
brevets_ipc <- unique(brevets_ipc[c("INSEE_Code", "IPC")])
brevets_ipc$IPC <- substr(brevets_ipc$IPC, 0, 7)
bip <- graph.data.frame(brevets_ipc)
V(bip)$type <- V(bip)$name %in% brevets_ipc[,2]
vgeo <- get.adjacency(bipartite.projection(bip)[[2]], attr="weight", sparse=TRUE)
vgeo[order(rownames(vgeo)), order(colnames(vgeo))]
g <- graph_from_adjacency_matrix(vgeo, mode="undirected", weighted = TRUE, diag = TRUE)
e_geo <- cbind( get.edgelist(g) , round( E(g)$weight, 3 ))
e_geo <- as.data.frame(e_geo)
colnames(e_geo)[1]<-"Source"
colnames(e_geo)[2]<-"Target"
colnames(e_geo)[3]<-"Weight"
e_geo$Weight <- as.numeric(as.character(sub("," , ".", e_geo$Weight)))
vgeo <- as.matrix(vgeo)

brevets_ipc_P1 <- unique(brevets_ipc_P1[c("INSEE_Code", "IPC")])
brevets_ipc_P1$IPC <- substr(brevets_ipc_P1$IPC, 0, 7)
bip <- graph.data.frame(brevets_ipc_P1)
V(bip)$type <- V(bip)$name %in% brevets_ipc_P1[,2]
vgeo <- get.adjacency(bipartite.projection(bip)[[2]], attr="weight", sparse=TRUE)
vgeo[order(rownames(vgeo)), order(colnames(vgeo))]
g <- graph_from_adjacency_matrix(vgeo, mode="undirected", weighted = TRUE, diag = TRUE)
e_geo <- cbind( get.edgelist(g) , round( E(g)$weight, 3 ))
e_geo <- as.data.frame(e_geo)
colnames(e_geo)[1]<-"Source"
colnames(e_geo)[2]<-"Target"
colnames(e_geo)[3]<-"Weight"
e_geo$Weight <- as.numeric(as.character(sub("," , ".", e_geo$Weight)))
e_geoP1 <- e_geo
e_geoP1$Type <- "Undirected"

write.csv(e_geoP1, file="e_geoP1.csv")









###### Inventeur relatedness ######

brevets_ipc <- read.table(file = "brevets_ipc_applicants.csv", sep = ";", header = TRUE, encoding = "UTF-8")

brevets_ipc <- unique(brevets_ipc[c("Person_id.x", "IPC")])
brevets_ipc$IPC <- substr(brevets_ipc$IPC, 0, 7)
bip <- graph.data.frame(brevets_ipc)
V(bip)$type <- V(bip)$name %in% brevets_ipc[,2]
vinv <- get.adjacency(bipartite.projection(bip)[[2]], attr="weight", sparse=TRUE)
vinv[order(rownames(vinv)), order(colnames(vinv))]
g <- graph_from_adjacency_matrix(vinv, mode="undirected", weighted = TRUE, diag = TRUE)
e_inv <- cbind( get.edgelist(g) , round( E(g)$weight, 3 ))
e_inv <- as.data.frame(e_inv)
colnames(e_inv)[1]<-"Source"
colnames(e_inv)[2]<-"Target"
colnames(e_inv)[3]<-"Weight"
e_inv$Weight <- as.numeric(as.character(sub("," , ".", e_inv$Weight)))
dinv <- as.data.frame(degree(g))
vinv <- as.matrix(vinv)
cor(dinv, dpat)

soc_prox_appinv <- unique(brevets_ipc[c("Person_id.x", "App_name")])
write.csv(soc_prox_appinv, file="soc_prox_appinv.csv", row.names = FALSE)



###### correlations ######

tab_graph <- array(dim = c(9,3867,3867))

vpat <- as.matrix(vpat)
vapp <- as.matrix(vapp) 
vinv <- as.matrix(vinv)
vinsee <- as.matrix(vinsee)
tab_graph[1,,] <- vpat
tab_graph[2,,] <- vapp
tab_graph[3,,] <- vinv
tab_graph[4,,] <- vinsee


Cpat_app <- cor(c(vpat), c(vapp))
Cpat_inv <- cor(c(vpat), c(vinv))
Cpat_ins <- cor(c(vpat), c(vinsee))
Capp_inv <- cor(c(vapp), c(vinv))
Capp_ins <- cor(c(vapp), c(vinsee))
Cinv_ins <- cor(c(vinv), c(vinsee))




app_ipc <- unique(brevets_ipc[c("App_name","IPC")])

colnames(app_ipc)[1] <- "Source"
app_ipc$IPC <- substr(app_ipc$IPC, 0, 7)
app_ipc <- unique(app_ipc[c("Source","IPC")])

e_app_ipc <- merge(e, app_ipc, by="Source")

colnames(e_app_ipc)[6] <- "IPC_SOURCE"
colnames(app_ipc)[1] <- "Target"

app_ipc2 <- merge(e_app_ipc, app_ipc, by="Target")

write.csv2(app_ipc2, file="app_ipc2.csv", row.names = FALSE)

#découpage fichier pour analyse sur excel

excel1 <- app_ipc2[1:1000000,]
excel2 <- app_ipc2[1000001:2000000,]
excel3 <- app_ipc2[2000001:3000000,]
excel4 <- app_ipc2[3000001:4000000,]

write.csv2(excel1, file = "excel1.csv", row.names = FALSE)
write.csv2(excel2, file = "excel2.csv", row.names = FALSE)
write.csv2(excel3, file = "excel3.csv", row.names = FALSE)
write.csv2(excel4, file = "excel4.csv", row.names = FALSE)

excel5 <- app_ipc2[4000001:5000000,]
excel6 <- app_ipc2[5000001:6000000,]

write.csv2(excel5, file = "excel5.csv", row.names = FALSE)
write.csv2(excel6, file = "excel6.csv", row.names = FALSE)

excel7 <- app_ipc2[6000001:7000000,]
excel8 <- app_ipc2[7000001:8000000,]
excel9 <- app_ipc2[8000001:9000000,]
excel10 <- app_ipc2[9000001:10000000,]


write.csv2(excel7, file = "excel7.csv", row.names = FALSE)
write.csv2(excel8, file = "excel8.csv", row.names = FALSE)
write.csv2(excel9, file = "excel9.csv", row.names = FALSE)
write.csv2(excel10, file = "excel10.csv", row.names = FALSE)



app_ipc <- unique(brevets_ipc[c("App_name","IPC")])

colnames(app_ipc)[1] <- "Source"
app_ipc$IPC <- substr(app_ipc$IPC, 0, 3)
app_ipc <- unique(app_ipc[c("Source","IPC")])

e_app_ipc <- merge(e, app_ipc, by="Source")

colnames(e_app_ipc)[6] <- "IPC_SOURCE"
colnames(app_ipc)[1] <- "Target"

app_ipc2 <- merge(e_app_ipc, app_ipc, by="Target")

write.csv(app_ipc2, file="appipc_rv.csv", row.names = FALSE)

app_ipc2$IDENTIQUE <- ""
for (x in 1:670446){
  if (app_ipc2[x,5] == app_ipc2[x,6]){app_ipc2[x,7] <- "VRAI"}
  else {app_ipc2[x,7] <- "FAUX"}
}

appipc3 <- unique(app_ipc2[c("Target", "Source", "IDENTIQUE")])
summary(appipc3$IDENTIQUE)


###### proxi geo inventeurs #######


geoinv <- unique(brevets_ipc[c("INSEE_Code","Inv_name")])
bip <- graph.data.frame(geoinv)
V(bip)$type <- V(bip)$name %in% geoinv[,2]
mat <- get.adjacency(bipartite.projection(bip)[[2]], attr="weight", sparse=TRUE)
mat[order(rownames(mat)), order(colnames(mat))]
mat <- as.matrix(mat)
g <- graph_from_adjacency_matrix(mat, mode="undirected", weighted = TRUE, diag = TRUE)
egeo <- cbind( get.edgelist(g) , round( E(g)$weight, 3 ))
egeo <- as.data.frame(egeo)
colnames(egeo)[1]<-"Source"
colnames(egeo)[2]<-"Target"
colnames(egeo)[3]<-"Weight"
egeo$Weight <- as.numeric(as.character(sub("," , ".", e$Weight)))
egeo$Type <- "Undirected"

e$rank = rank(-e$Weight)
reg <- lm(formula = log(e$Weight)~log(e$rank))
coeff <- coefficients(reg)
R2 <- summary(reg)[8]$r.squared
R2 <- round(R2, 4)
par(pty="s")
plot(x=log(e$rank), y=log((e$Weight)), bg="black",pch=21, xlab = "Log (Rang)", ylab = "Log (Valeur des liens)")
abline(lm(formula = log(e$Weight)~log(e$rank)), col = "red",lwd=3)
legend("topright", legend = c(paste("Beta = ", round(-coeff[2],4)), paste("R2 = ", R2)))



deg <- as.data.frame(degree(g, mode= c("all")))
colnames(deg)[1]<- "Degree"

deg$rank = rank(-deg$Degree)
deg <- subset(deg, deg$Degree>0)
reg <- lm(formula = log(deg$Degree)~log(deg$rank))
coeff <- coefficients(reg)
R2 <- summary(reg)[8]$r.squared
R2 <- round(R2, 4)
par(pty="s")
plot(x=log(deg$rank), y=log((deg$Degree)), bg="black",pch=21, xlab = "Log (Rang)", ylab = "Log (Degree)")
abline(lm(formula = log(deg$Degree)~log(deg$rank)), col = "red",lwd=3)
legend("topright", legend = c(paste("Beta = ", round(-coeff[2],4)), paste("R2 = ", R2)))


ecc <- as.data.frame(eccentricity(g, mode ="all"))
colnames(ecc)[1]<- "Ecc"

ecc <- as.data.frame(table(ecc$Ecc))
ecc$Var1 <- as.numeric(ecc$Var1)
plot(ecc, xlab = "Plus Grande Distance des noeuds", ylab = "Fréquence", bg="black",pch=21)
legend("topright", legend = c(paste("Plus grande distance moyenne = ", round(average.path.length(g),4 ))))

transitivity(g, type= "localaverageundirected")

write.csv(e, file = "geoinv.csv")





###### co-brev app ######

cobrev <- unique(brevets_ipc[c("Appln_id","App_name")])
bip <- graph.data.frame(cobrev)
V(bip)$type <- V(bip)$name %in% cobrev[,2]
mat <- get.adjacency(bipartite.projection(bip)[[2]], attr="weight", sparse=TRUE)
mat[order(rownames(mat)), order(colnames(mat))]
mat <- as.matrix(mat)
g <- graph_from_adjacency_matrix(mat, mode="undirected", weighted = TRUE, diag = TRUE)
e <- cbind( get.edgelist(g) , round( E(g)$weight, 3 ))
e <- as.data.frame(e)
colnames(e)[1]<-"Source"
colnames(e)[2]<-"Target"
colnames(e)[3]<-"Weight"
e$Weight <- as.numeric(as.character(sub("," , ".", e$Weight)))
e$Type <- "Undirected"

e$rank = rank(-e$Weight)
reg <- lm(formula = log(e$Weight)~log(e$rank))
coeff <- coefficients(reg)
R2 <- summary(reg)[8]$r.squared
R2 <- round(R2, 4)
par(pty="s")
plot(x=log(e$rank), y=log((e$Weight)), bg="black",pch=21, xlab = "Log (Rang)", ylab = "Log (Valeur des liens)")
abline(lm(formula = log(e$Weight)~log(e$rank)), col = "red",lwd=3)
legend("topright", legend = c(paste("Beta = ", round(-coeff[2],4)), paste("R2 = ", R2)))


fit_power_law(g)

deg <- as.data.frame(degree(g, mode= c("all")))
colnames(deg)[1]<- "Degree"

deg$rank = rank(-deg$Degree)
deg <- subset(deg, deg$Degree>0)
reg <- lm(formula = log(deg$Degree)~log(deg$rank))
coeff <- coefficients(reg)
R2 <- summary(reg)[8]$r.squared
R2 <- round(R2, 4)
par(pty="s")
plot(x=log(deg$rank), y=log((deg$Degree)), bg="black",pch=21, xlab = "Log (Rang)", ylab = "Log (Degree)")
abline(lm(formula = log(deg$Degree)~log(deg$rank)), col = "red",lwd=3)
legend("topright", legend = c(paste("Beta = ", round(-coeff[2],4)), paste("R2 = ", R2)))


ecc <- as.data.frame(eccentricity(g, mode ="all"))
colnames(ecc)[1]<- "Ecc"

ecc <- as.data.frame(table(ecc$Ecc))
ecc$Var1 <- as.numeric(ecc$Var1)
plot(ecc, xlab = "Plus Grande Distance des noeuds", ylab = "Fréquence", bg="black",pch=21)
legend("topright", legend = c(paste("Plus grande distance moyenne = ", round(average.path.length(g),4 ))))

transitivity(g, type= "localaverageundirected")
average.path.length(g)
write.csv(e, file = "cobrev.csv")





###### co-brev inv ######

cobrevinv <- unique(brevets_ipc[c("Appln_id","Inv_name")])
bip <- graph.data.frame(cobrevinv)
V(bip)$type <- V(bip)$name %in% cobrevinv[,2]
mat <- get.adjacency(bipartite.projection(bip)[[2]], attr="weight", sparse=TRUE)
mat[order(rownames(mat)), order(colnames(mat))]
mat <- as.matrix(mat)
g <- graph_from_adjacency_matrix(mat, mode="undirected", weighted = TRUE, diag = TRUE)
einv <- cbind( get.edgelist(g) , round( E(g)$weight, 3 ))
einv <- as.data.frame(einv)
colnames(einv)[1]<-"Source"
colnames(einv)[2]<-"Target"
colnames(einv)[3]<-"Weight"
einv$Weight <- as.numeric(as.character(sub("," , ".", einv$Weight)))
einv$Type <- "Undirected"

e$rank = rank(-e$Weight)
reg <- lm(formula = log(e$Weight)~log(e$rank))
coeff <- coefficients(reg)
R2 <- summary(reg)[8]$r.squared
R2 <- round(R2, 4)
par(pty="s")
plot(x=log(e$rank), y=log((e$Weight)), bg="black",pch=21, xlab = "Log (Rang)", ylab = "Log (Valeur des liens)")
abline(lm(formula = log(e$Weight)~log(e$rank)), col = "red",lwd=3)
legend("topright", legend = c(paste("Beta = ", round(-coeff[2],4)), paste("R2 = ", R2)))


deg <- as.data.frame(degree(g))
colnames(deg)[1]<- "Degree"

deg$rank = rank(-deg$Degree)
deg <- subset(deg, deg$Degree>0)
reg <- lm(formula = log(deg$Degree)~log(deg$rank))
coeff <- coefficients(reg)
R2 <- summary(reg)[8]$r.squared
R2 <- round(R2, 4)
par(pty="s")
plot(x=log(deg$rank), y=log((deg$Degree)), bg="black",pch=21, xlab = "Log (Rang)", ylab = "Log (Degree)")
abline(lm(formula = log(deg$Degree)~log(deg$rank)), col = "red",lwd=3)
legend("topright", legend = c(paste("Beta = ", round(-coeff[2],4)), paste("R2 = ", R2)))


ecc <- as.data.frame(eccentricity(g, mode ="all"))
colnames(ecc)[1]<- "Ecc"

ecc <- as.data.frame(table(ecc$Ecc))
ecc$Var1 <- as.numeric(ecc$Var1)
plot(ecc, xlab = "Plus Grande Distance des noeuds", ylab = "Fréquence", bg="black",pch=21)
legend("topright", legend = c(paste("Plus grande distance moyenne = ", round(average.path.length(g),4 ))))

transitivity(g, type= "localaverageundirected")
average.path.length(g)

inv_app <- unique(brevets_ipc[c("App_name","Inv_name")])

colnames(inv_app)[2] <- "Source"

cobrev_inv_app <- merge(e, inv_app, by="Source")

colnames(inv_app)[2] <- "Target"

cobrev_inv_app <- merge(cobrev_inv_app, inv_app, by="Target")

write.csv2(cobrev_inv_app, file="cobrev_inv_app.csv", row.names = FALSE)

inv_ipc <- unique(brevets_ipc[c("Inv_name","IPC")])

colnames(inv_ipc)[1] <- "Source"
inv_ipc$IPC <- substr(inv_ipc$IPC, 0, 7)
inv_ipc <- unique(inv_ipc[c("Source","IPC")])

e_inv_ipc <- merge(e, inv_ipc, by="Source")

colnames(e_inv_ipc)[6] <- "IPC_SOURCE"
colnames(inv_ipc)[1] <- "Target"

inv_ipc2 <- merge(e_inv_ipc, inv_ipc, by="Target")


invipc1 <- inv_ipc2[1:1000000,]
invipc2 <- inv_ipc2[1000001:2000000,]
invipc3 <- inv_ipc2[2000001:3000000,]
invipc4 <- inv_ipc2[3000001:3114782,]


invipc1$IDENTIQUE <- ""
for (x in 1:1000000){
  if (invipc1[x,5] == invipc1[x,6]){invipc1[x,7] <- "VRAI"}
  else {invipc1[x,7] <- "FAUX"}
}

invipc1 <- unique(invipc1[c("Target", "Source", "IDENTIQUE")])
summary(invipc1$IDENTIQUE)


write.csv2(app_ipc2, file="app_ipc2.csv", row.names = FALSE)



###### co-brev geo ######

brev_app_AML <- subset(brevets_ipc, brevets_ipc$AML.INSEE != "NA")
brev_app_noAML <- subset(brevets_ipc, is.na(brevets_ipc$AML.INSEE))

cobrevAML <- unique(brev_app_AML[c("Appln_id", "AML.INSEE")])
bip <- graph.data.frame(cobrevAML)
V(bip)$type <- V(bip)$name %in% cobrevAML[,2]
mat <- get.adjacency(bipartite.projection(bip)[[2]], attr="weight", sparse=TRUE)
mat[order(rownames(mat)), order(colnames(mat))]
mat <- as.matrix(mat)
g <- graph_from_adjacency_matrix(mat, mode="undirected", weighted = TRUE, diag = TRUE)
e <- cbind( get.edgelist(g) , round( E(g)$weight, 3 ))
e <- as.data.frame(e)
colnames(e)[1]<-"Source"
colnames(e)[2]<-"Target"
colnames(e)[3]<-"Weight"
e$Weight <- as.numeric(as.character(sub("," , ".", e$Weight)))
e$Type <- "Undirected"


write.csv(e, file = "cobrevAML.csv")




cobrevNoAML <- unique(brev_app_noAML[c("Appln_id", "HORS.AML.NUTS")])
nbcobrevNoAML <- as.data.frame(table(cobrevNoAML$HORS.AML.NUTS))
nbcobrevNoAML$Source <- "AML"
colnames(nbcobrevNoAML)[1] <- "Target"
colnames(nbcobrevNoAML)[2] <- "Weight"
nbcobrevNoAML <- nbcobrevNoAML[,c(3,1,2)]
nbcobrevNoAML <- nbcobrevNoAML[-1,]

write.csv(nbcobrevNoAML, file = "cobrevNoAML.csv")


###### Applicant tech map ######

brevets_ipc <- read.table(file = "brevets_ipc_applicants.csv", sep = ";", header = TRUE, encoding = "UTF-8")
brevets_ipc <- unique(brevets_ipc[c("App_name", "IPC")])
brevets_ipc$IPC <- substr(brevets_ipc$IPC, 0, 7)
brevets_ipc <- brevets_ipc[,c(2,1)]
bip <- graph.data.frame(brevets_ipc)
V(bip)$type <- V(bip)$name %in% brevets_ipc[,2]
vapp <- get.adjacency(bipartite.projection(bip)[[2]], attr="weight", sparse=TRUE)
vapp[order(rownames(vapp)), order(colnames(vapp))]
g <- graph_from_adjacency_matrix(vapp, mode="undirected", weighted = TRUE, diag = TRUE)
e_app <- cbind( get.edgelist(g) , round( E(g)$weight, 3 ))
e_app <- as.data.frame(e_app)
colnames(e_app)[1]<-"Source"
colnames(e_app)[2]<-"Target"
colnames(e_app)[3]<-"Weight"
e_app$Weight <- as.numeric(as.character(sub("," , ".", e_app$Weight)))



write.csv(e_app, file="apptechmap.csv")

#### concentration du brevetage  par période####

app_P1 <- unique(brevets_ipc_P1[c("Appln_id","App_name")])
app_P1 <- as.data.frame(table(brevets_ipc_P1$App_name))
app_P1 <- subset(app_P1, app_P1$Freq >0)


app_P2 <- unique(brevets_ipc_P2[c("Appln_id","App_name")])
app_P2 <- as.data.frame(table(brevets_ipc_P2$App_name))
app_P2 <- subset(app_P2, app_P2$Freq >0)

app_P3 <- unique(brevets_ipc_P3[c("Appln_id","App_name")])
app_P3 <- as.data.frame(table(brevets_ipc_P3$App_name))
app_P3 <- subset(app_P3, app_P3$Freq >0)


app_P4 <- unique(brevets_ipc_P4[c("Appln_id","App_name")])
app_P4 <- as.data.frame(table(brevets_ipc_P4$App_name))
app_P4 <- subset(app_P4, app_P4$Freq >0)

app_P1$rank = rank(-app_P1$Freq)
reg <- lm(formula = log(app_P1$Freq)~log(app_P1$rank))
coeff <- coefficients(reg)
R2 <- summary(reg)[8]$r.squared
R2 <- round(R2, 4)
par(pty="s")
plot(x=log(app_P1$rank), y=log((app_P1$Freq)), bg="black",pch=21, xlab = "Log (Rang)", ylab = "Log (Nombre de brevets par Applicant)")
abline(lm(formula = log(app_P1$Freq)~log(app_P1$rank)), col = "red",lwd=3)
legend("topright", legend = c(paste("Beta = ", round(-coeff[2],4)), paste("R2 = ", R2)))


app_P2$rank = rank(-app_P2$Freq)
reg <- lm(formula = log(app_P2$Freq)~log(app_P2$rank))
coeff <- coefficients(reg)
R2 <- summary(reg)[8]$r.squared
R2 <- round(R2, 4)
par(pty="s")
plot(x=log(app_P2$rank), y=log((app_P2$Freq)), bg="black",pch=21, xlab = "Log (Rang)", ylab = "Log (Nombre de brevets par Applicant)")
abline(lm(formula = log(app_P2$Freq)~log(app_P2$rank)), col = "red",lwd=3)
legend("topright", legend = c(paste("Beta = ", round(-coeff[2],4)), paste("R2 = ", R2)))


app_P3$rank = rank(-app_P3$Freq)
reg <- lm(formula = log(app_P3$Freq)~log(app_P3$rank))
coeff <- coefficients(reg)
R2 <- summary(reg)[8]$r.squared
R2 <- round(R2, 4)
par(pty="s")
plot(x=log(app_P3$rank), y=log((app_P3$Freq)), bg="black",pch=21, xlab = "Log (Rang)", ylab = "Log (Nombre de brevets par Applicant)")
abline(lm(formula = log(app_P3$Freq)~log(app_P3$rank)), col = "red",lwd=3)
legend("topright", legend = c(paste("Beta = ", round(-coeff[2],4)), paste("R2 = ", R2)))


app_P4$rank = rank(-app_P4$Freq)
reg <- lm(formula = log(app_P4$Freq)~log(app_P4$rank))
coeff <- coefficients(reg)
R2 <- summary(reg)[8]$r.squared
R2 <- round(R2, 4)
par(pty="s")
plot(x=log(app_P4$rank), y=log((app_P4$Freq)), bg="black",pch=21, xlab = "Log (Rang)", ylab = "Log (Nombre de brevets par Applicant)")
abline(lm(formula = log(app_P4$Freq)~log(app_P4$rank)), col = "red",lwd=3)
legend("topright", legend = c(paste("Beta = ", round(-coeff[2],4)), paste("R2 = ", R2)))







##### import citations prendre AML vers monde, monde vers AML ####


citations <- read.csv(file = "201703_EP_Citations.txt", sep="|", header = TRUE)

colnames(citations)[9]<- "Appln_id"


citAML <- merge(brevets_ipc, citations, by="Appln_id")


reg <- read.csv(file="201602_EPO_Inv_reg.txt", sep="|", header = TRUE)

colnames(reg)[2] <- "Citing_appln_id"


citAMLreg <- merge(citAML, reg, by="Citing_appln_id")

write.csv(citAMLreg, file ="citAMLreg.csv")

citAMLreg <- read.csv("citAMLreg.csv")

uniqueAMLciting <- unique(citAMLreg[c("Citing_appln_id","Reg_code")])
REG_CITING <- as.data.frame(table(uniqueAMLciting$Reg_code))
colnames(REG_CITING)[1] <- "Target"
colnames(REG_CITING)[2] <- "Weight"
REG_CITING$Source <- "AML"
REG_CITING <- REG_CITING[,c(3,1,2)]
REG_CITING <- subset(REG_CITING, REG_CITING$Weight>0)
uniqueIPCciting <- unique(citAMLreg[c("Citing_appln_id","Reg_code", "IPC")])
uniqueIPCciting$IPC <- substr(uniqueIPCciting$IPC, 0, 7)
uniqueIPCciting2 <- unique(uniqueIPCciting[c("Citing_appln_id","IPC")])
IPC_CITING <- as.data.frame(table(uniqueIPCciting2$IPC))

unique2 <- unique(citAMLreg[c("Citing_appln_id", "Reg_code", "IPC", "App_name")])

uniquecitapp <- unique(unique2[c("Citing_appln_id", "App_name")])
APP_CITING <-  as.data.frame(table(uniquecitapp$App_name))


CIT_REG <- unique(citAMLreg[c("App_nbr","Reg_code","Annee")])

CIT_REG_P1 <- subset(CIT_REG, CIT_REG$Annee > 1979 & CIT_REG$Annee < 1988)
CIT_REG_P1 <- as.data.frame(table(CIT_REG_P1$Reg_code))
CIT_REG_P1$Source <- "AML"
colnames(CIT_REG_P1)[1] <- "Target"
colnames(CIT_REG_P1)[2] <- "Weight"
CIT_REG_P1 <- CIT_REG_P1[,c(3,1,2)]
CIT_REG_P1$Type <- "Undirected"
CIT_REG_P1 <- subset(CIT_REG_P1, CIT_REG_P1$Weight >0)

write.csv(CIT_REG_P1, file = "CIT_REG_P1.csv")


CIT_REG_P2 <- subset(CIT_REG, CIT_REG$Annee > 1987 & CIT_REG$Annee < 1996)
CIT_REG_P3 <- subset(CIT_REG, CIT_REG$Annee > 1995 & CIT_REG$Annee < 2004)
CIT_REG_P4 <- subset(CIT_REG, CIT_REG$Annee > 2003 & CIT_REG$Annee < 2013)


## corr sna

vapp <- as.sociomatrix.sna(vapp)
vapp <- as.network(vapp, directed = FALSE)
vinv <- as.sociomatrix.sna(vinv)
vinv <- as.network(vinv, directed = FALSE)
vgeo <- as.sociomatrix.sna(vgeo)
vgeo <- as.network(vgeo, directed = FALSE)
vpat <- as.sociomatrix.sna(vpat)
vpat <- as.network(vpat, directed = FALSE)

y=c(vapp, vinv, vgeo)

nl<-netlm(y,vpat,reps=100)

#résultats
summary(nl)




#### mesures totales pour comparaison

rm(list = ls(all = TRUE))

ipctot <- read.table(file = "201602_EPO_IPC.txt", sep = "|", header = TRUE)
ipctot$IPC <- substr(ipctot$IPC, 0, 7)
ipctot <- unique(ipctot[c("Appln_id","IPC")])
ipctot <- as.data.frame(table(ipctot$IPC))
ipctot <- ipctot[-1,]
totalipc <- sum(ipctot$Freq)
ipctot$perc <- ipctot$Freq/totalipc
ipctot$rank = rank(-ipctot$Freq)
reg <- lm(formula = log(ipctot$Freq)~log(ipctot$rank))
coeff <- coefficients(reg)
R2 <- summary(reg)[8]$r.squared
R2 <- round(R2, 4)
par(pty="s")
plot(x=log(ipctot$rank), y=log((ipctot$Freq)), bg="black",pch=21, xlab = "Log (Rang)", ylab = "Log (Nombre de brevets par IPC (groupes) éch. total)")
abline(lm(formula = log(ipctot$Freq)~log(ipctot$rank)), col = "red",lwd=3)
legend("topright", legend = c(paste("Beta = ", round(-coeff[2],4)), paste("R2 = ", R2)))

colnames(ipctot)[1] <- "IPC_Group"
colnames(ipctot)[2] <- "Freq_total"
colnames(ipctot)[3] <- "perc_total"
colnames(ipctot)[4] <- "rank_total"

brevets_ipc <- read.table(file = "brevets_ipc_applicants.csv", sep = ";", header = TRUE, encoding = "UTF-8")
brevets_par_IPCgroup <- as.data.frame(unique(brevets_ipc[c("Appln_id", "IPC")]))

brevets_par_IPCgroup$IPC <- substr(brevets_par_IPCgroup$IPC, 0, 7)
brevets_par_IPCgroup <- as.data.frame(table(brevets_par_IPCgroup$IPC))
summary(brevets_par_IPCgroup$Freq)
brevets_par_IPCgroup$rank = rank(-brevets_par_IPCgroup$Freq)
reg <- lm(formula = log(brevets_par_IPCgroup$Freq)~log(brevets_par_IPCgroup$rank))
coeff <- coefficients(reg)
R2 <- summary(reg)[8]$r.squared
R2 <- round(R2, 4)
par(pty="s")
plot(x=log(brevets_par_IPCgroup$rank), y=log((brevets_par_IPCgroup$Freq)), bg="black",pch=21, xlab = "Log (Rang)", ylab = "Log (Nombre de brevets par IPC (groupes))")
abline(lm(formula = log(brevets_par_IPCgroup$Freq)~log(brevets_par_IPCgroup$rank)), col = "red",lwd=3)
legend("topright", legend = c(paste("Beta = ", round(-coeff[2],4)), paste("R2 = ", R2)))
totalipc <- sum(brevets_par_IPCgroup$Freq)
brevets_par_IPCgroup$perc <- brevets_par_IPCgroup$Freq/totalipc

colnames(brevets_par_IPCgroup)[1] <-  "IPC_Group"
colnames(brevets_par_IPCgroup)[2] <-  "Freq_AML"
colnames(brevets_par_IPCgroup)[3] <-  "rank_AML"
colnames(brevets_par_IPCgroup)[4] <-  "perc_AML"

LQ_IPC <- merge(ipctot, brevets_par_IPCgroup, by = "IPC_Group")
LQ_IPC$LQ <- LQ_IPC$perc_AML/LQ_IPC$perc_total
LQ_IPC <- subset(LQ_IPC, LQ_IPC$Freq_AML > 10)


###### positions gatekeepers ######
cobrev <- unique(brevets_ipc[c("Appln_id","App_name")])
bip <- graph.data.frame(cobrev)
V(bip)$type <- V(bip)$name %in% cobrev[,2]
mat <- get.adjacency(bipartite.projection(bip)[[2]], attr="weight", sparse=TRUE)
mat[order(rownames(mat)), order(colnames(mat))]
mat <- as.matrix(mat)
g <- graph_from_adjacency_matrix(mat, mode="undirected", weighted = TRUE, diag = TRUE)
e <- cbind( get.edgelist(g) , round( E(g)$weight, 3 ))
e <- as.data.frame(e)
colnames(e)[1]<-"Source"
colnames(e)[2]<-"Target"
colnames(e)[3]<-"Weight"
e$Weight <- as.numeric(as.character(sub("," , ".", e$Weight)))
e$Type <- "Undirected"


write.csv(e, "cobrev.csv")

gatekeepers <- read.csv(file= "gatekeepers.csv", sep = ";")
gatekeepers2 <- subset(gatekeepers, gatekeepers$AML>0)
gatekeepers3 <- subset(gatekeepers, gatekeepers$NO_AML>0)
mintern <- mean(gatekeepers2$AML)
mextern <- mean(gatekeepers3$NO_AML)
par(pty="s")
plot(x=gatekeepers$AML, y=gatekeepers$NO_AML, log="xy", bg="black",pch=21, xlab = "Relations internes", ylab = "Relations externes", asp=1)
text(gatekeepers$AML, gatekeepers$NO_AML, labels=gatekeepers$App_name, cex= 0.7, pos=3)
abline(v=mintern)
abline(h=mextern)




#### mesure comparaison prox tech   ####

ipctot <- read.csv("201602_EPO_IPC.txt",header = TRUE, sep = "|")
ipctot <- as.data.frame(ipctot[c("Appln_id", "IPC")])
ipctot$IPC <- substr(ipctot$IPC, 0, 7)
ipctot <- as.data.frame(unique(ipctot[c("Appln_id", "IPC")]))
bip <- graph.data.frame(ipctot)
V(bip)$type <- V(bip)$name %in% ipctot[,2]

vpat <- get.adjacency(bipartite.projection(bip)[[2]], attr="weight", sparse=TRUE)
vpat[order(rownames(vpat)), order(colnames(vpat))]
g <- graph_from_adjacency_matrix(vpat, mode="undirected", weighted = TRUE, diag = TRUE)
e_pat <- cbind( get.edgelist(g) , round( E(g)$weight, 3 ))
e_pat <- as.data.frame(e_pat)
colnames(e_pat)[1]<-"Source"
colnames(e_pat)[2]<-"Target"
colnames(e_pat)[3]<-"Weight"
e_pat$Weight <- as.numeric(as.character(sub("," , ".", e_pat$Weight)))
e_patP1 <- e_pat
e_patP1$Type <- "Undirected"




##### positions citations #####

citationsEPO <- read.csv(file = "201703_EP_Citations.txt", sep="|", header = TRUE)

reginv<- read.csv(file = "201602_EPO_Inv_Reg.txt", sep="|", header = TRUE)



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

nutsaero <- read.csv("NUTS_CENTROIDS.csv", sep = ";")


colnames(nutsaero)[1] <- "Reg_code"

nutsaero <- nutsaero[,1:2]

regeuroappvilles <- merge(regeuroinv, nutsaero, by="Reg_code")


regeuroappvilles <- regeuroappvilles[,- c(9:10)]

write.csv(regeuroappvilles,"regeuroappvilles.csv")

colnames (regeuroappvilles)[3] <- "Cited_Appln_id"

citedreg <- merge(citationsEPO, regeuroappvilles, by="Cited_Appln_id", suffixes = TRUE)

colnames (regeuroappvilles)[3] <- "Citing_appln_id"

citreg <- merge(citedreg, regeuroappvilles, by="Citing_appln_id", suffixes = TRUE)

colnames (citreg)[23] <- "Target"
colnames (citreg)[31] <- "Source"

citreg <- citreg[,c("Citing_appln_id","Cited_Appln_id","Source","Target")]

write.csv(citreg,"citreg.csv")

citreg <- read.csv("citreg.csv")

links <- citreg[,c(3:4)] 

links <- links[!(links$Target=="" | links$Source==""), ]

links <- as.matrix(get.adjacency(graph.data.frame(links)))

g <- graph_from_adjacency_matrix(links, mode = "directed", weighted = TRUE, diag = TRUE)



e <- cbind( get.edgelist(g) , round( E(g)$weight, 3 ))
e <- as.data.frame(e)
colnames(e)[1]<-"Source"
colnames(e)[2]<-"Target"
colnames(e)[3]<-"Weight"
e$Weight <- as.numeric(as.character(sub("," , ".", e$Weight)))
e$Type <- "Directed"


write.csv(e, "citationseurope.csv", row.names = FALSE)



##### correlations #####

inv_soc <- unique(brevets_ipc[c("Appln_id","Inv_name")])
inv_ipc <- unique(brevets_ipc[c("IPC", "Inv_name")])
inv_ipc$IPC <- substr(inv_ipc$IPC, 0, 7)
inv_ipc <- unique(inv_ipc)
inv_geo <- unique(brevets_ipc[c("INSEE_Code", "Inv_name")])
inv_app <- unique(brevets_ipc[c("Person_id.y", "Inv_name")])


bip <- graph.data.frame(inv_soc)
V(bip)$type <- V(bip)$name %in% inv_soc[,2]
mat <- get.adjacency(bipartite.projection(bip)[[2]], attr="weight", sparse=TRUE)
mat[order(rownames(mat)), order(colnames(mat))]
mat <- as.matrix(mat)
g <- graph_from_adjacency_matrix(mat, mode="undirected", weighted = TRUE, diag = TRUE)
esoc <- cbind( get.edgelist(g) , round( E(g)$weight, 3 ))
esoc <- as.data.frame(esoc)
colnames(esoc)[1]<-"Source"
colnames(esoc)[2]<-"Target"
colnames(esoc)[3]<-"Weight"
esoc$Weight <- as.numeric(as.character(sub("," , ".", esoc$Weight)))
esoc$Type <- "Undirected"
gsoc <- g

bip <- graph.data.frame(inv_ipc)
V(bip)$type <- V(bip)$name %in% inv_ipc[,2]
mat <- get.adjacency(bipartite.projection(bip)[[2]], attr="weight", sparse=TRUE)
mat[order(rownames(mat)), order(colnames(mat))]
mat <- as.matrix(mat)
g <- graph_from_adjacency_matrix(mat, mode="undirected", weighted = TRUE, diag = TRUE)
eipc <- cbind( get.edgelist(g) , round( E(g)$weight, 3 ))
eipc <- as.data.frame(eipc)
colnames(eipc)[1]<-"Source"
colnames(eipc)[2]<-"Target"
colnames(eipc)[3]<-"Weight"
eipc$Weight <- as.numeric(as.character(sub("," , ".", eipc$Weight)))
eipc$Type <- "Undirected"
gipc <- g

bip <- graph.data.frame(inv_geo)
V(bip)$type <- V(bip)$name %in% inv_geo[,2]
mat <- get.adjacency(bipartite.projection(bip)[[2]], attr="weight", sparse=TRUE)
mat[order(rownames(mat)), order(colnames(mat))]
mat <- as.matrix(mat)
g <- graph_from_adjacency_matrix(mat, mode="undirected", weighted = TRUE, diag = TRUE)
egeo <- cbind( get.edgelist(g) , round( E(g)$weight, 3 ))
egeo <- as.data.frame(egeo)
colnames(egeo)[1]<-"Source"
colnames(egeo)[2]<-"Target"
colnames(egeo)[3]<-"Weight"
egeo$Weight <- as.numeric(as.character(sub("," , ".", egeo$Weight)))
egeo$Type <- "Undirected"
ggeo <- g

bip <- graph.data.frame(inv_app)
V(bip)$type <- V(bip)$name %in% inv_app[,2]
mat <- get.adjacency(bipartite.projection(bip)[[2]], attr="weight", sparse=TRUE)
mat[order(rownames(mat)), order(colnames(mat))]
mat <- as.matrix(mat)
g <- graph_from_adjacency_matrix(mat, mode="undirected", weighted = TRUE, diag = TRUE)
eapp <- cbind( get.edgelist(g) , round( E(g)$weight, 3 ))
eapp <- as.data.frame(eapp)
colnames(eapp)[1]<-"Source"
colnames(eapp)[2]<-"Target"
colnames(eapp)[3]<-"Weight"
eapp$Weight <- as.numeric(as.character(sub("," , ".", eapp$Weight)))
eapp$Type <- "Undirected"
gapp <- g



dgapp <- as.data.frame(degree(gapp))
dgsoc <- as.data.frame(degree(gsoc))
dggeo <- as.data.frame(degree(ggeo))
dgipc <- as.data.frame(degree(gipc))

cor.test(dgapp, dgipc)
cor(dgapp, dgsoc)
cor(dgapp, dggeo)
cor(dgipc, dggeo)
cor(dgipc, dgsoc)
cor(dggeo, dgsoc)



###### temporel ######

brevets_ipc <- read.table(file = "brevets_ipc_applicants.csv", sep = ";", header = TRUE, encoding = "UTF-8")

brevets_annees <- unique(brevets_ipc[c("Appln_id", "Annee")])


annees <- as.data.frame(table(brevets_annees$Annee))
colnames(annees)[1]<-"Annee"
colnames(annees)[2]<-"Brevets"
annees$Annee <- as.numeric(as.character(annees$Annee))
plot(annees, bg="black",pch=21)
lines(annees)
abline (v=2012, col = "red", lwd = 3)
abline (v=2004, col = "red", lwd = 3)
abline (v=1996, col = "red", lwd = 3)
abline (v=1988, col = "red", lwd = 3)
abline (v=1980, col = "red", lwd = 3)


# P1 = 1980-1987
# P2 = 1988-1995
# P3 = 1996-2003
# P4 = 2004-2012


brevets_ipc_P1 <- subset(brevets_ipc, brevets_ipc$Annee > 1979 & brevets_ipc$Annee < 1988)
brevets_ipc_P2 <- subset(brevets_ipc, brevets_ipc$Annee > 1987 & brevets_ipc$Annee < 1996)
brevets_ipc_P3 <- subset(brevets_ipc, brevets_ipc$Annee > 1995 & brevets_ipc$Annee < 2004)
brevets_ipc_P4 <- subset(brevets_ipc, brevets_ipc$Annee > 2003 & brevets_ipc$Annee < 2013)


inventeurs_unique_P1 <- as.data.frame(unique(brevets_ipc_P1$Inv_name))
inventeurs_unique_P2 <- as.data.frame(unique(brevets_ipc_P2$Inv_name))
inventeurs_unique_P3 <- as.data.frame(unique(brevets_ipc_P3$Inv_name))
inventeurs_unique_P4 <- as.data.frame(unique(brevets_ipc_P4$Inv_name))



### tech

# p1

brevets_par_IPCgroup <- as.data.frame(unique(brevets_ipc_P1[c("Appln_id", "IPC")]))
brevets_par_IPCgroup$IPC <- substr(brevets_par_IPCgroup$IPC, 0, 7)

bip <- graph.data.frame(brevets_par_IPCgroup)
V(bip)$type <- V(bip)$name %in% brevets_par_IPCgroup[,2]
vpat <- get.adjacency(bipartite.projection(bip)[[2]], attr="weight", sparse=TRUE)
vpat[order(rownames(vpat)), order(colnames(vpat))]
g <- graph_from_adjacency_matrix(vpat, mode="undirected", weighted = TRUE, diag = TRUE)
e <- cbind( get.edgelist(g) , round( E(g)$weight, 3 ))
e <- as.data.frame(e)
colnames(e)[1]<-"Source"
colnames(e)[2]<-"Target"
colnames(e)[3]<-"Weight"
e$Weight <- as.numeric(as.character(sub("," , ".", e$Weight)))
e$Type <- "Undirected"
write.csv(e, file="proxtechP1.csv", row.names = FALSE)
summary(g) # nb noeuds et liens
graph.density(g) # densité
summary(degree(g)) # degré moyen
summary(strength(g)) # degré pondéré moyen
transitivity(g, type= "localaverageundirected") # coef clustering
cluster_louvain(g) # communautés et modularité
c <- components(g) 
summary (c$csize) # plus grande composante
average.path.length(g) # plus courts chemins moyens
fit_power_law(g)

brevets_par_IPCgroup <- as.data.frame(table(brevets_par_IPCgroup$IPC))
summary(brevets_par_IPCgroup$Freq)
brevets_par_IPCgroup$rank = rank(-brevets_par_IPCgroup$Freq)
brevets_par_IPCgroup <- as.data.frame(unique(brevets_par_IPCgroup[c("rank", "Freq")]))

reg <- lm(formula = log(brevets_par_IPCgroup$Freq)~log(brevets_par_IPCgroup$rank))
coeff <- coefficients(reg)
R2 <- summary(reg)[8]$r.squared
R2 <- round(R2, 4)
par(pty="s")
plot(x=log(brevets_par_IPCgroup$rank), y=log((brevets_par_IPCgroup$Freq)), bg="black",pch=21,xlab = "Log (Rang)", ylab = "Log (Nombre de brevets par IPC (groupes))")
abline(lm(formula = log(brevets_par_IPCgroup$Freq)~log(brevets_par_IPCgroup$rank)), col = "red",lwd=3)
legend("topright", legend = c(paste("Beta = ", round(-coeff[2],4)), paste("R2 = ", R2)))
totalipc <- sum(brevets_par_IPCgroup$Freq)
brevets_par_IPCgroup$perc <- brevets_par_IPCgroup$Freq/totalipc

#P2

brevets_par_IPCgroup <- as.data.frame(unique(brevets_ipc_P2[c("Appln_id", "IPC")]))
brevets_par_IPCgroup$IPC <- substr(brevets_par_IPCgroup$IPC, 0, 7)

bip <- graph.data.frame(brevets_par_IPCgroup)
V(bip)$type <- V(bip)$name %in% brevets_par_IPCgroup[,2]
vpat <- get.adjacency(bipartite.projection(bip)[[2]], attr="weight", sparse=TRUE)
vpat[order(rownames(vpat)), order(colnames(vpat))]
g <- graph_from_adjacency_matrix(vpat, mode="undirected", weighted = TRUE, diag = TRUE)
e <- cbind( get.edgelist(g) , round( E(g)$weight, 3 ))
e <- as.data.frame(e)
colnames(e)[1]<-"Source"
colnames(e)[2]<-"Target"
colnames(e)[3]<-"Weight"
e$Weight <- as.numeric(as.character(sub("," , ".", e$Weight)))
e$Type <- "Undirected"
write.csv(e, file="proxtechP2.csv", row.names = FALSE)

summary(g) # nb noeuds et liens
graph.density(g) # densité
summary(degree(g)) # degré moyen
summary(strength(g)) # degré pondéré moyen
transitivity(g, type= "localaverageundirected") # coef clustering
cluster_louvain(g) # communautés et modularité
c <- components(g) 
summary (c$csize) # plus grande composante
average.path.length(g) # plus courts chemins moyens
fit_power_law(g)

brevets_par_IPCgroup <- as.data.frame(table(brevets_par_IPCgroup$IPC))
summary(brevets_par_IPCgroup$Freq)
brevets_par_IPCgroup$rank = rank(-brevets_par_IPCgroup$Freq)
brevets_par_IPCgroup <- as.data.frame(unique(brevets_par_IPCgroup[c("rank", "Freq")]))

reg <- lm(formula = log(brevets_par_IPCgroup$Freq)~log(brevets_par_IPCgroup$rank))
coeff <- coefficients(reg)
R2 <- summary(reg)[8]$r.squared
R2 <- round(R2, 4)
par(pty="s")
plot(x=log(brevets_par_IPCgroup$rank), y=log((brevets_par_IPCgroup$Freq)), bg="black",pch=21,xlab = "Log (Rang)", ylab = "Log (Nombre de brevets par IPC (groupes))")
abline(lm(formula = log(brevets_par_IPCgroup$Freq)~log(brevets_par_IPCgroup$rank)), col = "red",lwd=3)
legend("topright", legend = c(paste("Beta = ", round(-coeff[2],4)), paste("R2 = ", R2)))
totalipc <- sum(brevets_par_IPCgroup$Freq)
brevets_par_IPCgroup$perc <- brevets_par_IPCgroup$Freq/totalipc

# P3

brevets_par_IPCgroup <- as.data.frame(unique(brevets_ipc_P3[c("Appln_id", "IPC")]))
brevets_par_IPCgroup$IPC <- substr(brevets_par_IPCgroup$IPC, 0, 7)

bip <- graph.data.frame(brevets_par_IPCgroup)
V(bip)$type <- V(bip)$name %in% brevets_par_IPCgroup[,2]
vpat <- get.adjacency(bipartite.projection(bip)[[2]], attr="weight", sparse=TRUE)
vpat[order(rownames(vpat)), order(colnames(vpat))]
g <- graph_from_adjacency_matrix(vpat, mode="undirected", weighted = TRUE, diag = TRUE)
e <- cbind( get.edgelist(g) , round( E(g)$weight, 3 ))
e <- as.data.frame(e)
colnames(e)[1]<-"Source"
colnames(e)[2]<-"Target"
colnames(e)[3]<-"Weight"
e$Weight <- as.numeric(as.character(sub("," , ".", e$Weight)))
e$Type <- "Undirected"
write.csv(e, file="proxtechP3.csv", row.names = FALSE)

summary(g) # nb noeuds et liens
graph.density(g) # densité
summary(degree(g)) # degré moyen
summary(strength(g)) # degré pondéré moyen
transitivity(g, type= "localaverageundirected") # coef clustering
cluster_louvain(g) # communautés et modularité
c <- components(g) 
summary (c$csize) # plus grande composante
average.path.length(g) # plus courts chemins moyens
fit_power_law(g)

brevets_par_IPCgroup <- as.data.frame(table(brevets_par_IPCgroup$IPC))
summary(brevets_par_IPCgroup$Freq)
brevets_par_IPCgroup$rank = rank(-brevets_par_IPCgroup$Freq)
brevets_par_IPCgroup <- as.data.frame(unique(brevets_par_IPCgroup[c("rank", "Freq")]))

reg <- lm(formula = log(brevets_par_IPCgroup$Freq)~log(brevets_par_IPCgroup$rank))
coeff <- coefficients(reg)
R2 <- summary(reg)[8]$r.squared
R2 <- round(R2, 4)
par(pty="s")
plot(x=log(brevets_par_IPCgroup$rank), y=log((brevets_par_IPCgroup$Freq)), bg="black",pch=21,xlab = "Log (Rang)", ylab = "Log (Nombre de brevets par IPC (groupes))")
abline(lm(formula = log(brevets_par_IPCgroup$Freq)~log(brevets_par_IPCgroup$rank)), col = "red",lwd=3)
legend("topright", legend = c(paste("Beta = ", round(-coeff[2],4)), paste("R2 = ", R2)))
totalipc <- sum(brevets_par_IPCgroup$Freq)
brevets_par_IPCgroup$perc <- brevets_par_IPCgroup$Freq/totalipc

#P4

brevets_par_IPCgroup <- as.data.frame(unique(brevets_ipc_P4[c("Appln_id", "IPC")]))
brevets_par_IPCgroup$IPC <- substr(brevets_par_IPCgroup$IPC, 0, 7)

bip <- graph.data.frame(brevets_par_IPCgroup)
V(bip)$type <- V(bip)$name %in% brevets_par_IPCgroup[,2]
vpat <- get.adjacency(bipartite.projection(bip)[[2]], attr="weight", sparse=TRUE)
vpat[order(rownames(vpat)), order(colnames(vpat))]
g <- graph_from_adjacency_matrix(vpat, mode="undirected", weighted = TRUE, diag = TRUE)
e <- cbind( get.edgelist(g) , round( E(g)$weight, 3 ))
e <- as.data.frame(e)
colnames(e)[1]<-"Source"
colnames(e)[2]<-"Target"
colnames(e)[3]<-"Weight"
e$Weight <- as.numeric(as.character(sub("," , ".", e$Weight)))
e$Type <- "Undirected"
write.csv(e, file="proxtechP4.csv", row.names = FALSE)

summary(g) # nb noeuds et liens
graph.density(g) # densité
summary(degree(g)) # degré moyen
summary(strength(g)) # degré pondéré moyen
transitivity(g, type= "localaverageundirected") # coef clustering
cluster_louvain(g) # communautés et modularité
c <- components(g) 
summary (c$csize) # plus grande composante
average.path.length(g) # plus courts chemins moyens
fit_power_law(g)

brevets_par_IPCgroup <- as.data.frame(table(brevets_par_IPCgroup$IPC))
summary(brevets_par_IPCgroup$Freq)
brevets_par_IPCgroup$rank = rank(-brevets_par_IPCgroup$Freq)
brevets_par_IPCgroup <- as.data.frame(unique(brevets_par_IPCgroup[c("rank", "Freq")]))

reg <- lm(formula = log(brevets_par_IPCgroup$Freq)~log(brevets_par_IPCgroup$rank))
coeff <- coefficients(reg)
R2 <- summary(reg)[8]$r.squared
R2 <- round(R2, 4)
par(pty="s")
plot(x=log(brevets_par_IPCgroup$rank), y=log((brevets_par_IPCgroup$Freq)), bg="black",pch=21,xlab = "Log (Rang)", ylab = "Log (Nombre de brevets par IPC (groupes))")
abline(lm(formula = log(brevets_par_IPCgroup$Freq)~log(brevets_par_IPCgroup$rank)), col = "red",lwd=3)
legend("topright", legend = c(paste("Beta = ", round(-coeff[2],4)), paste("R2 = ", R2)))
totalipc <- sum(brevets_par_IPCgroup$Freq)
brevets_par_IPCgroup$perc <- brevets_par_IPCgroup$Freq/totalipc


## soc
#P1
cobrevinv <- unique(brevets_ipc_P1[c("Appln_id","Inv_name")])
bip <- graph.data.frame(cobrevinv)
V(bip)$type <- V(bip)$name %in% cobrevinv[,2]
mat <- get.adjacency(bipartite.projection(bip)[[2]], attr="weight", sparse=TRUE)
mat[order(rownames(mat)), order(colnames(mat))]
mat <- as.matrix(mat)
g <- graph_from_adjacency_matrix(mat, mode="undirected", weighted = TRUE, diag = TRUE)
e <- cbind( get.edgelist(g) , round( E(g)$weight, 3 ))
e <- as.data.frame(e)
colnames(e)[1]<-"Source"
colnames(e)[2]<-"Target"
colnames(e)[3]<-"Weight"
e$Weight <- as.numeric(as.character(sub("," , ".", e$Weight)))
e$Type <- "Undirected"
sum(e$Weight)
write.csv(e, file="socproxP1.csv", row.names = FALSE)
summary(g) # nb noeuds et liens
graph.density(g) # densité
summary(degree(g)) # degré moyen
summary(strength(g)) # degré pondéré moyen
transitivity(g, type= "localaverageundirected") # coef clustering
cc<- cluster_louvain(g) # communautés et modularité
c <- components(g) 
summary (c$csize) # plus grande composante
average.path.length(g) # plus courts chemins moyens
summary(as.data.frame(sizes(cc)))


e$rank = rank(-e$Weight)
e <- as.data.frame(unique(e[c("rank", "Weight")]))

reg <- lm(formula = log(e$Weight)~log(e$rank))
coeff <- coefficients(reg)
R2 <- summary(reg)[8]$r.squared
R2 <- round(R2, 4)
par(pty="s")
plot(x=log(e$rank), y=log((e$Weight)), bg="black",pch=21, xlab = "Log (Rang)", ylab = "Log (Valeur des liens)")
abline(lm(formula = log(e$Weight)~log(e$rank)), col = "red",lwd=3)
legend("topright", legend = c(paste("Beta = ", round(-coeff[2],4)), paste("R2 = ", R2)))


deg <- as.data.frame(degree(g))
colnames(deg)[1]<- "Degree"

deg$rank = rank(-deg$Degree)
deg <- as.data.frame(unique(deg[c("rank", "Degree")]))

deg <- subset(deg, deg$Degree>0)
reg <- lm(formula = log(deg$Degree)~log(deg$rank))
coeff <- coefficients(reg)
R2 <- summary(reg)[8]$r.squared
R2 <- round(R2, 4)
par(pty="s")
plot(x=log(deg$rank), y=log((deg$Degree)), bg="black",pch=21, xlab = "Log (Rang)", ylab = "Log (Degré)")
abline(lm(formula = log(deg$Degree)~log(deg$rank)), col = "red",lwd=3)
legend("topright", legend = c(paste("Beta = ", round(-coeff[2],4)), paste("R2 = ", R2)))


fit_power_law(g)

# P2
cobrevinv <- unique(brevets_ipc_P2[c("Appln_id","Inv_name")])
bip <- graph.data.frame(cobrevinv)
V(bip)$type <- V(bip)$name %in% cobrevinv[,2]
mat <- get.adjacency(bipartite.projection(bip)[[2]], attr="weight", sparse=TRUE)
mat[order(rownames(mat)), order(colnames(mat))]
mat <- as.matrix(mat)
g <- graph_from_adjacency_matrix(mat, mode="undirected", weighted = TRUE, diag = TRUE)
e <- cbind( get.edgelist(g) , round( E(g)$weight, 3 ))
e <- as.data.frame(e)
colnames(e)[1]<-"Source"
colnames(e)[2]<-"Target"
colnames(e)[3]<-"Weight"
e$Weight <- as.numeric(as.character(sub("," , ".", e$Weight)))
e$Type <- "Undirected"
sum(e$Weight)
write.csv(e, file="socproxP2.csv", row.names = FALSE)

summary(g) # nb noeuds et liens
graph.density(g) # densité
summary(degree(g)) # degré moyen
summary(strength(g)) # degré pondéré moyen
transitivity(g, type= "localaverageundirected") # coef clustering
cc<- cluster_louvain(g) # communautés et modularité
c <- components(g) 
summary (c$csize) # plus grande composante
average.path.length(g) # plus courts chemins moyens
summary(as.data.frame(sizes(cc)))


e$rank = rank(-e$Weight)
e <- as.data.frame(unique(e[c("rank", "Weight")]))

reg <- lm(formula = log(e$Weight)~log(e$rank))
coeff <- coefficients(reg)
R2 <- summary(reg)[8]$r.squared
R2 <- round(R2, 4)
par(pty="s")
plot(x=log(e$rank), y=log((e$Weight)), bg="black",pch=21, xlab = "Log (Rang)", ylab = "Log (Valeur des liens)")
abline(lm(formula = log(e$Weight)~log(e$rank)), col = "red",lwd=3)
legend("topright", legend = c(paste("Beta = ", round(-coeff[2],4)), paste("R2 = ", R2)))


deg <- as.data.frame(degree(g))
colnames(deg)[1]<- "Degree"

deg$rank = rank(-deg$Degree)
deg <- as.data.frame(unique(deg[c("rank", "Degree")]))

deg <- subset(deg, deg$Degree>0)
reg <- lm(formula = log(deg$Degree)~log(deg$rank))
coeff <- coefficients(reg)
R2 <- summary(reg)[8]$r.squared
R2 <- round(R2, 4)
par(pty="s")
plot(x=log(deg$rank), y=log((deg$Degree)), bg="black",pch=21, xlab = "Log (Rang)", ylab = "Log (Degré)")
abline(lm(formula = log(deg$Degree)~log(deg$rank)), col = "red",lwd=3)
legend("topright", legend = c(paste("Beta = ", round(-coeff[2],4)), paste("R2 = ", R2)))


fit_power_law(g)

#P3

cobrevinv <- unique(brevets_ipc_P3[c("Appln_id","Inv_name")])
bip <- graph.data.frame(cobrevinv)
V(bip)$type <- V(bip)$name %in% cobrevinv[,2]
mat <- get.adjacency(bipartite.projection(bip)[[2]], attr="weight", sparse=TRUE)
mat[order(rownames(mat)), order(colnames(mat))]
mat <- as.matrix(mat)
g <- graph_from_adjacency_matrix(mat, mode="undirected", weighted = TRUE, diag = TRUE)
e <- cbind( get.edgelist(g) , round( E(g)$weight, 3 ))
e <- as.data.frame(e)
colnames(e)[1]<-"Source"
colnames(e)[2]<-"Target"
colnames(e)[3]<-"Weight"
e$Weight <- as.numeric(as.character(sub("," , ".", e$Weight)))
e$Type <- "Undirected"
sum(e$Weight)
write.csv(e, file="socproxP3.csv", row.names = FALSE)

summary(g) # nb noeuds et liens
graph.density(g) # densité
summary(degree(g)) # degré moyen
summary(strength(g)) # degré pondéré moyen
transitivity(g, type= "localaverageundirected") # coef clustering
cc<- cluster_louvain(g) # communautés et modularité
c <- components(g) 
summary (c$csize) # plus grande composante
average.path.length(g) # plus courts chemins moyens
summary(as.data.frame(sizes(cc)))


e$rank = rank(-e$Weight)
e <- as.data.frame(unique(e[c("rank", "Weight")]))

reg <- lm(formula = log(e$Weight)~log(e$rank))
coeff <- coefficients(reg)
R2 <- summary(reg)[8]$r.squared
R2 <- round(R2, 4)
par(pty="s")
plot(x=log(e$rank), y=log((e$Weight)), bg="black",pch=21, xlab = "Log (Rang)", ylab = "Log (Valeur des liens)")
abline(lm(formula = log(e$Weight)~log(e$rank)), col = "red",lwd=3)
legend("topright", legend = c(paste("Beta = ", round(-coeff[2],4)), paste("R2 = ", R2)))


deg <- as.data.frame(degree(g))
colnames(deg)[1]<- "Degree"

deg$rank = rank(-deg$Degree)
deg <- as.data.frame(unique(deg[c("rank", "Degree")]))

deg <- subset(deg, deg$Degree>0)
reg <- lm(formula = log(deg$Degree)~log(deg$rank))
coeff <- coefficients(reg)
R2 <- summary(reg)[8]$r.squared
R2 <- round(R2, 4)
par(pty="s")
plot(x=log(deg$rank), y=log((deg$Degree)), bg="black",pch=21, xlab = "Log (Rang)", ylab = "Log (Degré)")
abline(lm(formula = log(deg$Degree)~log(deg$rank)), col = "red",lwd=3)
legend("topright", legend = c(paste("Beta = ", round(-coeff[2],4)), paste("R2 = ", R2)))


fit_power_law(g)

#P4

cobrevinv <- unique(brevets_ipc_P4[c("Appln_id","Inv_name")])
bip <- graph.data.frame(cobrevinv)
V(bip)$type <- V(bip)$name %in% cobrevinv[,2]
mat <- get.adjacency(bipartite.projection(bip)[[2]], attr="weight", sparse=TRUE)
mat[order(rownames(mat)), order(colnames(mat))]
mat <- as.matrix(mat)
g <- graph_from_adjacency_matrix(mat, mode="undirected", weighted = TRUE, diag = TRUE)
e <- cbind( get.edgelist(g) , round( E(g)$weight, 3 ))
e <- as.data.frame(e)
colnames(e)[1]<-"Source"
colnames(e)[2]<-"Target"
colnames(e)[3]<-"Weight"
e$Weight <- as.numeric(as.character(sub("," , ".", e$Weight)))
e$Type <- "Undirected"
sum(e$Weight)
write.csv(e, file="socproxP4.csv", row.names = FALSE)

summary(g) # nb noeuds et liens
graph.density(g) # densité
summary(degree(g)) # degré moyen
summary(strength(g)) # degré pondéré moyen
transitivity(g, type= "localaverageundirected") # coef clustering
cc<- cluster_louvain(g) # communautés et modularité
c <- components(g) 
summary (c$csize) # plus grande composante
average.path.length(g) # plus courts chemins moyens
summary(as.data.frame(sizes(cc)))


e$rank = rank(-e$Weight)
e <- as.data.frame(unique(e[c("rank", "Weight")]))

reg <- lm(formula = log(e$Weight)~log(e$rank))
coeff <- coefficients(reg)
R2 <- summary(reg)[8]$r.squared
R2 <- round(R2, 4)
par(pty="s")
plot(x=log(e$rank), y=log((e$Weight)), bg="black",pch=21, xlab = "Log (Rang)", ylab = "Log (Valeur des liens)")
abline(lm(formula = log(e$Weight)~log(e$rank)), col = "red",lwd=3)
legend("topright", legend = c(paste("Beta = ", round(-coeff[2],4)), paste("R2 = ", R2)))


deg <- as.data.frame(degree(g))
colnames(deg)[1]<- "Degree"

deg$rank = rank(-deg$Degree)
deg <- as.data.frame(unique(deg[c("rank", "Degree")]))

deg <- subset(deg, deg$Degree>0)
reg <- lm(formula = log(deg$Degree)~log(deg$rank))
coeff <- coefficients(reg)
R2 <- summary(reg)[8]$r.squared
R2 <- round(R2, 4)
par(pty="s")
plot(x=log(deg$rank), y=log((deg$Degree)), bg="black",pch=21, xlab = "Log (Rang)", ylab = "Log (Degré)")
abline(lm(formula = log(deg$Degree)~log(deg$rank)), col = "red",lwd=3)
legend("topright", legend = c(paste("Beta = ", round(-coeff[2],4)), paste("R2 = ", R2)))


fit_power_law(g)

### org

#P1
orgprox <- unique(brevets_ipc_P1[c("Inv_name","App_name")])
orgprox$Inv_name <- as.character(orgprox$Inv_name)
orgprox$App_name <- as.character(orgprox$App_name)
bip <- graph.data.frame(orgprox)
V(bip)$type <- V(bip)$name %in% orgprox[,2]
e <- cbind( get.edgelist(bip))
e <- as.data.frame(e)
colnames(e)[1]<-"Source"
colnames(e)[2]<-"Target"
colnames(e)[3]<-"Weight"
e$Weight <- as.numeric(as.character(sub("," , ".", e$Weight)))
e$Type <- "Undirected"
e$Weight <- 1

write.csv(e, file="orgproxP1.csv", row.names = FALSE)


fit_power_law(bip)
summary(bip) # nb noeuds et liens
graph.density(bip) # densité
summary(degree(bip)) # degré moyen
summary(strength(bip)) # degré pondéré moyen
transitivity(bip, type= "localaverageundirected") # coef clustering
cc<- cluster_louvain(bip) # communautés et modularité
c <- components(bip) 
summary (c$csize) # plus grande composante
average.path.length(bip) # plus courts chemins moyens
summary(as.data.frame(sizes(cc)))
deg <- as.data.frame(degree(bip, mode= c("all")))
colnames(deg)[1]<- "Degree"
deg$rank = rank(-deg$Degree)
deg <- subset(deg, deg$Degree>0)
deg <- unique(deg[c("Degree","rank")])
reg <- lm(formula = log(deg$Degree)~log(deg$rank))
coeff <- coefficients(reg)
R2 <- summary(reg)[8]$r.squared
R2 <- round(R2, 4)
par(pty="s")
plot(x=log(deg$rank), y=log((deg$Degree)), bg="black",pch=21, xlab = "Log (Rang)", ylab = "Log (Degré)")
abline(lm(formula = log(deg$Degree)~log(deg$rank)), col = "red",lwd=3)
legend("topright", legend = c(paste("Beta = ", round(-coeff[2],4)), paste("R2 = ", R2)))

#P2
orgprox <- unique(brevets_ipc_P2[c("Inv_name","App_name")])
orgprox$Inv_name <- as.character(orgprox$Inv_name)
orgprox$App_name <- as.character(orgprox$App_name)
bip <- graph.data.frame(orgprox)
V(bip)$type <- V(bip)$name %in% orgprox[,2]
e <- cbind( get.edgelist(bip))
e <- as.data.frame(e)
colnames(e)[1]<-"Source"
colnames(e)[2]<-"Target"
colnames(e)[3]<-"Weight"
e$Weight <- as.numeric(as.character(sub("," , ".", e$Weight)))
e$Type <- "Undirected"
e$Weight <- 1
write.csv(e, file="orgproxP2.csv", row.names = FALSE)

fit_power_law(bip)
summary(bip) # nb noeuds et liens
graph.density(bip) # densité
summary(degree(bip)) # degré moyen
summary(strength(bip)) # degré pondéré moyen
transitivity(bip, type= "localaverageundirected") # coef clustering
cc<- cluster_louvain(bip) # communautés et modularité
c <- components(bip) 
summary (c$csize) # plus grande composante
average.path.length(bip) # plus courts chemins moyens
summary(as.data.frame(sizes(cc)))
deg <- as.data.frame(degree(bip, mode= c("all")))
colnames(deg)[1]<- "Degree"
deg$rank = rank(-deg$Degree)
deg <- subset(deg, deg$Degree>0)
deg <- unique(deg[c("Degree","rank")])
reg <- lm(formula = log(deg$Degree)~log(deg$rank))
coeff <- coefficients(reg)
R2 <- summary(reg)[8]$r.squared
R2 <- round(R2, 4)
par(pty="s")
plot(x=log(deg$rank), y=log((deg$Degree)), bg="black",pch=21, xlab = "Log (Rang)", ylab = "Log (Degré)")
abline(lm(formula = log(deg$Degree)~log(deg$rank)), col = "red",lwd=3)
legend("topright", legend = c(paste("Beta = ", round(-coeff[2],4)), paste("R2 = ", R2)))

#P3

orgprox <- unique(brevets_ipc_P3[c("Inv_name","App_name")])
orgprox$Inv_name <- as.character(orgprox$Inv_name)
orgprox$App_name <- as.character(orgprox$App_name)

bip <- graph.data.frame(orgprox)
V(bip)$type <- V(bip)$name %in% orgprox[,2]
e <- cbind( get.edgelist(bip))
e <- as.data.frame(e)
colnames(e)[1]<-"Source"
colnames(e)[2]<-"Target"
colnames(e)[3]<-"Weight"
e$Weight <- as.numeric(as.character(sub("," , ".", e$Weight)))
e$Type <- "Undirected"
e$Weight <- 1
write.csv(e, file="orgproxP3.csv", row.names = FALSE)

fit_power_law(bip)
summary(bip) # nb noeuds et liens
graph.density(bip) # densité
summary(degree(bip)) # degré moyen
summary(strength(bip)) # degré pondéré moyen
transitivity(bip, type= "localaverageundirected") # coef clustering
cc<- cluster_louvain(bip) # communautés et modularité
c <- components(bip) 
summary (c$csize) # plus grande composante
average.path.length(bip) # plus courts chemins moyens
summary(as.data.frame(sizes(cc)))
deg <- as.data.frame(degree(bip, mode= c("all")))
colnames(deg)[1]<- "Degree"
deg$rank = rank(-deg$Degree)
deg <- subset(deg, deg$Degree>0)
deg <- unique(deg[c("Degree","rank")])
reg <- lm(formula = log(deg$Degree)~log(deg$rank))
coeff <- coefficients(reg)
R2 <- summary(reg)[8]$r.squared
R2 <- round(R2, 4)
par(pty="s")
plot(x=log(deg$rank), y=log((deg$Degree)), bg="black",pch=21, xlab = "Log (Rang)", ylab = "Log (Degré)")
abline(lm(formula = log(deg$Degree)~log(deg$rank)), col = "red",lwd=3)
legend("topright", legend = c(paste("Beta = ", round(-coeff[2],4)), paste("R2 = ", R2)))

#P4

orgprox <- unique(brevets_ipc_P4[c("Inv_name","App_name")])
orgprox$Inv_name <- as.character(orgprox$Inv_name)
orgprox$App_name <- as.character(orgprox$App_name)
bip <- graph.data.frame(orgprox)
V(bip)$type <- V(bip)$name %in% orgprox[,2]
e <- cbind( get.edgelist(bip))
e <- as.data.frame(e)
colnames(e)[1]<-"Source"
colnames(e)[2]<-"Target"
colnames(e)[3]<-"Weight"
e$Weight <- as.numeric(as.character(sub("," , ".", e$Weight)))
e$Type <- "Undirected"
e$Weight <- 1
write.csv(e, file="orgproxP4.csv", row.names = FALSE)

fit_power_law(bip)
summary(bip) # nb noeuds et liens
graph.density(bip) # densité
summary(degree(bip)) # degré moyen
summary(strength(bip)) # degré pondéré moyen
transitivity(bip, type= "localaverageundirected") # coef clustering
cc<- cluster_louvain(bip) # communautés et modularité
c <- components(bip) 
summary (c$csize) # plus grande composante
average.path.length(bip) # plus courts chemins moyens
summary(as.data.frame(sizes(cc)))
deg <- as.data.frame(degree(bip, mode= c("all")))
colnames(deg)[1]<- "Degree"
deg$rank = rank(-deg$Degree)
deg <- subset(deg, deg$Degree>0)
deg <- unique(deg[c("Degree","rank")])
reg <- lm(formula = log(deg$Degree)~log(deg$rank))
coeff <- coefficients(reg)
R2 <- summary(reg)[8]$r.squared
R2 <- round(R2, 4)
par(pty="s")
plot(x=log(deg$rank), y=log((deg$Degree)), bg="black",pch=21, xlab = "Log (Rang)", ylab = "Log (Degré)")
abline(lm(formula = log(deg$Degree)~log(deg$rank)), col = "red",lwd=3)
legend("topright", legend = c(paste("Beta = ", round(-coeff[2],4)), paste("R2 = ", R2)))


### geo

points_brevets = readOGR(dsn = "C:/Users/rlx/Desktop/memoire_tmp/l_commune_scot_bdt_r82", layer="points_brevets", stringsAsFactors=FALSE)
points_brevets$Nombre.de <- as.numeric(as.character(sub("," , ".", points_brevets$Nombre.de)))

dm <- DistanceMatrix(points_brevets, unit = 1000, "insee_comm")

g <- graph_from_adjacency_matrix(dm, mode="undirected", weighted = TRUE, diag = TRUE)
dm <- cbind( get.edgelist(g) , round( E(g)$weight, 3 ))
dm <- as.data.frame(dm)
colnames(dm)[1]<-"Source"
colnames(dm)[2]<-"Target"
colnames(dm)[3]<-"Weight"
dm$Weight <- as.numeric(as.character(sub("," , ".", dm$Weight)))
dm$Type <- "Undirected"

dm[dm   < 0] = 0



# P1
cobrevinv <- unique(brevets_ipc_P1[c("Appln_id","Inv_name")])
bip <- graph.data.frame(cobrevinv)
V(bip)$type <- V(bip)$name %in% cobrevinv[,2]
mat <- get.adjacency(bipartite.projection(bip)[[2]], attr="weight", sparse=TRUE)
mat[order(rownames(mat)), order(colnames(mat))]
mat <- as.matrix(mat)
g <- graph_from_adjacency_matrix(mat, mode="undirected", weighted = TRUE, diag = TRUE)
e <- cbind( get.edgelist(g) , round( E(g)$weight, 3 ))
e <- as.data.frame(e)
colnames(e)[1]<-"Source"
colnames(e)[2]<-"Target"
colnames(e)[3]<-"Weight"
e$Weight <- as.numeric(as.character(sub("," , ".", e$Weight)))
e$Type <- "Undirected"


colnames(brevets_ipc_P1)[7] <- "Source"
inv_geo <- merge(e, brevets_ipc_P1[c("Source", "INSEE_Code")], by="Source")
inv_geo <- unique(inv_geo)
colnames(brevets_ipc_P1)[7] <- "Target"
inv_geod <- merge(inv_geo, brevets_ipc_P1[c("Target", "INSEE_Code")], by="Target")
inv_geod <- unique(inv_geod)
inv_geod$merg <- paste(inv_geod$INSEE_Code.x,inv_geod$INSEE_Code.y)
dm$merg <- paste(dm$Source, dm$Target)
inv_geod <- merge(inv_geod, dm[c("merg", "Weight")], by="merg")
colnames(inv_geod)[8]<- "Distance"
inv_geod$Distance <- as.numeric(as.character(sub("," , ".", inv_geod$Distance)))

summary(inv_geod$Distance)
plot(inv_geod$Distance, type="h")
d <- density(inv_geod$Distance)
plot(d, main = "Distribution des distances entre inventeurs", ylab="Densité", xlab="Distance (km)", col = "black",lwd=3)


# P2

cobrevinv <- unique(brevets_ipc_P2[c("Appln_id","Inv_name")])
bip <- graph.data.frame(cobrevinv)
V(bip)$type <- V(bip)$name %in% cobrevinv[,2]
mat <- get.adjacency(bipartite.projection(bip)[[2]], attr="weight", sparse=TRUE)
mat[order(rownames(mat)), order(colnames(mat))]
mat <- as.matrix(mat)
g <- graph_from_adjacency_matrix(mat, mode="undirected", weighted = TRUE, diag = TRUE)
e <- cbind( get.edgelist(g) , round( E(g)$weight, 3 ))
e <- as.data.frame(e)
colnames(e)[1]<-"Source"
colnames(e)[2]<-"Target"
colnames(e)[3]<-"Weight"
e$Weight <- as.numeric(as.character(sub("," , ".", e$Weight)))
e$Type <- "Undirected"


colnames(brevets_ipc_P2)[7] <- "Source"
inv_geo <- merge(e, brevets_ipc_P2[c("Source", "INSEE_Code")], by="Source")
inv_geo <- unique(inv_geo)
colnames(brevets_ipc_P2)[7] <- "Target"
inv_geod <- merge(inv_geo, brevets_ipc_P2[c("Target", "INSEE_Code")], by="Target")
inv_geod <- unique(inv_geod)
inv_geod$merg <- paste(inv_geod$INSEE_Code.x,inv_geod$INSEE_Code.y)
dm$merg <- paste(dm$Source, dm$Target)
inv_geod <- merge(inv_geod, dm[c("merg", "Weight")], by="merg")
colnames(inv_geod)[8]<- "Distance"
inv_geod$Distance <- as.numeric(as.character(sub("," , ".", inv_geod$Distance)))

summary(inv_geod$Distance)
d <- density(inv_geod$Distance)
lines(d, col="red",lwd=3)


#P3

cobrevinv <- unique(brevets_ipc_P3[c("Appln_id","Inv_name")])
bip <- graph.data.frame(cobrevinv)
V(bip)$type <- V(bip)$name %in% cobrevinv[,2]
mat <- get.adjacency(bipartite.projection(bip)[[2]], attr="weight", sparse=TRUE)
mat[order(rownames(mat)), order(colnames(mat))]
mat <- as.matrix(mat)
g <- graph_from_adjacency_matrix(mat, mode="undirected", weighted = TRUE, diag = TRUE)
e <- cbind( get.edgelist(g) , round( E(g)$weight, 3 ))
e <- as.data.frame(e)
colnames(e)[1]<-"Source"
colnames(e)[2]<-"Target"
colnames(e)[3]<-"Weight"
e$Weight <- as.numeric(as.character(sub("," , ".", e$Weight)))
e$Type <- "Undirected"


colnames(brevets_ipc_P3)[7] <- "Source"
inv_geo <- merge(e, brevets_ipc_P3[c("Source", "INSEE_Code")], by="Source")
inv_geo <- unique(inv_geo)
colnames(brevets_ipc_P3)[7] <- "Target"
inv_geod <- merge(inv_geo, brevets_ipc_P3[c("Target", "INSEE_Code")], by="Target")
inv_geod <- unique(inv_geod)
inv_geod$merg <- paste(inv_geod$INSEE_Code.x,inv_geod$INSEE_Code.y)
dm$merg <- paste(dm$Source, dm$Target)
inv_geod <- merge(inv_geod, dm[c("merg", "Weight")], by="merg")
colnames(inv_geod)[8]<- "Distance"
inv_geod$Distance <- as.numeric(as.character(sub("," , ".", inv_geod$Distance)))

summary(inv_geod$Distance)
d <- density(inv_geod$Distance)
lines(d, col="green",lwd=3)

#P4

cobrevinv <- unique(brevets_ipc_P4[c("Appln_id","Inv_name")])
bip <- graph.data.frame(cobrevinv)
V(bip)$type <- V(bip)$name %in% cobrevinv[,2]
mat <- get.adjacency(bipartite.projection(bip)[[2]], attr="weight", sparse=TRUE)
mat[order(rownames(mat)), order(colnames(mat))]
mat <- as.matrix(mat)
g <- graph_from_adjacency_matrix(mat, mode="undirected", weighted = TRUE, diag = TRUE)
e <- cbind( get.edgelist(g) , round( E(g)$weight, 3 ))
e <- as.data.frame(e)
colnames(e)[1]<-"Source"
colnames(e)[2]<-"Target"
colnames(e)[3]<-"Weight"
e$Weight <- as.numeric(as.character(sub("," , ".", e$Weight)))
e$Type <- "Undirected"


colnames(brevets_ipc_P4)[7] <- "Source"
inv_geo <- merge(e, brevets_ipc_P4[c("Source", "INSEE_Code")], by="Source")
inv_geo <- unique(inv_geo)
colnames(brevets_ipc_P4)[7] <- "Target"
inv_geod <- merge(inv_geo, brevets_ipc_P4[c("Target", "INSEE_Code")], by="Target")
inv_geod <- unique(inv_geod)
inv_geod$merg <- paste(inv_geod$INSEE_Code.x,inv_geod$INSEE_Code.y)
dm$merg <- paste(dm$Source, dm$Target)
inv_geod <- merge(inv_geod, dm[c("merg", "Weight")], by="merg")
colnames(inv_geod)[8]<- "Distance"
inv_geod$Distance <- as.numeric(as.character(sub("," , ".", inv_geod$Distance)))

summary(inv_geod$Distance)
d <- density(inv_geod$Distance)
lines(d, col="blue",lwd=3)
legend("topright", c("1980-1987","1988-1995","1996-2003","2004-2012"),lty=c(4,4), lwd=c(3,3),col=c("black","red","green","blue")) # gives the legend lines the correct color and width


### cobrevets

cobrev <- unique(brevets_ipc_P1[c("Appln_id","App_name")])
bip <- graph.data.frame(cobrev)
V(bip)$type <- V(bip)$name %in% cobrev[,2]
mat <- get.adjacency(bipartite.projection(bip)[[2]], attr="weight", sparse=TRUE)
mat[order(rownames(mat)), order(colnames(mat))]
mat <- as.matrix(mat)
g <- graph_from_adjacency_matrix(mat, mode="undirected", weighted = TRUE, diag = TRUE)
e <- cbind( get.edgelist(g) , round( E(g)$weight, 3 ))
e <- as.data.frame(e)
colnames(e)[1]<-"Source"
colnames(e)[2]<-"Target"
colnames(e)[3]<-"Weight"
e$Weight <- as.numeric(as.character(sub("," , ".", e$Weight)))
e$Type <- "Undirected"

sum(e$Weight)

e$rank = rank(-e$Weight)
reg <- lm(formula = log(e$Weight)~log(e$rank))
coeff <- coefficients(reg)
R2 <- summary(reg)[8]$r.squared
R2 <- round(R2, 4)
par(pty="s")
plot(x=log(e$rank), y=log((e$Weight)), bg="black",pch=21, xlab = "Log (Rang)", ylab = "Log (Valeur des liens)")
abline(lm(formula = log(e$Weight)~log(e$rank)), col = "red",lwd=3)
legend("topright", legend = c(paste("Beta = ", round(-coeff[2],4)), paste("R2 = ", R2)))


fit_power_law(g)

deg <- as.data.frame(degree(g, mode= c("all")))
colnames(deg)[1]<- "Degree"

deg$rank = rank(-deg$Degree)
deg <- subset(deg, deg$Degree>0)
reg <- lm(formula = log(deg$Degree)~log(deg$rank))
coeff <- coefficients(reg)
R2 <- summary(reg)[8]$r.squared
R2 <- round(R2, 4)
par(pty="s")
plot(x=log(deg$rank), y=log((deg$Degree)), bg="black",pch=21, xlab = "Log (Rang)", ylab = "Log (Degree)")
abline(lm(formula = log(deg$Degree)~log(deg$rank)), col = "red",lwd=3)
legend("topright", legend = c(paste("Beta = ", round(-coeff[2],4)), paste("R2 = ", R2)))

average.path.length(g)
write.csv(e, file = "cobrevP1.csv")


#P2

cobrev <- unique(brevets_ipc_P2[c("Appln_id","App_name")])
bip <- graph.data.frame(cobrev)
V(bip)$type <- V(bip)$name %in% cobrev[,2]
mat <- get.adjacency(bipartite.projection(bip)[[2]], attr="weight", sparse=TRUE)
mat[order(rownames(mat)), order(colnames(mat))]
mat <- as.matrix(mat)
g <- graph_from_adjacency_matrix(mat, mode="undirected", weighted = TRUE, diag = TRUE)
e <- cbind( get.edgelist(g) , round( E(g)$weight, 3 ))
e <- as.data.frame(e)
colnames(e)[1]<-"Source"
colnames(e)[2]<-"Target"
colnames(e)[3]<-"Weight"
e$Weight <- as.numeric(as.character(sub("," , ".", e$Weight)))
e$Type <- "Undirected"

sum(e$Weight)

e$rank = rank(-e$Weight)
reg <- lm(formula = log(e$Weight)~log(e$rank))
coeff <- coefficients(reg)
R2 <- summary(reg)[8]$r.squared
R2 <- round(R2, 4)
par(pty="s")
plot(x=log(e$rank), y=log((e$Weight)), bg="black",pch=21, xlab = "Log (Rang)", ylab = "Log (Valeur des liens)")
abline(lm(formula = log(e$Weight)~log(e$rank)), col = "red",lwd=3)
legend("topright", legend = c(paste("Beta = ", round(-coeff[2],4)), paste("R2 = ", R2)))


fit_power_law(g)

deg <- as.data.frame(degree(g, mode= c("all")))
colnames(deg)[1]<- "Degree"

deg$rank = rank(-deg$Degree)
deg <- subset(deg, deg$Degree>0)
reg <- lm(formula = log(deg$Degree)~log(deg$rank))
coeff <- coefficients(reg)
R2 <- summary(reg)[8]$r.squared
R2 <- round(R2, 4)
par(pty="s")
plot(x=log(deg$rank), y=log((deg$Degree)), bg="black",pch=21, xlab = "Log (Rang)", ylab = "Log (Degree)")
abline(lm(formula = log(deg$Degree)~log(deg$rank)), col = "red",lwd=3)
legend("topright", legend = c(paste("Beta = ", round(-coeff[2],4)), paste("R2 = ", R2)))

average.path.length(g)
write.csv(e, file = "cobrevP2.csv")


#??? P3

cobrev <- unique(brevets_ipc_P3[c("Appln_id","App_name")])
bip <- graph.data.frame(cobrev)
V(bip)$type <- V(bip)$name %in% cobrev[,2]
mat <- get.adjacency(bipartite.projection(bip)[[2]], attr="weight", sparse=TRUE)
mat[order(rownames(mat)), order(colnames(mat))]
mat <- as.matrix(mat)
g <- graph_from_adjacency_matrix(mat, mode="undirected", weighted = TRUE, diag = TRUE)
e <- cbind( get.edgelist(g) , round( E(g)$weight, 3 ))
e <- as.data.frame(e)
colnames(e)[1]<-"Source"
colnames(e)[2]<-"Target"
colnames(e)[3]<-"Weight"
e$Weight <- as.numeric(as.character(sub("," , ".", e$Weight)))
e$Type <- "Undirected"

sum(e$Weight)

e$rank = rank(-e$Weight)
reg <- lm(formula = log(e$Weight)~log(e$rank))
coeff <- coefficients(reg)
R2 <- summary(reg)[8]$r.squared
R2 <- round(R2, 4)
par(pty="s")
plot(x=log(e$rank), y=log((e$Weight)), bg="black",pch=21, xlab = "Log (Rang)", ylab = "Log (Valeur des liens)")
abline(lm(formula = log(e$Weight)~log(e$rank)), col = "red",lwd=3)
legend("topright", legend = c(paste("Beta = ", round(-coeff[2],4)), paste("R2 = ", R2)))


fit_power_law(g)

deg <- as.data.frame(degree(g, mode= c("all")))
colnames(deg)[1]<- "Degree"

deg$rank = rank(-deg$Degree)
deg <- subset(deg, deg$Degree>0)
reg <- lm(formula = log(deg$Degree)~log(deg$rank))
coeff <- coefficients(reg)
R2 <- summary(reg)[8]$r.squared
R2 <- round(R2, 4)
par(pty="s")
plot(x=log(deg$rank), y=log((deg$Degree)), bg="black",pch=21, xlab = "Log (Rang)", ylab = "Log (Degree)")
abline(lm(formula = log(deg$Degree)~log(deg$rank)), col = "red",lwd=3)
legend("topright", legend = c(paste("Beta = ", round(-coeff[2],4)), paste("R2 = ", R2)))

average.path.length(g)
write.csv(e, file = "cobrevP3.csv")

#P4

cobrev <- unique(brevets_ipc_P4[c("Appln_id","App_name")])
bip <- graph.data.frame(cobrev)
V(bip)$type <- V(bip)$name %in% cobrev[,2]
mat <- get.adjacency(bipartite.projection(bip)[[2]], attr="weight", sparse=TRUE)
mat[order(rownames(mat)), order(colnames(mat))]
mat <- as.matrix(mat)
g <- graph_from_adjacency_matrix(mat, mode="undirected", weighted = TRUE, diag = TRUE)
e <- cbind( get.edgelist(g) , round( E(g)$weight, 3 ))
e <- as.data.frame(e)
colnames(e)[1]<-"Source"
colnames(e)[2]<-"Target"
colnames(e)[3]<-"Weight"
e$Weight <- as.numeric(as.character(sub("," , ".", e$Weight)))
e$Type <- "Undirected"

sum(e$Weight)

e$rank = rank(-e$Weight)
reg <- lm(formula = log(e$Weight)~log(e$rank))
coeff <- coefficients(reg)
R2 <- summary(reg)[8]$r.squared
R2 <- round(R2, 4)
par(pty="s")
plot(x=log(e$rank), y=log((e$Weight)), bg="black",pch=21, xlab = "Log (Rang)", ylab = "Log (Valeur des liens)")
abline(lm(formula = log(e$Weight)~log(e$rank)), col = "red",lwd=3)
legend("topright", legend = c(paste("Beta = ", round(-coeff[2],4)), paste("R2 = ", R2)))


fit_power_law(g)

deg <- as.data.frame(degree(g, mode= c("all")))
colnames(deg)[1]<- "Degree"

deg$rank = rank(-deg$Degree)
deg <- subset(deg, deg$Degree>0)
reg <- lm(formula = log(deg$Degree)~log(deg$rank))
coeff <- coefficients(reg)
R2 <- summary(reg)[8]$r.squared
R2 <- round(R2, 4)
par(pty="s")
plot(x=log(deg$rank), y=log((deg$Degree)), bg="black",pch=21, xlab = "Log (Rang)", ylab = "Log (Degree)")
abline(lm(formula = log(deg$Degree)~log(deg$rank)), col = "red",lwd=3)
legend("topright", legend = c(paste("Beta = ", round(-coeff[2],4)), paste("R2 = ", R2)))

average.path.length(g)
write.csv(e, file = "cobrevP4.csv")


#P1 mobiinv

mobiinv <- unique(brevets_ipc_P1[c("Person_id.x","App_name")])


bip <- graph.data.frame(mobiinv)
V(bip)$type <- V(bip)$name %in% mobiinv[,2]
mat <- get.adjacency(bipartite.projection(bip)[[2]], attr="weight", sparse=TRUE)
mat[order(rownames(mat)), order(colnames(mat))]
mat <- as.matrix(mat)
g <- graph_from_adjacency_matrix(mat, mode="undirected", weighted = TRUE, diag = TRUE)
e <- cbind( get.edgelist(g) , round( E(g)$weight, 3 ))
e <- as.data.frame(e)
colnames(e)[1]<-"Source"
colnames(e)[2]<-"Target"
colnames(e)[3]<-"Weight"
e$Weight <- as.numeric(as.character(sub("," , ".", e$Weight)))
e$Type <- "Undirected"

write.csv(e, file = "mobiinvP1.csv", row.names = FALSE)

freqmobiinv <- as.data.frame(table(e$Source)) # fréquences de mobilité pro
sum(e$Weight)

#p2

mobiinv <- unique(brevets_ipc_P2[c("Person_id.x","App_name")])


bip <- graph.data.frame(mobiinv)
V(bip)$type <- V(bip)$name %in% mobiinv[,2]
mat <- get.adjacency(bipartite.projection(bip)[[2]], attr="weight", sparse=TRUE)
mat[order(rownames(mat)), order(colnames(mat))]
mat <- as.matrix(mat)
g <- graph_from_adjacency_matrix(mat, mode="undirected", weighted = TRUE, diag = TRUE)
e <- cbind( get.edgelist(g) , round( E(g)$weight, 3 ))
e <- as.data.frame(e)
colnames(e)[1]<-"Source"
colnames(e)[2]<-"Target"
colnames(e)[3]<-"Weight"
e$Weight <- as.numeric(as.character(sub("," , ".", e$Weight)))
e$Type <- "Undirected"

write.csv(e, file = "mobiinvP2.csv", row.names = FALSE)

freqmobiinv <- as.data.frame(table(e$Source)) # fréquences de mobilité pro
sum(e$Weight)

#P3

mobiinv <- unique(brevets_ipc_P3[c("Person_id.x","App_name")])


bip <- graph.data.frame(mobiinv)
V(bip)$type <- V(bip)$name %in% mobiinv[,2]
mat <- get.adjacency(bipartite.projection(bip)[[2]], attr="weight", sparse=TRUE)
mat[order(rownames(mat)), order(colnames(mat))]
mat <- as.matrix(mat)
g <- graph_from_adjacency_matrix(mat, mode="undirected", weighted = TRUE, diag = TRUE)
e <- cbind( get.edgelist(g) , round( E(g)$weight, 3 ))
e <- as.data.frame(e)
colnames(e)[1]<-"Source"
colnames(e)[2]<-"Target"
colnames(e)[3]<-"Weight"
e$Weight <- as.numeric(as.character(sub("," , ".", e$Weight)))
e$Type <- "Undirected"

write.csv(e, file = "mobiinvP3.csv", row.names = FALSE)

freqmobiinv <- as.data.frame(table(e$Source)) # fréquences de mobilité pro
sum(e$Weight)

#p4


mobiinv <- unique(brevets_ipc_P4[c("Person_id.x","App_name")])


bip <- graph.data.frame(mobiinv)
V(bip)$type <- V(bip)$name %in% mobiinv[,2]
mat <- get.adjacency(bipartite.projection(bip)[[2]], attr="weight", sparse=TRUE)
mat[order(rownames(mat)), order(colnames(mat))]
mat <- as.matrix(mat)
g <- graph_from_adjacency_matrix(mat, mode="undirected", weighted = TRUE, diag = TRUE)
e <- cbind( get.edgelist(g) , round( E(g)$weight, 3 ))
e <- as.data.frame(e)
colnames(e)[1]<-"Source"
colnames(e)[2]<-"Target"
colnames(e)[3]<-"Weight"
e$Weight <- as.numeric(as.character(sub("," , ".", e$Weight)))
e$Type <- "Undirected"

write.csv(e, file = "mobiinvP4.csv", row.names = FALSE)

freqmobiinv <- as.data.frame(table(e$Source)) # fréquences de mobilité pro
sum(e$Weight)


## soc P1

cobrevinv <- unique(brevets_ipc_P1[c("Appln_id","Inv_name")])
bip <- graph.data.frame(cobrevinv)
V(bip)$type <- V(bip)$name %in% cobrevinv[,2]
mat <- get.adjacency(bipartite.projection(bip)[[2]], attr="weight", sparse=TRUE)
mat[order(rownames(mat)), order(colnames(mat))]
mat <- as.matrix(mat)
g <- graph_from_adjacency_matrix(mat, mode="undirected", weighted = TRUE, diag = TRUE)
einv <- cbind( get.edgelist(g) , round( E(g)$weight, 3 ))
einv <- as.data.frame(einv)
colnames(einv)[1]<-"Source"
colnames(einv)[2]<-"Target"
colnames(einv)[3]<-"Weight"
einv$Weight <- as.numeric(as.character(sub("," , ".", einv$Weight)))
einv$Type <- "Undirected"

einv$rank = rank(-einv$Weight)
einv <- as.data.frame(unique(einv[c("rank", "Weight")]))

reg <- lm(formula = log(einv$Weight)~log(einv$rank))
coeff <- coefficients(reg)
R2 <- summary(reg)[8]$r.squared
R2 <- round(R2, 4)
par(pty="s")
plot(x=log(einv$rank), y=log((einv$Weight)), bg="black",pch=21, xlab = "Log (Rang)", ylab = "Log (Valeur des liens)")
abline(lm(formula = log(einv$Weight)~log(einv$rank)), col = "red",lwd=3)
legend("topright", legend = c(paste("Beta = ", round(-coeff[2],4)), paste("R2 = ", R2)))

deg <- as.data.frame(degree(g, mode= c("all")))
colnames(deg)[1]<- "Degree"

deg$rank = rank(-deg$Degree)
deg <- subset(deg, deg$Degree>0)
deg <- unique(deg)
reg <- lm(formula = log(deg$Degree)~log(deg$rank))
coeff <- coefficients(reg)
R2 <- summary(reg)[8]$r.squared
R2 <- round(R2, 4)
par(pty="s")
plot(x=log(deg$rank), y=log((deg$Degree)), bg="black",pch=21, xlab = "Log (Rang)", ylab = "Log (Degré)")
abline(lm(formula = log(deg$Degree)~log(deg$rank)), col = "red",lwd=3)
legend("topright", legend = c(paste("Beta = ", round(-coeff[2],4)), paste("R2 = ", R2)))
bc <- as.data.frame(betweenness(g, directed = FALSE))
colnames(bc)[1]<- "BC"
bc$rank = rank(-bc$BC)
bc <- subset(bc, bc$BC>0)
bc <- unique(bc[c("BC","rank")])
reg <- lm(formula = log(bc$BC)~log(bc$rank))
coeff <- coefficients(reg)
R2 <- summary(reg)[8]$r.squared
R2 <- round(R2, 4)
par(pty="s")
plot(x=log(bc$rank), y=log((bc$BC)), bg="black",pch=21, xlab = "Log (Rang)", ylab = "Log (Centralité d'intermédiarité)")
abline(lm(formula = log(bc$BC)~log(bc$rank)), col = "red",lwd=3)
legend("topright", legend = c(paste("Beta = ", round(-coeff[2],4)), paste("R2 = ", R2)))

#P2

cobrevinv <- unique(brevets_ipc_P2[c("Appln_id","Inv_name")])
bip <- graph.data.frame(cobrevinv)
V(bip)$type <- V(bip)$name %in% cobrevinv[,2]
mat <- get.adjacency(bipartite.projection(bip)[[2]], attr="weight", sparse=TRUE)
mat[order(rownames(mat)), order(colnames(mat))]
mat <- as.matrix(mat)
g <- graph_from_adjacency_matrix(mat, mode="undirected", weighted = TRUE, diag = TRUE)
einv <- cbind( get.edgelist(g) , round( E(g)$weight, 3 ))
einv <- as.data.frame(einv)
colnames(einv)[1]<-"Source"
colnames(einv)[2]<-"Target"
colnames(einv)[3]<-"Weight"
einv$Weight <- as.numeric(as.character(sub("," , ".", einv$Weight)))
einv$Type <- "Undirected"

einv$rank = rank(-einv$Weight)
einv <- as.data.frame(unique(einv[c("rank", "Weight")]))

reg <- lm(formula = log(einv$Weight)~log(einv$rank))
coeff <- coefficients(reg)
R2 <- summary(reg)[8]$r.squared
R2 <- round(R2, 4)
par(pty="s")
plot(x=log(einv$rank), y=log((einv$Weight)), bg="black",pch=21, xlab = "Log (Rang)", ylab = "Log (Valeur des liens)")
abline(lm(formula = log(einv$Weight)~log(einv$rank)), col = "red",lwd=3)
legend("topright", legend = c(paste("Beta = ", round(-coeff[2],4)), paste("R2 = ", R2)))

deg <- as.data.frame(degree(g, mode= c("all")))
colnames(deg)[1]<- "Degree"

deg$rank = rank(-deg$Degree)
deg <- subset(deg, deg$Degree>0)
deg <- unique(deg)
reg <- lm(formula = log(deg$Degree)~log(deg$rank))
coeff <- coefficients(reg)
R2 <- summary(reg)[8]$r.squared
R2 <- round(R2, 4)
par(pty="s")
plot(x=log(deg$rank), y=log((deg$Degree)), bg="black",pch=21, xlab = "Log (Rang)", ylab = "Log (Degré)")
abline(lm(formula = log(deg$Degree)~log(deg$rank)), col = "red",lwd=3)
legend("topright", legend = c(paste("Beta = ", round(-coeff[2],4)), paste("R2 = ", R2)))
bc <- as.data.frame(betweenness(g, directed = FALSE))
colnames(bc)[1]<- "BC"
bc$rank = rank(-bc$BC)
bc <- subset(bc, bc$BC>0)
bc <- unique(bc[c("BC","rank")])
reg <- lm(formula = log(bc$BC)~log(bc$rank))
coeff <- coefficients(reg)
R2 <- summary(reg)[8]$r.squared
R2 <- round(R2, 4)
par(pty="s")
plot(x=log(bc$rank), y=log((bc$BC)), bg="black",pch=21, xlab = "Log (Rang)", ylab = "Log (Centralité d'intermédiarité)")
abline(lm(formula = log(bc$BC)~log(bc$rank)), col = "red",lwd=3)
legend("topright", legend = c(paste("Beta = ", round(-coeff[2],4)), paste("R2 = ", R2)))

#P3 

cobrevinv <- unique(brevets_ipc_P3[c("Appln_id","Inv_name")])
bip <- graph.data.frame(cobrevinv)
V(bip)$type <- V(bip)$name %in% cobrevinv[,2]
mat <- get.adjacency(bipartite.projection(bip)[[2]], attr="weight", sparse=TRUE)
mat[order(rownames(mat)), order(colnames(mat))]
mat <- as.matrix(mat)
g <- graph_from_adjacency_matrix(mat, mode="undirected", weighted = TRUE, diag = TRUE)
einv <- cbind( get.edgelist(g) , round( E(g)$weight, 3 ))
einv <- as.data.frame(einv)
colnames(einv)[1]<-"Source"
colnames(einv)[2]<-"Target"
colnames(einv)[3]<-"Weight"
einv$Weight <- as.numeric(as.character(sub("," , ".", einv$Weight)))
einv$Type <- "Undirected"

einv$rank = rank(-einv$Weight)
einv <- as.data.frame(unique(einv[c("rank", "Weight")]))

reg <- lm(formula = log(einv$Weight)~log(einv$rank))
coeff <- coefficients(reg)
R2 <- summary(reg)[8]$r.squared
R2 <- round(R2, 4)
par(pty="s")
plot(x=log(einv$rank), y=log((einv$Weight)), bg="black",pch=21, xlab = "Log (Rang)", ylab = "Log (Valeur des liens)")
abline(lm(formula = log(einv$Weight)~log(einv$rank)), col = "red",lwd=3)
legend("topright", legend = c(paste("Beta = ", round(-coeff[2],4)), paste("R2 = ", R2)))

deg <- as.data.frame(degree(g, mode= c("all")))
colnames(deg)[1]<- "Degree"

deg$rank = rank(-deg$Degree)
deg <- subset(deg, deg$Degree>0)
deg <- unique(deg)
reg <- lm(formula = log(deg$Degree)~log(deg$rank))
coeff <- coefficients(reg)
R2 <- summary(reg)[8]$r.squared
R2 <- round(R2, 4)
par(pty="s")
plot(x=log(deg$rank), y=log((deg$Degree)), bg="black",pch=21, xlab = "Log (Rang)", ylab = "Log (Degré)")
abline(lm(formula = log(deg$Degree)~log(deg$rank)), col = "red",lwd=3)
legend("topright", legend = c(paste("Beta = ", round(-coeff[2],4)), paste("R2 = ", R2)))
bc <- as.data.frame(betweenness(g, directed = FALSE))
colnames(bc)[1]<- "BC"
bc$rank = rank(-bc$BC)
bc <- subset(bc, bc$BC>0)
bc <- unique(bc[c("BC","rank")])
reg <- lm(formula = log(bc$BC)~log(bc$rank))
coeff <- coefficients(reg)
R2 <- summary(reg)[8]$r.squared
R2 <- round(R2, 4)
par(pty="s")
plot(x=log(bc$rank), y=log((bc$BC)), bg="black",pch=21, xlab = "Log (Rang)", ylab = "Log (Centralité d'intermédiarité)")
abline(lm(formula = log(bc$BC)~log(bc$rank)), col = "red",lwd=3)
legend("topright", legend = c(paste("Beta = ", round(-coeff[2],4)), paste("R2 = ", R2)))

# P4

cobrevinv <- unique(brevets_ipc_P4[c("Appln_id","Inv_name")])
bip <- graph.data.frame(cobrevinv)
V(bip)$type <- V(bip)$name %in% cobrevinv[,2]
mat <- get.adjacency(bipartite.projection(bip)[[2]], attr="weight", sparse=TRUE)
mat[order(rownames(mat)), order(colnames(mat))]
mat <- as.matrix(mat)
g <- graph_from_adjacency_matrix(mat, mode="undirected", weighted = TRUE, diag = TRUE)
einv <- cbind( get.edgelist(g) , round( E(g)$weight, 3 ))
einv <- as.data.frame(einv)
colnames(einv)[1]<-"Source"
colnames(einv)[2]<-"Target"
colnames(einv)[3]<-"Weight"
einv$Weight <- as.numeric(as.character(sub("," , ".", einv$Weight)))
einv$Type <- "Undirected"

einv$rank = rank(-einv$Weight)
einv <- as.data.frame(unique(einv[c("rank", "Weight")]))

reg <- lm(formula = log(einv$Weight)~log(einv$rank))
coeff <- coefficients(reg)
R2 <- summary(reg)[8]$r.squared
R2 <- round(R2, 4)
par(pty="s")
plot(x=log(einv$rank), y=log((einv$Weight)), bg="black",pch=21, xlab = "Log (Rang)", ylab = "Log (Valeur des liens)")
abline(lm(formula = log(einv$Weight)~log(einv$rank)), col = "red",lwd=3)
legend("topright", legend = c(paste("Beta = ", round(-coeff[2],4)), paste("R2 = ", R2)))

deg <- as.data.frame(degree(g, mode= c("all")))
colnames(deg)[1]<- "Degree"

deg$rank = rank(-deg$Degree)
deg <- subset(deg, deg$Degree>0)
deg <- unique(deg)
reg <- lm(formula = log(deg$Degree)~log(deg$rank))
coeff <- coefficients(reg)
R2 <- summary(reg)[8]$r.squared
R2 <- round(R2, 4)
par(pty="s")
plot(x=log(deg$rank), y=log((deg$Degree)), bg="black",pch=21, xlab = "Log (Rang)", ylab = "Log (Degré)")
abline(lm(formula = log(deg$Degree)~log(deg$rank)), col = "red",lwd=3)
legend("topright", legend = c(paste("Beta = ", round(-coeff[2],4)), paste("R2 = ", R2)))
bc <- as.data.frame(betweenness(g, directed = FALSE))
colnames(bc)[1]<- "BC"
bc$rank = rank(-bc$BC)
bc <- subset(bc, bc$BC>0)
bc <- unique(bc[c("BC","rank")])
reg <- lm(formula = log(bc$BC)~log(bc$rank))
coeff <- coefficients(reg)
R2 <- summary(reg)[8]$r.squared
R2 <- round(R2, 4)
par(pty="s")
plot(x=log(bc$rank), y=log((bc$BC)), bg="black",pch=21, xlab = "Log (Rang)", ylab = "Log (Centralité d'intermédiarité)")
abline(lm(formula = log(bc$BC)~log(bc$rank)), col = "red",lwd=3)
legend("topright", legend = c(paste("Beta = ", round(-coeff[2],4)), paste("R2 = ", R2)))

## co brev-geo

brev_app_AML <- subset(brevets_ipc_P1, brevets_ipc_P1$AML.INSEE != "NA")
brev_app_noAML <- subset(brevets_ipc_P1, is.na(brevets_ipc_P1$AML.INSEE))

cobrevAML <- unique(brev_app_AML[c("Appln_id", "AML.INSEE")])
bip <- graph.data.frame(cobrevAML)
V(bip)$type <- V(bip)$name %in% cobrevAML[,2]
mat <- get.adjacency(bipartite.projection(bip)[[2]], attr="weight", sparse=TRUE)
mat[order(rownames(mat)), order(colnames(mat))]
mat <- as.matrix(mat)
g <- graph_from_adjacency_matrix(mat, mode="undirected", weighted = TRUE, diag = TRUE)
e <- cbind( get.edgelist(g) , round( E(g)$weight, 3 ))
e <- as.data.frame(e)
colnames(e)[1]<-"Source"
colnames(e)[2]<-"Target"
colnames(e)[3]<-"Weight"
e$Weight <- as.numeric(as.character(sub("," , ".", e$Weight)))
e$Type <- "Undirected"


write.csv(e, file = "cobrevAMLP1.csv")

cobrevNoAML <- unique(brev_app_noAML[c("Appln_id", "HORS.AML.NUTS")])
nbcobrevNoAML <- as.data.frame(table(cobrevNoAML$HORS.AML.NUTS))
nbcobrevNoAML$Source <- "AML"
colnames(nbcobrevNoAML)[1] <- "Target"
colnames(nbcobrevNoAML)[2] <- "Weight"
nbcobrevNoAML <- nbcobrevNoAML[,c(3,1,2)]
nbcobrevNoAML <- nbcobrevNoAML[-1,]

write.csv(nbcobrevNoAML, file = "cobrevNoAMLP1.csv")


# P2

brev_app_AML <- subset(brevets_ipc_P2, brevets_ipc_P2$AML.INSEE != "NA")
brev_app_noAML <- subset(brevets_ipc_P2, is.na(brevets_ipc_P2$AML.INSEE))

cobrevAML <- unique(brev_app_AML[c("Appln_id", "AML.INSEE")])
bip <- graph.data.frame(cobrevAML)
V(bip)$type <- V(bip)$name %in% cobrevAML[,2]
mat <- get.adjacency(bipartite.projection(bip)[[2]], attr="weight", sparse=TRUE)
mat[order(rownames(mat)), order(colnames(mat))]
mat <- as.matrix(mat)
g <- graph_from_adjacency_matrix(mat, mode="undirected", weighted = TRUE, diag = TRUE)
e <- cbind( get.edgelist(g) , round( E(g)$weight, 3 ))
e <- as.data.frame(e)
colnames(e)[1]<-"Source"
colnames(e)[2]<-"Target"
colnames(e)[3]<-"Weight"
e$Weight <- as.numeric(as.character(sub("," , ".", e$Weight)))
e$Type <- "Undirected"


write.csv(e, file = "cobrevAMLP2.csv")

cobrevNoAML <- unique(brev_app_noAML[c("Appln_id", "HORS.AML.NUTS")])
nbcobrevNoAML <- as.data.frame(table(cobrevNoAML$HORS.AML.NUTS))
nbcobrevNoAML$Source <- "AML"
colnames(nbcobrevNoAML)[1] <- "Target"
colnames(nbcobrevNoAML)[2] <- "Weight"
nbcobrevNoAML <- nbcobrevNoAML[,c(3,1,2)]
nbcobrevNoAML <- nbcobrevNoAML[-1,]

write.csv(nbcobrevNoAML, file = "cobrevNoAMLP2.csv")


# P3

brev_app_AML <- subset(brevets_ipc_P3, brevets_ipc_P3$AML.INSEE != "NA")
brev_app_noAML <- subset(brevets_ipc_P3, is.na(brevets_ipc_P3$AML.INSEE))

cobrevAML <- unique(brev_app_AML[c("Appln_id", "AML.INSEE")])
bip <- graph.data.frame(cobrevAML)
V(bip)$type <- V(bip)$name %in% cobrevAML[,2]
mat <- get.adjacency(bipartite.projection(bip)[[2]], attr="weight", sparse=TRUE)
mat[order(rownames(mat)), order(colnames(mat))]
mat <- as.matrix(mat)
g <- graph_from_adjacency_matrix(mat, mode="undirected", weighted = TRUE, diag = TRUE)
e <- cbind( get.edgelist(g) , round( E(g)$weight, 3 ))
e <- as.data.frame(e)
colnames(e)[1]<-"Source"
colnames(e)[2]<-"Target"
colnames(e)[3]<-"Weight"
e$Weight <- as.numeric(as.character(sub("," , ".", e$Weight)))
e$Type <- "Undirected"


write.csv(e, file = "cobrevAMLP3.csv")

cobrevNoAML <- unique(brev_app_noAML[c("Appln_id", "HORS.AML.NUTS")])
nbcobrevNoAML <- as.data.frame(table(cobrevNoAML$HORS.AML.NUTS))
nbcobrevNoAML$Source <- "AML"
colnames(nbcobrevNoAML)[1] <- "Target"
colnames(nbcobrevNoAML)[2] <- "Weight"
nbcobrevNoAML <- nbcobrevNoAML[,c(3,1,2)]
nbcobrevNoAML <- nbcobrevNoAML[-1,]

write.csv(nbcobrevNoAML, file = "cobrevNoAMLP3.csv")


# P4

brev_app_AML <- subset(brevets_ipc_P4, brevets_ipc_P4$AML.INSEE != "NA")
brev_app_noAML <- subset(brevets_ipc_P4, is.na(brevets_ipc_P4$AML.INSEE))

cobrevAML <- unique(brev_app_AML[c("Appln_id", "AML.INSEE")])
bip <- graph.data.frame(cobrevAML)
V(bip)$type <- V(bip)$name %in% cobrevAML[,2]
mat <- get.adjacency(bipartite.projection(bip)[[2]], attr="weight", sparse=TRUE)
mat[order(rownames(mat)), order(colnames(mat))]
mat <- as.matrix(mat)
g <- graph_from_adjacency_matrix(mat, mode="undirected", weighted = TRUE, diag = TRUE)
e <- cbind( get.edgelist(g) , round( E(g)$weight, 3 ))
e <- as.data.frame(e)
colnames(e)[1]<-"Source"
colnames(e)[2]<-"Target"
colnames(e)[3]<-"Weight"
e$Weight <- as.numeric(as.character(sub("," , ".", e$Weight)))
e$Type <- "Undirected"


write.csv(e, file = "cobrevAMLP4.csv")

cobrevNoAML <- unique(brev_app_noAML[c("Appln_id", "HORS.AML.NUTS")])
nbcobrevNoAML <- as.data.frame(table(cobrevNoAML$HORS.AML.NUTS))
nbcobrevNoAML$Source <- "AML"
colnames(nbcobrevNoAML)[1] <- "Target"
colnames(nbcobrevNoAML)[2] <- "Weight"
nbcobrevNoAML <- nbcobrevNoAML[,c(3,1,2)]
nbcobrevNoAML <- nbcobrevNoAML[-1,]

write.csv(nbcobrevNoAML, file = "cobrevNoAMLP4.csv")


### citations distances moyennes ###

brevets_ipc_P1 <- subset(brevets_ipc, brevets_ipc$Annee > 1979 & brevets_ipc$Annee < 1988)
brevets_ipc_P2 <- subset(brevets_ipc, brevets_ipc$Annee > 1987 & brevets_ipc$Annee < 1996)
brevets_ipc_P3 <- subset(brevets_ipc, brevets_ipc$Annee > 1995 & brevets_ipc$Annee < 2004)
brevets_ipc_P4 <- subset(brevets_ipc, brevets_ipc$Annee > 2003 & brevets_ipc$Annee < 2013)

citAMLAML <- read.csv(file="citdeAMLversAML.csv")
citAMLAML <- subset(citAMLAML, citAMLAML$Source %in% brevets_ipc_P1$Appln_id)
citAMLAML <- subset(citAMLAML, citAMLAML$Target %in% brevets_ipc_P1$Appln_id)

dm <- read.csv(file = "dist_matrix.csv")

colnames(brevets_ipc_P1)[1] <- "Source"
cit_geo <- merge(citAMLAML, brevets_ipc_P1[c("Source", "INSEE_Code")], by="Source")
cit_geo <- unique(cit_geo)
colnames(brevets_ipc_P1)[1] <- "Target"
cit_geod <- merge(cit_geo, brevets_ipc_P1[c("Target", "INSEE_Code")], by="Target")
cit_geod <- unique(cit_geod)

cit_geod$merg <- paste(cit_geod$INSEE_Code.x,cit_geod$INSEE_Code.y)
dm$merg <- paste(dm$Source, dm$Target)
cit_geod <- merge(cit_geod, dm[c("merg", "Weight")], by="merg")
colnames(cit_geod)[8]<- "Distance"
cit_geod$Distance <- as.numeric(as.character(sub("," , ".", cit_geod$Distance)))

summary(cit_geod$Distance)
d <- density(cit_geod$Distance)
plot(d, main = "Distribution des distances entre inventeurs se citant", ylab="Densité", xlab="Distance (km)", col = "black",lwd=3)



e <- cit_geod[c("INSEE_Code.x","INSEE_Code.y")]
bip <- graph.data.frame(e)
V(bip)$type <- V(bip)$name %in% cobrevAML[,2]
mat <- get.adjacency(bipartite.projection(bip)[[2]], attr="weight", sparse=TRUE)
mat[order(rownames(mat)), order(colnames(mat))]
mat <- as.matrix(mat)
g <- graph_from_adjacency_matrix(mat, mode="undirected", weighted = TRUE, diag = TRUE)
e <- cbind( get.edgelist(g) , round( E(g)$weight, 3 ))
e <- as.data.frame(e)
colnames(e)[1]<-"Source"
colnames(e)[2]<-"Target"
colnames(e)[3]<-"Weight"
e$Weight <- as.numeric(as.character(sub("," , ".", e$Weight)))
e$Type <- "Directed"


write.csv(e, file = "citationsAMLgeoP1.csv", row.names = FALSE)

citations <- read.csv(file = "201703_EP_Citations.txt", sep="|", header = TRUE)
citations <- citations[c("Citing_appln_id","Cited_Appln_id")]
citationsAML <- subset(citations, citations$Citing_appln_id %in% brevets_ipc_P1$Appln_id)
reg <- read.csv(file="201602_EPO_Inv_reg.txt", sep="|", header = TRUE)

colnames(citations)[1]<- "Source"
colnames(citations)[2]<- "Target"


colnames(reg)[2]<-"Source"
citationsregsource <- merge(citations, reg[c("Source","Reg_code")], by="Source" )
colnames(reg)[2]<-"Target"
citationsregsourcetarget <- merge(citationsregsource, reg[c("Target","Reg_code")], by="Target" )
colnames(citationsregsourcetarget)[3]<-"Source_reg"
colnames(citationsregsourcetarget)[4]<-"Target_reg"

citationdeAML <- subset(citationsregsourcetarget, citationsregsourcetarget$Source %in% brevets_ipc_P1$Appln_id)
citationversAML <- subset(citationsregsourcetarget, citationsregsourcetarget$Target %in% brevets_ipc_P1$Appln_id)

freqcitversAML <- as.data.frame(table(citationversAML$Source_reg))
freqcitdeAML <- as.data.frame(table(citationdeAML$Target_reg))

write.csv(freqcitdeAML,file="freqcitdeAMLP1.csv", row.names = FALSE)
write.csv(freqcitversAML,file="freqcitversAMLP1.csv", row.names = FALSE)

# P2

citAMLAML <- read.csv(file="citdeAMLversAML.csv")
citAMLAML <- subset(citAMLAML, citAMLAML$Source %in% brevets_ipc_P2$Appln_id)
citAMLAML <- subset(citAMLAML, citAMLAML$Target %in% brevets_ipc_P2$Appln_id)

dm <- read.csv(file = "dist_matrix.csv")

colnames(brevets_ipc_P2)[1] <- "Source"
cit_geo <- merge(citAMLAML, brevets_ipc_P2[c("Source", "INSEE_Code")], by="Source")
cit_geo <- unique(cit_geo)
colnames(brevets_ipc_P2)[1] <- "Target"
cit_geod <- merge(cit_geo, brevets_ipc_P2[c("Target", "INSEE_Code")], by="Target")
cit_geod <- unique(cit_geod)

cit_geod$merg <- paste(cit_geod$INSEE_Code.x,cit_geod$INSEE_Code.y)
dm$merg <- paste(dm$Source, dm$Target)
cit_geod <- merge(cit_geod, dm[c("merg", "Weight")], by="merg")
colnames(cit_geod)[8]<- "Distance"
cit_geod$Distance <- as.numeric(as.character(sub("," , ".", cit_geod$Distance)))

summary(cit_geod$Distance)
d <- density(cit_geod$Distance)
lines(d, col="red",lwd=3)



e <- cit_geod[c("INSEE_Code.x","INSEE_Code.y")]
bip <- graph.data.frame(e)
V(bip)$type <- V(bip)$name %in% cobrevAML[,2]
mat <- get.adjacency(bipartite.projection(bip)[[2]], attr="weight", sparse=TRUE)
mat[order(rownames(mat)), order(colnames(mat))]
mat <- as.matrix(mat)
g <- graph_from_adjacency_matrix(mat, mode="undirected", weighted = TRUE, diag = TRUE)
e <- cbind( get.edgelist(g) , round( E(g)$weight, 3 ))
e <- as.data.frame(e)
colnames(e)[1]<-"Source"
colnames(e)[2]<-"Target"
colnames(e)[3]<-"Weight"
e$Weight <- as.numeric(as.character(sub("," , ".", e$Weight)))
e$Type <- "Directed"


write.csv(e, file = "citationsAMLgeoP2.csv", row.names = FALSE)
citationdeAML <- subset(citationsregsourcetarget, citationsregsourcetarget$Source %in% brevets_ipc_P2$Appln_id)
citationversAML <- subset(citationsregsourcetarget, citationsregsourcetarget$Target %in% brevets_ipc_P2$Appln_id)

freqcitversAML <- as.data.frame(table(citationversAML$Source_reg))
freqcitdeAML <- as.data.frame(table(citationdeAML$Target_reg))

write.csv(freqcitdeAML,file="freqcitdeAMLP2.csv", row.names = FALSE)
write.csv(freqcitversAML,file="freqcitversAMLP2.csv", row.names = FALSE)

#P3
citAMLAML <- read.csv(file="citdeAMLversAML.csv")
citAMLAML <- subset(citAMLAML, citAMLAML$Source %in% brevets_ipc_P3$Appln_id)
citAMLAML <- subset(citAMLAML, citAMLAML$Target %in% brevets_ipc_P3$Appln_id)

dm <- read.csv(file = "dist_matrix.csv")

colnames(brevets_ipc_P3)[1] <- "Source"
cit_geo <- merge(citAMLAML, brevets_ipc_P3[c("Source", "INSEE_Code")], by="Source")
cit_geo <- unique(cit_geo)
colnames(brevets_ipc_P3)[1] <- "Target"
cit_geod <- merge(cit_geo, brevets_ipc_P3[c("Target", "INSEE_Code")], by="Target")
cit_geod <- unique(cit_geod)

cit_geod$merg <- paste(cit_geod$INSEE_Code.x,cit_geod$INSEE_Code.y)
dm$merg <- paste(dm$Source, dm$Target)
cit_geod <- merge(cit_geod, dm[c("merg", "Weight")], by="merg")
colnames(cit_geod)[8]<- "Distance"
cit_geod$Distance <- as.numeric(as.character(sub("," , ".", cit_geod$Distance)))

summary(cit_geod$Distance)
d <- density(cit_geod$Distance)
lines(d, col="green",lwd=3)



e <- cit_geod[c("INSEE_Code.x","INSEE_Code.y")]
bip <- graph.data.frame(e)
V(bip)$type <- V(bip)$name %in% cobrevAML[,2]
mat <- get.adjacency(bipartite.projection(bip)[[2]], attr="weight", sparse=TRUE)
mat[order(rownames(mat)), order(colnames(mat))]
mat <- as.matrix(mat)
g <- graph_from_adjacency_matrix(mat, mode="undirected", weighted = TRUE, diag = TRUE)
e <- cbind( get.edgelist(g) , round( E(g)$weight, 3 ))
e <- as.data.frame(e)
colnames(e)[1]<-"Source"
colnames(e)[2]<-"Target"
colnames(e)[3]<-"Weight"
e$Weight <- as.numeric(as.character(sub("," , ".", e$Weight)))
e$Type <- "Directed"


write.csv(e, file = "citationsAMLgeoP3.csv", row.names = FALSE)
citationdeAML <- subset(citationsregsourcetarget, citationsregsourcetarget$Source %in% brevets_ipc_P3$Appln_id)
citationversAML <- subset(citationsregsourcetarget, citationsregsourcetarget$Target %in% brevets_ipc_P3$Appln_id)

freqcitversAML <- as.data.frame(table(citationversAML$Source_reg))
freqcitdeAML <- as.data.frame(table(citationdeAML$Target_reg))

write.csv(freqcitdeAML,file="freqcitdeAMLP3.csv", row.names = FALSE)
write.csv(freqcitversAML,file="freqcitversAMLP3.csv", row.names = FALSE)

# P4

citAMLAML <- read.csv(file="citdeAMLversAML.csv")
citAMLAML <- subset(citAMLAML, citAMLAML$Source %in% brevets_ipc_P4$Appln_id)
citAMLAML <- subset(citAMLAML, citAMLAML$Target %in% brevets_ipc_P4$Appln_id)

dm <- read.csv(file = "dist_matrix.csv")

colnames(brevets_ipc_P4)[1] <- "Source"
cit_geo <- merge(citAMLAML, brevets_ipc_P4[c("Source", "INSEE_Code")], by="Source")
cit_geo <- unique(cit_geo)
colnames(brevets_ipc_P4)[1] <- "Target"
cit_geod <- merge(cit_geo, brevets_ipc_P4[c("Target", "INSEE_Code")], by="Target")
cit_geod <- unique(cit_geod)

cit_geod$merg <- paste(cit_geod$INSEE_Code.x,cit_geod$INSEE_Code.y)
dm$merg <- paste(dm$Source, dm$Target)
cit_geod <- merge(cit_geod, dm[c("merg", "Weight")], by="merg")
colnames(cit_geod)[8]<- "Distance"
cit_geod$Distance <- as.numeric(as.character(sub("," , ".", cit_geod$Distance)))

summary(cit_geod$Distance)
d <- density(cit_geod$Distance)
lines(d, col="blue",lwd=3)
legend("topright", c("1980-1987","1988-1995","1996-2003","2004-2012"),lty=c(4,4), lwd=c(3,3),col=c("black","red","green","blue")) # gives the legend lines the correct color and width



e <- cit_geod[c("INSEE_Code.x","INSEE_Code.y")]
bip <- graph.data.frame(e)
V(bip)$type <- V(bip)$name %in% cobrevAML[,2]
mat <- get.adjacency(bipartite.projection(bip)[[2]], attr="weight", sparse=TRUE)
mat[order(rownames(mat)), order(colnames(mat))]
mat <- as.matrix(mat)
g <- graph_from_adjacency_matrix(mat, mode="undirected", weighted = TRUE, diag = TRUE)
e <- cbind( get.edgelist(g) , round( E(g)$weight, 3 ))
e <- as.data.frame(e)
colnames(e)[1]<-"Source"
colnames(e)[2]<-"Target"
colnames(e)[3]<-"Weight"
e$Weight <- as.numeric(as.character(sub("," , ".", e$Weight)))
e$Type <- "Directed"


write.csv(e, file = "citationsAMLgeoP4.csv", row.names = FALSE)
citationdeAML <- subset(citationsregsourcetarget, citationsregsourcetarget$Source %in% brevets_ipc_P4$Appln_id)
citationversAML <- subset(citationsregsourcetarget, citationsregsourcetarget$Target %in% brevets_ipc_P4$Appln_id)

freqcitversAML <- as.data.frame(table(citationversAML$Source_reg))
freqcitdeAML <- as.data.frame(table(citationdeAML$Target_reg))

write.csv(freqcitdeAML,file="freqcitdeAMLP4.csv", row.names = FALSE)
write.csv(freqcitversAML,file="freqcitversAMLP4.csv", row.names = FALSE)


## corr par période

# P1

inv_soc <- unique(brevets_ipc_P1[c("Appln_id","Inv_name")])
inv_ipc <- unique(brevets_ipc_P1[c("IPC", "Inv_name")])
inv_ipc$IPC <- substr(brevets_ipc_P1$IPC, 0, 7)
inv_ipc <- unique(brevets_ipc_P1)
inv_geo <- unique(brevets_ipc_P1[c("INSEE_Code", "Inv_name")])
inv_app <- unique(brevets_ipc_P1[c("Person_id.y", "Inv_name")])


bip <- graph.data.frame(inv_soc)
V(bip)$type <- V(bip)$name %in% inv_soc[,2]
mat <- get.adjacency(bipartite.projection(bip)[[2]], attr="weight", sparse=TRUE)
mat[order(rownames(mat)), order(colnames(mat))]
mat <- as.matrix(mat)
g <- graph_from_adjacency_matrix(mat, mode="undirected", weighted = TRUE, diag = TRUE)
esoc <- cbind( get.edgelist(g) , round( E(g)$weight, 3 ))
esoc <- as.data.frame(esoc)
colnames(esoc)[1]<-"Source"
colnames(esoc)[2]<-"Target"
colnames(esoc)[3]<-"Weight"
esoc$Weight <- as.numeric(as.character(sub("," , ".", esoc$Weight)))
esoc$Type <- "Undirected"
gsoc <- g

bip <- graph.data.frame(inv_ipc)
V(bip)$type <- V(bip)$name %in% inv_ipc[,2]
mat <- get.adjacency(bipartite.projection(bip)[[2]], attr="weight", sparse=TRUE)
mat[order(rownames(mat)), order(colnames(mat))]
mat <- as.matrix(mat)
g <- graph_from_adjacency_matrix(mat, mode="undirected", weighted = TRUE, diag = TRUE)
eipc <- cbind( get.edgelist(g) , round( E(g)$weight, 3 ))
eipc <- as.data.frame(eipc)
colnames(eipc)[1]<-"Source"
colnames(eipc)[2]<-"Target"
colnames(eipc)[3]<-"Weight"
eipc$Weight <- as.numeric(as.character(sub("," , ".", eipc$Weight)))
eipc$Type <- "Undirected"
gipc <- g

bip <- graph.data.frame(inv_geo)
V(bip)$type <- V(bip)$name %in% inv_geo[,2]
mat <- get.adjacency(bipartite.projection(bip)[[2]], attr="weight", sparse=TRUE)
mat[order(rownames(mat)), order(colnames(mat))]
mat <- as.matrix(mat)
g <- graph_from_adjacency_matrix(mat, mode="undirected", weighted = TRUE, diag = TRUE)
egeo <- cbind( get.edgelist(g) , round( E(g)$weight, 3 ))
egeo <- as.data.frame(egeo)
colnames(egeo)[1]<-"Source"
colnames(egeo)[2]<-"Target"
colnames(egeo)[3]<-"Weight"
egeo$Weight <- as.numeric(as.character(sub("," , ".", egeo$Weight)))
egeo$Type <- "Undirected"
ggeo <- g

bip <- graph.data.frame(inv_app)
V(bip)$type <- V(bip)$name %in% inv_app[,2]
mat <- get.adjacency(bipartite.projection(bip)[[2]], attr="weight", sparse=TRUE)
mat[order(rownames(mat)), order(colnames(mat))]
mat <- as.matrix(mat)
g <- graph_from_adjacency_matrix(mat, mode="undirected", weighted = TRUE, diag = TRUE)
eapp <- cbind( get.edgelist(g) , round( E(g)$weight, 3 ))
eapp <- as.data.frame(eapp)
colnames(eapp)[1]<-"Source"
colnames(eapp)[2]<-"Target"
colnames(eapp)[3]<-"Weight"
eapp$Weight <- as.numeric(as.character(sub("," , ".", eapp$Weight)))
eapp$Type <- "Undirected"
gapp <- g


dgapp <- as.data.frame(igraph::degree(gapp))
dgsoc <- as.data.frame(igraph::degree(gsoc))
dggeo <- as.data.frame(igraph::degree(ggeo))
dgipc <- as.data.frame(igraph::degree(gipc))

cor(dgapp, dgipc)
cor(dgapp, dgsoc)
cor(dgapp, dggeo)
cor(dgipc, dggeo)
cor(dgipc, dgsoc)
cor(dggeo, dgsoc)

#P2


inv_soc <- unique(brevets_ipc_P2[c("Appln_id","Inv_name")])
inv_ipc <- unique(brevets_ipc_P2[c("IPC", "Inv_name")])
inv_ipc$IPC <- substr(brevets_ipc_P2$IPC, 0, 7)
inv_geo <- unique(brevets_ipc_P2[c("INSEE_Code", "Inv_name")])
inv_app <- unique(brevets_ipc_P2[c("Person_id.y", "Inv_name")])


bip <- graph.data.frame(inv_soc)
V(bip)$type <- V(bip)$name %in% inv_soc[,2]
mat <- get.adjacency(bipartite.projection(bip)[[2]], attr="weight", sparse=TRUE)
mat[order(rownames(mat)), order(colnames(mat))]
mat <- as.matrix(mat)
g <- graph_from_adjacency_matrix(mat, mode="undirected", weighted = TRUE, diag = TRUE)
esoc <- cbind( get.edgelist(g) , round( E(g)$weight, 3 ))
esoc <- as.data.frame(esoc)
colnames(esoc)[1]<-"Source"
colnames(esoc)[2]<-"Target"
colnames(esoc)[3]<-"Weight"
esoc$Weight <- as.numeric(as.character(sub("," , ".", esoc$Weight)))
esoc$Type <- "Undirected"
gsoc <- g

bip <- graph.data.frame(inv_ipc)
V(bip)$type <- V(bip)$name %in% inv_ipc[,2]
mat <- get.adjacency(bipartite.projection(bip)[[2]], attr="weight", sparse=TRUE)
mat[order(rownames(mat)), order(colnames(mat))]
mat <- as.matrix(mat)
g <- graph_from_adjacency_matrix(mat, mode="undirected", weighted = TRUE, diag = TRUE)
eipc <- cbind( get.edgelist(g) , round( E(g)$weight, 3 ))
eipc <- as.data.frame(eipc)
colnames(eipc)[1]<-"Source"
colnames(eipc)[2]<-"Target"
colnames(eipc)[3]<-"Weight"
eipc$Weight <- as.numeric(as.character(sub("," , ".", eipc$Weight)))
eipc$Type <- "Undirected"
gipc <- g

bip <- graph.data.frame(inv_geo)
V(bip)$type <- V(bip)$name %in% inv_geo[,2]
mat <- get.adjacency(bipartite.projection(bip)[[2]], attr="weight", sparse=TRUE)
mat[order(rownames(mat)), order(colnames(mat))]
mat <- as.matrix(mat)
g <- graph_from_adjacency_matrix(mat, mode="undirected", weighted = TRUE, diag = TRUE)
egeo <- cbind( get.edgelist(g) , round( E(g)$weight, 3 ))
egeo <- as.data.frame(egeo)
colnames(egeo)[1]<-"Source"
colnames(egeo)[2]<-"Target"
colnames(egeo)[3]<-"Weight"
egeo$Weight <- as.numeric(as.character(sub("," , ".", egeo$Weight)))
egeo$Type <- "Undirected"
ggeo <- g

bip <- graph.data.frame(inv_app)
V(bip)$type <- V(bip)$name %in% inv_app[,2]
mat <- get.adjacency(bipartite.projection(bip)[[2]], attr="weight", sparse=TRUE)
mat[order(rownames(mat)), order(colnames(mat))]
mat <- as.matrix(mat)
g <- graph_from_adjacency_matrix(mat, mode="undirected", weighted = TRUE, diag = TRUE)
eapp <- cbind( get.edgelist(g) , round( E(g)$weight, 3 ))
eapp <- as.data.frame(eapp)
colnames(eapp)[1]<-"Source"
colnames(eapp)[2]<-"Target"
colnames(eapp)[3]<-"Weight"
eapp$Weight <- as.numeric(as.character(sub("," , ".", eapp$Weight)))
eapp$Type <- "Undirected"
gapp <- g


dgapp <- as.data.frame(igraph::degree(gapp))
dgsoc <- as.data.frame(igraph::degree(gsoc))
dggeo <- as.data.frame(igraph::degree(ggeo))
dgipc <- as.data.frame(igraph::degree(gipc))

cor(dgapp, dgipc)
cor(dgapp, dgsoc)
cor(dgapp, dggeo)
cor(dgipc, dggeo)
cor(dgipc, dgsoc)
cor(dggeo, dgsoc)

#P3

inv_soc <- unique(brevets_ipc_P3[c("Appln_id","Inv_name")])
inv_ipc <- unique(brevets_ipc_P3[c("IPC", "Inv_name")])
inv_ipc$IPC <- substr(brevets_ipc_P3$IPC, 0, 7)
inv_geo <- unique(brevets_ipc_P3[c("INSEE_Code", "Inv_name")])
inv_app <- unique(brevets_ipc_P3[c("Person_id.y", "Inv_name")])


bip <- graph.data.frame(inv_soc)
V(bip)$type <- V(bip)$name %in% inv_soc[,2]
mat <- get.adjacency(bipartite.projection(bip)[[2]], attr="weight", sparse=TRUE)
mat[order(rownames(mat)), order(colnames(mat))]
mat <- as.matrix(mat)
g <- graph_from_adjacency_matrix(mat, mode="undirected", weighted = TRUE, diag = TRUE)
esoc <- cbind( get.edgelist(g) , round( E(g)$weight, 3 ))
esoc <- as.data.frame(esoc)
colnames(esoc)[1]<-"Source"
colnames(esoc)[2]<-"Target"
colnames(esoc)[3]<-"Weight"
esoc$Weight <- as.numeric(as.character(sub("," , ".", esoc$Weight)))
esoc$Type <- "Undirected"
gsoc <- g

bip <- graph.data.frame(inv_ipc)
V(bip)$type <- V(bip)$name %in% inv_ipc[,2]
mat <- get.adjacency(bipartite.projection(bip)[[2]], attr="weight", sparse=TRUE)
mat[order(rownames(mat)), order(colnames(mat))]
mat <- as.matrix(mat)
g <- graph_from_adjacency_matrix(mat, mode="undirected", weighted = TRUE, diag = TRUE)
eipc <- cbind( get.edgelist(g) , round( E(g)$weight, 3 ))
eipc <- as.data.frame(eipc)
colnames(eipc)[1]<-"Source"
colnames(eipc)[2]<-"Target"
colnames(eipc)[3]<-"Weight"
eipc$Weight <- as.numeric(as.character(sub("," , ".", eipc$Weight)))
eipc$Type <- "Undirected"
gipc <- g

bip <- graph.data.frame(inv_geo)
V(bip)$type <- V(bip)$name %in% inv_geo[,2]
mat <- get.adjacency(bipartite.projection(bip)[[2]], attr="weight", sparse=TRUE)
mat[order(rownames(mat)), order(colnames(mat))]
mat <- as.matrix(mat)
g <- graph_from_adjacency_matrix(mat, mode="undirected", weighted = TRUE, diag = TRUE)
egeo <- cbind( get.edgelist(g) , round( E(g)$weight, 3 ))
egeo <- as.data.frame(egeo)
colnames(egeo)[1]<-"Source"
colnames(egeo)[2]<-"Target"
colnames(egeo)[3]<-"Weight"
egeo$Weight <- as.numeric(as.character(sub("," , ".", egeo$Weight)))
egeo$Type <- "Undirected"
ggeo <- g

bip <- graph.data.frame(inv_app)
V(bip)$type <- V(bip)$name %in% inv_app[,2]
mat <- get.adjacency(bipartite.projection(bip)[[2]], attr="weight", sparse=TRUE)
mat[order(rownames(mat)), order(colnames(mat))]
mat <- as.matrix(mat)
g <- graph_from_adjacency_matrix(mat, mode="undirected", weighted = TRUE, diag = TRUE)
eapp <- cbind( get.edgelist(g) , round( E(g)$weight, 3 ))
eapp <- as.data.frame(eapp)
colnames(eapp)[1]<-"Source"
colnames(eapp)[2]<-"Target"
colnames(eapp)[3]<-"Weight"
eapp$Weight <- as.numeric(as.character(sub("," , ".", eapp$Weight)))
eapp$Type <- "Undirected"
gapp <- g


dgapp <- as.data.frame(igraph::degree(gapp))
dgsoc <- as.data.frame(igraph::degree(gsoc))
dggeo <- as.data.frame(igraph::degree(ggeo))
dgipc <- as.data.frame(igraph::degree(gipc))

cor(dgapp, dgipc)
cor(dgapp, dgsoc)
cor(dgapp, dggeo)
cor(dgipc, dggeo)
cor(dgipc, dgsoc)
cor(dggeo, dgsoc)

# P4

inv_soc <- unique(brevets_ipc_P4[c("Appln_id","Inv_name")])
inv_ipc <- unique(brevets_ipc_P4[c("IPC", "Inv_name")])
inv_ipc$IPC <- substr(brevets_ipc_P4$IPC, 0, 7)
inv_geo <- unique(brevets_ipc_P4[c("INSEE_Code", "Inv_name")])
inv_app <- unique(brevets_ipc_P4[c("Person_id.y", "Inv_name")])


bip <- graph.data.frame(inv_soc)
V(bip)$type <- V(bip)$name %in% inv_soc[,2]
mat <- get.adjacency(bipartite.projection(bip)[[2]], attr="weight", sparse=TRUE)
mat[order(rownames(mat)), order(colnames(mat))]
mat <- as.matrix(mat)
g <- graph_from_adjacency_matrix(mat, mode="undirected", weighted = TRUE, diag = TRUE)
esoc <- cbind( get.edgelist(g) , round( E(g)$weight, 3 ))
esoc <- as.data.frame(esoc)
colnames(esoc)[1]<-"Source"
colnames(esoc)[2]<-"Target"
colnames(esoc)[3]<-"Weight"
esoc$Weight <- as.numeric(as.character(sub("," , ".", esoc$Weight)))
esoc$Type <- "Undirected"
gsoc <- g

bip <- graph.data.frame(inv_ipc)
V(bip)$type <- V(bip)$name %in% inv_ipc[,2]
mat <- get.adjacency(bipartite.projection(bip)[[2]], attr="weight", sparse=TRUE)
mat[order(rownames(mat)), order(colnames(mat))]
mat <- as.matrix(mat)
g <- graph_from_adjacency_matrix(mat, mode="undirected", weighted = TRUE, diag = TRUE)
eipc <- cbind( get.edgelist(g) , round( E(g)$weight, 3 ))
eipc <- as.data.frame(eipc)
colnames(eipc)[1]<-"Source"
colnames(eipc)[2]<-"Target"
colnames(eipc)[3]<-"Weight"
eipc$Weight <- as.numeric(as.character(sub("," , ".", eipc$Weight)))
eipc$Type <- "Undirected"
gipc <- g

bip <- graph.data.frame(inv_geo)
V(bip)$type <- V(bip)$name %in% inv_geo[,2]
mat <- get.adjacency(bipartite.projection(bip)[[2]], attr="weight", sparse=TRUE)
mat[order(rownames(mat)), order(colnames(mat))]
mat <- as.matrix(mat)
g <- graph_from_adjacency_matrix(mat, mode="undirected", weighted = TRUE, diag = TRUE)
egeo <- cbind( get.edgelist(g) , round( E(g)$weight, 3 ))
egeo <- as.data.frame(egeo)
colnames(egeo)[1]<-"Source"
colnames(egeo)[2]<-"Target"
colnames(egeo)[3]<-"Weight"
egeo$Weight <- as.numeric(as.character(sub("," , ".", egeo$Weight)))
egeo$Type <- "Undirected"
ggeo <- g

bip <- graph.data.frame(inv_app)
V(bip)$type <- V(bip)$name %in% inv_app[,2]
mat <- get.adjacency(bipartite.projection(bip)[[2]], attr="weight", sparse=TRUE)
mat[order(rownames(mat)), order(colnames(mat))]
mat <- as.matrix(mat)
g <- graph_from_adjacency_matrix(mat, mode="undirected", weighted = TRUE, diag = TRUE)
eapp <- cbind( get.edgelist(g) , round( E(g)$weight, 3 ))
eapp <- as.data.frame(eapp)
colnames(eapp)[1]<-"Source"
colnames(eapp)[2]<-"Target"
colnames(eapp)[3]<-"Weight"
eapp$Weight <- as.numeric(as.character(sub("," , ".", eapp$Weight)))
eapp$Type <- "Undirected"
gapp <- g


dgapp <- as.data.frame(igraph::degree(gapp))
dgsoc <- as.data.frame(igraph::degree(gsoc))
dggeo <- as.data.frame(igraph::degree(ggeo))
dgipc <- as.data.frame(igraph::degree(gipc))

cor(dgapp, dgipc)
cor(dgapp, dgsoc)
cor(dgapp, dggeo)
cor(dgipc, dggeo)
cor(dgipc, dgsoc)
cor(dggeo, dgsoc)