install.packages("NbClust")
install.packages("cluster")
install.packages("phyloseq")
help(phyloseq)
library(cluster)
library(factoextra)
library(readr)
library(openxlsx)
library(dplyr)
library(rio)
library(fpc)
library(ggplot2)
library(NbClust)
library(plot_clusgap)
library(plyr)


#untuk import datakm exell ke R
datakm=read.delim("clipboard")
datakm
view(datakm)
#ekspor hasil tabel
export(datakmfix, "datakmfix.xlsx")

#mengisi datakm yang kosong (NA) dengan mean atau rata"
datakm$Stok.5=ifelse(is.na(datakm$Stok.5), mean(datakm$Stok.5, na.rm = TRUE), datakm$Stok.5)
datakm$Stok.7=ifelse(is.na(datakm$Stok.7), mean(datakm$Stok.7, na.rm = TRUE), datakm$Stok.7)

#cek datakm 
datakm
summary(datakm)
mean(datakm$Stok.5)
mean(datakm$Stok.7)
view(datakm)
#standarisasi data/transformasi dengan scale
datakmfix=scale(datakm)
datakmfix=scale(datakm[, 3:15])
View(datakmfix)

### penentuan jumlah cluster ###
# silhouette
fviz_nbclust(datakmfix, pam, method = "silhouette")
fviz_nbclust(datakmfix, pam, method = "wss")

library("cluster")
pam1 = function(x, k){list(cluster = pam(x, k, cluster.only=TRUE))}
gs.pam = clusGap(datakmfix, FUN = pam1, K.max = 10, B = 519)
gs.pam
plot(gs.pam)
mtext
plot_clusgap(gs.pam)


fviz_nbclust(datakmfix, pam, method = "gap_stat",k.max=10, nboot = 500)
gap_stat
# pamk
library(fpc)
pamk.result <- pamk(datakm)
pamk.result$nc

pam.res <- pam(datakm, 2)
print(pam.res)
pam.res
pam(datakm, 2)

# Clust
datakmfix <- cbind(datakm, cluster = pam.res$cluster)
head(datakmfix, n = 2)
View(datakmfix)
 pam.res$medoids
head(pam.res$clustering)
fviz_cluster(datakmfix)

# Visualize silhouhette information
require("cluster")
sil <- silhouette(pam.res)
fviz_silhouette(sil)

#untuk melihat index silhoutte
summary(sil)

si.sumkmean <- summary(sil)

#average silhoutte setiap cluster
si.sumkmean$clus.avg.widths

#total average silhouette
si.sumkmean$avg.width

#ukuran setiap cluster
si.sumkmean$clus.sizes

# Identify observation with negative silhouette
neg_sil_index <- which(sil[, "sil_width"] < 0)
sil[neg_sil_index, , drop = FALSE]

