install.packages("ggplot2")
install.packages("lattice")
install.packages("psych")

library("ggplot2")
library("lattice")
library("psych")

setwd("/Users/stanselitskiy/R")
ekg <- read.table("foetal_ecg.dat")

#remove time
dim <- c("V2","V3","V4","V5","V6","V7","V8","V9")
ekg2 <- ekg[, dim]


p2 <- ggplot(ekg, aes(x=V1))+
  geom_line(aes(y=V2))+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())
p3 <- ggplot(ekg, aes(x=V1))+
  geom_line(aes(y=V3))+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())
p4 <- ggplot(ekg, aes(x=V1))+
  geom_line(aes(y=V4))+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())
p5 <- ggplot(ekg, aes(x=V1))+
  geom_line(aes(y=V5))+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())
p6 <- ggplot(ekg, aes(x=V1))+
  geom_line(aes(y=V6))+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())
p7 <- ggplot(ekg, aes(x=V1))+
  geom_line(aes(y=V7))+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())
p8 <- ggplot(ekg, aes(x=V1))+
  geom_line(aes(y=V8))+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())
p9 <- ggplot(ekg, aes(x=V1))+
  geom_line(aes(y=V9))+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

multiplot(p2,p3,p4,p5,p6,p7,p8,p9, cols=1)


#horizontal matrix PCA - ECG chanells
pca_model <- ks_eigen_rotate_cov(ekg2)
ds <- pca_model$ds
#ds_o <- pca_model$ds
#ds <- ks_norm_ds(ds)
pca_model$A
pca_model$An1

#relable eigenbasis
names(ds) <- c("V2","V3","V4","V5","V6","V7","V8","V9")

#noise
ds_n <- ds[, dim]
ds_n$V2 <- 0 #mean(ds_n$V2)
ds_n$V3 <- 0 #mean(ds_n$V3)
ds_n$V4 <- 0 #mean(ds_n$V4)
ds_n$V5 <- 0 #mean(ds_n$V5)
ds_n$V6 <- 0 #mean(ds_n$V6)
ds_n$V7 <- 0 #mean(ds_n$V7)
ds_n <- as.data.frame(as.matrix(ds_n) %*% pca_model$An1)


#mother
ds_m <- ds[,dim]
#names(ds_m) <- c("V2","V3","V4","V5","V6","V7","V8","V9")
#ds_m$V2 <- mean(ds_m$V2)
#ds_m$V3 <- mean(ds_m$V3)
#ds_m$V4 <- mean(ds_m$V4)
ds_m$V5 <- 0 #mean(ds_m$V5)
ds_m$V6 <- 0 #mean(ds_m$V6)
ds_m$V7 <- 0 #mean(ds_m$V7)
ds_m$V8 <- 0 #mean(ds_m$V8)
ds_m$V9 <- 0 #mean(ds_m$V9)
ds_m <- as.data.frame(as.matrix(ds_m) %*% pca_model$An1)

#child
ds_c <- ds[,dim]
ds_c$V2 <- 0 #mean(ds_c$V2)
ds_c$V3 <- 0 #mean(ds_c$V3)
ds_c$V4 <- 0 #mean(ds_c$V4)
#ds_m$V5 <- mean(ds_m$V5)
#ds_m$V6 <- mean(ds_m$V6)
#ds_m$V7 <- 0 #mean(ds_m$V7)
ds_c$V8 <- 0 #mean(ds_c$V8)
ds_c$V9 <- 0 #mean(ds_c$V9)
ds_c <- as.data.frame(as.matrix(ds_c) %*% pca_model$An1)

#show child
ds <- ds_c

#show mother
ds <- ds_m

#no noise
ds <- ds_c + ds_m

#noise
ds <- ds_n

#all
ds <- ds_c + ds_m + ds_n

#put time and original lables back
names(ds) <- c("V2","V3","V4","V5","V6","V7","V8","V9")
ds[,"V1"] <- ekg[,"V1"]

describe(ds)
cov(ds)
cor(ds)


p2 <- ggplot(ds, aes(x=V1))+
  geom_line(aes(y=V2), color="black")+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())
p3 <- ggplot(ds, aes(x=V1))+
  geom_line(aes(y=V3), color="black")+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())
p4 <- ggplot(ds, aes(x=V1))+
  geom_line(aes(y=V4), color="black")+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())
p5 <- ggplot(ds, aes(x=V1))+
  geom_line(aes(y=V5), color="black")+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())
p6 <- ggplot(ds, aes(x=V1))+
  geom_line(aes(y=V6), color="black")+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())
p7 <- ggplot(ds, aes(x=V1))+
  geom_line(aes(y=V7), color="black")+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())
p8 <- ggplot(ds, aes(x=V1))+
  geom_line(aes(y=V8), color="black")+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())
p9 <- ggplot(ds, aes(x=V1))+
  geom_line(aes(y=V9), color="black")+ #, color="darkred")+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

multiplot(p2,p3,p4,p5,p6,p7,p8,p9, cols=1)

multiplot(p2,p3,p4,p5,p6,p7,p8,p9, cols=8)

  
p2 <- ggplot(ds, aes(x=V2))+
  geom_histogram(data=ds, aes(y=..density..), fill="blue", bins=40, alpha=0.6)+
  geom_density(data=ds,adjust=0.5) 
  
p3 <- ggplot(ds, aes(x=V3))+
  geom_histogram(data=ds, aes(y=..density..), fill="blue", bins=40, alpha=0.6)+
  geom_density(data=ds,adjust=0.5) 

p4 <- ggplot(ds, aes(x=V4))+
  geom_histogram(data=ds, aes(y=..density..), fill="blue", bins=40, alpha=0.6)+
  geom_density(data=ds,adjust=0.5) 

p5 <- ggplot(ds, aes(x=V5))+
  geom_histogram(data=ds, aes(y=..density..), fill="blue", bins=40, alpha=0.6)+
  geom_density(data=ds,adjust=0.5) 

p6 <- ggplot(ds, aes(x=V6))+
  geom_histogram(data=ds, aes(y=..density..), fill="blue", bins=40, alpha=0.6)+
  geom_density(data=ds,adjust=0.5) 

p7 <- ggplot(ds, aes(x=V7))+
  geom_histogram(data=ds, aes(y=..density..), fill="blue", bins=40, alpha=0.6)+
  geom_density(data=ds,adjust=0.5) 

p8 <- ggplot(ds, aes(x=V8))+
  geom_histogram(data=ds, aes(y=..density..), fill="blue", bins=40, alpha=0.6)+
  geom_density(data=ds,adjust=0.5) 

p9 <- ggplot(ds, aes(x=V9))+
  geom_histogram(data=ds, aes(y=..density..), fill="blue", bins=40, alpha=0.6)+
  geom_density(data=ds,adjust=0.5) 

multiplot(p2,p3,p4,p5,p6,p7,p8,p9, cols=2)


#vertical matrix PCA - 2500 time chanells
ekg3 <- as.data.frame(t(ekg2))
pca_model3 <- ks_eigen_rotate_cov(ekg3)
dst <- pca_model3$ds

#invariants
ds <- dst[,1:8]
names(ds) <- c("V2","V3","V4","V5","V6","V7","V8","V9")
ds[,"V1"] <- c(1,2,3,4,5,6,7,8) #ekg[1:8,"V1"]


#"remove" extra subspaces
dst_1 <- dst
dst_1[,2:ncol(dst)] <- 0
dst_1o <- as.data.frame(as.matrix(dst_1) %*% pca_model3$An1)
ds <- as.data.frame(t(dst_1o))
ds[,"V1"] <- ekg[,"V1"]

dst_2 <- dst
dst_2[,1] <- 0
dst_2[,3:ncol(dst)] <- 0
dst_2o <- as.data.frame(as.matrix(dst_2) %*% pca_model3$An1)
ds <- as.data.frame(t(dst_2o))
ds[,"V1"] <- ekg[,"V1"]

dst_3 <- dst
dst_3[,1:2] <- 0
dst_3[,4:ncol(dst)] <- 0
dst_3o <- as.data.frame(as.matrix(dst_3) %*% pca_model3$An1)
ds <- as.data.frame(t(dst_3o))
ds[,"V1"] <- ekg[,"V1"]

dst_4 <- dst
dst_4[,1:3] <- 0
dst_4[,5:ncol(dst)] <- 0
dst_4o <- as.data.frame(as.matrix(dst_4) %*% pca_model3$An1)
ds <- as.data.frame(t(dst_4o))
ds[,"V1"] <- ekg[,"V1"]

dst_5 <- dst
dst_5[,1:4] <- 0
dst_5[,6:ncol(dst)] <- 0
dst_5o <- as.data.frame(as.matrix(dst_5) %*% pca_model3$An1)
ds <- as.data.frame(t(dst_5o))
ds[,"V1"] <- ekg[,"V1"]

dst_6 <- dst
dst_6[,1:5] <- 0
dst_6[,7:ncol(dst)] <- 0
dst_6o <- as.data.frame(as.matrix(dst_6) %*% pca_model3$An1)
ds <- as.data.frame(t(dst_6o))
ds[,"V1"] <- ekg[,"V1"]

dst_7 <- dst
dst_7[,1:6] <- 0
dst_7[,8:ncol(dst)] <- 0
dst_7o <- as.data.frame(as.matrix(dst_7) %*% pca_model3$An1)
ds <- as.data.frame(t(dst_7o))
ds[,"V1"] <- ekg[,"V1"]

dst_8 <- dst
dst_8[,1:1500] <- 0
dst_8[,1502:ncol(dst)] <- 0
dst_8o <- as.data.frame(as.matrix(dst_8) %*% pca_model3$An1)
ds <- as.data.frame(t(dst_8o))
ds[,"V1"] <- ekg[,"V1"]

#"remove" subspaces with not much structure
dst_c <- dst
dst_c[,8:ncol(dst)] <- 0

#reverse transform
dst_o <- as.data.frame(as.matrix(dst_c) %*% pca_model3$An1)
ds <- as.data.frame(t(dst_o))

#delta
ds <- ekg2 - ds
ds[,"V1"] <- ekg[,"V1"]


#again 8 ecg channels
ekg4 <- ds
pca_model <- ks_eigen_rotate_cov(ekg4)
ds <- pca_model$ds
names(ds) <- c("V2","V3","V4","V5","V6","V7","V8","V9")
ds[,"V1"] <- ekg[,"V1"]



# K-means, 2 clusters
ds <- pca_model$ds
#ds <- ds_c
names(ds) <- c("V2","V3","V4","V5","V6","V7","V8","V9")
X2 <- as.matrix(ds[,"V4"]^2)

vds <- as.data.frame(ds[,"V4"])
names(vds) <- c("V4")
vds[,"Ones"] <- rep_len(1, nrow(vds))
V <- as.matrix(vds)
Vt <- t(V)

C <- -1 * solve(t(V) %*% V) %*% Vt %*% X2

Cl <- rev(c(1,C))
polyroot(Cl)

m1a <- ( -C[1] + sqrt(C[1]^2 - 4*C[2]) )/2
m1b <- ( -C[1] - sqrt(C[1]^2 - 4*C[2]) )/2

m2a <- C[2]/m1a
m2b <- C[2]/m1b

ds[,"Cls"] <- 1
ds[ (ds$V4-m1a)^2 < (ds$V4-m1b)^2, "Cls"] <- 2

ds[,"V1"] <- ekg[,"V1"]
ggplot(ds, aes(x=V1))+
  geom_line(aes(y=V4, color=Cls))+
  scale_colour_gradientn(colours=rainbow(4))


# create eigen-transform for subset
ds2 <- ds[ds$Cls < 2,]
#ds2 <- ds[ds$Cls > 1,]
dim <- c("V2","V3","V4","V5","V6","V7","V8","V9")
ds3 <- ds2[, dim]
pca_model2 <- ks_eigen_rotate_cov(ds3)
ds <- as.data.frame(as.matrix(ekg2) %*% pca_model2$A)


#persperation
#relable eigenbasis
names(ds) <- c("V2","V3","V4","V5","V6","V7","V8","V9")
ds_p <- ds[,dim]
#ds_p$V2 <- 0 
#ds_p$V3 <- 0 
#ds_p$V4 <- 0 
ds_p$V5 <- 0
ds_p$V6 <- 0
ds_p$V7 <- 0 
ds_p$V8 <- 0 
ds_p$V9 <- 0 
ds_p <- as.data.frame(as.matrix(ds_p) %*% pca_model2$An1)

#show persperation
ds <- ds_p


# K-means 3 clusters
ds <- pca_model$ds
#ds <- ds_c
names(ds) <- c("V2","V3","V4","V5","V6","V7","V8","V9")

# 1st variable
X3 <- as.matrix(ds[,"V5"]^3)
vds <- as.data.frame(ds[,"V5"]^2)
names(vds) <- c("V5^2")
vds[,"V5"] <- ds[,"V5"]
vds[,"Ones"] <- rep_len(1, nrow(vds))
V <- as.matrix(vds)
Vt <- t(V)

C <- -1 * solve(t(V) %*% V) %*% Vt %*% X3
Cl <- rev(c(1,C))

Mu1 <- Re(polyroot(Cl))

# 2nd variable
X3 <- as.matrix(ds[,"V6"]^3)
vds <- as.data.frame(ds[,"V6"]^2)
names(vds) <- c("V6^2")
vds[,"V6"] <- ds[,"V6"]
vds[,"Ones"] <- rep_len(1, nrow(vds))
V <- as.matrix(vds)
Vt <- t(V)

C <- -1 * solve(t(V) %*% V) %*% Vt %*% X3
Cl <- rev(c(1,C))

Mu2 <- Re(polyroot(Cl))

# 2rd variable
X3 <- as.matrix(ds[,"V7"]^3)
vds <- as.data.frame(ds[,"V7"]^2)
names(vds) <- c("V7^2")
vds[,"V7"] <- ds[,"V7"]
vds[,"Ones"] <- rep_len(1, nrow(vds))
V <- as.matrix(vds)
Vt <- t(V)

C <- -1 * solve(t(V) %*% V) %*% Vt %*% X3
Cl <- rev(c(1,C))

Mu3 <- Re(polyroot(Cl))

# clusterization
ds[,"Cls"] <- 3
d1 <- (ds$V5-Mu1[1])^2+(ds$V6-Mu2[1])^2+(ds$V7-Mu3[1])^2
d2 <- (ds$V5-Mu1[2])^2+(ds$V6-Mu2[2])^2+(ds$V7-Mu3[2])^2
d3 <- (ds$V5-Mu1[3])^2+(ds$V6-Mu2[3])^2+(ds$V7-Mu3[3])^2
ds[ (d1 < d2) & (d1 < d3), "Cls"] <- 1
ds[ (d2 < d1) & (d2 < d3), "Cls"] <- 2

ds[,"V1"] <- ekg[,"V1"]
ggplot(ds, aes(x=V1))+
  geom_point(aes(y=V6, color=Cls, alpha=0.5))+
  scale_colour_gradientn(colours=rainbow(4))


ds[,"Cls"] <- 3
d1 <- (ds$V7-Mu3[1])^2 + (ds$V5-Mu1[1])^2
d2 <- (ds$V7-Mu3[2])^2 + (ds$V5-Mu1[2])^2
d3 <- (ds$V7-Mu3[3])^2 + (ds$V5-Mu1[3])^2
ds[ (d1 < d2) & (d1 < d3), "Cls"] <- 1
ds[ (d2 < d1) & (d2 < d3), "Cls"] <- 2

ds[,"V1"] <- ekg[,"V1"]
ggplot(ds[ds$Cls>1,], aes(x=V1))+
  geom_line(aes(y=V6, color=Cls, alpha=0.5))+
  scale_colour_gradientn(colours=rainbow(4))
ggplot(ds[ds$Cls<2,], aes(x=V1))+
  geom_line(aes(y=V6, color=Cls, alpha=0.5))+
  scale_colour_gradientn(colours=rainbow(4))

#inverse subset transform
ts <- as.data.frame(ds[ds$Cls>1,"V1"])
names(ts) <- c("V1")
ds_c <- ds[ds$Cls>1,dim]
ds <- as.data.frame(as.matrix(ds_c) %*% pca_model$An1)
names(ds) <- c("V2","V3","V4","V5","V6","V7","V8","V9")
ds[,"V1"] <- ts[,"V1"]


# create eigen-transform for subset
ds2 <- ds[ds$Cls > 1,]
dim <- c("V2","V3","V4","V5","V6","V7","V8","V9")
ds3 <- ds2[, dim]
pca_model2 <- ks_eigen_rotate_cov(ds3)
ds <- as.data.frame(as.matrix(ekg2) %*% pca_model2$A)


# K-means 3 clusters for original data
ds <- ekg2

# 1st variable
X3 <- as.matrix(ds[,"V2"]^3)
vds <- as.data.frame(ds[,"V2"]^2)
names(vds) <- c("V2^2")
vds[,"V2"] <- ds[,"V2"]
vds[,"Ones"] <- rep_len(1, nrow(vds))
V <- as.matrix(vds)
Vt <- t(V)

C <- -1 * solve(t(V) %*% V) %*% Vt %*% X3
Cl <- rev(c(1,C))

Mu1 <- Re(polyroot(Cl))

# 2nd variable
X3 <- as.matrix(ds[,"V3"]^3)
vds <- as.data.frame(ds[,"V3"]^2)
names(vds) <- c("V3^2")
vds[,"V3"] <- ds[,"V3"]
vds[,"Ones"] <- rep_len(1, nrow(vds))
V <- as.matrix(vds)
Vt <- t(V)

C <- -1 * solve(t(V) %*% V) %*% Vt %*% X3
Cl <- rev(c(1,C))

Mu2 <- Re(polyroot(Cl))

# 3rd variable
X3 <- as.matrix(ds[,"V4"]^3)
vds <- as.data.frame(ds[,"V4"]^2)
names(vds) <- c("V4^2")
vds[,"V4"] <- ds[,"V4"]
vds[,"Ones"] <- rep_len(1, nrow(vds))
V <- as.matrix(vds)
Vt <- t(V)

C <- -1 * solve(t(V) %*% V) %*% Vt %*% X3
Cl <- rev(c(1,C))

Mu3 <- Re(polyroot(Cl))


Mu <- ks_kmeans_1d_means(ds, "V7", 3)
ds <- ks_kmeans_1d_clusters(ds, "V7", Mu)
ggplot(ds, aes(x=V1))+
  geom_point(aes(y=V6, color=Cls, alpha=0.5))+
  scale_colour_gradientn(colours=rainbow(5))

  
# clusterization
ds[,"Cls"] <- 3
d1 <- (ds$V2-Mu1[1])^2 + (ds$V3-Mu2[1])^2 + (ds$V4-Mu3[1])^2
d2 <- (ds$V2-Mu1[2])^2 + (ds$V3-Mu2[2])^2 + (ds$V4-Mu3[2])^2
d3 <- (ds$V2-Mu1[3])^2 + (ds$V3-Mu2[3])^2 + (ds$V4-Mu3[3])^2
ds[ (d1 < d2) & (d1 < d3), "Cls"] <- 1
ds[ (d2 < d1) & (d2 < d3), "Cls"] <- 2

ds[,"V1"] <- ekg[,"V1"]
ggplot(ds, aes(x=V1))+
  geom_point(aes(y=V2, color=Cls, alpha=0.5))+
  scale_colour_gradientn(colours=rainbow(4))


# create eigen-transform for subset
ds2 <- ds[ds$Cls > 1,]
dim <- c("V2","V3","V4","V5","V6","V7","V8","V9")
ds3 <- ds2[, dim]
pca_model2 <- ks_eigen_rotate_cov(ds3)
ds <- as.data.frame(as.matrix(ekg2) %*% pca_model2$A)