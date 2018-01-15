install.packages("ggplot2")
install.packages("lattice")
install.packages("psych")

library("ggplot2")
library("lattice")
library("psych")

setwd("/Users/stanselitskiy/R")
ekg <- read.table("foetal_ecg.dat")

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


dim <- c("V2","V3","V4","V5","V6","V7","V8","V9")
ekg2 <- ekg[, dim]

pci_model <- ks_eigen_rotate_cov(ekg2)
ds <- pci_model$ds
#ds <- ks_norm_ds(ds)

#remove noise
names(ds) <- c("V2","V3","V4","V5","V6","V7","V8","V9")
ds$V2 <- mean(ds$V2)
ds$V3 <- mean(ds$V3)
ds$V4 <- mean(ds$V4)
#ds$V5 <- 0
ds$V7 <- mean(ds$V7)
#ds$V8 <- 0
#ds$V9 <- 0
ds <- as.data.frame(as.matrix(ds) %*% pci_model$An1)

#put time back
names(ds) <- c("V2","V3","V4","V5","V6","V7","V8","V9")
ds[,"V1"] <- ekg[,"V1"]


describe(ds)


p2 <- ggplot(ds, aes(x=V1))+
  geom_line(aes(y=V2))+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())
p3 <- ggplot(ds, aes(x=V1))+
  geom_line(aes(y=V3))+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())
p4 <- ggplot(ds, aes(x=V1))+
  geom_line(aes(y=V4), color="darkblue")+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())
p5 <- ggplot(ds, aes(x=V1))+
  geom_line(aes(y=V5), color="darkgreen")+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())
p6 <- ggplot(ds, aes(x=V1))+
  geom_line(aes(y=V6))+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())
p7 <- ggplot(ds, aes(x=V1))+
  geom_line(aes(y=V7))+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())
p8 <- ggplot(ds, aes(x=V1))+
  geom_line(aes(y=V8))+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())
p9 <- ggplot(ds, aes(x=V1))+
  geom_line(aes(y=V9))+ #, color="darkred")+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

multiplot(p2,p3,p4,p5,p6,p7,p8,p9, cols=1)
  
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