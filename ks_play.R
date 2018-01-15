#data <- read.csv(file.choose())

install.packages("ggplot2")
install.packages("lattice")
install.packages("psych")
install.packages("httr")
install.packages("jsonlite")

library("ggplot2")
library("lattice")
library("psych")
library("httr")
library("jsonlite")

#rm(diabetes)
#setwd("/Users/stanselitskiy/R")
#getwd()
#load("diabetes.sav")
load(url("http://biostat.mc.vanderbilt.edu/wiki/pub/Main/DataSets/diabetes.sav")) 

#str(diabetes)
#install.packages("dplyr")


#clean.diabetes <- diabetes[complete.cases(diabetes),]
#anyNA(clean.diabetes)
#num.cols <- sapply(clean.diabetes[1,],is.numeric)
#cor.data <- cor(clean.diabetes[,num.cols])
#cor.data <- cor(clean.diabetes)
#cor.data

#cols <- c("chol","glyhb","ratio","bp.1s","bp.1d")
#clean2.diabetes <- diabetes[complete.cases(diabetes[,cols]),cols]
#lmodel <- lm("chol ~ glyhb + ratio + bp.1s + bp.1d", clean2.diabetes)
#lmodel
#ggplot(clean2.diabetes, aes(y=chol, x=glyhb, z=ratio)) + geom_point()

#clean.df <- df[complete.cases(df[,c("chol","glyhb")]),c("chol","glyhb")]


ks_model <- ks_lm( diabetes, c("chol"), c("glyhb","ratio","bp.1s","bp.1d") )

ks_model <- ks_lm( diabetes, c("glyhb"), c("ratio","bp.1s") )


ks_model$X[,1:5]
ks_model$X0[,1:5]
ks_model$Xl[,1:5]
ks_model$Y[,1:5]
ks_model$h0
ks_model$R
ks_model$Nm1
ks_model$Nm1R
ks_model$DimNames
ks_model$e2
ks_model$e2n
ks_model$en
#all.names <- c("ratio","bp.1s","glyhb") 
#matrix_symvect_mult(ks_model$Nm1R, all.names)

ydf <- as.data.frame(t(ks_model$Y))
ydf[, "V3"] <- rep_len(2, nrow(ydf))
xdf <- as.data.frame(t(ks_model$X))
xdf[,3] <- NULL
xdf[, "V3"] <- rep_len(1, nrow(xdf))
xdf[1:5,]
ydf[1:5,]
xydf <- rbind(ydf, xdf)

ggplot(xydf, aes(x=V1, y=V2, colour=V3))+
  geom_point(alpha=0.5)


xdf <- as.data.frame(t(ks_model$X))
xdf[, "V4"] <- rep_len(1, nrow(xdf))
xldf <- as.data.frame(t(ks_model$Xl))
xldf[, "V4"] <- rep_len(2, nrow(xldf))
xxldf <- rbind(xdf, xldf)
xxldf
xdf[1:5,]
xldf[1:5,]
#colnames(xxldf)[3]
#anyNA(xxldf)

cloud(glyhb ~ ratio*bp.1s, xxldf, groups=V4)
cloud(ratio ~ bp.1s*glyhb, xxldf, groups=V4)
cloud(bp.1s ~ glyhb*ratio, xxldf, groups=V4)


#All variables model
ks_model <- ks_lm( diabetes, c("glyhb"), 
                   c("chol", "hdl", "ratio", "stab.glu", 
                     "bp.1s", "bp.1d", "age", "height", "weight",
                     "waist", "hip") )

ks_model$e2
ks_model$e2n
ks_model$en

#rownames(ks_model$Y) <- rownames(ks_model$X[1:ncol(ks_model$X)-1])
#cov(t(ks_model$Y))
#ei <- eigen(cov(as.data.frame(t(ks_model$X))))
#ei$vectors
#ei$values

#cov(as.matrix(as.data.frame(t(ks_model$X))) %*% ei$vectors)
#cov(t(ks_model$X) %*% ei$vectors)

#t(ei$vectors) %*% rep_len(1,nrow(ei$vectors))

#t(ei$vectors) %*% ks_model$X
#matrix_symvect_mult(t(ei$vectors), row.names(ks_model$X))

df <- as.data.frame(t(ks_model$X))
ei <- eigen(cov(df))
ds <- as.data.frame(as.matrix(df) %*% ei$vectors)
colnames(ds) <- matrix_symvect_mult(t(ei$vectors), names(df))
cov(ds)
describe(ds[1])

# normalize variables by their range 
dim <- colnames(ds)
n_ds <-lapply(dim, norm_ds, ds)
norm.ds <- as.data.frame(n_ds)
colnames(norm.ds) <- rownames(ks_model$X)
cov(norm.ds)



ei <- eigen(cov(diabetes2))
ei$vectors[,1] %*% ei$vectors[,4]


# Dimesionality reduction
dim <- c("glyhb", "chol", "hdl", "ratio", "stab.glu", 
         "bp.1s", "bp.1d", "age", "height", "weight",
         "waist", "hip")
diabetes2 <- diabetes[complete.cases(diabetes[, dim]), dim]

dim3 <- c("chol", "hdl", "ratio", "bp.1s", "bp.1d", "age", 
         "height", "weight", "waist", "hip")
diabetes3 <- diabetes[complete.cases(diabetes[, dim]), dim3]


ds <- ks_lm_dim_red(diabetes2, dim, sd_dim=0.05)

ds <- ks_lm_dim_red(diabetes2, dim, n_dim=2)

ds <- ks_lm_dim_red(diabetes3, dim3, n_dim=2, norm=FALSE, std=FALSE)
ds <- ks_eigen_rotate_cov(ds, std=FALSE)
ds <- ks_lm_dim_red(ds, eigen=FALSE)

ds <- ks_lm_dim_red(diabetes2, dim, n_dim=3, norm=FALSE, std=FALSE, eigen=FALSE)
ds <- ks_eigen_rotate_cor(ds, std=FALSE)
ds <- ks_lm_dim_red(ds, eigen=FALSE)

ds <- ks_lm_dim_red(diabetes2, dim, eigen=FALSE)
ds <- ks_lm_dim_red(diabetes2, dim)

#PCA
ds <- ks_eigen_rotate_cov(diabetes2)
#ds <- ks_norm_ds(ds)
names(ds) <- c("v1","v2","v3","v4", "v5","v6", "v7", "v8", "v9", "v10","v11","v12")
pairs.panels(ds)
describe(ds)

ds <- diabetes3
ds <- ks_norm_ds(ds)
ds <- ks_eigen_rotate_cov(ds)
#ds <- ks_norm_ds(ds)
names(ds) <- c("v1","v2","v3","v4", "v5","v6", "v7", "v8", "v9", "v10")
pairs.panels(ds)
describe(ds)

ds <- ds[,c(1,3,4,5)]
ds <- ks_lm_dim_red(ds, eigen=FALSE)

#diabetes2 <- diabetes
#diabetes2$height_weight <- diabetes2$height / diabetes2$weight
#diabetes2$waist_hip <- diabetes2$waist / diabetes2$hip
#dim <- c("glyhb", "chol", "hdl", "ratio", "stab.glu",
#  "bp.1s", "bp.1d", "age", "height_weight",
#  "waist_hip")
#dim <- c("glyhb", "chol", "hdl",
#         "bp.1d", "age", "waist_hip")
#ds <- ks_lm_dim_red(diabetes2, dim, n_dim=1)


resp_name <- names(ds)[1]
names(ds)[1] <- "v1"
ggplot(ds, aes(x=v1))+
  geom_histogram(data=ds,aes(y=..density..),fill="blue", bins=40, alpha=0.5)+
  geom_density(data=ds,adjust=0.5) 


# +Glycosylated hemoglobin pdf
ds$glyhb <- diabetes2$glyhb
ds_nd <- ds[ds$glyhb<6,]
ds_pd <- ds[ds$glyhb>=6,]

ggplot(ds, aes(x=v1))+
  geom_histogram(data=ds, fill="blue", bins=40, alpha=0.6)+
  geom_histogram(data=ds_nd, fill="green", bins=40, alpha=0.6)+
  geom_histogram(data=ds_pd, fill="red", bins=40, alpha=0.6)
  
ggplot(ds, aes(x=v1, colour=glyhb))+
  geom_point(data=ds, y=.5, alpha=0.5)+
  geom_point(data=ds_nd, y=.75, alpha=0.5)+
  geom_point(data=ds_pd, y=.25, alpha=0.5)+
  scale_colour_gradientn(colours=rainbow(4))


# +Systolic blood pressure pdf
ds$bp.1s <- diabetes2$bp.1s
ds_nh <- ds[ds$bp.1s<150,]
ds_ph <- ds[ds$bp.1s>=150,]

ggplot(ds, aes(x=v1))+
  geom_histogram(data=ds, fill="blue", bins=40, alpha=0.6)+
  geom_histogram(data=ds_nh, fill="green", bins=40, alpha=0.6)+
  geom_histogram(data=ds_ph, fill="red", bins=40, alpha=0.6)


# +Choleterol pdf
ds$chol <- diabetes2$chol
ds_nc <- ds[ds$chol<230,]
ds_pc <- ds[ds$chol>=230,]

ggplot(ds, aes(x=v1))+
  geom_histogram(data=ds, fill="blue", bins=40, alpha=0.6)+
  geom_histogram(data=ds_nc, fill="green", bins=40, alpha=0.6)+
  geom_histogram(data=ds_pc, fill="red", bins=40, alpha=0.6)


#order.diabetes$syn <- as.integer( ds[[1]]/25 )
clean.diabetes$syn <- ds[[1]]
clean.diabetes$syn_c <- 2
clean.diabetes[clean.diabetes$syn<0.2, "syn_c"] <- 1
#clean.diabetes[clean.diabetes$syn>=350 & clean.diabetes$syn<500, "syn_c"] <- 2
clean.diabetes[clean.diabetes$syn>=0.35, "syn_c"] <- 3
#clean.diabetes[clean.diabetes$syn>=0.5, "syn_c"] <- 4
cloud(chol ~ glyhb * bp.1s, clean.diabetes, groups=syn_c)


colnames(diabetes2)
describe(clean.diabetes)
describe(ds[[1]])
describe(ds_nd[[1]])
describe(ds_pd[[1]])


#run immediatelly after ks_lm_dim_red
names(ds)[1:3] <- c ("v1", "v2", "v3")
ds$glyhb <- diabetes2$glyhb
ds$glyhb_c <- 0
ds[ds$glyhb>=7, "glyhb_c"] <- 1
cloud(v5 ~ v3 * v4, ds, groups=glyhb_c, pretty=TRUE)


#run immediatelly after ks_lm_dim_red(dim=2)
names(ds)[1:2] <- c ("v1", "v2")
ds$glyhb <- diabetes2$glyhb
ds_nd <- ds[ds$glyhb<7,]
ds_pd <- ds[ds$glyhb>=7,]

ggplot(ds, aes(x=v1, y=v2, colour=glyhb))+
  geom_point(data=ds, alpha=0.5)+
  geom_point(data=ds_nd, alpha=0.5)+
  geom_point(data=ds_pd, alpha=0.5)+
  scale_colour_gradientn(colours=rainbow(4))


# PCA. Run immediatelly after eigen rotation
names(ds) <- c("v1", "v2", "v3", "v4", "v5", "v6", "v7", "v8","v9", "v10")
ds$glyhb <- diabetes2$glyhb
ds_nd <- ds[ds$glyhb<6,]
ds_pd <- ds[ds$glyhb>=6,]

p1 <- ggplot(ds, aes(x=v1))+
  geom_histogram(data=ds, fill="blue", bins=40, alpha=0.6)+
  geom_histogram(data=ds_nd, fill="green", bins=40, alpha=0.6)+
  geom_histogram(data=ds_pd, fill="red", bins=40, alpha=0.6)

p2 <- ggplot(ds, aes(x=v2))+
  geom_histogram(data=ds, fill="blue", bins=40, alpha=0.6)+
  geom_histogram(data=ds_nd, fill="green", bins=40, alpha=0.6)+
  geom_histogram(data=ds_pd, fill="red", bins=40, alpha=0.6)

p3 <- ggplot(ds, aes(x=v3))+
  geom_histogram(data=ds, fill="blue", bins=40, alpha=0.6)+
  geom_histogram(data=ds_nd, fill="green", bins=40, alpha=0.6)+
  geom_histogram(data=ds_pd, fill="red", bins=40, alpha=0.6)

p4 <- ggplot(ds, aes(x=v4))+
  geom_histogram(data=ds, fill="blue", bins=40, alpha=0.6)+
  geom_histogram(data=ds_nd, fill="green", bins=40, alpha=0.6)+
  geom_histogram(data=ds_pd, fill="red", bins=40, alpha=0.6)

p5 <- ggplot(ds, aes(x=v5))+
  geom_histogram(data=ds, fill="blue", bins=40, alpha=0.6)+
  geom_histogram(data=ds_nd, fill="green", bins=40, alpha=0.6)+
  geom_histogram(data=ds_pd, fill="red", bins=40, alpha=0.6)

p6 <- ggplot(ds, aes(x=v6))+
  geom_histogram(data=ds, fill="blue", bins=40, alpha=0.6)+
  geom_histogram(data=ds_nd, fill="green", bins=40, alpha=0.6)+
  geom_histogram(data=ds_pd, fill="red", bins=40, alpha=0.6)

p7 <- ggplot(ds, aes(x=v7))+
  geom_histogram(data=ds, fill="blue", bins=40, alpha=0.6)+
  geom_histogram(data=ds_nd, fill="green", bins=40, alpha=0.6)+
  geom_histogram(data=ds_pd, fill="red", bins=40, alpha=0.6)

p8 <- ggplot(ds, aes(x=v8))+
  geom_histogram(data=ds, fill="blue", bins=40, alpha=0.6)+
  geom_histogram(data=ds_nd, fill="green", bins=40, alpha=0.6)+
  geom_histogram(data=ds_pd, fill="red", bins=40, alpha=0.6)

p9 <- ggplot(ds, aes(x=v9))+
  geom_histogram(data=ds, fill="blue", bins=40, alpha=0.6)+
  geom_histogram(data=ds_nd, fill="green", bins=40, alpha=0.6)+
  geom_histogram(data=ds_pd, fill="red", bins=40, alpha=0.6)

p10 <- ggplot(ds, aes(x=v10))+
  geom_histogram(data=ds, fill="blue", bins=40, alpha=0.6)+
  geom_histogram(data=ds_nd, fill="green", bins=40, alpha=0.6)+
  geom_histogram(data=ds_pd, fill="red", bins=40, alpha=0.6)

multiplot(p1,p2,p3,p4,p5,p6,p7,p8,p9,p10, cols=3)

p11 <- ggplot(ds, aes(x=v11))+
  geom_histogram(data=ds, fill="blue", bins=40, alpha=0.6)+
  geom_histogram(data=ds_nd, fill="green", bins=40, alpha=0.6)+
  geom_histogram(data=ds_pd, fill="red", bins=40, alpha=0.6)

p12 <- ggplot(ds, aes(x=v12))+
  geom_histogram(data=ds, fill="blue", bins=40, alpha=0.6)+
  geom_histogram(data=ds_nd, fill="green", bins=40, alpha=0.6)+
  geom_histogram(data=ds_pd, fill="red", bins=40, alpha=0.6)


# Multiple plot function
#
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
#
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}