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


# Dimesionality reduction
diabetes2 <- diabetes
dim <- c("glyhb", "chol", "hdl", "ratio", "stab.glu", 
         "bp.1s", "bp.1d", "age", "height", "weight",
         "waist", "hip")
ds <- ks_lm_dim_red(diabetes2, dim, sd_dim=0.05)

ds <- ks_lm_dim_red(diabetes2, dim, n_dim=3)

ds <- ks_lm_dim_red(diabetes2, dim)


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
clean.diabetes <- diabetes2[complete.cases(diabetes2[, dim]), dim]
ds$glyhb <- clean.diabetes$glyhb
ds$glyhb_c <- 0
ds[ds$glyhb>=7, "glyhb_c"] <- 1

ds_nd <- ds[ds$glyhb<7,]
ds_pd <- ds[ds$glyhb>=7,]

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
clean.diabetes <- diabetes2[complete.cases(diabetes2[, dim]), dim]
ds$bp.1s <- clean.diabetes$bp.1s

ds_nh <- ds[ds$bp.1s<150,]
ds_ph <- ds[ds$bp.1s>=150,]

ggplot(ds, aes(x=v1))+
  geom_histogram(data=ds, fill="blue", bins=40, alpha=0.6)+
  geom_histogram(data=ds_nh, fill="green", bins=40, alpha=0.6)+
  geom_histogram(data=ds_ph, fill="red", bins=40, alpha=0.6)

# +Choleterol pdf
clean.diabetes <- diabetes2[complete.cases(diabetes2[, dim]), dim]
ds$chol <- clean.diabetes$chol

ds_nc <- ds[ds$chol<250,]
ds_pc <- ds[ds$chol>=250,]

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
clean.diabetes <- diabetes2[complete.cases(diabetes2[, dim]), dim]
ds$glyhb <- clean.diabetes$glyhb
ds$glyhb_c <- 0
ds[ds$glyhb>=7, "glyhb_c"] <- 1
cloud(v1 ~ v2 * v3, ds, groups=glyhb_c, pretty=TRUE)

