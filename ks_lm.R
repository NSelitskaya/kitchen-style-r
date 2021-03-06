#####################################################
# Multivariable linear stepwise regression functions
#
# Author: Natalya Selitskaya
#
# Initial revision date: Oct 20 2017
#####################################################

# Linear univariate regression that minimizes rss   
# of variable 'yname' of 'df' dataset
ks_lm <- function (df, yname, xnames, full=1, error=0){
  all.names = c(unlist(xnames), unlist(yname))
  clean.df <- df[complete.cases(df[, all.names]), all.names]
  
  nr <- nrow(clean.df)
  nc <- ncol(clean.df)
  
  model <- list()
  model$X <- matrix(do.call(rbind,clean.df), nc)
  rownames(model$X) <- all.names
  
  x <- clean.df
  x[, yname] <- NULL
  x[, nc] <- rep_len(1, nr)

  y <- clean.df[, yname]
  
  X <- matrix( do.call(cbind,x), nrow(x) )

  b1 <- solve(t(X) %*% X) %*% t(X) %*% y
  
  if(full | error){
    e <- X %*% b1 - y
    model$e2 <- t(e) %*% e
    model$e2n <- model$e2/nr
    model$en <- sqrt(model$e2)/nr
  }
  
  if(full){
    model$Xcov <- cov(t(ks_model$X))
    
    #Projection onto 0 member of quotient space X/Y
    model$R <- diag(rep_len(1, nc))
    model$R[nc,1:nc] <- b1
    model$R[nc,nc] <- 0
  
    #X to Y Basis rotation
    #||x||=||y|| (a 0 a)(1 0 1)
    #a1 <- rep_len(1/sqrt(2),nc)
    #||y||^2 = 1 (a 0 a)(1 0 1)
    #a1 <- rep_len(0.5,nc)
    #||x||=||y|| (a 0 a)(1 0 1/t)
    #a1 <- sqrt((1+as.vector(b1)**2)/(1+as.vector(b1))**2)
    #||y||^2 = 1 (a 0 a)(1 0 1/t)
    #a1 <- sqrt(1/(1+as.vector(b1))**2)
    #model$Nm1 <- diag(a1)
    #model$Nm1[,nc] <- a1
  
    da1 <- 1/(1+as.vector(b1)**2)
    model$Nm1 <- diag(da1)
    #db1 <- 1/(1/as.vector(b1)+as.vector(b1))
    #model$Nm1[,nc] <- db1
    dbt1 <- 1/(1/as.vector(b1)**2+1)
    model$Nm1[,nc] <- dbt1

    model$Nm1 <- model$Nm1[-nc,]
    

    model$h0 <- rep_len(0, nc)
    model$h0[nc] <- b1[ncol(b1)]
  
    model$X0 <- model$R %*% model$X
    rownames(model$X0) <- all.names
    model$Xl <- model$X0[, 1:nr] + model$h0
    rownames(model$Xl) <- all.names
  
    model$Nm1R <- model$Nm1 %*% model$R
    model$DimNames <- matrix_symvect_mult(model$Nm1R, all.names)
  
    model$Y <- model$Nm1 %*% model$X0
    model$Ycov <- cov(t(ks_model$Y))
    rownames(model$Y) <- model$DimNames
  }
  
  model
}

# Reduce dimensionality of dataframe 'df' on variables listed in 'dim'
# until 'n_dim" dimansions left
ks_lm_dim_red <- function(df, dim=NULL, n_dim=1, 
                          ord_asc=TRUE, reorder=TRUE, norm=TRUE, eigen=TRUE, 
                          sd_dim=0, std=TRUE, hb=TRUE){
  if(identical(dim, NULL)){
    dim <- colnames(df)
  }
  clean.df <- df[complete.cases(df[, dim]), dim]
  
  #rotate dimensions to eigenvectors
  if(eigen)
    pc.df <- ks_eigen_rotate_cov(clean.df)
  else
    pc.df <- clean.df
  
  # normalize variables by their range
  if(norm)
    ds <- ks_norm_ds(ds)
  else
    norm.df <- pc.df
  
  #order variables by rss of their univariate regressions
  dim <- colnames(norm.df)
  v_dim <- sapply(dim, norm_rss_ds, norm.df)
  i_dim <- order(v_dim[2,], decreasing = !ord_asc)
  order.df <- norm.df[, i_dim]
  
  # run univariate regressions for variables in rss order 
  # until n_dim dimensions left
  ds <- order.df
  n_max <- length(dim)-n_dim
  i <- 0
  while(i < n_max){
    
    if(hb)
      print(i)
    
    resp_name <- colnames(ds)[1]
    pred_names <- colnames(ds)[-1]
    
    mod <- ks_lm(ds, resp_name, pred_names)
    ds <- as.data.frame(t(mod$Y))
    
    i <- i+1
    if(i >= n_max)
      break
    
    # check if we already have effectively eigenvector (all coordinates are the same)
    if(sd_dim > 0){
      m_sd <- mean(sapply(as.data.frame(t(ds)), norm_sd))
      if(m_sd < sd_dim){
        # convert length to 1D eigenbasis
        col_name <- names(ds)[1]
        ds <- as.data.frame((ds[,1] * sqrt(length(dim)-i)))
        names(ds)[1] <- sym_lc_mult(col_name, sqrt(length(dim)-i))
        break
      }
    }
    
    #rotate dimensions to eigenvectors
    if(eigen)
      ds <- ks_eigen_rotate_cov(ds)
    
    #reorder synthetic variables by rss on each iteration
    if(reorder){
      tmp_dim <- colnames(ds)
      v_dim <- sapply(tmp_dim, norm_rss_ds, ds)
      i_dim <- order(v_dim[2,])
      ds <- ds[, i_dim]
    }
      
  }
  
  if(std)
    ds <- ks_std_ds(ds)
  
  ds
}

ks_eigen_rotate_cov <- function(df, std=FALSE, sym=FALSE){
  
  ei <- eigen(cov(df))
  #print(ei$values)
  #print(ei$vectors)
  
  #t(ei$vectors) %*% t(as.matrix(df))
  ds <- as.data.frame(as.matrix(df) %*% ei$vectors)
  if(sym)
    colnames(ds) <- matrix_symvect_mult(t(ei$vectors), names(df))
  
  model <- list()
  model$A <- ei$vectors
  model$An1 <- solve(ei$vectors)
  
  if(std)
    ds <- ks_std_ds(ds)
  
  model$ds <- ds
  model
}

ks_eigen_rotate_cor <- function(df, std=FALSE){
  
  ei <- eigen(cor(df))
  #print(ei$values)
  #print(ei$vectors)
  
  ds <- as.data.frame(as.matrix(df) %*% ei$vectors)
  colnames(ds) <- matrix_symvect_mult(t(ei$vectors), names(df))
  
  model <- list()
  model$A <- ei$vectors
  model$An1 <- solve(ei$vectors)
  
  if(std)
    ds <- ks_std_ds(ds)
  
  model$ds <- ds
  model
}

ks_norm_ds <- function(ds){
  dimn <- colnames(ds)
  n_ds <- lapply(dimn, norm_ds, ds)
  norm.ds <- as.data.frame(n_ds)
  
  dimn <- sapply(dimn, norm_names_ds, ds)
  colnames(norm.ds) <- dimn
  
  norm.ds
}

ks_std_ds <- function(ds){
  dimn <- colnames(ds)
  n_ds <- lapply(dimn, std_norm_ds, ds)
  norm.ds <- as.data.frame(n_ds)
  colnames(norm.ds) <- dimn
  
  norm.ds
}

# Create list of rss for univariate regression on each normalized variable
norm_rss_ds <- function(item, norm.diabetes){
  i_range <- range(norm.diabetes[,item])
  norm.diabetes[,item] <- norm.diabetes[,item]/(i_range[2]-i_range[1])
  
  predict_dim <- colnames(norm.diabetes)
  predict_dim <- predict_dim[which(predict_dim!=item)]
  
  mod <- ks_lm( norm.diabetes, item, predict_dim, full=0, error=1 )
  
  dim_rss <- c(item, mod$en)
  dim_rss
}

# Normalize a variable by its range
norm_ds <- function(item, norm.diabetes){
  i_range <- range(norm.diabetes[,item])

  scale_v = 1/(i_range[2]-i_range[1])
  norm_col <- norm.diabetes[,item]*scale_v
  
  norm_col
}

norm_names_ds <- function(item, norm.diabetes){
  i_range <- range(norm.diabetes[,item])
  scale_v = 1/(i_range[2]-i_range[1])
  
  sym_lc_mult(item, scale_v)
}

# Normalize and standartize to 0-1 a variable by its range
std_norm_ds <- function(item, norm.diabetes){
  i_range <- range(norm.diabetes[,item])
  
  norm_col <- (norm.diabetes[,item]-i_range[1])/(i_range[2]-i_range[1])
  norm_col
}

# Normalize st.dev. by mean
norm_sd <- function(item){
  sd(item)/mean(item)
}

ks_kmeans_1d_means <- function(ds, var, k){
  
  Xk <- as.matrix(ds[,var]^k)
  X <- matrix( rep_len(1, nrow(ds)), nrow=nrow(ds), ncol=1 )
  for(i in 1:(k-1)){
    X <- cbind(matrix(ds[,var]^i), X)
  } 
  
  Xt <- t(X)
  
  C <- -1 * solve(t(X) %*% X) %*% Xt %*% Xk
  
  model <- list()
  model$SSD <- t(Xk + X %*% C) %*% (Xk + X %*% C)
  
  Cl <- rev(c(1,C))
  model$Mu <- Re(polyroot(Cl))
  
  model
}


ks_kmeans_1d_clusters <- function(ds, var, Mu, dd_logic=FALSE){
  
  k <- length(Mu)
  
  d <- as.data.frame(ds[,var])
  names(d) <- c(var)
  
  # set no cluster association, yet
  d[,"Cls"] <- 0
 
  
  all_lables <- seq(1,k)
  
  if(dd_logic){
    
    # add disjoint distances form a point to all other jth means (except the current ith one)
    for(i in 1:k){
      d[,as.character(i)] <- 1 
      other_lables <- all_lables[-i]
      for(j in other_lables){
        d[,as.character(i)] <- d[,as.character(i)] * (d[,var] - Mu[j])^2
      }
    }
    
    for(i in 1:k){
      d[,as.character(k+i)] <- 0 
      other_lables <- all_lables[-i]
      for(j in other_lables){
        # calculate how many times disjoint distance to all but ith mean is bigger than other distances to all but jth means
        d[,as.character(k+i)] <- d[,as.character(k+i)] + as.integer(d[,as.character(i)] > d[,as.character(j)])
      }
    }
    
    # mark as particular cluster if ith distance is the smallest
    for(i in 1:k){
      d[d[,as.character(k+i)]==(k-1) & d[,"Cls"]==0,"Cls"] <- i
      
      # if other cluster is assigned, set it to fuzzy
      d[d[,as.character(k+i)]==(k-1) & d[,"Cls"]!=i,"Cls"] <- d[d[,as.character(k+i)]==(k-1) & d[,"Cls"]!=i,"Cls"]+k+i
    }
    
  }
  else{
    
    # add distances from a point to each cluster mean
    for(i in 1:k){
      d[,as.character(i)] <- (d[,var] - Mu[i])^2
    }

    for(i in 1:k){
      d[,as.character(k+i)] <- 0 
      other_lables <- all_lables[-i]
      for(j in other_lables){
        # calculate how many times distance to ith mean is smaller than other distances jth means
        d[,as.character(k+i)] <- d[,as.character(k+i)] + as.integer(d[,as.character(i)] < d[,as.character(j)])
      }
    }
  
    # mark as particular cluster if ith distance is the smallest
    for(i in 1:k){
      d[d[,as.character(k+i)]==(k-1),"Cls"] <- i
    }
  }
  
  ds[,"Cls"] <- d[,"Cls"]
  
  ds
}


ks_kmeans_nd_means <- function(ds, vars, k){

  n <- length(vars)
  
  Xk <- as.matrix(ds[,vars[1]]^k)
  for(l in 2:n){
    Xk[,1] <- Xk[,1] + ds[,vars[l]]^k
  }
  
  X <- matrix(ds[,vars[1]], nrow=nrow(ds), ncol=1)
  if(k>=2){
    for(i in 2:(k-1)){
      X <- cbind(matrix(ds[,vars[1]]^i), X)
    }
  }
  for(l in 2:n){
    for(i in 1:(k-1)){
      X <- cbind(matrix(ds[,vars[l]]^i), X)
    } 
  }
  
  Xt <- t(X)
  
  C <- -1 * solve(Xt %*% X) %*% Xt %*% Xk
  
  model <- list()
  model$SSD <- t(Xk + X %*% C) %*% (Xk + X %*% C)
  
  Cl <- rev(c(1, C[1:(k-1)], 0))
  for(l in 1:(n-1)){ 
    Cl <- c(rev( c(1, C[((k-1)*l+1):((k-1)*l+k-1)], 0) ), Cl)
  }
  Clm <- matrix(Cl, nrow=k+1, ncol=n)
  
  model$Mu <- matrix( rep_len(0, n*k), nrow=k, ncol=n )
  for(l in 1:n){ 
    model$Mu[,l] <- Re(polyroot(Clm[,l]))
  }
  
  model
}

ks_kmeans_nd_clusters <- function(ds, vars, Mu, dd_logic=FALSE){
  #vars = c("V5",V6","V7")
  
  n <- length(vars)  
  k <- nrow(Mu)
  
  d <- as.data.frame(ds[,vars])
  names(d) <- vars
  
  # set no cluster association, yet
  d[,"Cls"] <- 0
  
  if(dd_logic){
    
    # add disjoint distances form a point to all other jth means (except the current ith one)
    for(i in 1:k){
      d[,as.character(i)] <- 1 
      other_lables <- all_lables[-i]
      for(j in other_lables){
        d[,as.character(i)] <- d[,as.character(i)] * (d[,var] - Mu[j])^2
      }
    }
    
    for(i in 1:k){
      d[,as.character(k+i)] <- 0 
      other_lables <- all_lables[-i]
      for(j in other_lables){
        # calculate how many times disjoint distance to all but ith mean is bigger than other distances to all but jth means
        d[,as.character(k+i)] <- d[,as.character(k+i)] + as.integer(d[,as.character(i)] > d[,as.character(j)])
      }
    }
    
    # mark as particular cluster if ith distance is the smallest
    for(i in 1:k){
      d[d[,as.character(k+i)]==(k-1) & d[,"Cls"]==0,"Cls"] <- i
      
      # if other cluster is assigned, set it to fuzzy
      d[d[,as.character(k+i)]==(k-1) & d[,"Cls"]!=i,"Cls"] <- d[d[,as.character(k+i)]==(k-1) & d[,"Cls"]!=i,"Cls"]+k+i
    }
    
  }
  else{
    
    # add distances from a point to each cluster mean
    for(i in 1:k){
      d[,as.character(i)] <- (d[,vars[1]] - Mu[i,1])^2
    }
    for(l in 2:n){
      for(i in 1:k){
        d[,as.character(i)] <- d[,as.character(i)] + (d[,vars[l]] - Mu[i,l])^2
      }
    }
  
    all_lables <- seq(1,k)
    
    for(i in 1:k){
      d[,as.character(k+i)] <- 0 
      other_lables <- all_lables[-i]
      for(j in other_lables){
        # calculate how many times distance to ith mean is smaller than other distances jth means
        d[,as.character(k+i)] <- d[,as.character(k+i)] + as.integer(d[,as.character(i)] < d[,as.character(j)])
      }
    }
    
    # mark as particular cluster if ith distance is the smallest
    for(i in 1:k){
      d[d[,as.character(k+i)]==(k-1),"Cls"] <- i
    }
  }
  
  ds[,"Cls"] <- d[,"Cls"]
  
  ds
}
