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
  rownames(model$Y) <- model$DimNames
  }
  
  model
}

# Reduce dimensionality of dataframe 'df' on variables listed in 'dim'
# until 'n_dim" dimansions left
ks_lm_dim_red <- function(df, dim, n_dim=1, reorder=1, sd_dim=0, hb=1){
  clean.df <- df[complete.cases(df[, dim]), dim]

  # normalize variables by their range  
  n_ds <-lapply(dim, norm_ds, clean.df)
  names(n_ds) <- dim
  norm.df <- as.data.frame(n_ds)
  
  #order variables by rss of their univariate regressions
  v_dim <- sapply(dim, norm_rss_ds, norm.df)
  i_dim <- order(v_dim[2,])
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
    
    #print(names(ds)[1])
    
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
    
    #reorder synthetic variables by rss on each iteration
    if(reorder){
      tmp_dim <- colnames(ds)
      v_dim <- sapply(tmp_dim, norm_rss_ds, ds)
      i_dim <- order(v_dim[2,])
      ds <- ds[, i_dim]
    }
      
  }
  
  ds
}

# Create list of rss for univariate regression on each normalized variable
norm_rss_ds <- function(item, norm.diabetes){
  i_range <-range(norm.diabetes[,item])
  norm.diabetes[,item] <- norm.diabetes[,item]/(i_range[2]-i_range[1])
  
  predict_dim <- colnames(norm.diabetes)
  predict_dim <- predict_dim[which(predict_dim!=item)]
  
  mod <- ks_lm( norm.diabetes, item, predict_dim, full=0, error=1 )
  
  dim_rss <- c(item, mod$en)
  dim_rss
}

# Normalize a variable by its range
norm_ds <- function(item, norm.diabetes){
  i_range <-range(norm.diabetes[,item])
  
  norm_col <- (norm.diabetes[,item]-i_range[1])/(i_range[2]-i_range[1])
  norm_col
}

# Normalize st.dev. by mean
norm_sd <- function(item){
  sd(item)/mean(item)
}