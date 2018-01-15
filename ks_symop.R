###############################################################
# Symbolic multiplication and addition of matrices and vectors
#
# Author: Natalya Selitskaya
#
# Initial revision date: Nov 24 2017
###############################################################

#Examples:
str1 <- "V1"
str1 <- "2.*a-3.1*b-0.4*c-d"
str2 <- ".01*c+35*b+3.24*e-5.*d"
# Symbolic multiplication of string 'str1' by number 2.5
sym_lc_mult(str1, -3.283654)
sym_lc_mult(str1, -3.283654e-08)
# Symbolic sum of 2 strings 'str1' and 'st22'
sym_lc_add(str1, str2)


str_v <- c("a+.5*b","b-2*c","3*c+d")
str_u <- c("","","")
m <- matrix(c(1,2,3,4,5,6,7,8,9),3,byrow=TRUE) 
# Symbolic multiplication of numeric matrix 'm' by string vector 'str_v',
# result is a string vector 'str_u'
str_u <- matrix_symvect_mult(m, str_v)
str_u


# Symbolic multiplication of numeric matrix 'm' by string vector 'str_v',
# result is a string vector
matrix_symvect_mult <- function(m, str_v){
  nr <- nrow(m)
  nc <- ncol(m)
  str_u <- rep_len("", nr)
  for(i in 1:nr){
    for(j in 1:nc){
      str_u[i] <- sym_lc_add(str_u[i], sym_lc_mult(str_v[j],m[i,j]))
    }
  }
  
  str_u
}


# Symbolic sum of 2 strings
sym_lc_add <- function(str1, str2){
  str_out = NULL
  str_t1 <- gsub("-","+-", str1)
  l_tup1 <- strsplit(strsplit(str_t1, "[+]")[[1]],"[*]")
  str_t2 <- gsub("-","+-", str2)
  l_tup2 <- strsplit(strsplit(str_t2, "[+]")[[1]],"[*]")

  for(item1 in l_tup1){
    item1 <- item_conv(item1)
    found = FALSE
    
    for(item2 in l_tup2){
      item2 <- item_conv(item2)

      # item in both strings    
      if(identical(item1[2], item2[2])){
        found = TRUE
        mult = as.double(item1[1]) + as.double(item2[1])
      
        if(mult != 0){
          if(!is.null(str_out))
            str_out = paste(str_out,"+", sep="")
        
          str_out = paste(str_out, 
                      as.character(mult), 
                      "*", item1[2], sep="")
        }
      }
    
    }

    # item in first string but not in second  
    if(!found){
      if(as.double(item1[1]) != 0){
        if(!is.null(str_out))
          str_out = paste(str_out,"+", sep="")
    
        str_out = paste(str_out, item1[1], 
                      "*", item1[2], sep="")
      }
    }
  }

  # item in second string but not in first
  for(item2 in l_tup2){
    item2 <- item_conv(item2)
    found = FALSE
  
    for(item1 in l_tup1){
      item1 <- item_conv(item1)
    
      # item in both strings - not processing    
      if(identical(item1[2], item2[2])){
        found = TRUE
      }
    }
  
    if(!found){
      if(as.double(item2[1]) != 0){     
        if(!is.null(str_out))
          str_out = paste(str_out,"+", sep="")
    
        str_out = paste(str_out, item2[1], 
                        "*", item2[2], sep="")
      }
    }
  }

  if(is.null(str_out))
    str_out = "0"

  str_out2 <- gsub("[+]-","-", str_out)
  str_out2
}


# Symbolic multiplication of string 'str' by number m
sym_lc_mult <- function(str, m=1){
  if(identical(str,NULL))
    return(NULL)
    
  str_t <- gsub("-","+-", str)
  l_tup <- strsplit(strsplit(str_t, "[+]")[[1]],"[*]")

  res <- lapply(l_tup, sym_var_mult, m)

  str_out = NULL
  for(item in res){
    if(!identical(as.double(item[1]), 0)){
      if(!is.null(str_out))
        str_out = paste(str_out,"+", sep="")
  
      str_out = paste(str_out, item[1], "*", item[2], sep="")
    }
  }
  
  if(is.null(str_out))
    str_out = "0"
  
  str_out2 <- gsub("[+]-","-", str_out)
  str_out2
}

item_conv <- function(item){
  if(length(item)==1){
    if(is.na(as.double(item[1]))){
      if(identical(substr(item[1],1,1),"-"))
        item = c("-1", sub("-","",item[1]))
      else
        item = c("1", item[1])
    }
    else
      item = c("0", "0")
  }
  else if(length(item)==0){
    item = c("0", "0")
  }
  
  item
}

sym_var_mult <- function(item, m=1, round_len=5){
  item <- item_conv(item)
  
  mult_res <- round(as.double(item[1])*m, round_len)
  if(abs(mult_res) < 1e-04)
    mult_res <- 0
  
  c(as.character(mult_res), item[2])
}