# Script for calculating community-weighted variance (CWV) and community-weighted mean (CWM) for multiple sites

# Requirements: 
# a species abundance data table ('a') with sites (rows) and species (columns)
# a trait value data table ('t') with species (rows) and traits (columns)

# Attention: make sure to verify that the species list in table 'a' is at the same 

# Load
library(Hmisc)

Func.variance <- function(t, a){
  m <- matrix(0, dim(a)[1], dim(t)[2])
  v <- matrix(0, dim(a)[1], dim(t)[2])
  sum.a <- apply(a, 1, sum)
  a <- a/sum.a
   for(i in 1:dim(a)[1]) {
    for(j in 1:dim(t)[2]) {
    m[i, j] <- weighted.mean(t[, j], a[i, ])
    v[i, j] <- wtd.var(t[, j], weights = a[i, ], 
                       method = "ML",  na.rm = TRUE, normwt = TRUE)
    }
   }
  colnames(v) <- colnames(t)
  rownames(v) <- rownames(a)
  colnames(m) <- colnames(t)
  rownames(m) <- rownames(a)
  return(list(m = m, v = v))
  return(list(v = v))
}




