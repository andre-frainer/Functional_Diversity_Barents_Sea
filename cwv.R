# Community-weighted variance

# abund |2300 x 54|
# trait |54 x 11|
# cwm   |2300 x 11|

# Weighted variance
library(Hmisc)
# wtd.var

CWV <- function(x, a){
  m <- matrix(0, dim(a)[1], dim(x)[2])
  v <- matrix(0, dim(a)[1], dim(x)[2])
  sum.a <- apply(a, 1, sum)
  a <- a/sum.a
   for(i in 1:dim(a)[1]) {
    for(j in 1:dim(x)[2]) {
    m[i, j] <- weighted.mean(x[, j], a[i, ])
    v[i, j] <- wtd.var(x[, j], weights = a[i, ], 
                       method = "ML",  na.rm = TRUE, normwt = TRUE)
    }
   }
  colnames(v) <- colnames(x)
  rownames(v) <- rownames(a)
  colnames(m) <- colnames(x)
  rownames(m) <- rownames(a)
  return(list(m = m, v = v))
  return(list(v = v))
}

cw <- function(x, w){
  a <- x %>% 
    group_by(., year) %>% 
    mutate(map(x, sum))
  aa <- x/a
  purrr::map(wtd.var(w, weights = aa, 
              method = "ML",  na.rm = TRUE, normwt = TRUE))
}




