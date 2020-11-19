fn1 <- normalizePath(file.path('../tables/res_basic.rds'))
fn2 <- normalizePath(file.path('../tables/res_heter.rds'))

res1 <- readRDS(fn1)
res2 <- readRDS(fn2)

# Helper function
get_disprots <- function(res, thresh = -1, return_inds = FALSE){
  sel <- res$selected
  L <- length(sel)
  has_eff <- rep(0, L)
  for(i in 1:L){
    a  <- as.character(sel[i])
    ok <- grepl('diseaseAge', a)
    if(ok){
      if(thresh != -1){
        aff <- res$N_b05[i]
      }else{
        aff <- 1
      }
      
      if(aff >= thresh){
        has_eff[i] <- 1
      }
    }
  }
  idx   <- which(has_eff==1)

  # Return
  if(return_inds){
    return(idx)
  }else{
    names <- res$name
    names <- as.character(names[idx])
    return(names)
  }
}

# Get names of some interesting proteins
thresh <- 3
aa <- get_disprots(res1)
bb <- get_disprots(res2, thresh = thresh)
cc <- intersect(aa,bb)

info <- paste0('Proteins found by homogeneous model: ', length(aa), '\n',
               'Proteins found by heterogeneous model: ', length(bb), 
               ' (using threshold of ', thresh, ' affected individuals) \n',
               'Intersection: ', length(cc), '\n')
cat(info)
print(aa)
print(bb)
print(cc)