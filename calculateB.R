## sfs = site frequency spectrum of GC-changing sites with GC-frequency ranges or discrete freqeuncies as categories
## neutSFS = neutral site frequency spectrum (from A<->T and G<->C sites)
## fr = determines which frequency category to keep - if set to 1, all frequency categories are considered when calculating B, 
## if set to 2, the singletons will be excluded, if set to 3, singletons and doubletons are excluded, etc...
## sfsSize = number of frequency categories in the SFS

calculateB <- function(sfs, neutSFS, fr, sfsSize){
  ## make the neutral SFS symmetric
  if(sfsSize%%2==1){
    neutSFS2 <- c(c(neutSFS[1:floor(sfsSize/2)]+rev(neutSFS[(ceiling(sfsSize/2)+1):sfsSize])), 2*neutSFS[ceiling(sfsSize/2)],
                  rev(c(neutSFS[1:floor(sfsSize/2)]+rev(neutSFS[(ceiling(sfsSize/2)+1):sfsSize]))))
  }else{
    neutSFS2 <- c(c(neutSFS[1:(sfsSize/2)]+rev(neutSFS[(sfsSize/2):sfsSize])),
                  rev(c(neutSFS[1:(sfsSize/2)]+rev(neutSFS[(sfsSize/2):sfsSize]))))
  }
  
  ## calculate r_y coefficients
  neutRel2 <- neutSFS2/sum(neutSFS2)
  exp <- (((sfsSize+1)/((1:sfsSize)*(sfsSize:1)))/sum((sfsSize+1)/((1:sfsSize)*(sfsSize:1))))
  rs <-  neutRel2/exp
  rs <- rs[fr:(sfsSize-(fr-1))]
  
  ## calculate B
  M = sfsSize + 1
  vy = fr:(sfsSize-(fr-1))
  margData = (sfs)[fr:(sfsSize-(fr-1))]/sum((sfs)[fr:(sfsSize-(fr-1))])
  vcoeff=rs*M/(vy*(M-vy))*((sum(vy*margData)/M)-vy/M)
  vr=polyroot(vcoeff)
  realRoots=Re(vr[abs(Im(vr))<0.000001])
  return(log(max(realRoots))*(M))
}
