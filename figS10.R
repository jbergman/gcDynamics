library(ggplot2)

## load data
humChr <- c("chr1","chr2","chr3","chr4","chr5","chr6","chr7","chr8","chr9","chr10","chr11","chr12","chr13","chr14","chr15","chr16","chr17","chr18","chr19","chr20","chr21","chr22")

for(i in 1:(length(humChr))){
  if(i==1){
    afYRI <- read.table(paste("chr1YRI.af", sep=""), head=T, sep = "\t")
  }else{
    temp <- read.table(paste(humChr[i],"YRI.af", sep=""), head=T, sep = "\t")
    afYRI <- rbind(afYRI, temp)
  }
}

## Figure S10
gcFreqTVcpg <- subset(afYRI, fType%in%c("A_G", "C_G", "G_G", "T_G")&mType=="TV")$freqGC

fS10 <- ggplot() + 
      geom_histogram(aes(x=gcFreqTVcpg,y=0.05*..density..),alpha=0.75,position='identity',binwidth=0.05) +
      geom_segment(aes(x = mean(gcFreqTVcpg) , y = 0, xend = mean(gcFreqTVcpg), yend = Inf), lty=1, col="black") +
      geom_segment(aes(x = median(gcFreqTVcpg) , y = 0, xend = median(gcFreqTVcpg), yend = Inf), lty=2, col="black") +
      xlab(expression(italic(f[GC]))) + ylab("Proportion") + theme_bw()

ggsave("figS10.pdf", fS10, width=10, height=7, units = "cm")
