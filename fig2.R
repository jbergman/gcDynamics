library(ggplot2)
library(cowplot)

humChr <- c("chr1","chr2","chr3","chr4","chr5","chr6","chr7","chr8","chr9","chr10","chr11","chr12","chr13","chr14","chr15","chr16","chr17","chr18","chr19","chr20","chr21","chr22")

################################################################################################################################ 
#######################################################--- 1000G YRI ---########################################################
################################################################################################################################

for(i in 1:(length(humChr))){
  if(i==1){
    afYRI <- read.table(paste("chr1YRI.af", sep=""), head=T, sep = "\t")
  }else{
    temp <- read.table(paste(humChr[i],"YRI.af", sep=""), head=T, sep = "\t")
    afYRI <- rbind(afYRI, temp)
  }
}

gcFreqTScpgYRI <- subset(afYRI, fType%in%c("A_G", "C_G", "G_G", "T_G")&mType=="TS")$freqGC
gcFreqTSnoncpgYRI <- subset(afYRI, !(fType%in%c("A_G", "C_G", "G_G", "T_G"))&mType=="TS")$freqGC
gcFreqTVcpgYRI <- subset(afYRI, fType%in%c("A_G", "C_G", "G_G", "T_G")&mType=="TV")$freqGC
gcFreqTVnoncpgYRI <- subset(afYRI, !(fType%in%c("A_G", "C_G", "G_G", "T_G"))&mType=="TV")$freqGC

countTScpgYRI <- c()
countTSnoncpgYRI <- c()
countTVnoncpgYRI <- c()

for(i in 0.05*(1:20)){
  countTScpgYRI <- c(countTScpgYRI, length(which(gcFreqTScpgYRI>(i-0.05) & gcFreqTScpgYRI<i)))
  countTSnoncpgYRI <- c(countTSnoncpgYRI, length(which(gcFreqTSnoncpgYRI>(i-0.05) & gcFreqTSnoncpgYRI<i)))
  countTVnoncpgYRI <- c(countTVnoncpgYRI, length(which(gcFreqTVnoncpgYRI>(i-0.05) & gcFreqTVnoncpgYRI<i)))
}

ylimYRI = max(countTScpgYRI/sum(countTScpgYRI))


dataYRI<- data.frame(cat = rep(c("0.00-0.05", "0.05-0.10", "0.10-0.15", "0.15-0.20", "0.20-0.25",
                             "0.25-0.30", "0.30-0.35", "0.35-0.40", "0.40-0.45", "0.45-0.50",
                             "0.50-0.55", "0.55-0.60", "0.60-0.65", "0.65-0.70", "0.70-0.75",
                             "0.75-0.80", "0.80-0.85", "0.85-0.90", "0.90-0.95", "0.95-1.00"), 3), 
                 freq = c(countTSnoncpgYRI/sum(countTSnoncpgYRI), countTScpgYRI/sum(countTScpgYRI), countTVnoncpgYRI/sum(countTVnoncpgYRI)),
                 mType = c(rep("TS (CpG-)", length(countTScpgYRI)), rep("TS (CpG+)", length(countTSnoncpgYRI)), rep("TV (CpG-)", length(countTVnoncpgYRI))))

f2a <- ggplot(dataYRI, aes(x=cat, y = freq, fill=mType)) + geom_bar(stat = "identity", position=position_dodge()) + theme_bw() + theme(legend.position = c(0.2,0.75), legend.box.background = element_rect(colour = "black"),axis.text.x = element_text(angle = 45, hjust = 1),legend.title = element_blank(),plot.title = element_text(hjust = 0.5))+
       scale_fill_manual(values = c("#377eb8", "#e41a1c", "#4daf4a")) + xlab(expression(italic(f[GC]))) + ylab("Proportion")+ggtitle("YRI")+ylim(0,ylimYRI)+
       geom_segment(aes(x = mean(gcFreqTSnoncpgYRI)*20 , y = 0, xend = mean(gcFreqTSnoncpgYRI)*20, yend = Inf), lty=1, col="#377eb8") +
       geom_segment(aes(x = median(gcFreqTSnoncpgYRI*20) , y = 0, xend = median(gcFreqTSnoncpgYRI)*20, yend = Inf), lty=2, col="#377eb8") +
       geom_segment(aes(x = mean(gcFreqTScpgYRI)*20 , y = 0, xend = mean(gcFreqTScpgYRI)*20, yend = Inf), lty=1, col="#e41a1c") +
       geom_segment(aes(x = median(gcFreqTScpgYRI)*20 , y = 0, xend = median(gcFreqTScpgYRI)*20, yend = Inf), lty=2, col="#e41a1c") +
       geom_segment(aes(x = mean(gcFreqTVnoncpgYRI)*20 , y = 0, xend = mean(gcFreqTVnoncpgYRI)*20, yend = Inf), lty=1, col="#4daf4a") +
       geom_segment(aes(x = median(gcFreqTVnoncpgYRI)*20 , y = 0, xend = median(gcFreqTVnoncpgYRI)*20, yend = Inf), lty=2, col="#4daf4a")
f2a

################################################################################################################################ 
#######################################################--- 1000G TSI ---########################################################
################################################################################################################################

for(i in 1:(length(humChr))){
  if(i==1){
    afTSI <- read.table(paste("chr1TSI.af", sep=""), head=T, sep = "\t")
  }else{
    temp <- read.table(paste(humChr[i],"TSI.af", sep=""), head=T, sep = "\t")
    afTSI <- rbind(afTSI, temp)
  }
}

gcFreqTScpgTSI <- subset(afTSI, fType%in%c("A_G", "C_G", "G_G", "T_G")&mType=="TS")$freqGC
gcFreqTSnoncpgTSI <- subset(afTSI, !(fType%in%c("A_G", "C_G", "G_G", "T_G"))&mType=="TS")$freqGC
gcFreqTVcpgTSI <- subset(afTSI, fType%in%c("A_G", "C_G", "G_G", "T_G")&mType=="TV")$freqGC
gcFreqTVnoncpgTSI <- subset(afTSI, !(fType%in%c("A_G", "C_G", "G_G", "T_G"))&mType=="TV")$freqGC

countTScpgTSI <- c()
countTSnoncpgTSI <- c()
countTVnoncpgTSI <- c()

for(i in 0.05*(1:20)){
  countTScpgTSI <- c(countTScpgTSI, length(which(gcFreqTScpgTSI>(i-0.05) & gcFreqTScpgTSI<i)))
  countTSnoncpgTSI <- c(countTSnoncpgTSI, length(which(gcFreqTSnoncpgTSI>(i-0.05) & gcFreqTSnoncpgTSI<i)))
  countTVnoncpgTSI <- c(countTVnoncpgTSI, length(which(gcFreqTVnoncpgTSI>(i-0.05) & gcFreqTVnoncpgTSI<i)))
}

dataTSI<- data.frame(cat = rep(c("0.00-0.05", "0.05-0.10", "0.10-0.15", "0.15-0.20", "0.20-0.25",
                                 "0.25-0.30", "0.30-0.35", "0.35-0.40", "0.40-0.45", "0.45-0.50",
                                 "0.50-0.55", "0.55-0.60", "0.60-0.65", "0.65-0.70", "0.70-0.75",
                                 "0.75-0.80", "0.80-0.85", "0.85-0.90", "0.90-0.95", "0.95-1.00"), 3), 
                     freq = c(countTSnoncpgTSI/sum(countTSnoncpgTSI), countTScpgTSI/sum(countTScpgTSI), countTVnoncpgTSI/sum(countTVnoncpgTSI)),
                     mType = c(rep("TS (CpG-)", length(countTScpgTSI)), rep("TS (CpG+)", length(countTSnoncpgTSI)), rep("TV (CpG-)", length(countTVnoncpgTSI))))

f2b <- ggplot(dataTSI, aes(x=cat, y = freq, fill=mType)) + geom_bar(stat = "identity", position=position_dodge()) + theme_bw() + theme(legend.position = "none", axis.text.x = element_text(angle = 45, hjust = 1),legend.title = element_blank(),plot.title = element_text(hjust = 0.5))+
  scale_fill_manual(values = c("#377eb8", "#e41a1c", "#4daf4a")) + xlab(expression(italic(f[GC]))) + ylab("Proportion")+ggtitle("TSI")+ylim(0,ylimYRI)+
  geom_segment(aes(x = mean(gcFreqTSnoncpgTSI)*20 , y = 0, xend = mean(gcFreqTSnoncpgTSI)*20, yend = Inf), lty=1, col="#377eb8") +
  geom_segment(aes(x = median(gcFreqTSnoncpgTSI*20) , y = 0, xend = median(gcFreqTSnoncpgTSI)*20, yend = Inf), lty=2, col="#377eb8") +
  geom_segment(aes(x = mean(gcFreqTScpgTSI)*20 , y = 0, xend = mean(gcFreqTScpgTSI)*20, yend = Inf), lty=1, col="#e41a1c") +
  geom_segment(aes(x = median(gcFreqTScpgTSI)*20 , y = 0, xend = median(gcFreqTScpgTSI)*20, yend = Inf), lty=2, col="#e41a1c") +
  geom_segment(aes(x = mean(gcFreqTVnoncpgTSI)*20 , y = 0, xend = mean(gcFreqTVnoncpgTSI)*20, yend = Inf), lty=1, col="#4daf4a") +
  geom_segment(aes(x = median(gcFreqTVnoncpgTSI)*20 , y = 0, xend = median(gcFreqTVnoncpgTSI)*20, yend = Inf), lty=2, col="#4daf4a")
f2b

################################################################################################################################ 
#######################################################--- 1000G CHB ---########################################################
################################################################################################################################

for(i in 1:(length(humChr))){
  if(i==1){
    afCHB <- read.table(paste("chr1CHB.af", sep=""), head=T, sep = "\t")
  }else{
    temp <- read.table(paste(humChr[i],"CHB.af", sep=""), head=T, sep = "\t")
    afCHB <- rbind(afCHB, temp)
  }
}

gcFreqTScpgCHB <- subset(afCHB, fType%in%c("A_G", "C_G", "G_G", "T_G")&mType=="TS")$freqGC
gcFreqTSnoncpgCHB <- subset(afCHB, !(fType%in%c("A_G", "C_G", "G_G", "T_G"))&mType=="TS")$freqGC
gcFreqTVcpgCHB <- subset(afCHB, fType%in%c("A_G", "C_G", "G_G", "T_G")&mType=="TV")$freqGC
gcFreqTVnoncpgCHB <- subset(afCHB, !(fType%in%c("A_G", "C_G", "G_G", "T_G"))&mType=="TV")$freqGC

countTScpgCHB <- c()
countTSnoncpgCHB <- c()
countTVnoncpgCHB <- c()

for(i in 0.05*(1:20)){
  countTScpgCHB <- c(countTScpgCHB, length(which(gcFreqTScpgCHB>(i-0.05) & gcFreqTScpgCHB<i)))
  countTSnoncpgCHB <- c(countTSnoncpgCHB, length(which(gcFreqTSnoncpgCHB>(i-0.05) & gcFreqTSnoncpgCHB<i)))
  countTVnoncpgCHB <- c(countTVnoncpgCHB, length(which(gcFreqTVnoncpgCHB>(i-0.05) & gcFreqTVnoncpgCHB<i)))
}

dataCHB<- data.frame(cat = rep(c("0.00-0.05", "0.05-0.10", "0.10-0.15", "0.15-0.20", "0.20-0.25",
                                 "0.25-0.30", "0.30-0.35", "0.35-0.40", "0.40-0.45", "0.45-0.50",
                                 "0.50-0.55", "0.55-0.60", "0.60-0.65", "0.65-0.70", "0.70-0.75",
                                 "0.75-0.80", "0.80-0.85", "0.85-0.90", "0.90-0.95", "0.95-1.00"), 3), 
                     freq = c(countTSnoncpgCHB/sum(countTSnoncpgCHB), countTScpgCHB/sum(countTScpgCHB), countTVnoncpgCHB/sum(countTVnoncpgCHB)),
                     mType = c(rep("TS (CpG-)", length(countTScpgCHB)), rep("TS (CpG+)", length(countTSnoncpgCHB)), rep("TV (CpG-)", length(countTVnoncpgCHB))))

f2c <- ggplot(dataCHB, aes(x=cat, y = freq, fill=mType)) + geom_bar(stat = "identity", position=position_dodge()) + theme_bw() + theme(legend.position = "none", axis.text.x = element_text(angle = 45, hjust = 1),legend.title = element_blank(),plot.title = element_text(hjust = 0.5))+
  scale_fill_manual(values = c("#377eb8", "#e41a1c", "#4daf4a")) + xlab(expression(italic(f[GC]))) + ylab("Proportion")+ggtitle("CHB")+ylim(0,ylimYRI)+
  geom_segment(aes(x = mean(gcFreqTSnoncpgCHB)*20 , y = 0, xend = mean(gcFreqTSnoncpgCHB)*20, yend = Inf), lty=1, col="#377eb8") +
  geom_segment(aes(x = median(gcFreqTSnoncpgCHB*20) , y = 0, xend = median(gcFreqTSnoncpgCHB)*20, yend = Inf), lty=2, col="#377eb8") +
  geom_segment(aes(x = mean(gcFreqTScpgCHB)*20 , y = 0, xend = mean(gcFreqTScpgCHB)*20, yend = Inf), lty=1, col="#e41a1c") +
  geom_segment(aes(x = median(gcFreqTScpgCHB)*20 , y = 0, xend = median(gcFreqTScpgCHB)*20, yend = Inf), lty=2, col="#e41a1c") +
  geom_segment(aes(x = mean(gcFreqTVnoncpgCHB)*20 , y = 0, xend = mean(gcFreqTVnoncpgCHB)*20, yend = Inf), lty=1, col="#4daf4a") +
  geom_segment(aes(x = median(gcFreqTVnoncpgCHB)*20 , y = 0, xend = median(gcFreqTVnoncpgCHB)*20, yend = Inf), lty=2, col="#4daf4a")
f2c

################################################################################################################################ 
##########################################################--- ISL ---###########################################################
################################################################################################################################

for(i in 1:(length(humChr))){
  if(i==1){
    afISL <- read.table(paste("chr1ISL.af", sep=""), head=T, sep = "\t")
  }else{
    temp <- read.table(paste(humChr[i],"ISL.af", sep=""), head=T, sep = "\t")
    afISL <- rbind(afISL, temp)
  }
}

gcFreqTScpgISL <- subset(afISL, fType%in%c("A_G", "C_G", "G_G", "T_G")&mType=="TS")$freqGC
gcFreqTSnoncpgISL <- subset(afISL, !(fType%in%c("A_G", "C_G", "G_G", "T_G"))&mType=="TS")$freqGC
gcFreqTVcpgISL <- subset(afISL, fType%in%c("A_G", "C_G", "G_G", "T_G")&mType=="TV")$freqGC
gcFreqTVnoncpgISL <- subset(afISL, !(fType%in%c("A_G", "C_G", "G_G", "T_G"))&mType=="TV")$freqGC

countTScpgISL <- c()
countTSnoncpgISL <- c()
countTVnoncpgISL <- c()

for(i in 0.05*(1:20)){
  countTScpgISL <- c(countTScpgISL, length(which(gcFreqTScpgISL>(i-0.05) & gcFreqTScpgISL<i)))
  countTSnoncpgISL <- c(countTSnoncpgISL, length(which(gcFreqTSnoncpgISL>(i-0.05) & gcFreqTSnoncpgISL<i)))
  countTVnoncpgISL <- c(countTVnoncpgISL, length(which(gcFreqTVnoncpgISL>(i-0.05) & gcFreqTVnoncpgISL<i)))
}

dataISL<- data.frame(cat = rep(c("0.00-0.05", "0.05-0.10", "0.10-0.15", "0.15-0.20", "0.20-0.25",
                                 "0.25-0.30", "0.30-0.35", "0.35-0.40", "0.40-0.45", "0.45-0.50",
                                 "0.50-0.55", "0.55-0.60", "0.60-0.65", "0.65-0.70", "0.70-0.75",
                                 "0.75-0.80", "0.80-0.85", "0.85-0.90", "0.90-0.95", "0.95-1.00"), 3), 
                     freq = c(countTSnoncpgISL/sum(countTSnoncpgISL), countTScpgISL/sum(countTScpgISL), countTVnoncpgISL/sum(countTVnoncpgISL)),
                     mType = c(rep("TS (CpG-)", length(countTScpgISL)), rep("TS (CpG+)", length(countTSnoncpgISL)), rep("TV (CpG-)", length(countTVnoncpgISL))))

f2d <- ggplot(dataISL, aes(x=cat, y = freq, fill=mType)) + geom_bar(stat = "identity", position=position_dodge()) + theme_bw() + theme(axis.text.x = element_text(angle = 45, hjust = 1),legend.title = element_blank(),plot.title = element_text(hjust = 0.5))+
  scale_fill_manual(values = c("#377eb8", "#e41a1c", "#4daf4a")) + xlab(expression(italic(f[GC]))) + ylab("Proportion")+ggtitle("ISL")+ylim(0,ylimYRI)+
  geom_segment(aes(x = mean(gcFreqTSnoncpgISL)*20 , y = 0, xend = mean(gcFreqTSnoncpgISL)*20, yend = Inf), lty=1, col="#377eb8") +
  geom_segment(aes(x = median(gcFreqTSnoncpgISL*20) , y = 0, xend = median(gcFreqTSnoncpgISL)*20, yend = Inf), lty=2, col="#377eb8") +
  geom_segment(aes(x = mean(gcFreqTScpgISL)*20 , y = 0, xend = mean(gcFreqTScpgISL)*20, yend = Inf), lty=1, col="#e41a1c") +
  geom_segment(aes(x = median(gcFreqTScpgISL)*20 , y = 0, xend = median(gcFreqTScpgISL)*20, yend = Inf), lty=2, col="#e41a1c") +
  geom_segment(aes(x = mean(gcFreqTVnoncpgISL)*20 , y = 0, xend = mean(gcFreqTVnoncpgISL)*20, yend = Inf), lty=1, col="#4daf4a") +
  geom_segment(aes(x = median(gcFreqTVnoncpgISL)*20 , y = 0, xend = median(gcFreqTVnoncpgISL)*20, yend = Inf), lty=2, col="#4daf4a")
f2d

legend <- get_legend(f2d)
f2d <- f2d + theme(legend.position = "none")
blank <- ggplot() + theme_void()
tt <- plot_grid(f2a,f2b,blank,f2c,f2d,legend, ncol = 3, align="v",axis = "l", rel_widths = c(2,2,0.5,2,2,0.5))
tt <- plot_grid(f2a,f2b,f2c,f2d, ncol = 2, align="v",axis = "l")
ggsave("fig2.pdf", tt, width=28, height=22, units = "cm")


