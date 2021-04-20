library(ggplot2)
library(cowplot)

## load data
data <- read.table("fig1.txt", sep = "\t", header = T)
dataFilt = subset(data, countTV>0&countTS>0&countGC>999&cMmb>0&cMmb<5)

dataTemp <- data.frame(POS = rep(dataFilt$POS, 3),
                       chr = rep(dataFilt$chr, 3),
                       cMmb = rep(dataFilt$cMmb, 3),
                       freq = c(dataFilt$freqTSnoncpg, dataFilt$freqTScpg, dataFilt$freqTVnoncpg),
                       count  = c(dataFilt$countTSnoncpg, dataFilt$countTScpg, dataFilt$countTVnoncpg),
                       mType = c(rep("TS (CpG-)", dim(dataFilt)[1]), rep("TS (CpG+)", dim(dataFilt)[1]), rep("TV (CpG-)", dim(dataFilt)[1])))

## plots
f1a <- ggplot(dataFilt, aes(x=log2(cMmb), y=log2(freqGC)))  + geom_point(size=0.005, shape=21, alpha=0.5) + 
  geom_smooth(method="lm", lty =2, lwd=0.7,col="#e41a1c") +theme_bw() +
  xlab(expression(italic(log[2])(cM/Mb))) + ylab(expression(italic(log[2])(italic(f[GC]))))

f1b <- ggplot(dataTemp, aes(x=log2(cMmb), y=log2(freq),colour = mType))  + geom_point(size=0.005,alpha=0.2) + 
  geom_smooth(method="lm", lty =2, lwd=0.7) +theme_bw() + theme(legend.title = element_blank())+
  xlab(expression(italic(log[2])(cM/Mb))) + ylab(expression(italic(log[2])(italic(f[GC])))) +
  scale_color_manual(values = c("#377eb8", "#e41a1c", "#4daf4a"))

legf1b <- get_legend(f1b)
f1b <- f1b + theme(legend.position = "none")

f1c <- ggplot(dataTemp, aes(x=log2(cMmb), y=log2(count),colour = mType,))  + geom_point(size=0.005,alpha=0.2) + 
  geom_smooth(method="lm", lty =2, lwd=0.7) +theme_bw() + theme(legend.position = "none")+
  xlab(expression(italic(log[2])(cM/Mb))) + ylab(expression(italic(log[2])(Count))) +
  scale_color_manual(values = c("#377eb8", "#e41a1c", "#4daf4a"))

f1d <- ggplot(dataTemp, aes(x=log2(freq), y=log2(count),colour = mType,))  + geom_point(size=0.005,alpha=0.2) + 
  geom_smooth(method="lm", lty =2, lwd=0.7) +theme_bw() + theme(legend.title = element_blank())+
  xlab(expression(italic(log[2])(italic(f[GC])))) + ylab(expression(italic(log[2])(Count))) +
  scale_color_manual(values = c("#377eb8", "#e41a1c", "#4daf4a"))

legf1d <- get_legend(f1d)
f1d <- f1d + theme(legend.position = "none")

tt <- plot_grid(f1a,f1b,legf1b,f1c,f1d,legf1d, ncol = 3, align="bt", rel_widths = c(2,2,0.8), labels = c("A", "B", "", "C", "D", ""))
ggsave("fig1.pdf", tt, width=18.5, height=12, units = "cm")
