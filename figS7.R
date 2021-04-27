library(ggplot2)
library(cowplot)

## load data
data <- read.table("figS7.txt", header =T,  sep="\t")

#### Supplementary figure 7
fS4a <- ggplot(subset(data, mType =="TS (CpG-)"), aes(x=log2(freqYRI), y=log2(freqTSI))) + geom_point(size=0.5) + theme_bw()+
      xlab(expression(italic(log[2])(italic(f[GC]))-YRI)) + ylab(expression(italic(log[2])(italic(f[GC]))-TSI))
fS4b <- ggplot(subset(data, mType =="TS (CpG-)"), aes(x=log2(countYRI), y=log2(countTSI))) + geom_point(size=0.5) + theme_bw()+
  xlab(expression(italic(log[2])(Count)-YRI)) + ylab(expression(italic(log[2])(Count)-TSI))
fS4c <- ggplot(subset(data, mType =="TS (CpG-)"), aes(x=log2(cMmbYRI), y=log2(cMmbTSI))) + geom_point(size=0.5) + theme_bw()+
  xlab(expression(italic(log[2])(cM/Mb)-YRI)) + ylab(expression(italic(log[2])(cM/Mb)-TSI))

tt <- plot_grid(fS4a, fS4b, fS4c, ncol = 3, labels = c("A", "B", "C"))

ggsave("figS7.pdf", tt, width=27, height=9, units = "cm")
