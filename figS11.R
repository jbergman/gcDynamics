library(ggplot2)
library(cowplot)

## load data
data <- read.table("fig1_S11.txt", sep = "\t", header = T)
dataFilt = subset(data, countTV>0&countTS>0&countGC>999&cMmb>0&cMmb<5)

## Supplementary figure S11
fS11a <- ggplot(dataFilt, aes(x=log2(cMmb), y=log2(freqTVcpg)))  + geom_point(size=0.005, shape=21, alpha=0.5) + 
  geom_smooth(method="lm", lty =2, lwd=0.7,col="#e41a1c") +theme_bw() +
  xlab(expression(italic(log[2])(cM/Mb))) + ylab(expression(italic(log[2])(italic(f[GC]))))

fS11b <- ggplot(dataFilt, aes(x=log2(cMmb), y=log2(countTVcpg)))  + geom_point(size=0.005, shape=21, alpha=0.5) + 
  geom_smooth(method="lm", lty =2, lwd=0.7,col="#e41a1c") +theme_bw() +
  xlab(expression(italic(log[2])(cM/Mb))) + ylab(expression(italic(log[2])(Count)))

fS11c <- ggplot(dataFilt, aes(x=log2(freqTVcpg), y=log2(countTVcpg)))  + geom_point(size=0.005, shape=21, alpha=0.5) + 
  geom_smooth(method="lm", lty =2, lwd=0.7,col="#e41a1c") +theme_bw() +
  xlab(expression(italic(log[2])(italic(f[GC])))) + ylab(expression(italic(log[2])(Count)))

tt <- plot_grid(fS11a,fS11b,fS11c, ncol = 3, align="bt", labels = c("A", "B", "C"))
ggsave("figS11.pdf", tt, width=26, height=6, units = "cm")
