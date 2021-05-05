library(ggplot2)
library(cowplot)

## load data
data <- read.table("figS12.txt", header=T, sep = "\t")

## Supplementary figure 12
fS12a <- ggplot(subset(data, pop == "PPA"), aes(x=log2(cMmb), y=log2(freq),colour = mType,))  + geom_point(size=0.005,alpha=0.2) + 
  geom_smooth(method="lm", lty =2, lwd=0.7) +theme_bw() + theme(legend.title = element_blank())+
  xlab(expression(italic(log[2])(cM/Mb))) + ylab(expression(italic(log[2])(italic(f[GC])))) +
  scale_color_manual(values = c("#377eb8", "#e41a1c", "#4daf4a"))

legfS12a <- get_legend(fS12a)
fS12a <- fS12a + theme(legend.position = "none")

fS12b <- ggplot(subset(data, pop == "PPA"), aes(x=log2(cMmb), y=log2(count),colour = mType,))  + geom_point(size=0.005,alpha=0.2) + 
  geom_smooth(method="lm", lty =2, lwd=0.7) +theme_bw() + theme(legend.position = "none")+
  xlab(expression(italic(log[2])(cM/Mb))) + ylab(expression(italic(log[2])(Count))) +
  scale_color_manual(values = c("#377eb8", "#e41a1c", "#4daf4a"))

fS12c <- ggplot(subset(data, pop == "GGG"), aes(x=log2(cMmb), y=log2(freq),colour = mType,))  + geom_point(size=0.005,alpha=0.2) + 
  geom_smooth(method="lm", lty =2, lwd=0.7) +theme_bw() + theme(legend.title = element_blank())+
  xlab(expression(italic(log[2])(cM/Mb))) + ylab(expression(italic(log[2])(italic(f[GC])))) +
  scale_color_manual(values = c("#377eb8", "#e41a1c", "#4daf4a"))

legfS12c <- get_legend(fS12c)
fS12c <-fS12c + theme(legend.position = "none")

fS12d <- ggplot(subset(data, pop == "GGG"), aes(x=log2(cMmb), y=log2(count),colour = mType,))  + geom_point(size=0.005,alpha=0.2) + 
  geom_smooth(method="lm", lty =2, lwd=0.7) +theme_bw() + theme(legend.position = "none")+
  xlab(expression(italic(log[2])(cM/Mb))) + ylab(expression(italic(log[2])(Count))) +
  scale_color_manual(values = c("#377eb8", "#e41a1c", "#4daf4a"))

fS12e <- ggplot(subset(data, pop == "GGG"&cMmb>0.25), aes(x=log2(cMmb), y=log2(freq),colour = mType,))  + geom_point(size=0.005,alpha=0.2) + 
  geom_smooth(method="lm", lty =2, lwd=0.7) +theme_bw() + theme(legend.title = element_blank())+
  xlab(expression(italic(log[2])(cM/Mb))) + ylab(expression(italic(log[2])(italic(f[GC])))) +
  scale_color_manual(values = c("#377eb8", "#e41a1c", "#4daf4a"))

legfS12e <- get_legend(fS12e)
fS12e <-fS12e + theme(legend.position = "none")

fS12f <- ggplot(subset(data, pop == "GGG"&cMmb>0.25), aes(x=log2(cMmb), y=log2(count),colour = mType,))  + geom_point(size=0.005,alpha=0.2) + 
  geom_smooth(method="lm", lty =2, lwd=0.7) +theme_bw() + theme(legend.position = "none")+
  xlab(expression(italic(log[2])(cM/Mb))) + ylab(expression(italic(log[2])(Count))) +
  scale_color_manual(values = c("#377eb8", "#e41a1c", "#4daf4a"))

fS12g <- ggplot(subset(data, pop == "PAB"), aes(x=log2(cMmb), y=log2(freq),colour = mType,))  + geom_point(size=0.005,alpha=0.2) + 
  geom_smooth(method="lm", lty =2, lwd=0.7) +theme_bw() + theme(legend.title = element_blank())+
  xlab(expression(italic(log[2])(cM/Mb))) + ylab(expression(italic(log[2])(italic(f[GC])))) +
  scale_color_manual(values = c("#377eb8", "#e41a1c", "#4daf4a"))

legfS12g <- get_legend(fS12g)
fS12g <- fS12g + theme(legend.position = "none")

fS12h <- ggplot(subset(data, pop == "PAB"), aes(x=log2(cMmb), y=log2(count),colour = mType,))  + geom_point(size=0.005,alpha=0.2) + 
  geom_smooth(method="lm", lty =2, lwd=0.7) +theme_bw() + theme(legend.position = "none")+
  xlab(expression(italic(log[2])(cM/Mb))) + ylab(expression(italic(log[2])(Count))) +
  scale_color_manual(values = c("#377eb8", "#e41a1c", "#4daf4a"))

tt <- plot_grid(fS12a, fS12b, legfS12a,
                fS12c, fS12d, legfS12c,
                fS12e, fS12f, legfS12e, 
                fS12g, fS12h, legfS12g, ncol = 3, align="bt", rel_widths = c(2,2,0.8), labels = c("A", "B", "", "C", "D", "", "E", "F", "", "G", "H", ""))

ggsave("figS12.pdf", tt, width=18.5, height=18, units = "cm")
