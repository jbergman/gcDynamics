library(ggplot2)
library(cowplot)

## load data
data <- read.table("/Users/au612643/Desktop/par/rScripts/github/fig5_S9.txt", header = T, sep = "\t")
data$pop <- factor(data$pop, levels = c("YRI", "TSI", "CHB", "ISL"))

## Figure 5
f5a <- ggplot(data, aes(tp, fp, fill= (prop))) +  geom_tile() + facet_grid(pop~mType) + xlab("3' end") + ylab("5' end") + 
  theme_bw() + theme(legend.title = element_blank())+ scale_fill_distiller(palette = "RdBu")

f5b <- ggplot(data, aes(tp, fp, fill= B)) +  geom_tile() + facet_grid(pop~mType) + xlab("3' end") + ylab("5' end") + 
  theme_bw() + theme(legend.title = element_blank())+ scale_fill_distiller(palette = "RdBu", limits=c(-1*max(data$B),max(data$B)))

tt <- plot_grid(f5a, f5b, ncol = 2, labels = c("A", "B"))
ggsave("/Users/au612643/Desktop/par/rScripts/github/fig5.pdf", tt, width=20, height=9, units = "cm")

## Supplementary figure 9
ratios <- data.frame(fp = rep(c(rep("A", 4),rep("C", 4),rep("G", 4),rep("T", 4)),8), tp = rep(c("A", "C", "G", "T"),32), mType = rep(c(rep("TS",16), rep("TV",16)),4),
                   rat = c(subset(data, pop=="TSI")$prop/subset(data, pop=="YRI")$prop,subset(data, pop=="TSI")$prop/subset(data, pop=="CHB")$prop,
                           subset(data, pop=="ISL")$prop/subset(data, pop=="YRI")$prop,subset(data, pop=="ISL")$prop/subset(data, pop=="CHB")$prop),
                   comp = c(rep("YRI-TSI",32),rep("CHB-TSI",32),rep("YRI-ISL",32),rep("CHB-ISL",32)),
                   pop = c(rep("TSI", 64), rep("ISL", 64)))
ratios$comp <- factor(ratios$comp, levels = c("YRI-TSI", "CHB-TSI", "YRI-ISL", "CHB-ISL"))

fS9a <- ggplot(subset(ratios,pop=="TSI"), aes(tp, fp, fill= (rat))) +  geom_tile() + facet_grid(comp~mType) + xlab("3' end") + ylab("5' end") + 
  theme_bw() + theme(legend.title = element_blank())+ scale_fill_distiller(palette = "RdBu")

fS9b <- ggplot(subset(ratios,pop=="ISL"), aes(tp, fp, fill= (rat))) +  geom_tile() + facet_grid(comp~mType) + xlab("3' end") + ylab("5' end") + 
  theme_bw() + theme(legend.title = element_blank())+ scale_fill_distiller(palette = "RdBu")

tt <- plot_grid(fS9a, fS9b, ncol = 2, labels = c("A", "B"))
ggsave("/Users/au612643/Desktop/par/rScripts/github/figS9.pdf", tt, width=20, height=6, units = "cm")