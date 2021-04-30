library(ggplot2)
library(cowplot)

### load data
dataA <- read.table("/Users/jura/Desktop/rev/rscripts/figS5a.txt", header=T, sep="\t")
dataB <- read.table("/Users/jura/Desktop/rev/rscripts/figS5b.txt", header=T, sep="\t")

### Supplementary figure S5
dataA$pop <- factor(dataA$pop, levels = c("YRI", "TSI", "CHB"))
dataA$dataset <- factor(dataA$dataset,levels = c("original","TS (CpG-)", "TS (CpG+)", "TV (CpG-)"))

figS5a <- ggplot(subset(dataA, conv=="OK"&mType!="TV (CpG+)"), aes(x=mType, y=B, col=dataset)) + geom_boxplot() + facet_wrap(.~pop) + theme_bw() + 
  theme(legend.title = element_blank(), axis.title.x = element_blank(), axis.text.x = element_text(angle = 90, hjust = 1)) +
  ylab(expression(italic(B)))+ scale_color_manual(values=c("gray", "#377eb8", "#e41a1c", "#4daf4a"))

dataB$pop <- factor(dataB$pop, levels = c("YRI", "TSI", "CHB"))
dataB$dataset <- factor(dataB$dataset,levels = c("original","TS (CpG-)", "TS (CpG+)", "TV (CpG-)"))
figS5b <- ggplot(subset(dataB, conv=="OK"&mType!="TV (CpG+)"), aes(x=mType, y=B, col=dataset)) + geom_boxplot() + facet_wrap(.~pop) + theme_bw() + 
  theme(legend.title = element_blank(), axis.title.x = element_blank(), axis.text.x = element_text(angle = 90, hjust = 1)) +
  ylab(expression(italic(B)))+ scale_color_manual(values=c("gray", "#377eb8", "#e41a1c", "#4daf4a"))

tt <- plot_grid(figS5a, figS5b, ncol = 1, align = "v", rel_heights = c(1,1), labels = c("A", "B"))
ggsave("/Users/jura/Desktop/rev/plots/figS5.pdf", tt, width=15, height=12, units = "cm")