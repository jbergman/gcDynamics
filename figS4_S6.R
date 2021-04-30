library(ggplot2)
library(cowplot)

### load data
data <- read.table("figS4_S6.txt", header = T, sep = "\t")
data$pop <- factor(data$pop, levels = c("YRI", "TSI", "CHB"))

## Supplementary figure S4
figS4a <- ggplot(subset(data, conv=="OK"), aes(x=mType, y=B_glem, col=mType)) + geom_boxplot() + facet_wrap(.~pop) + theme_bw() + theme(legend.position = "none", axis.title.x = element_blank(), axis.text.x = element_text(angle = 90, hjust = 1)) +
  ylab(expression(italic(p[GC])))+ scale_color_manual(values=c("#377eb8", "#e41a1c", "#4daf4a"))

figS4b <- ggplot(subset(data, conv=="OK"), aes(x=cMmb, y=B_glem, colour = mType)) + 
  geom_point(size=1.25, shape=21, stroke=0, aes(fill=factor(mType))) + 
  facet_wrap(.~pop, scales = "free_x", ncol = 3) + theme_bw() + 
  geom_smooth(method="lm",lty =2, lwd=0.7) + theme(legend.position = "none") +xlab("cM/Mb")+ ylab(expression(italic(B)))+
  scale_fill_manual(values = c("#377eb8", "#e41a1c", "#4daf4a")) +  scale_color_manual(values = c("#377eb8", "#e41a1c", "#4daf4a")) + 
  scale_x_continuous(breaks = c(0.9,1.2,1.5,1.8,2.1))

tt <- plot_grid(figS4a, figS4b, ncol = 1, align = "v", rel_heights = c(1.2,1), labels = c("A", "B"))
ggsave("figS4.pdf", tt, width=15, height=12, units = "cm")

## Supplementary figure S6
figS6 <- ggplot(data, aes(x=B_berg, y=B_glem)) + geom_point() + theme_bw() + facet_grid(.~pop) +
         xlab(expression(italic(B) - "this study")) + ylab(expression(italic(B) - "Glemin et al. (2015)"))

ggsave("figS6.pdf", figS6, width=25, height=8, units = "cm")
