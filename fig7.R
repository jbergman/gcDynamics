library(ggplot2)
library(cowplot)

## load data
data <- read.table("/Users/au612643/Desktop/par/rScripts/github/fig7.txt", header = T, sep = "\t")

## Figure 7
data$pop <- factor(data$pop, levels = c("PTE","PTS","PTT","PTV","PPA","GGG","PAB","PPY"))

f3a <- ggplot(data, aes(x=mType, y=B, color=mType)) + geom_boxplot(outlier.size = 0.5,shape=21) + facet_wrap(.~pop, ncol = 8, nrow = 4) +
  theme_bw() + theme(legend.position = "none", axis.title.x = element_blank(), axis.text.x = element_text(angle = 90, hjust = 1)) +
  ylab(expression(italic(B)))+ scale_color_manual(values=c("#377eb8", "#e41a1c", "#4daf4a"))+ scale_y_continuous(breaks = c(0.0,0.4,0.8,1.2))+
  geom_point(aes(x=mType,y=B_full), shape=18, size = 4, colour=c(rep("#377eb8",8*23), rep("#e41a1c",8*23), rep("#4daf4a",8*23)))

f3b <- ggplot(data, aes(x=cMmb, y=B, colour = mType)) + geom_point(size=1.25, shape=21, stroke=0, aes(fill=factor(mType))) +
  facet_wrap(.~pop, scales = "free_x", ncol = 8, nrow = 4) + theme_bw() +
  geom_smooth(method="lm",lty =2, lwd=0.7) + theme(legend.position = "none") +xlab("cM/Mb")+ ylab(expression(italic(B)))+
  scale_fill_manual(values = c("#377eb8", "#e41a1c", "#4daf4a")) +  scale_color_manual(values = c("#377eb8", "#e41a1c", "#4daf4a")) +
  scale_x_continuous(breaks = c(0.3,0.5,0.6,0.8,1.0,1.2,1.4,1.6)) + scale_y_continuous(breaks = c(0.0,0.4,0.8,1.2))

tt <- plot_grid(f3a, f3b, ncol = 1, align = "v", rel_heights = c(1.2,1), labels = c("A", "B"))
ggsave("/Users/au612643/Desktop/par/rScripts/github/fig7.pdf", tt, width=23.5, height=11, units = "cm")
