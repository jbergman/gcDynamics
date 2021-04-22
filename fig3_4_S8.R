library(ggplot2)
library(cowplot)

## load data
data <- read.table("fig3_4_S8.txt", header = T, sep="\t")

#### Figure 3
data3 <- subset(data, pop%in%c("YRI", "TSI", "CHB", "ISL"))
data3$pop <- factor(data3$pop, levels = c("YRI", "TSI", "CHB", "ISL"))

f3a <- ggplot(data3, aes(x=mType, y=B, color=mType)) + geom_boxplot(outlier.size = 0.5,shape=21) + facet_wrap(.~pop, ncol = 4, nrow = 1) +
  theme_bw() + theme(legend.position = "none", axis.title.x = element_blank(), axis.text.x = element_text(angle = 90, hjust = 1)) +
  ylab(expression(italic(B)))+ scale_color_manual(values=c("#377eb8", "#e41a1c", "#4daf4a"))+
  geom_point(aes(x=mType,y=B_full), shape=18, size = 4, colour=c(rep("#377eb8",88), rep("#e41a1c",88), rep("#4daf4a",88)))


f3b <- ggplot(data3, aes(x=cMmb, y=B, colour = mType)) + geom_point(size=1.25, shape=21, stroke=0, aes(fill=factor(mType))) + 
  facet_wrap(.~pop, scales = "free_x", ncol = 4, nrow = 1) + theme_bw() + 
  geom_smooth(method="lm",lty =2, lwd=0.7) + theme(legend.position = "none") +xlab("cM/Mb")+ ylab(expression(italic(B)))+
  scale_fill_manual(values = c("#377eb8", "#e41a1c", "#4daf4a")) +  scale_color_manual(values = c("#377eb8", "#e41a1c", "#4daf4a")) + 
  scale_x_continuous(breaks = c(0.9,1.2,1.5,1.8,2.1))

tt <- plot_grid(f3a, f3b, ncol = 1, align = "v", rel_heights = c(1.2,1), labels = c("A", "B"))
ggsave("fig3.pdf", tt, width=15, height=12, units = "cm")

#### Figure 4
data$superpop <- factor(data$superpop, levels = c("AFR", "SAS", "EAS", "EUR", "AMR"))

f4 <- ggplot(data, aes(x=mType, y=B_full, color=mType)) + geom_boxplot(outlier.size = 0.5,shape=21) + facet_wrap(.~superpop, ncol = 7, nrow = 4) +
  theme_bw() + theme(legend.position = "none", axis.title.x = element_blank(), axis.text.x = element_text(angle = 90, hjust = 1)) +
  ylab(expression(italic(B)))+ scale_color_manual(values=c("#377eb8", "#e41a1c", "#4daf4a"))

ggsave("fig4.pdf",f4, width=15, height=7, units = "cm")

#### Supplementary figure S8
data$superpop <- factor(data$superpop, levels = c("AFR", "AMR", "EAS", "EUR","SAS"))
fS8 <- ggplot(data, aes(x=mType, y=B, color=mType)) + geom_boxplot(outlier.size = 0.5,shape=21) + facet_wrap(superpop~pop, ncol = 7, nrow = 4) +
  theme_bw() + theme(legend.position = "none", axis.title.x = element_blank(), axis.text.x = element_text(angle = 90, hjust = 1)) +
  ylab(expression(italic(B)))+ scale_color_manual(values=c("#377eb8", "#e41a1c", "#4daf4a"))+
  geom_point(aes(x=mType,y=B_full), shape=18, size = 2.5, colour=c(rep("#377eb8",length(unique(data$pop))*22), rep("#e41a1c",length(unique(data$pop))*22), rep("#4daf4a",length(unique(data$pop))*22)))
fS8
ggsave("figS8.pdf", fS8, width=18, height=22, units = "cm")
