library(ggplot2)
library(cowplot)

## load data
data <- read.table("/Users/au612643/Desktop/par/rScripts/github/fig8.txt", header = T, sep = "\t")
data$pop <- factor(data$pop, levels = c("PPA", "GGG", "PAB"))
## Figure 8
f8a <- ggplot(data, aes(tp, fp, fill= (prop))) +  geom_tile() + facet_grid(pop~mType) + xlab("3' end") + ylab("5' end") + 
       theme_bw() + theme(legend.title = element_blank())+ scale_fill_distiller(palette = "RdBu")

f8b <- ggplot(data, aes(tp, fp, fill= B)) +  geom_tile() + facet_grid(pop~mType) + xlab("3' end") + ylab("5' end") + 
       theme_bw() + theme(legend.title = element_blank())+ scale_fill_distiller(palette = "RdBu", limits=c(-1.7297,1.7297))

tt <- plot_grid(f8a, f8b, ncol = 2, labels = c("A", "B"))

ggsave("/Users/au612643/Desktop/par/rScripts/github/fig8.pdf", tt, width=20, height=7.25, units = "cm")