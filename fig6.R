library(ggplot2)

## load data
data <- read.table("/Users/au612643/Desktop/par/rScripts/github/fig6.txt", header = T, sep = "\t")

## plot
f6 <- ggplot(data, aes(x=countTri,y=B, col=mType))+geom_point()+geom_smooth(method="lm") + theme_bw() + theme(legend.title = element_blank()) + 
      xlab("# tri-allelic sites") + ylab(expression(italic(B)))+scale_color_manual(values = c("#e41a1c","#4daf4a")) 

ggsave("/Users/au612643/Desktop/par/rScripts/github/fig6.pdf",f6, width=11, height=6.5, units = "cm")