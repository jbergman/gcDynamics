library(ggplot2)

## load data
data1 <- read.table("/Users/au612643/Desktop/par/rScripts/github/fig3_4_S8.txt", header = T, sep="\t")
data1 <- subset(data1, pop=="YRI")
data2 <- read.table("/Users/au612643/Desktop/par/rScripts/github/figS13.txt", header = T, sep="\t")

data <- data.frame(chr = c(data1$chr, data2$chr), B = c(data1$B, data2$B), mType = c(data1$mType, data2$mType), dataset = c(rep("original", dim(data1)[1]),rep("no-CDS/no-REG", dim(data1)[1])))
data$dataset <- factor(data$dataset,levels = c("original","no-CDS/no-REG"))

#### Figure S13

fS13 <- ggplot(data, aes(x=mType, y=B, col=dataset)) + geom_boxplot() + theme_bw() + 
  theme(legend.title = element_blank(), axis.title.x = element_blank(), axis.text.x = element_text(angle = 90, hjust = 1)) +
  ylab(expression(italic(B)))+ scale_color_manual(values=c("#e41a1c","gray"))
fS13

ggsave("/Users/au612643/Desktop/par/rScripts/github/figS13.pdf", fS13, width=12, height=10, units = "cm")
