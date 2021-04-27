library(ggplot2)

## load data
data <- read.table("/Users/au612643/Desktop/par/rScripts/github/figS2.txt", header = T, sep = "\t")

## Figure S2
dataHUM <- subset(data, pop%in%c("YRI", "TSI", "CHB", "ISL (avg.)", "ISL (mat.)", "ISL (pat.)"))
dataHUM$pop <- factor(dataHUM$pop, levels = c("YRI", "TSI", "CHB", "ISL (avg.)", "ISL (mat.)", "ISL (pat.)"))

fS2a <- ggplot(data=dataHUM, aes(x=log2(cmMb), col=pop)) + geom_density() + xlab(expression(italic(log[2])(cM/Mb))) + ylab("Density") +
  theme_bw() + theme(legend.title =  element_blank(), legend.position = c(0.15, 0.6))
fS2a
dataGA <- subset(data, pop%in%c("PTE", "PTT", "PTS", "PTV", "PPA", "GGG", "PAB", "PPY"))
dataGA$pop <- factor(dataGA$pop, levels = c("PTE", "PTT", "PTS", "PTV", "PPA", "GGG", "PAB", "PPY"))

fS2b <- ggplot(data=dataGA, aes(x=log2(cmMb), col=pop)) + geom_density() + xlab(expression(italic(log[2])(cM/Mb))) + ylab("Density") +
  theme_bw() + theme(legend.title =  element_blank(), legend.position = c(0.15, 0.6))

tt <- plot_grid(fS2a, fS2b, ncol = 1, align = "v", rel_heights = c(1,1), labels = c("A", "B"))
ggsave("/Users/au612643/Desktop/par/rScripts/github/figS2.pdf", tt, width=15, height=18, units = "cm")
