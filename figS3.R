nes=-2*0.8878121

eqWF <- function(x, ne, mrate1, mrate2, s){
  return((exp(x*ne*s))*(x**(ne*mrate1-1))*((1-x)**(ne*mrate2-1)))
}

xs = seq(0.025, 0.975, by = 0.05)

plot(eqWF(xs, 40000, 0.67*1.29*(10**(-8)),0.33*1.29*(10**(-8)),0))

dist=eqWF(xs, 40000, 0.67*1.29*(10**(-8)),0.33*1.29*(10**(-8)),nes/40000)

dist=eqWF(xs, 40000, 0,0,nes/40000)
plot(dist/sum(dist))

nes=0
dist=eqWF(xs, 40000, 0.67*1.29*(10**(-8)),0.33*1.29*(10**(-8)),nes/40000)
dist2=eqWF(xs, 40000, 11.61*(10**(-8)),0,nes/40000)
dist3=eqWF(xs, 40000, 5*11.61*(10**(-8)),0,nes/40000)
dist4=eqWF(xs, 40000, 110.61*(10**(-8)),0,nes/40000)
dist5=eqWF(xs, 40000, 1100.61*(10**(-8)),0,nes/40000)
plot(dist/sum(dist))
lines(dist4/sum(dist4))

df <- data.frame(freq = rep(xs,5),
                 dist = c(dist/sum(dist), dist2/sum(dist2), dist3/sum(dist3), dist4/sum(dist4), dist5/sum(dist5)),
                 SFS = c(rep("expected genome-wide (v; u)",20),rep("expected CpG (v; 10x u)",20),rep("v; 50x u",20),rep("v; 100x u",20),rep("v; 1000x u",20)))
df$SFS <- factor(df$SFS, levels = c("expected genome-wide (v; u)", "expected CpG (v; 10x u)", "v; 50x u", "v; 100x u", "v; 1000x u"))

aa <- ggplot(df, aes(x=freq, y=dist, fill=SFS)) + geom_bar(position="dodge", stat="identity") + theme_bw() + xlab("Frequency") +
      ylab("Proportion") +theme(legend.title = element_blank())
aa
ggsave("/Users/jura/Desktop/suppFig3.png", aa, width=15, height=8, units = "cm")

