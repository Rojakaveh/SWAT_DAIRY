
####ORGN
NORG <- data.frame(supp=rep(c("SWAT-VSA", "SWAT-Dairy"), each=3),
                  dose=rep(c("Allandra", "Four Hills", "Monkton Rd"),2),
                  len=c(0.76, 1.2, 0.856, 0.717, 1.128, 0.804))

p1=ggplot(data=NORG, aes(x=dose, y=len, fill=supp)) +
  geom_bar(stat="identity", position=position_dodge(),width=0.5)+
  labs(y = 'kg/ha')+
  ggtitle("ORGN") +
  theme(axis.text.y = element_text(size = 16,colour = "black"),
        axis.title.y=element_text(size = 16),
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        plot.title=element_text(size = 20),
        legend.position ="none",
        aspect.ratio=1/1)

  

####ORGP
ORGP <- data.frame(supp=rep(c("SWAT-VSA", "SWAT-Dairy"), each=3),
                   dose=rep(c("Allandra", "Four Hills", "Monkton Rd"),2),
                   len=c(0.015, 0.031, 0.018, 0.011, 0.028, 0.017))

p2=ggplot(data=ORGP, aes(x=dose, y=len, fill=supp)) +
  geom_bar(stat="identity", position=position_dodge(),width=0.5)+
  labs(y = 'kg/ha')+
  ggtitle("ORGP") +
  theme(axis.text.y = element_text(size = 16,colour = "black"),
        axis.title.y=element_text(size = 16),
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        plot.title=element_text(size = 20),
        legend.position ="none",
        aspect.ratio=1/1)

####NSURQ
NSURQ <- data.frame(supp=rep(c("SWAT-VSA", "SWAT-Dairy"), each=3),
                   dose=rep(c("Allandra", "Four Hills", "Monkton Rd"),2),
                   len=c(19.966, 29.759, 28.793, 25.635, 30.293, 29.282))

p3=ggplot(data=NSURQ, aes(x=dose, y=len, fill=supp)) +
  geom_bar(stat="identity", position=position_dodge(),width=0.5)+
  labs(y = 'kg/ha')+
  ggtitle("NSURQ") +
  theme(axis.text.y = element_text(size = 16,colour = "black"),
        axis.title.y=element_text(size = 16),
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        plot.title=element_text(size = 20),
        legend.position ="none",
        aspect.ratio=1/1)


#####SEDP
SEDP <- data.frame(supp=rep(c("SWAT-VSA", "SWAT-Dairy"), each=3),
                    dose=rep(c("Allandra", "Four Hills", "Monkton Rd"),2),
                    len=c(0.038, 0.054, 0.029, 0.033, 0.057, 0.03))

p4=ggplot(data=SEDP, aes(x=dose, y=len, fill=supp)) +
  geom_bar(stat="identity", position=position_dodge(),width=0.5)+
  labs(y = 'kg/ha')+
  ggtitle("SEDP") +
  theme(axis.text.y = element_text(size = 16,colour = "black"),
        axis.title.y=element_text(size = 16),
        axis.title.x=element_blank(),
        axis.text.x=element_text(size = 12,face = "bold",colour = "black"),
        axis.ticks.x=element_blank(),
        plot.title=element_text(size = 20),
        legend.position ="none",
        aspect.ratio=1/1)
####NAPP
NAPP <- data.frame(supp=rep(c("SWAT-VSA", "SWAT-Dairy"), each=3),
                   dose=rep(c("Allandra", "Four Hills", "Monkton Rd"),2),
                   len=c(760, 1140, 760, 840, 1200, 800))

p5=ggplot(data=NAPP, aes(x=dose, y=len, fill=supp)) +
  geom_bar(stat="identity", position=position_dodge(),width=0.5)+
  labs(y = 'kg/ha')+
  ggtitle("N-APP") +
  theme(axis.text.y = element_text(size = 16),
        axis.title.y=element_text(size = 16),
        axis.title.x=element_blank(),
        axis.text.x=element_text(size = 12,face = "bold",colour = "black"),
        axis.ticks.x=element_blank(),
        plot.title=element_text(size = 20),
        legend.position ="none",
        aspect.ratio=1/1)
#####PAPP

PAPP <- data.frame(supp=rep(c("SWAT-VSA", "SWAT-Dairy"), each=3),
                   dose=rep(c("Allandra", "Four Hills", "Monkton Rd"),2),
                   len=c(160, 240, 160, 80, 210, 140))

p6=ggplot(data=PAPP, aes(x=dose, y=len, fill=supp)) +
  geom_bar(stat="identity", position=position_dodge(),width=0.5)+
  labs(y = 'kg/ha')+
  ggtitle("P-APP") +
  theme(axis.text.y = element_text(size = 16,colour = "black"),
        axis.title.y=element_text(size = 16),
        axis.title.x=element_blank(),
        axis.text.x=element_text(size = 12,face = "bold",colour = "black"),
        axis.ticks.x=element_blank(),
        plot.title=element_text(size = 20),
        legend.title=element_blank(),
        legend.text = element_text(size = 16),
        aspect.ratio=1/1)

p1+p2+p3+p4+p5+p6+plot_layout(ncol = 3, widths = c(1, 1))

