library(ggplot2)

# ORGN
ORGN <- data.frame(
  name=c("Subbasin 1","Subbasin 2") ,  
  value=c(-1533,-420)
)

# Barplot
p1=ggplot(ORGN, aes(x=name, y=value)) + 
  geom_bar(stat = "identity", width=0.5)+
labs(y = 'D-Value (grams)')+
  ggtitle("ORGN") +
  theme(axis.text.y = element_text(size = 16),
        axis.title.y=element_text(size = 16),
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        plot.title=element_text(size = 20),
        aspect.ratio=1/1)

## ORGP
ORGP <- data.frame(
  name=c("Subbasin 1","Subbasin 2") ,  
  value=c( -345.5,-99)
)

p2=ggplot(ORGP, aes(x=name, y=value)) + 
  geom_bar(stat = "identity", width=0.5)+
  labs(y = 'D-Value (grams)')+
  ggtitle("ORGP") +
  theme(axis.text.y = element_text(size = 16),
        axis.title.y=element_blank(),
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        plot.title=element_text(size = 20),
        aspect.ratio=1/1)
##### NSURQ
NSURQ <- data.frame(
  name=c("Subbasin 1","Subbasin 2") ,  
  value=c( 165450,2886)
)

p3=ggplot(NSURQ, aes(x=name, y=value)) + 
  geom_bar(stat = "identity", width=0.5)+
  labs(y = 'D-Value (grams)')+
  ggtitle("NSURQ") +
  theme(axis.text.y = element_text(size = 16),
        axis.title.y=element_text(size = 16),
        axis.title.x=element_blank(),
        axis.text.x=element_text(size = 16),
        axis.ticks.x=element_blank(),
        plot.title=element_text(size = 20),
        aspect.ratio=1/1)
###SEDP
SEDP <- data.frame(
  name=c("Subbasin 1","Subbasin 2") ,  
  value=c(-362.8,-288)
)

p4=ggplot(SEDP, aes(x=name, y=value)) + 
  geom_bar(stat = "identity", width=0.5)+
  labs(y = 'D-Value (grams)')+
  ggtitle("SEDP") +
  theme(axis.text.y = element_text(size = 16),
        axis.title.y=element_blank(),
        axis.title.x=element_blank(),
        axis.text.x=element_text(size = 16),
        axis.ticks.x=element_blank(),
        plot.title=element_text(size = 20),
        aspect.ratio=1/1)


p1+p2+p3+p4+plot_layout(ncol = 2, widths = c(1, 1))


