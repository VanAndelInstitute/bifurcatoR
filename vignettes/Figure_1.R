library(ggplot2)

p1 = ggplot(data.frame(x = c(-4, 4)), aes(x = x)) +
  stat_function(fun = dnorm,color="darkblue") + theme_void() +
  geom_line(aes(x = c(0,0),y=c(0,.4)),linetype="dotted") + 
  annotate(geom="text", x= 0 ,y = -0.1,label="0",size=4) +
  annotate(geom="text", x = .75 ,y =-0.095,label="->",size=4) +
  annotate(geom="text", x= -.75 ,y =-0.095,label="<-",size=4) +
  annotate(geom="text", x= 1.5 ,y =-0.095,label="+",size=4) +
  annotate(geom="text", x= -1.5 ,y =-0.095,label="-",size=4) +
  geom_hline(yintercept = -0.005) +
  xlab("Deviation from trait mean") +
  ggtitle("Normal") + scale_y_continuous(limits =c(-0.1,1.3)) +
  theme(axis.title.x = element_text(),
        plot.title = element_text(hjust = 0.5,color="red3"))

p2 = ggplot() +
  stat_function(fun = function(x){dnorm(x,mean = -2,sd = .75)*.5 + dnorm(x,mean = 2,sd = .75)*.5},color="darkblue") + theme_void() +
  geom_line(aes(x = c(0,0),y=c(0,.4)),linetype="dotted") + 
  annotate(geom="text", x= 0 ,y = -0.1,label="0",size=4) +
  annotate(geom="text", x = .75 ,y =-0.095,label="->",size=4) +
  annotate(geom="text", x= -.75 ,y =-0.095,label="<-",size=4) +
  annotate(geom="text", x= 1.5 ,y =-0.095,label="+",size=4) +
  annotate(geom="text", x= -1.5 ,y =-0.095,label="-",size=4) +
  geom_hline(yintercept = -0.005) +
  xlab("Deviation from trait mean") +
  ggtitle("Bimodal") +scale_y_continuous(limits =c(-0.1,1.3)) +
  theme(axis.title.x = element_text(),
        plot.title = element_text(hjust = 0.5,color="red3")) + 
  scale_x_continuous(limits = c(-5,5))

p3 = ggplot() +
  stat_function(fun = function(x) {dlnorm(x,0,1) },color="darkblue") + theme_void() +
  geom_line(aes(x = c(1.62346,1.62346),y=c(0,.225)),linetype="dotted") + 
  annotate(geom="text", x= 1.62346+0 ,y = -0.1,label="0",size=4) +
  annotate(geom="text", x = 1.62346+.75 ,y =-0.095,label="->",size=4) +
  annotate(geom="text", x= 1.62346 -.75 ,y =-0.095,label="<-",size=4) +
  annotate(geom="text", x= 1.62346+1.5 ,y =-0.095,label="+",size=4) +
  annotate(geom="text", x= 1.62346 -1.5 ,y =-0.095,label="-",size=4) +
  geom_hline(yintercept =  -0.005) +
  xlab("Deviation from trait mean") +
  ggtitle("Skewed") +scale_y_continuous(limits =c(-0.1,1.3)) +
  theme(axis.title.x = element_text(),
        plot.title = element_text(hjust = 0.5,color="red3")) +
  scale_x_continuous(limits=c(0,10))

p4 = ggplot() +
  stat_function(fun = function(x) {(dnorm(x, mean = 0, sd = 1) * 0.25)  +
      (dnorm(x, mean = 0, sd = 0.25) *0.75) },
      alpha=0.5,linewidth=0.7,color = "darkblue")+
  scale_x_continuous(limits=c(-5,5)) +
  geom_line(aes(x = c(0,0),y=c(0,1.3)),linetype="dotted") + 
  annotate(geom="text", x= 0 ,y = -0.1,label="0",size=4) +
  annotate(geom="text", x = .75 ,y =-0.095,label="->",size=4) +
  annotate(geom="text", x= -.75 ,y =-0.095,label="<-",size=4) +
  annotate(geom="text", x= 1.5 ,y =-0.095,label="+",size=4) +
  annotate(geom="text", x= -1.5 ,y =-0.095,label="-",size=4) +
  geom_hline(yintercept = -0.005) +scale_y_continuous(limits = c(-0.1,1.3)) +
  xlab("Deviation from trait mean") +
  ggtitle("Platykurtic") + theme_void() + 
  theme(axis.title.x = element_text(),
        plot.title = element_text(hjust = 0.5,color="red3"))


library("PearsonDS")
moments <- c(mean = 0,variance = 1,skewness = 0, kurtosis = 100)

p5 = ggplot() +
  stat_function(fun = function(x) {(dpearson(x,moments = moments))},
                alpha=0.5,linewidth=0.7,color = "darkblue")+
  scale_x_continuous(limits=c(-5,5)) +
  geom_line(aes(x = c(0,0),y=c(0,.5)),linetype="dotted") + 
  annotate(geom="text", x= 0 ,y = -0.1,label="0",size=4) +
  annotate(geom="text", x = .75 ,y =-0.095,label="->",size=4) +
  annotate(geom="text", x= -.75 ,y =-0.095,label="<-",size=4) +
  annotate(geom="text", x= 1.5 ,y =-0.095,label="+",size=4) +
  annotate(geom="text", x= -1.5 ,y =-0.095,label="-",size=4) +
  geom_hline(yintercept = -0.005) +scale_y_continuous(limits =c(-0.1,1.3)) +
  xlab("Deviation from trait mean") +
  ggtitle("Leptokurtic") + theme_void() + 
  theme(axis.title.x = element_text(),
        plot.title = element_text(hjust = 0.5,color="red3"))
library(patchwork)
tiff("~/bbc-secondary/research/POSA_20230314_TR01Shiny_VBCS-718/fig1b.tif",width=700,height=450)
p1+p2+p3+p4+p5 + plot_layout(guides="collect")&  theme(plot.margin = margin(.75,.75,.75,.75, "cm"))
dev.off()

