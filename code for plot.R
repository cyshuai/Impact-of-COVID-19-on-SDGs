library(ggplot2)
library(ggalt)
library(reshape2)
###plot Figure 1
dumbell<-read.csv(file="dumbell.csv")
colnames(dumbell)[1] = "SDG"
dumbell$SDG<- c("Overall Score", paste("No Poverty\n" , "(SDG 1: Target 1.2)", sep=" "), paste("Zero Hunger\n" , "(SDG 2: Target 2.1)", sep=" "), paste("Good Health and Well-Being\n " , "(SDG 3: Targets 3.2,3.4,3.9,3.b and 3.c)", sep=" "), paste("Quality Education\n" , "(SDG 4: Target 4.1)", sep=" "), paste("Clean Water and Sanitation\n" , "(SDG 6: Targets 6.1 and 6.2)", sep=" "), paste("Affordable and Clean Energy\n", "(SDG 7: Target 7.1)", sep=" "), paste("Decent Work and Economic Growth\n" , "(SDG 8: Targets 8.1,8.2,8.5 and 8.10)", sep=" "), paste("Industry, Innovation and Infrastructure\n" , "(SDG 9: Targets 9.1,9.2,9.5 and 9.a)", sep=" "), paste("Reduced Inequalities\n" , "(SDG 10: Target 10.4)", sep=" "), paste("Sustainable Cities and Communities\n" , "(SDG 11: Target 11.2)", sep=" "), paste("Responsible Consumption and Production\n" , "(SDG 12: Targets 12.2 and 12.4)", sep=" "), paste("Climate Action\n" , "(SDG 13: Target 13.1)", sep=" "), paste("Life Below Water\n" , "(SDG 14: Target 14.4)", sep=" "), paste("Life on Land\n" , "(SDG 15: Target 15.6)", sep=" "), paste("Peace, Justice and Strong Institutions\n" , "(SDG 16: Target 16.5)", sep=" "), paste("Partenerships for the Goals\n" , "(SDG 17: Targets 17.1,17.3,17.6 and 17.11)", sep=" "))

dumbell$SDG<-factor(dumbell$SDG, levels= rev(dumbell$SDG))

ggplot(aes(x=without,xend=with,y=SDG),data=dumbell)+
  geom_dumbbell(colour_x = "black",colour_xend = "red",size_x = 2,size_xend = 2,size=0.5,color="gray")+
  geom_point(data=dumbell,aes(x=with,y=SDG,size=4*abs(with-without)),alpha=0.2,color="red")+
  geom_rect(data=dumbell, aes(xmin=113, xmax=116, ymin=-Inf, ymax=Inf), fill="grey") +
  geom_text(data=dumbell, aes(label=round(with-without,1), y=SDG, x=114.5), fontface="bold", size=3,color=ifelse(dumbell$with- dumbell$without>0,"black","red")) +
  annotate("text", x =93, y ="Overall Score", label ="With COVID-19", size=3.5, vjust=-0.8, fontface="bold", color="red")+
  annotate("text", x =114.5, y ="Overall Score", label ="Diff", size=3.5, vjust=-0.8, fontface="bold", color="black")+
  annotate("text", x =106, y ="Overall Score", label ="No COVID-19", size=3.5, vjust=-0.8, fontface="bold",color="black")+
  geom_vline(xintercept=100,lty=2,lwd=0.5)+
  theme_light()+
  theme(panel.grid.minor.x =element_blank(),
        legend.position = c("none")
  )+
  xlab("SDG score")

###plot Figure 2
SDG_year_PI.m<- read.csv(file="SDG_year_PI.m.csv")
colnames(SDG_year_PI.m)[1] = "Year"
SDG_year_PI.m$SDG<-factor(SDG_year_PI.m $SDG, levels=c("Overall score","SDG 1","SDG 2","SDG 3","SDG 4","SDG 5","SDG 6","SDG 7","SDG 8","SDG 9","SDG 10","SDG 11","SDG 12","SDG 13","SDG 14","SDG 15","SDG 16","SDG 17"))
ggplot(SDG_year_PI.m,aes(x= Year,y=median,color=factor(Index))) + 
  geom_point()+
  geom_line()+
  facet_wrap(.~SDG, nrow=3, scales="free")+scale_x_continuous(breaks=seq(2020,2024,2))+  ylab("SDG score")+ xlab("Year")+ scale_colour_manual(name  =" ", values=c("red","black"),labels = c("With COVID-19","No COVID-19"))+ scale_fill_manual(name  =" ", values=c("red","black"),labels = c("With COVID-19","No COVID-19"))+
  theme(
    panel.background = element_blank(),
    panel.border = element_rect(fill = NA),
    text = element_text(size =14), 
    axis.text.y = element_text(size=13),
    legend.position = c(0.92,0.13),
    strip.background =element_rect(fill="white"),
    axis.text.x = element_text(size=13,hjust=0.8),
    panel.grid.major.x = element_line(color = "grey80",size=0.1),
    panel.grid.major.y = element_line(color = "grey80",size=0.1),
    panel.grid.minor.x = element_line(color = "grey80",size=0.05),
    panel.grid.minor.y = element_line(color = "grey80",size=0.05))

###plot Figure 3
Boxplot<- read.csv(file="Boxplot.csv")

Boxplot1<-Boxplot[,1:2]
Boxplot1.m<-melt(Boxplot1,measure.vars=factor(colnames(Boxplot1)[c(1:2)]))

ggplot( aes(x= variable, y= value,fill=variable),data=Boxplot1.m)+
  geom_boxplot() + xlab("variable")+ ylab("Difference in SDG indicator scores between\n with and no-COVID-19 scenarios")+
  scale_x_discrete(name="",labels=c("Short-term impact on EMDE","Short-term impact on AE"))+
  geom_dotplot(binaxis='y', stackdir='center', dotsize=0.5)+
  theme(
    legend.position="none",
    panel.background = element_blank(),
    panel.border = element_rect(fill = NA),
    axis.text.y = element_text(size = 12),
    axis.text.x= element_text(size=12, angle =45, hjust=0.9,vjust=0.9),
    panel.grid.major.x = element_line(color = "grey80",size=0.1),
    panel.grid.major.y = element_line(color = "grey80",size=0.1),
    panel.grid.minor.x = element_line(color = "grey80",size=0.05),
    panel.grid.minor.y = element_line(color = "grey80",size=0.05),
    axis.title.x = element_text(size=12),
    axis.title.y = element_text(size=12)
  )

Boxplot2<-Boxplot[,3:4]
Boxplot2.m<-melt(Boxplot2,measure.vars=factor(colnames(Boxplot2)[c(1:2)]))

ggplot( aes(x= variable, y= value,fill=variable),data=Boxplot2.m)+
  geom_boxplot() + xlab("variable")+ ylab("Difference in SDG indicator scores between\n with and no-COVID-19 scenarios")+
  geom_dotplot(binaxis='y', stackdir='center', dotsize=0.5)+
  scale_x_discrete(name="",labels=c("Long-term impact on EMDE","Long-term impact on AE"))+
  geom_dotplot(binaxis='y', stackdir='center', dotsize=0.5)+
  theme(
    legend.position="none",
    panel.background = element_blank(),
    panel.border = element_rect(fill = NA),
    axis.text.y = element_text(size = 12),
    axis.text.x= element_text(size=12, angle =45, hjust=0.9,vjust=0.9),
    panel.grid.major.x = element_line(color = "grey80",size=0.1),
    panel.grid.major.y = element_line(color = "grey80",size=0.1),
    panel.grid.minor.x = element_line(color = "grey80",size=0.05),
    panel.grid.minor.y = element_line(color = "grey80",size=0.05),
    axis.title.x = element_text(size=12),
    axis.title.y = element_text(size=12)
  )

Boxplot3<-Boxplot[,5:6]
Boxplot3.m<-melt(Boxplot3,measure.vars=factor(colnames(Boxplot3)[c(1:2)]))

ggplot( aes(x= variable, y= value,fill=variable),data=Boxplot3.m)+
  geom_boxplot() + xlab("variable")+ ylab("Number of lagged-behind\n years due to COVID-19")+
  scale_x_discrete(name="",labels=c("Long-term impact on EMDE","Long-term impact on AE"))+
  geom_dotplot(binaxis='y', stackdir='center', dotsize=0.5)+
  theme(
    legend.position="none",
    panel.background = element_blank(),
    panel.border = element_rect(fill = NA),
    axis.text.y = element_text(size = 12),
    axis.text.x= element_text(size=12, angle =45, hjust=0.9,vjust=0.9),
    panel.grid.major.x = element_line(color = "grey80",size=0.1),
    panel.grid.major.y = element_line(color = "grey80",size=0.1),
    panel.grid.minor.x = element_line(color = "grey80",size=0.05),
    panel.grid.minor.y = element_line(color = "grey80",size=0.05),
    axis.title.x = element_text(size=12),
    axis.title.y = element_text(size=12)
  )
