
####################Figure 4A ########################################################3
# Load your data
data4A <- read.csv(choose.files(), header = TRUE)

# Define the theme
good_theme2 <- theme(
  legend.position = 'top',
  plot.tag = element_text(colour = 'black', size = 22, face = 'bold'),
  plot.tag.position = c(0.02, .98),
  axis.text.x = element_text(colour = "black", size = 16, face = "bold"),
  axis.text.y = element_text(colour = "black", size = 16, face = "bold"),
  axis.title = element_text(colour = "black", size = 18, face = "bold"),
  plot.title = element_text(hjust = 0.5, colour = "black", size = 22, face = "plain"),
  panel.background = element_rect(fill = 'white'),
  axis.line = element_line(colour = "black", size = 1))

# Make the plot
plot_4A <- ggplot(data4A, aes(x = Letter, y = Aperture, fill = Ttreatment)) + 
  geom_boxplot(width = 0.5, alpha = 0.8, outlier.shape = NA, position = position_dodge(width = 0.6)) +
  geom_jitter(aes(fill = Ttreatment), 
              position = position_jitterdodge(jitter.width = 0.15, dodge.width = 0.6), 
              size = 2, alpha = 0.8, color = "black", shape = 21, stroke = 0.7,) +
  labs(y = expression(paste(italic('A. thaliana'), " Stomatal  Aperture (", mu, "m)")), x = "", title = '', tag = 'a') +
  scale_x_discrete(
    limit = c("B", "C", "D", "E", "F", "G", "H"),
    labels = c("B" = "Dark", "C" = "0 mM", "D" = "0.4 mM", "E" = "1 mM", "F" = "30 mM", "G" = "50 mM", "H" = "100 mM")
  ) +
  scale_fill_manual(
    breaks = c('B', 'C', 'D', 'E'),
    values = c("gray", "red4", "#FF0000", "pink"),
    labels = c("B" = "Dark", "C" = "Control", "D" = "Sucrose", "E" = "Sorbitol")
  ) +
  good_theme2 +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme(legend.title = element_blank(),
        legend.text = element_text(colour = "black", size = 16),
  ylim(0, 3.6)) +
  annotate("text", x = 1.5, y = 3.5, label = "Anova, p = 1e-6", size = 5)+
  # Add annotations as in original plot
  geom_segment(aes(x = 1.5, y = 3.2, xend = 7.5, yend = 3.2), size = 1, col = "red") +

#############Figure 4B

data4B<-read.csv(choose.files(),header = TRUE)




good_theme2D<-theme(legend.position =  'top',
                    plot.tag =  element_text(colour='black',size=22,face='bold'),
                    plot.tag.position = c(0.02,.98),
                    axis.text.x = element_text(colour="black",size=16,face="bold"),
                    axis.text.y = element_text(colour="black",size=16,face="bold"),
                    axis.title = element_text(colour="black",size=18,face="bold"),
                    plot.title = element_text(hjust = 0.5,colour="black",size=22,face="plain"),
                    panel.background = element_rect(fill = 'white'),
                    axis.line = element_line(colour = "black", size = 1))

plot_4B<-ggplot(data4B, aes(fill=Genotype, y=Aperture, x=Letter)) + 
  geom_boxplot(width = 0.5, alpha = 0.8, outlier.shape = NA) +
  geom_jitter(aes(fill = Genotype), 
              position = position_jitterdodge(jitter.width = 0.15, dodge.width = 0.6), 
              size = 2, alpha = 0.8, color = "black", shape = 21, stroke = 0.7,) +
  labs(y=expression(paste(italic('A. thaliana')," Stomatal  Aperture "( mu*m))), x="",title='',tag='b')+
  scale_x_discrete(limit = c("B","C","D","E"),labels=c("A" = "Initial","B" = "Dark", "C"=" 0 mM","D"="1 mM","E"="100 mM"))+
  good_theme2D+theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  scale_fill_manual(breaks=c('B','Control','DCMU'),values=c("gray","red","pink","Pink","white"),labels=c("B"="Dark","Control" = "No inhibitor","DCMU" = "DCMU"))+
  scale_color_manual(breaks=c('B','Control','DCMU'),values=c('black','black','black','black'),labels=c("B"="Dark","Control" = "No inhibitor","DCMU" = "DCMU"))+
  theme(legend.title=element_blank())+
  theme(legend.text = element_text(colour="black", size=16 ))+
  ylim(0,2.4)+
  annotate("text", x = 1.2, y = 2.4, label = "Anova, p = 1e-5", size = 5)+
  geom_text(x=3, y=2.4, label="Red light",col="red", size=7, fontface="bold")+
  geom_segment(aes(x = 1.5,  y = 2.2, xend = 4.5,yend = 2.2),size=1,col="red")+
  
#############Figure 4C


data4C <- read.csv(choose.files(), header = TRUE)


good_theme4C <- theme(
  legend.position = 'top',
  plot.tag = element_text(colour = 'black', size = 22, face = 'bold'),
  plot.tag.position = c(0.02, .98),
  axis.text.x = element_text(colour = "black", size = 16, face = "bold"),
  axis.text.y = element_text(colour = "black", size = 16, face = "bold"),
  axis.title = element_text(colour = "black", size = 18, face = "bold"),
  plot.title = element_text(hjust = 0.5, colour = "black", size = 22, face = "plain"),
  panel.background = element_rect(fill = 'white'),
  axis.line = element_line(colour = "black", size = 1),
  text = element_text(family = "")
)

plot_4C<-ggplot(data4C, aes(fill=Genotype, y=Aperture, x=Letter)) + 
  geom_boxplot(width = 0.5, alpha = 0.8, outlier.shape = NA) +
  geom_jitter(aes(fill = Genotype), 
              position = position_jitterdodge(jitter.width = 0.15, dodge.width = 0.6), 
              size = 2, alpha = 0.8, color = "black", shape = 21, stroke = 0.7,) +
  labs(y=expression(paste(italic('A. thaliana')," Stomatal  Aperture "( mu*m))), x="",title='',tag='c')+
  scale_x_discrete(limit = c("B","D","E","F","G","H","I"),labels=c("B" = "Dark", "D"="0 mM","E"="0.4 mM","F"="1 mM","G"="30 mM","H"="50 mM","I"="100 mM"))+
  good_theme4C+theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  theme(legend.title=element_blank())+
  theme(legend.text = element_text(colour="black", size=14 ))+
  scale_fill_manual(breaks=c('A','B','Col','PhyB'),values=c("gray","gray50","red", "pink"),labels=c("A"="Col-0 (Dark)","B"=  expression(paste(italic('aha1-6'),(Dark)))  ,  "Col" = "Col-0","PhyB" = expression(paste(italic('phyB')))))+
  scale_color_manual(breaks=c('A','B','Col','PhyB'),values=c("black", "black","black", "black"),labels=c("A"="Col-0 (Dark)","B"=  expression(paste(italic('aha1-6'),(Dark)))  ,  "Col-0" = "Col-0","PhyB" = expression(paste(italic('aha1-6')))))+
  theme(legend.title=element_blank())+
  theme(legend.text = element_text(colour="black", size=16 ))+
  ylim(0,2.8)+
  geom_segment(aes(x = 1.5, y = 2.55, xend =7.5, yend = 2.55),size=1,col="red")+
  
  geom_text(x=4.7, y=2.8, label="Red light",col="red", size=7, fontface="bold")+
  
  annotate("text", x = 1.9, y = 2.8, label = "Anova,  p < 2e-14", size = 5)+
  
  theme(legend.position="top")

######################Figure 4D

data4D<-read.csv(choose.files(),header = TRUE)


good_theme4D<-theme(legend.position =  c(0.15,.75),
                    plot.tag =  element_text(colour='black',size=22,face='bold'),
                    plot.tag.position = c(0.02,.98),
                    axis.text.x = element_text(colour="black",size=16,face="bold"),
                    axis.text.y = element_text(colour="black",size=16,face="bold"),
                    axis.title = element_text(colour="black",size=18,face="bold"),
                    plot.title = element_text(hjust = 0.5,colour="black",size=22,face="plain"),
                    panel.background = element_rect(fill = 'white'),
                    axis.line = element_line(colour = "black", size = 1))


plot_4D<-ggplot(data4D, aes( y=gsw,x=obs,color=Genotype,shape=Genotype))+
  stat_summary(geom = "point", fun = mean, position = "dodge",size=5) +
  stat_summary(geom = "errorbar", fun.data = mean_se)+
  labs(y=expression(paste(    'g'['sw']*' (mol H'['2']*'O m'^'2'*'s'^'-1'*')')), x='Time (min)',tag='d',title=expression(paste(italic(''))))+
  good_theme4D+
  ylim(0,0.17)+
  xlim(3,60)+
  scale_color_manual(breaks=c('Col-0','PhyB'),values=c("green", "blue","red", "pink"),labels=c("A"="Col-0 (Dark)","B"=  expression(paste(italic('aha1-6'),(Dark)))  ,  "Col-0" = "Col-0","PhyB" = expression(paste(italic('phyB')))))+
  scale_shape_manual(breaks=c('Col-0','PhyB'),values=c(15,16,17,18),labels=c("A"="Col-0 (Dark)","B"=  expression(paste(italic('aha1-6'),(Dark)))  ,  "Col-0" = "Col-0","PhyB" = expression(paste(italic('phyB')))))+
  geom_segment(aes(x = 13, y = 0.155, xend = 60, yend = 0.155),size=2,color='red')+
  
  geom_text(x=30, y=0.165, label="Red light",col="red", size=6, fontface="bold")+
  
  geom_segment(aes(x = 5,  y = 0.155, xend = 13,  yend = 0.155),size=2,color='black')+
  
  geom_text(x=9, y=0.165, label="Dark",col="Black", size=6, fontface="bold")+
  theme(legend.title=element_blank())+
  theme(legend.text = element_text(colour="black", size=16 ))

############Figure 4E

data4E<-read.csv(choose.files(),header = TRUE)


good_theme4E<-theme(legend.position =  'top',,
                    plot.tag =  element_text(colour='black',size=22,face='bold'),
                    plot.tag.position = c(0.02,.98),
                    axis.text.x = element_text(colour="black",size=16,face="bold"),
                    axis.text.y = element_text(colour="black",size=16,face="bold"),
                    axis.title = element_text(colour="black",size=18,face="bold"),
                    plot.title = element_text(hjust = 0.5,colour="black",size=22,face="plain"),
                    panel.background = element_rect(fill = 'white'),
                    axis.line = element_line(colour = "black", size = 1))

plot_4E<-ggplot(data4E, aes(fill=group, y=Aperture, x=Letter)) + 
  geom_boxplot(width = 0.5, alpha = 0.8, outlier.shape = NA) +
  geom_jitter(aes(fill = group), 
              position = position_jitterdodge(jitter.width = 0.15, dodge.width = 0.6), 
              size = 2, alpha = 0.8, color = "black", shape = 21, stroke = 0.7,) +
  labs(y=expression(paste(italic('A. thaliana')," Stomatal  Aperture "( mu*m))), x="",title='',tag='e')+
  scale_x_discrete(limit = c("A","B","C","D"),labels=c("A" = "Dark", "B"=" 0 mM","C"="1 mM","D"="100 mM"))+
  good_theme4E+theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  scale_fill_manual(breaks=c('A','Control','NAG'),values=c('gray','red','pink'),labels=c("A"="Dark","Control" = "No inhibitor","NAG" = "NAG"))+
  scale_color_manual(breaks=c('A','Control','NAG'),values=c('black','black','black'),labels=c("A"="Dark","Control" = "No inhibitor","NAG" = "NAG"))+
  theme(legend.title=element_blank())+
  theme(legend.text = element_text(colour="black", size=16 ))+
  ylim(0,2.7)+annotate("text", x = 1.2, y = 2.6, label = "Anova, p = 1.1e-7", size = 5)+
  geom_segment(aes(x = 1.5,  y = 2.4,  xend =4.5,  yend = 2.4),size=0.5,col="red")+
  geom_text(x=2.7, y=2.65, label="Red light",col="red", size=7, fontface="bold")+

  
  ##############Figure 4F
  
  
data4F<-read.csv(choose.files(),header = TRUE)


plot_4F<-ggplot(data4F, aes(fill=Genotype, y=Aperture, x=Letter)) + 
  geom_boxplot(width = 0.5, alpha = 0.8, outlier.shape = NA) +
  geom_jitter(aes(fill = Genotype), 
              position = position_jitterdodge(jitter.width = 0.15, dodge.width = 0.6), 
              size = 2, alpha = 0.8, color = "black", shape = 21, stroke = 0.7,) +
  labs(y=expression(paste(italic('A. thaliana')," Stomatal  Aperture "( mu*m))), x="",title='',tag='f')+
  scale_x_discrete(limit = c("A","B","C","D"),labels=c("A" = "Dark", "B"=" 0 mM","C"="1 mM","D"="100 mM"))+
  good_theme4E+theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  scale_fill_manual(
    breaks = c("A", "B", "C", "D"),
    values = c("gray", "Gray50", "red", "pink"),
    labels = c(
      "A" = "Col-0 (Dark)",
      "B" = bquote(italic("35S::HXK1")~"(Dark)"),
      "C" = "Col-0",
      "D" = bquote(italic("35S::HXK1"))
    )
  ) +
  scale_color_manual(
    breaks = c("A", "B", "C", "D"),
    values = c("black", "black", "black", "black"),
    labels = c(
      "A" = "Dark (Col-0)",
      "B" = bquote("Dark ("~italic("35S::HXK1")~")"),
      "C" = "Col-0",
      "D" = bquote(italic("35S::HXK1"))
    ))+
 theme(legend.title=element_blank())+
  theme(legend.text = element_text(colour="black", size=10 ))+
  ylim(0,2.7)+
  geom_segment(aes(x = 1.5, y = 2.4,xend =4.5, yend = 2.4),size=1,col="red")+
  geom_text(x=2.7, y=2.6, label="Red light",col="red", size=7, fontface="bold")+
  
  theme(legend.position="top")+
annotate("text", x = 1.2, y = 2.6, label = "Anova, p = 4e-6", size = 5)





grid.arrange(plot_4A,plot_4B,plot_4C,plot_4D,plot_4E,plot_4F, nrow=3)
