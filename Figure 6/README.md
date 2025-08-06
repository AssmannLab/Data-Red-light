
##################################Figure 6################################



#Col-0 9AC

data6A<-read.csv(choose.files(),header = TRUE)

good_theme6A<-theme(legend.position =  c(0.8,.7),
                    plot.tag =  element_text(colour='black',size=22,face='bold'),
                    plot.tag.position = c(0.02,.98),
                    axis.text.x = element_text(colour="black",size=16,face="bold"),
                    axis.text.y = element_text(colour="black",size=16,face="bold"),
                    axis.title = element_text(colour="black",size=18,face="bold"),
                    plot.title = element_text(hjust = 0.5,colour="black",size=22,face="plain"),
                    panel.background = element_rect(fill = 'white'),
                    axis.line = element_line(colour = "black", size = 1))


plot6A <-  ggplot(data6A, aes(x = Letter, y = Aperture, fill = Genotype)) + 
  geom_boxplot(width = 0.5, alpha = 0.8, outlier.shape = NA, position = position_dodge(width = 0.6)) +
  geom_jitter(aes(fill = Genotype), 
              position = position_jitterdodge(jitter.width = 0.15, dodge.width = 0.6), 
              size = 2, alpha = 0.8, color = "black", shape = 21, stroke = 0.7,) +
  
  labs(y = expression(paste(italic('A. thaliana'), " Stomatal  Aperture (" * mu * "m)")), x = "", title = '', tag = 'a') +
  
  scale_x_discrete(limit = c("B", "D", "E", "F", "G", "H", "I"),
                   labels = c("B" = "Dark", "D" = "0 mM", "E" = "0.4 mM", "F" = "1 mM", "G" = "30 mM", "H" = "50 mM", "I" = "100 mM")) +
  
  good_theme6A +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.title = element_blank(),
        legend.text = element_text(colour = "black", size = 16),
        legend.position = "top") +
  
  scale_fill_manual(breaks = c("Dark", 'A', 'B'),
                    values = c("gray", "red", "pink"),  # all white fills for points
                    labels = c("Dark" = "Dark", "A" = "Control", "B" = "9-AC")) +
  
  scale_color_manual(breaks = c("Dark", 'A', 'B'),
                     values = c("black", "black", "black"),
                     labels = c("Dark" = "Dark", "A" = "Control", "B" = "9-AC")) +
  
########Figure 6B
  
data6B<-read.csv(choose.files(),header = TRUE)

plot6B<-ggplot(data6B,aes(fill=Genotype, y=Aperture, x=Letter)) + 
  geom_boxplot(width = 0.5, alpha = 0.8, outlier.shape = NA, position = position_dodge(width = 0.6)) +
  geom_jitter(aes(fill = Genotype), 
              position = position_jitterdodge(jitter.width = 0.15, dodge.width = 0.6), 
              size = 2, alpha = 0.8, color = "black", shape = 21, stroke = 0.7,) +
  labs(y=expression(paste(italic('A. thaliana')," Stomatal  Aperture "( mu*m))), x="",title='',tag='b')+
  scale_x_discrete(limit = c("B","D","E","F","G","H","I"),labels=c("B" = "Dark","D"="0 mM","E"="0.4 mM","F"="1 mM","G"="30 mM","H"="50 mM","I"="100 mM"))+
  good_theme6A+theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  theme(legend.title=element_blank())+
  theme(legend.text = element_text(colour="black", size=14 ))+
  scale_fill_manual(breaks=c('A','B','Col-0','slac1-3'),values=c("Black","gray50","red", "pink"),labels=c("A"="Col-0 (Dark)","B"=  expression(paste(italic('slac1-3 '), (Dark))), "Col-0" = "Col-0","slac1-3" = expression(paste(italic('slac1-3')))))+
  scale_color_manual(breaks=c('A','B','Col-0','slac1-3'),values=c("black", "black","black", "black"),labels=c("A"="Dark","B"="Dark", "Col-0" = "Col-0","slac1-3" = expression(paste(italic('slac1-3')))))+
  theme(legend.title=element_blank())+
  theme(legend.text = element_text(colour="black", size=12 ))+
  geom_segment(aes(x = 1.5,  y = 3.5, xend = 7.5,yend = 3.5),size=1,col="red")+
  ylim(0,4)+
  geom_text(x=4.6, y=3.9, label="Red light",col="red", size=7, fontface="bold")+
  
  
#########Figure 6C

good_theme6C<-theme(legend.position =  c(0.75,.15),
                    plot.tag =  element_text(colour='black',size=22,face='bold'),
                    plot.tag.position = c(0.02,.98),
                    axis.text.x = element_text(colour="black",size=16,face="bold"),
                    axis.text.y = element_text(colour="black",size=16,face="bold"),
                    axis.title = element_text(colour="black",size=18,face="bold"),
                    plot.title = element_text(hjust = 0.5,colour="black",size=22,face="plain"),
                    panel.background = element_rect(fill = 'white'),
                    axis.line = element_line(colour = "black", size = 1))

data6C<-read.csv(choose.files(),header = TRUE)
# Corrected ggplot code
plot6C <- ggplot(data6C, aes(y = gsw, x = obs, color = Group, shape = Group)) +
  stat_summary(geom = "point", fun = mean, position = "dodge", size = 5) +
  stat_summary(geom = "errorbar", fun.data = mean_se) +
  labs(
    y = expression(paste('g'['sw'] * ' (mol H'['2'] * 'O m'^'2' * 's'^'-1' * ')')),
    x = 'Time (min)',
    tag = 'c',
    title = expression(paste(italic('')))
  ) +
  good_theme6C +
  ylim(0, 0.22) +
  xlim(3, 59) +
  scale_color_manual(
    breaks = c('a', 'b', 'c', 'd'),
    values = c("black", "gray", "green", "blue"),
    labels = c(
      "a" = "Col-0",
      "b" = "Col-0+Sucrose",
      "c" = expression(paste(italic('slac1-3'))),
      "d" = expression(paste(italic('slac1-3'), "+Sucrose")))) +
  scale_shape_manual(
    breaks = c('a', 'b', 'c', 'd'),
    values = c(15, 16, 17, 18),
    labels = c(
      "a" = "Col-0",
      "b" = "Col-0+Sucrose",
      "c" = expression(paste(italic('slac1-3'))),
      "d" = expression(paste(italic('slac1-3'), "+Sucrose"))
    )
  ) +
  geom_segment(aes(x = 13, y = 0.18, xend = 59, yend = 0.18), size = 2, color = 'red') +
  geom_text(x = 30, y = 0.195, label = "Red light", col = "red", size = 6, fontface = "bold") +
  geom_segment(aes(x = 3, y = 0.18, xend = 13, yend = 0.18), size = 2, color = 'black') +
  geom_text(x = 9, y = 0.195, label = "Dark", col = "black", size = 6, fontface = "bold") +
  theme(legend.title = element_blank()) +
  theme(legend.text = element_text(colour = "black", size = 16))+


grid.arrange(plot6A, plot6B, plot6C,  nrow=3, heights=c(1, 1, 1))
