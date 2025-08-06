
#########################Figure 5A



aha

good_theme5A<-theme(legend.position =  c(0.75,.55),
                    plot.tag =  element_text(colour='black',size=22,face='bold'),
                    plot.tag.position = c(0.02,.98),
                    axis.text.x = element_text(colour="black",size=16,face="bold"),
                    axis.text.y = element_text(colour="black",size=16,face="bold"),
                    axis.title = element_text(colour="black",size=18,face="bold"),
                    plot.title = element_text(hjust = 0.5,colour="black",size=22,face="plain"),
                    panel.background = element_rect(fill = 'white'),
                    axis.line = element_line(colour = "black", size = 1))


data5A<-read.csv(choose.files(),header = TRUE)

plot5A<-ggplot(data5A, aes(fill=Genotype, y=Aperture, x=Letter)) + 
  geom_boxplot(width = 0.5, alpha = 0.8, outlier.shape = NA, position = position_dodge(width = 0.6)) +
  geom_jitter(aes(fill = Genotype), 
              position = position_jitterdodge(jitter.width = 0.15, dodge.width = 0.6), 
              size = 2, alpha = 0.8, color = "black", shape = 21, stroke = 0.7,) +
  labs(y=expression(paste(italic('A. thaliana')," Stomatal  Aperture "( mu*m))), x="",title='',tag='a')+
  scale_x_discrete(limit = c("B","D","E","F","G","H","I"),labels=c("B" = "Dark", "D"="0 mM","E"="0.4 mM","F"="1 mM","G"="30 mM","H"="50 mM","I"="100 mM"))+
  good_theme5A+theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  theme(legend.title=element_blank())+
  theme(legend.text = element_text(colour="black", size=14 ))+
  scale_fill_manual(breaks=c('A','B','Col-0','Taha1-6'),values=c("black","gray50","red", "pink"),labels=c("A"="Col-0 (Dark)","B"=  expression(paste(italic('aha1-6'),(Dark)))  ,  "Col-0" = "Col-0","Taha1-6" = expression(paste(italic('aha1-6')))))+
  scale_color_manual(breaks=c('A','B','Col-0','Taha1-6'),values=c("black", "black","black", "black"),labels=c("A"="Dark","B"=  "Dark",  "Col-0" = "Col-0","Taha1-6" = expression(paste(italic('aha1-6')))))+
  theme(legend.title=element_blank())+
  theme(legend.text = element_text(colour="black", size=16 ))+
  ylim(0,2.5)+
  geom_segment(aes(x = 1.5, y = 2.1, xend =7.5, yend = 2.1),size=1,col="red")+
  
  geom_text(x=4.7, y=2.3, label="Red light",col="red", size=7, fontface="bold")+
  

  theme(legend.position="top")

###############Figure 5B

plot5B <- ggplot() + 
  theme_void() 
labs(tag = 'B')+ good_theme5C

good_theme5C<-theme(legend.position =  'top',
                   plot.tag =  element_text(colour='black',size=22,face='bold'),
                   plot.tag.position = c(0.02,.98),
                   axis.text.x = element_text(colour="black",size=16,face="bold"),
                   axis.text.y = element_text(colour="black",size=16,face="bold"),
                   axis.title = element_text(colour="black",size=18,face="bold"),
                   plot.title = element_text(hjust = 0.5,colour="black",size=22,face="plain"),
                   panel.background = element_rect(fill = 'white'),
                   axis.line = element_line(colour = "black", size = 1))


data5C<-read.csv(choose.files(),header = TRUE)
# Set factor levels in correct order: Mock (left), Mannitol (middle), Sucrose (right)
data5B$Treatment <- factor(data5B$Treatment, levels = c("Mock", "Mannitol", "Sucrose"))


# Build plot
plot5C <- ggplot(data5B, aes(fill = Treatment, y = Value, x = Group)) + 
  geom_boxplot(width = 0.5, alpha = 0.8, outlier.shape = NA,
               position = position_dodge(width = 0.6)) +
  geom_jitter(aes(fill = Treatment), 
              position = position_jitterdodge(jitter.width = 0.15, dodge.width = 0.6), 
              size = 2, alpha = 0.8, color = "black", shape = 21, stroke = 0.7) +
  labs(y = "Phosphorylation level", x = "", title = '', tag = 'c') +
  scale_x_discrete(limits = c("A", "B"), 
                   labels = c("A" = "pH 6.5", "B" = "pH 5.5")) +
  scale_fill_manual(values = c("Mock" = "gray", "Mannitol" = "green", "Sucrose" = "blue"),
                    breaks = c("Mock", "Mannitol", "Sucrose"),
                    labels = c("Mock", "Mannitol", "Sucrose")) +
  good_theme5C +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.title = element_blank(),
        legend.text = element_text(colour = "black", size = 16)) +
  
  # Add bracket lines
  geom_segment(data = bracket_data1,
               aes(x = x_start, xend = x_end, y =y - 0.15, yend = y - 0.15),
               inherit.aes = FALSE, size = 0.6, color = "black") +
  
  # Add asterisks above brackets
  geom_text(data = bracket_data1,
            aes(x = x_text, y = y_text, label = label),
            inherit.aes = FALSE, size = 5, color = "black")

#############Figure 5D

data5D<-read.csv(choose.files(),header = TRUE)


plot5D<-ggplot(data5D, aes(fill = Group, y = Value, x = Letter)) + 
  geom_boxplot(width = 0.5, alpha = 0.8, outlier.shape = NA, position = position_dodge(width = 0.6)) +
  geom_jitter(aes(fill = Group), 
              position = position_jitterdodge(jitter.width = 0.15, dodge.width = 0.6), 
              size = 2, alpha = 0.8, color = "black", shape = 21, stroke = 0.7) +
  labs(y = "Phosphorylation level", x = "", title = '', tag = 'd') +
  scale_x_discrete(limits = c("A", "B", "C"), labels = c("A" = "1 mM", "B" = "30 mM", "C" = "100 mM")) +
  good_theme5C + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1, family = "Arial")) +  # Change font here
  theme(legend.title = element_blank(),
        legend.text = element_text(colour = "black", size = 16, family = "Arial")) +  # Change font here
  scale_fill_manual(breaks = c('A', 'B'), values = c("green", "blue"),
                    labels = c("A" = "Mannitol", "B" = "Sucrose")) +
##geom_segment(data = bracket_data,
  geom_segment(data = bracket_data,
               aes(x = x - 0.15, xend = x + 0.15, y = y - 0.15, yend = y - 0.15),
               inherit.aes = FALSE, color = "black", size = 0.6) +
  
  # Asterisk and ns labels
  geom_text(data = bracket_data,
            aes(x = x, y = y, label = label),
            inherit.aes = FALSE, size = 5, color = "black")

#############Figure 5E

# Load your data
data5E <- read.csv(file.choose(), header = TRUE)

library(ggplot2)
library(dplyr)
library(ggpubr)

# Load the data
plot5E <- ggplot(data5E, aes(x = Time, y = Value, fill = Treatment)) +
  geom_boxplot(position = position_dodge(0.75), width = 0.6, alpha = 0.8, outlier.shape = NA) +
  geom_jitter(position = position_jitterdodge(jitter.width = 0.1, dodge.width = 0.75),
              shape = 21, size = 2.5, stroke = 0.4, color = "black", alpha = 0.8) +
  scale_fill_manual(values = c("Mannitol" = "green", "Sucrose" = "blue")) +
  labs(title = "", x = "Time (minutes)", y = "Phosphorylation level", tag = 'e') +
  geom_segment(data = bracket_dataE,
               aes(x = time_x - 0.2, xend = time_x + 0.2, y = y - 0.15, yend = y - 0.15),
               inherit.aes = FALSE, color = "black", size = 0.6) +
  geom_text(data = bracket_dataE,
            aes(x = time_x, y = y, label = label),
            inherit.aes = FALSE, size = 5) +
  theme(legend.title = element_blank(),
        legend.text = element_text(colour = "black", size = 16, family = "Arial")) +
  good_theme5C




grid.arrange(plot5A, plot5B, plot5C, plot5D, plot5E, ncol=2, nrow=3, heights=c(1, 1, 1))
