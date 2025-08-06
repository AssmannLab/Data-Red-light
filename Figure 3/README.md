
##################Figure 3A

data3A <- read.csv(choose.files(), header = TRUE)

good_theme3 <- theme(
  legend.position = c(0.8, .9),
  plot.tag = element_text(colour = 'black', size = 22, face = 'bold'),
  plot.tag.position = c(0.02, .98),
  axis.text.x = element_text(colour = "black", size = 16, face = "bold"),
  axis.text.y = element_text(colour = "black", size = 16, face = "bold"),
  axis.title = element_text(colour = "black", size = 16, face = "bold"),
  plot.title = element_text(hjust = 0.5, colour = "black", size = 22, face = "plain"),
  panel.background = element_rect(fill = 'white'),
  axis.line = element_line(colour = "black", size = 1)
)

plot_3A <- ggplot(data3A, aes(x = Letter, y = Aperture, fill = Letter)) +
  geom_boxplot(width = 0.5, alpha = 0.8, outlier.shape = NA) +
  geom_jitter(width = 0.15, size = 3, alpha = 0.8, fill = "white", color = "black", shape = 21, stroke = 0.7) +
  labs(
    y = expression(paste(italic('V. faba'), " Stomatal Aperture (" ,mu*m , ")")),
    x = "",
    title = 'Fructose',
    tag = 'a'
  ) +
  scale_x_discrete(
    limits = c("A", "B", "C", "D"),
    labels = c("A" = "Dark", "B" = "0 mM", "C" = "1 mM", "D" = "100 mM")
  ) +
  scale_fill_manual(
    breaks = c("A", "B", "C", "D"),
    values = c("black", "#fa6b5c", "#fa6b5c", "#fa6b5c"),
    labels = c("Dark", "0 mM", "1 mM", "100 mM")
  ) +
  ylim(0, 4.2) +
  good_theme3 +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.text = element_text(colour = "black", size = 16),
    legend.position = "none"
  ) +
  geom_segment(aes(x = 1.5, y = 3.4, xend = 4.5, yend = 3.4), size = 1, col = 'red') +
  geom_text(x = 2.6, y = 3.65, label = "Red light", col = "red", size = 7, fontface = "bold") +
  stat_compare_means(method = "anova", label.x = 0.9, label.y = 4, size = 5) +
  stat_compare_means(label = "p.signif", method = "t.test", ref.group = "A", size = 6, label.y = c(2, 1.5, 2.5, 1.55, 2.1, 0.9)) +
  stat_compare_means(method = "t.test", label = "p.format", label.y = 2.65, size = 5, comparisons = list(c("B", "C"))) +
  stat_compare_means(method = "t.test", label = "p.format", label.y = 2.95, size = 5, comparisons = list(c("B", "D")))

############Figure 3B


data3B <- read.csv(choose.files(), header = TRUE)

plot_3B <- ggplot(data3B, aes(x = Group, y = Aperture, fill = Group)) +
  geom_boxplot(width = 0.5, alpha = 0.8, outlier.shape = NA) +
  geom_jitter(width = 0.15, size = 3, alpha = 0.8, fill = "white", color = "black", shape = 21, stroke = 0.7) +
  labs(
    y = expression(paste(italic('A. thaliana'), " Stomatal Aperture (", mu*m, ")")),
    x = "",
    title = 'Fructose',
    tag = 'b'
  ) +
  scale_x_discrete(
    limits = c("B", "C", "D", "E", "F"),
    labels = c("B" = "Dark", "C" = "0 mM", "D" = "0.4 mM", "E" = "1 mM", "F" = "50 mM")
  ) +
  scale_fill_manual(
    breaks = c("B", "C", "D", "E", "F"),
    values = c("black", "#fa6b5c", "#fa6b5c", "#fa6b5c", "#fa6b5c"),
    labels = c("Dark", "0 mM", "0.4 mM", "1 mM", "50 mM")
  ) +
  ylim(0, 3.3) +
  good_theme3 +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.text = element_text(colour = "black", size = 16),
    legend.position = "none"
  ) +
  geom_segment(aes(x = 1.5, y = 2.9, xend = 5.5, yend = 2.9), size = 1, col = 'red') +
  geom_text(x = 3.6, y = 3.1, label = "Red light", col = "red", size = 7, fontface = "bold") +
  stat_compare_means(method = "anova", label.x = 1.05, label.y = 3.2, size = 5) +
  stat_compare_means(label = "p.signif", method = "t.test", ref.group = "B", size = 6, label.y = c(1, 1.1, 1.7, 2.3, 1.8, 1.6)) +
  stat_compare_means(method = "t.test", label = "p.format", label.y = 1.9, size = 5, comparisons = list(c("C", "D"))) +
  stat_compare_means(method = "t.test", label = "p.format", label.y = 2.2, size = 5, comparisons = list(c("C", "E"))) +
  stat_compare_means(method = "t.test", label = "p.format", label.y = 2.5, size = 5, comparisons = list(c("C", "F")))

######################Figure 3C


data3C <- read.csv(choose.files(), header = TRUE)

plot_3C <- ggplot(data3C, aes(x = Group, y = Aperture, fill = Group)) +
  geom_boxplot(width = 0.5, alpha = 0.8, outlier.shape = NA) +
  geom_jitter(width = 0.15, size = 3, alpha = 0.8, fill = "white", color = "black", shape = 21, stroke = 0.7) +
  labs(
    y = expression(paste(italic('V. faba'), " Stomatal Aperture (",mu*m, ")")),
    x = "",
    title = 'Sucrose',
    tag = 'c'
  ) +
  scale_x_discrete(
    limits = c("B", "C", "D", "E", "F", "G", "H"),
    labels = c("B" = "Dark", "C" = "0 mM", "D" = "0.4 mM", "E" = "1 mM", "F" = "30 mM", "G" = "50 mM", "H" = "100 mM")
  ) +
  scale_fill_manual(
    breaks = c("B", "C", "D", "E", "F", "G", "H"),
    values = c("black", "#fa6b5c", "#fa6b5c", "#fa6b5c", "#fa6b5c", "#fa6b5c", "#fa6b5c"),
    labels = c("Dark", "0 mM", "0.4 mM", "1 mM", "30 mM", "50 mM", "100 mM")
  ) +
  ylim(0, 5.8) +
  good_theme3 +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.text = element_text(colour = "black", size = 16),
    legend.position = "none"
  ) +
  geom_segment(aes(x = 1.5, y = 4.9, xend = 7.5, yend = 4.9), size = 1, col = 'red') +
  geom_text(x = 4.3, y = 5.25, label = "Red light", col = "red", size = 7, fontface = "bold") +
  stat_compare_means(method = "anova", label.x = 1.3, label.y = 5.8, size = 5) +
  stat_compare_means(label = "p.signif", method = "t.test", ref.group = "B", size = 6, label.y = c(2, 2.7, 3.1, 3.3, 2.8, 2.4, 1.9)) +
  stat_compare_means(method = "t.test", label = "p.format", label.y = 3.5, size = 5, comparisons = list(c("C", "D"))) +
  stat_compare_means(method = "t.test", label = "p.format", label.y = 3.9, size = 5, comparisons = list(c("C", "E"))) +
  stat_compare_means(method = "t.test", label = "p.format", label.y = 4.3, size = 5, comparisons = list(c("C", "F")))


#######################Figure 3D

data3D <- read.csv(choose.files(), header = TRUE)

plot_3D <- ggplot(data3D, aes(x = Letter, y = Aperture, fill = Letter)) +
  geom_boxplot(width = 0.5, alpha = 0.8, outlier.shape = NA) +
  geom_jitter(width = 0.15, size = 3, alpha = 0.8, fill = "white", color = "black", shape = 21, stroke = 0.7) +
  labs(
    y = expression(paste(italic('A. thaliana'), " Stomatal Aperture (", mu*m, ")")),
    x = "",
    title = 'Sucrose',
    tag = 'd'
  ) +
  scale_x_discrete(
    limits = c("B", "C", "D", "E", "F", "G", "H"),
    labels = c("B" = "Dark", "C" = "0 mM", "D" = "0.4 mM", "E" = "1 mM", "F" = "30 mM", "G" = "50 mM", "H" = "100 mM")
  ) +
  scale_fill_manual(
    breaks = c("B", "C", "D", "E", "F", "G", "H"),
    values = c("black", "#fa6b5c", "#fa6b5c", "#fa6b5c", "#fa6b5c", "#fa6b5c", "#fa6b5c"),
    labels = c("Dark", "0 mM", "0.4 mM", "1 mM", "30 mM", "50 mM", "100 mM")
  ) +
  ylim(0, 2.5) +
  good_theme3 +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.text = element_text(colour = "black", size = 16),
    legend.position = "none"
  ) +
  geom_segment(aes(x = 1.5, y = 2.2, xend = 7.5, yend = 2.2), size = 1, col = 'red') +
  geom_text(x = 4.3, y = 2.35, label = "Red light", col = "red", size = 7, fontface = "bold") +
  stat_compare_means(method = "anova", label.x = 1.3, label.y = 2.5, size = 5) +
  stat_compare_means(label = "p.signif", method = "t.test", ref.group = "B", size = 6, label.y = c(1, 1, 1.45, 1.65, 1.1, 1, 0.9)) +
  stat_compare_means(method = "t.test", label = "p.format", label.y = 1.5, size = 5, comparisons = list(c("C", "D"))) +
  stat_compare_means(method = "t.test", label = "p.format", label.y = 1.7, size = 5, comparisons = list(c("C", "E"))) +
  stat_compare_means(method = "t.test", label = "p.format", label.y = 1.9, size = 5, comparisons = list(c("C", "F")))



## Figure 3E  Separate Boxplots for Glucose, Fructose, and Sucrose
library(ggplot2)
library(ggpubr)
library(gridExtra)

# Load individual datasets
glucose_df <-read.csv(choose.files(),header = TRUE)
fructose_df <- read.csv(choose.files(),header = TRUE)
sucrose_df <- read.csv(choose.files(),header = TRUE)

good_theme3F<-theme(legend.position =  c(0.3,.6),,
                   plot.tag =  element_text(colour='black',size=22,face='bold'),
                   plot.tag.position = c(0.02,.98),
                   axis.text.x = element_text(colour="black",size=16,face="bold"),
                   axis.text.y = element_text(colour="black",size=16,face="bold"),
                   axis.title = element_text(colour="black",size=16,face="bold"),
                   plot.title = element_text(hjust = 0.5,colour="black",size=22,face="plain"),
                   panel.background = element_rect(fill = 'white'),
                   axis.line = element_line(colour = "black", size = 1))


# Ensure factor levels
glucose_df$Time <- factor(glucose_df$Time, levels = c("0", "60", "120"))
fructose_df$Time <- factor(fructose_df$Time, levels = c("0", "60", "120"))
sucrose_df$Time <- factor(sucrose_df$Time, levels = c("0", "60", "120"))

# Define t-test comparisons
comparisons <- list(c("0", "60"), c("0", "120"))

# Plot for Glucose
plot_glucose <- ggplot(glucose_df, aes(x = Time, y = Value)) +
  geom_boxplot(fill = "skyblue", outlier.shape = NA) +
  geom_jitter(width = 0.15, shape = 21, size = 2.5, fill = "white", color = "black") +
  labs(title = "Glucose", x = "Time (min)", y = "Apoplastic concentration (mM)") +
  stat_compare_means(method = "t.test", label = "p.format", comparisons = comparisons, label.y = c(0.8, 0.9), size = 5) +
good_theme3F+
ylim(0,1)

# Plot for Fructose
plot_fructose <- ggplot(fructose_df, aes(x = Time, y = Value)) +
  geom_boxplot(fill = "lightgreen", outlier.shape = NA) +
  geom_jitter(width = 0.15, shape = 21, size = 2.5, fill = "white", color = "black") +
  labs(title = "Fructose", x = "Time (min)", y = "Apoplastic concentration (mM)") +
  stat_compare_means(method = "t.test", label = "p.format", comparisons = comparisons, label.y = c(1.6, 1.8), size = 5) +
good_theme3F+
ylim(0,2)

# Plot for Sucrose

plot_sucrose <- ggplot(sucrose_df, aes(x = Time, y = Value)) +
  geom_boxplot(fill = "greenyellow", outlier.shape = NA) +
  geom_jitter(width = 0.15, shape = 21, size = 2.5, fill = "white", color = "black") +
  labs(title = "Sucrose", x = "Time (min)", y = "Apoplastic concentration (mM)") +
  stat_compare_means(method = "t.test", label = "p.format", comparisons = comparisons, label.y = c(3.2, 3.6), size = 5) +
  good_theme3F+
ylim(0,4)

plot_3E <- grid.arrange(
  textGrob("e", x = 0.02, y = 0.6, gp = gpar(fontsize = 22, fontface = "bold")),
  arrangeGrob(plot_glucose, plot_fructose, plot_sucrose, ncol = 3),
  nrow = 2,
  heights = c(0.05, 0.6)
)

###############Figure 3F

data3F<-read.csv(choose.files(),header = TRUE)
good_theme1<-theme(legend.position =  c(0.3,.6),,
                   plot.tag =  element_text(colour='black',size=22,face='bold'),
                   plot.tag.position = c(0.02,.98),
                   axis.text.x = element_text(colour="black",size=16,face="bold"),
                   axis.text.y = element_text(colour="black",size=16,face="bold"),
                   axis.title = element_text(colour="black",size=18,face="bold"),
                   plot.title = element_text(hjust = 0.5,colour="black",size=22,face="plain"),
                   panel.background = element_rect(fill = 'white'),
                   axis.line = element_line(colour = "black", size = 1))

plot_3F<-ggplot(data3F, aes(shape=Group, y=gsw, x=Time,color=Group))+
  stat_summary(geom = "point", fun = mean, position = "dodge",size=5) +
  stat_summary(geom = "errorbar", fun.data = mean_se)+
  labs(y=expression(paste( italic('V. faba')  ,   '  g'['sw']*' (mol H'['2']*'O m'^'-2'*'s'^'-1'*')')), x='Time (min)',tag='f')+
  good_theme1+
  scale_shape_manual(breaks=c('a','b','c','d','e'),values=c(17,16,15,18,19),labels=c("a"='Control',"b" = "1 mM Sucrose","c" = "1 mM Mannitol","d" = "100 mM Sucrose","e"= "100 mM Mannitol"))+
  scale_color_manual(breaks=c('a','b','c','d','e'),values=c("purple","greenyellow","darkgreen","cyan",'blue'),labels=c("a"='Control',"b" = "1 mM Sucrose","c" = "1 mM Mannitol","d" = "100 mM Sucrose","e"= "100 mM Mannitol"))+
  theme(legend.title=element_blank())+
  theme(legend.text = element_text(colour="black", size=14 ))+                  
  ylim(0,0.23)+
  xlim(2,80)+
  geom_segment(aes(x = 10, y = 0.22, xend = 80, yend = 0.22),size=2,color='red')+
  
  geom_text(x=45, y=0.23, label="Red light",col="red", size=7, fontface="bold")+
  
  geom_segment(aes(x = 2,  y = 0.22, xend = 10,  yend = 0.22),size=2,color='black')+
  
  geom_text(x=4.4, y=0.23, label="Dark",col="Black", size=7, fontface="bold")+
  
  

grid.arrange(plot_3A,plot_3B,plot_3C,plot_3D,plot_3E,plot_3F, nrow=3)









