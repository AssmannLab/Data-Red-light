
require(ggplot2)
require(gridExtra)
require(grid)
require(viridis)
require(ggpubr)
require(ggplot2)
library(ggpubr)
library(ggpmisc)
require(plyr)
library(ggpubr)
library(ggpmisc)
require(plyr)
library(agricolae)
library(extrafont)
library(ggprism)
library(patchwork)
library(magrittr)
library(reshape2)
font_import()
loadfonts(device="win")       #Register fonts for Windows bitmap output



good_theme<-theme(legend.position =  c(0.8,.9),
                  plot.tag =  element_text(colour='black',size=22,face='bold'),
                  plot.tag.position = c(0.02,.98),
                  axis.text.x = element_text(colour="black",size=16,face="bold"),
                  axis.text.y = element_text(colour="black",size=16,face="bold"),
                  axis.title = element_text(colour="black",size=18,face="bold"),
                  plot.title = element_text(hjust = 0.5,colour="black",size=16,face="plain"),
                  panel.background = element_rect(fill = 'white'),
                  axis.line = element_line(colour = "black", size = 1))




##############################################################################Figure 1


data1A<-read.csv(choose.files(),header = TRUE)

data1A <- read.csv(choose.files(), header = TRUE)

plot_1A <- ggplot(data1A, aes(x = Treatment, y = Aperture, fill = Treatment)) +
  geom_boxplot(width = 0.5, alpha = 0.8, outlier.shape = NA) +
  geom_jitter(width = 0.15, size = 3, alpha = 0.8, fill = "white", color = "black", shape = 21, stroke = 0.7) +
  labs(
    y = expression(paste(italic('V. faba'), " Stomatal Aperture (", mu*m, ")")),
    x = "",
    title = expression(paste(italic('V. faba to V. faba '))),
    tag = 'a'
  ) +
  scale_x_discrete(
    limits = c("A", "B", "C"),
    labels = c("A" = "Mock", "B" = "Dark-AF", "C" = "Red light-AF")
  ) +
  scale_fill_manual(
    breaks = c("A", "B", "C"),
    values = c("#fa6b5c", "#fa6b5c", "#fa6b5c"),
    labels = c("Mock", "Dark-AF", "Red light-AF")
  ) +
  ylim(0, NA) +
  good_theme +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.text = element_text(colour = "black", size = 16),
    legend.position = "none"
  ) +
  stat_compare_means(method = "anova", label.x = 1, label.y = 9, size = 5) +
  stat_compare_means(method = "t.test", label = "p.format", label.y = 6.4, size = 5, comparisons = list(c("A", "B"))) +
  stat_compare_means(method = "t.test", label = "p.format", label.y = 7.3, size = 5, comparisons = list(c("A", "C"))) +
  stat_compare_means(method = "t.test", label = "p.format", label.y = 8.1, size = 5, comparisons = list(c("B", "C")))

# Display plot
data1B <- read.csv(choose.files(), header = TRUE)

plot_1B <- ggplot(data1B, aes(x = Treatment, y = Aperture, fill = Treatment)) +
  geom_boxplot(width = 0.5, alpha = 0.8, outlier.shape = NA) +
  geom_jitter(width = 0.15, size = 3, alpha = 0.8, fill = "white", color = "black", shape = 21, stroke = 0.7) +
  labs(
    y = expression(paste(italic('A. thaliana'), " Stomatal Aperture (", mu*m, ")")),
    x = "",
    title = expression(paste(italic('A. thaliana to A. thaliana '))),
    tag = 'b'
  ) +
  scale_x_discrete(
    limits = c("A", "B", "C"),
    labels = c("A" = "Mock", "B" = "Dark-AF", "C" = "Red light-AF")
  ) +
  scale_fill_manual(
    breaks = c("A", "B", "C"),
    values = c("#fa6b5c", "#fa6b5c", "#fa6b5c"),
    labels = c("Mock", "Dark-AF", "Red light-AF")
  ) +
  ylim(0, NA) +
  good_theme +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.text = element_text(colour = "black", size = 16),
    legend.position = "none"
  ) +
  stat_compare_means(method = "anova", label.x = 1, label.y = 2.5, size = 5) +
  stat_compare_means(method = "t.test", label = "p.format", label.y = 1.7, size = 5, comparisons = list(c("A", "B"))) +
  stat_compare_means(method = "t.test", label = "p.format", label.y = 2, size = 5, comparisons = list(c("A", "C"))) +
  stat_compare_means(method = "t.test", label = "p.format", label.y = 2.2, size = 5, comparisons = list(c("B", "C")))


data1C <- read.csv(choose.files(), header = TRUE)

plot_1C <- ggplot(data1C, aes(x = Treatment, y = Aperture, fill = Treatment)) +
  geom_boxplot(width = 0.5, alpha = 0.8, outlier.shape = NA) +
  geom_jitter(width = 0.15, size = 3, alpha = 0.8, fill = "white", color = "black", shape = 21, stroke = 0.7) +
  labs(
    y = expression(paste(italic('A. thaliana'), " Stomatal Aperture (", mu*m, ")")),
    x = "",
    title = expression(paste(italic('V. faba to A. thaliana '))),
    tag = 'c'
  ) +
  scale_x_discrete(
    limits = c("A", "B", "C"),
    labels = c("A" = "Mock", "B" = "Dark-AF", "C" = "Red light-AF")
  ) +
  scale_fill_manual(
    breaks = c("A", "B", "C"),
    values = c("#fa6b5c", "#fa6b5c", "#fa6b5c"),
    labels = c("Mock", "Dark-AF", "Red light-AF")
  ) +
  ylim(0, NA) +
  good_theme +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.text = element_text(colour = "black", size = 16),
    legend.position = "none"
  ) +
  stat_compare_means(method = "anova", label.x = 1, label.y = 3.1, size = 5) +
  stat_compare_means(method = "t.test", label = "p.format", label.y = 1.7, size = 5, comparisons = list(c("A", "B"))) +
  stat_compare_means(method = "t.test", label = "p.format", label.y = 2.4, size = 5, comparisons = list(c("A", "C"))) +
  stat_compare_means(method = "t.test", label = "p.format", label.y = 2.7, size = 5, comparisons = list(c("B", "C")))



data1D <- read.csv(choose.files(), header = TRUE)

plot_1D <- ggplot(data1D, aes(x = Treatment, y = Aperture, fill = Treatment)) +
  geom_boxplot(width = 0.5, alpha = 0.8, outlier.shape = NA) +
  geom_jitter(width = 0.15, size = 3, alpha = 0.8, fill = "white", color = "black", shape = 21, stroke = 0.7) +
  labs(
    y = expression(paste(italic('V. faba'), " Stomatal Aperture (", mu*m, ")")),
    x = "",
    title = expression(paste(italic('A. thaliana to V. faba '))),
    tag = 'd'
  ) +
  scale_x_discrete(
    limits = c("A", "B", "C"),
    labels = c("A" = "Mock", "B" = "Dark-AF", "C" = "Red light-AF")
  ) +
  scale_fill_manual(
    breaks = c("A", "B", "C"),
    values = c("#fa6b5c", "#fa6b5c", "#fa6b5c"),
    labels = c("Mock", "Dark-AF", "Red light-AF")
  ) +
  ylim(0, NA) +
  good_theme +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.text = element_text(colour = "black", size = 16),
    legend.position = "none"
  ) +
  stat_compare_means(method = "anova", label.x = 1, label.y = 9, size = 5) +
  stat_compare_means(method = "t.test", label = "p.format", label.y = 6, size = 5, comparisons = list(c("A", "B"))) +
  stat_compare_means(method = "t.test", label = "p.format", label.y = 7.2, size = 5, comparisons = list(c("A", "C"))) +
  stat_compare_means(method = "t.test", label = "p.format", label.y = 8, size = 5, comparisons = list(c("B", "C")))



data1E <- read.csv(choose.files(), header = TRUE)

plot_1E <- ggplot(data1E, aes(x = Treatment, y = Aperture, fill = Treatment)) +
  geom_boxplot(width = 0.5, alpha = 0.8, outlier.shape = NA) +
  geom_jitter(width = 0.15, size = 3, alpha = 0.8, fill = "white", color = "black", shape = 21, stroke = 0.7) +
  labs(
    y = expression(paste(italic('V. faba'), " Stomatal Aperture (", mu*m, ")")),
    x = "",
    title = "",
    tag = 'e'
  ) +
  scale_x_discrete(
    limits = c("A", "B", "C"),
    labels = c("A" = "Mock", "B" = "Unfractionated", "C" = "< 3kDa fraction")
  ) +
  scale_fill_manual(
    breaks = c("A", "B", "C"),
    values = c("#fa6b5c", "#fa6b5c", "#fa6b5c"),
    labels = c("Mock", "Unfractionated", "< 3kDa fraction")
  ) +
  ylim(0, NA) +
  good_theme +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.text = element_text(colour = "black", size = 16),
    legend.position = "none"
  ) +
  stat_compare_means(method = "anova", label.x = 1, label.y = 10, size = 5) +
  stat_compare_means(method = "t.test", label = "p.format", label.y = 7, size = 5, comparisons = list(c("A", "B"))) +
  stat_compare_means(method = "t.test", label = "p.format", label.y = 7.8, size = 5, comparisons = list(c("A", "C"))) +
  stat_compare_means(method = "t.test", label = "p.format", label.y = 8.6, size = 5, comparisons = list(c("B", "C")))




data1F <- read.csv(choose.files(), header = TRUE)

plot_1F <- ggplot(data1F, aes(x = Treatment, y = Aperture, fill = Treatment)) +
  geom_boxplot(width = 0.5, alpha = 0.8, outlier.shape = NA) +
  geom_jitter(width = 0.15, size = 3, alpha = 0.8, fill = "white", color = "black", shape = 21, stroke = 0.7) +
  labs(
    y = expression(paste(italic('V. faba'), " Stomatal Aperture (", mu*m, ")")),
    x = "",
    title = "",
    tag = 'f'
  ) +
  scale_x_discrete(
    limits = c("A", "B", "C"),
    labels = c("A" = "Mock", "B" = "Untreated", "C" = "Heat")
  ) +
  scale_fill_manual(
    breaks = c("A", "B", "C"),
    values = c("#fa6b5c", "#fa6b5c", "#fa6b5c"),
    labels = c("Mock", "Untreated", "Heat")
  ) +
  ylim(0, NA) +
  good_theme +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.text = element_text(colour = "black", size = 16),
    legend.position = "none"
  ) +
  stat_compare_means(method = "anova", label.x = 1, label.y = 10, size = 5) +
  stat_compare_means(method = "t.test", label = "p.format", label.y = 7.2, size = 5, comparisons = list(c("A", "B"))) +
  stat_compare_means(method = "t.test", label = "p.format", label.y = 7.9, size = 5, comparisons = list(c("A", "C"))) +
  stat_compare_means(method = "t.test", label = "p.format", label.y = 8.6, size = 5, comparisons = list(c("B", "C")))

# Display plot
plot_1F

data1G <- read.csv(choose.files(), header = TRUE)

plot_1G <- ggplot(data1G, aes(x = Treatment, y = Aperture, fill = Treatment)) +
  geom_boxplot(width = 0.5, alpha = 0.8, outlier.shape = NA) +
  geom_jitter(width = 0.15, size = 3, alpha = 0.8, fill = "white", color = "black", shape = 21, stroke = 0.7) +
  labs(
    y = expression(paste(italic('V. faba'), " Stomatal Aperture (", mu*m, ")")),
    x = "",
    title = "",
    tag = 'g'
  ) +
  scale_x_discrete(
    limits = c("A", "B", "C"),
    labels = c("A" = "Mock", "B" = "No Pepsin", "C" = "Pepsin")
  ) +
  scale_fill_manual(
    breaks = c("A", "B", "C"),
    values = c("#fa6b5c", "#fa6b5c", "#fa6b5c"),
    labels = c("Mock", "No Pepsin", "Pepsin")
  ) +
  ylim(0, NA) +
  good_theme +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.text = element_text(colour = "black", size = 16),
    legend.position = "none"
  ) +
  stat_compare_means(method = "anova", label.x = 1.2, label.y = 10, size = 5) +
  stat_compare_means(method = "t.test", label = "p.format", label.y = 7.2, size = 5, comparisons = list(c("A", "B"))) +
  stat_compare_means(method = "t.test", label = "p.format", label.y = 7.9, size = 5, comparisons = list(c("A", "C"))) +
  stat_compare_means(method = "t.test", label = "p.format", label.y = 8.6, size = 5, comparisons = list(c("B", "C")))

grid.arrange(plot_1A,plot_1B,plot_1C,plot_1D,plot_1E,plot_1F,plot_1G, nrow=2)

#######################################plot 2
library(ggplot2)
library(dplyr)
library(ggpubr)
good_theme2e <- theme(
  legend.position = 'none',
  plot.tag = element_text(colour = 'black', size = 22, face = 'bold'),
  plot.tag.position = c(0.02, .98),
  axis.text.x = element_text(colour = "black", size = 14, face = "bold"),
  axis.text.y = element_text(colour = "black", size = 14, face = "bold"),
  axis.title = element_text(colour = "black", size = 14, face = "bold"),
  plot.title = element_text(hjust = 0.5, colour = "black", size = 16, face = "plain"),
  panel.background = element_rect(fill = 'white'),
  axis.line = element_line(colour = "black", size = 1)
)

# Load your data
dfmalicacid <- read.csv(file.choose(), header = TRUE)
Plotmalic<- ggplot(dfmalicacid, aes(x = Letter, y = Fold.change, fill = Letter)) +
  geom_boxplot(outlier.shape = NA, alpha = 0.6, color = "black") +
  geom_jitter(width = 0.15, size = 3, alpha = 0.8, fill = "white", color = "black", shape = 21, stroke = 0.7) +
  stat_compare_means( method = "t.test",  label = "p.format", size = 5, comparisons = list(c("A", "B")), 
    label.y = 8) +
  stat_compare_means(method = "t.test", label = "p.format", size = 5,comparisons = list(c("C", "D")), 
    label.y = 8) +
  scale_x_discrete(
    limits = c("A", "B", "C", "D"),labels = c(
      "A" = "At D",
      "B" = "At R",
      "C" = "Vf D",
      "D" = "Vf R")) +
  scale_fill_manual(  values = c("A" = "black", "B" = "#fa6b5c", "C" = "black", "D" = "#fa6b5c")) +
 theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    strip.text = element_text(face = "bold"),
    legend.position = "none") +
  labs( x = "", y = "Log Intensity",title="Malic acid")+ good_theme2e +ylim(2,10)


dfsucraose <- read.csv(file.choose(), header = TRUE)
Plotsucrose<- ggplot(dfsucraose, aes(x = Letter, y = Fold.change, fill = Letter)) +
  geom_boxplot(outlier.shape = NA, alpha = 0.6, color = "black") +
  geom_jitter(width = 0.15, size = 3, alpha = 0.8, fill = "white", color = "black", shape = 21, stroke = 0.7) +
  stat_compare_means( method = "t.test",  label = "p.format", size = 5, comparisons = list(c("A", "B")), 
                      label.y = 14) +
  stat_compare_means(method = "t.test", label = "p.format", size = 5,comparisons = list(c("C", "D")), 
                     label.y = 22) +
  scale_x_discrete(
    limits = c("A", "B", "C", "D"),labels = c(
      "A" = "At D",
      "B" = "At R",
      "C" = "Vf D",
      "D" = "Vf R")) +
  scale_fill_manual(  values = c("A" = "black", "B" = "#fa6b5c", "C" = "black", "D" = "#fa6b5c")) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    strip.text = element_text(face = "bold"),
    legend.position = "none") +
  labs( x = "", y = "Log Intensity",title="Sucrose")+ good_theme2e +ylim(0,24)


dfglucose <- read.csv(file.choose(), header = TRUE)
Plotglucose<- ggplot(dfglucose, aes(x = Letter, y = Fold.change, fill = Letter)) +
  geom_boxplot(outlier.shape = NA, alpha = 0.6, color = "black") +
  geom_jitter(width = 0.15, size = 3, alpha = 0.8, fill = "white", color = "black", shape = 21, stroke = 0.7) +
  stat_compare_means( method = "t.test",  label = "p.format", size = 5, comparisons = list(c("A", "B")), 
                      label.y = 6) +
  stat_compare_means(method = "t.test", label = "p.format", size = 5,comparisons = list(c("C", "D")), 
                     label.y = 6) +
  scale_x_discrete(
    limits = c("A", "B", "C", "D"),labels = c(
      "A" = "At D",
      "B" = "At R",
      "C" = "Vf D",
      "D" = "Vf R")) +
  scale_fill_manual(  values = c("A" = "black", "B" = "#fa6b5c", "C" = "black", "D" = "#fa6b5c")) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    strip.text = element_text(face = "bold"),
    legend.position = "none") +
  labs( x = "", y = "Log Intensity",title="Glucose")+ good_theme2e +ylim(3,7)




dftheronic <- read.csv(file.choose(), header = TRUE)
Plottheronic<- ggplot(dftheronic , aes(x = Letter, y = Fold.change, fill = Letter)) +
  geom_boxplot(outlier.shape = NA, alpha = 0.6, color = "black") +
  geom_jitter(width = 0.15, size = 3, alpha = 0.8, fill = "white", color = "black", shape = 21, stroke = 0.7) +
  stat_compare_means( method = "t.test",  label = "p.format", size = 5, comparisons = list(c("A", "B")), 
                      label.y = 3) +
  stat_compare_means(method = "t.test", label = "p.format", size = 5,comparisons = list(c("C", "D")), 
                     label.y = 3) +
  scale_x_discrete(
    limits = c("A", "B", "C", "D"),labels = c(
      "A" = "At D",
      "B" = "At R",
      "C" = "Vf D",
      "D" = "Vf R")) +
  scale_fill_manual(  values = c("A" = "black", "B" = "#fa6b5c", "C" = "black", "D" = "#fa6b5c")) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    strip.text = element_text(face = "bold"),
    legend.position = "none") +
  labs( x = "", y = "Log Intensity",title="Threonic acid")+ good_theme2e +ylim(0,4)


dfMethylindoleacetate<- read.csv(file.choose(), header = TRUE)
PlotMethylindoleacetate<- ggplot(dfMethylindoleacetate , aes(x = Letter, y = Fold.change, fill = Letter)) +
  geom_boxplot(outlier.shape = NA, alpha = 0.6, color = "black") +
  geom_jitter(width = 0.15, size = 3, alpha = 0.8, fill = "white", color = "black", shape = 21, stroke = 0.7) +
  stat_compare_means( method = "t.test",  label = "p.format", size = 5, comparisons = list(c("A", "B")), 
                      label.y = 6) +
  stat_compare_means(method = "t.test", label = "p.format", size = 5,comparisons = list(c("C", "D")), 
                     label.y = 6) +
  scale_x_discrete(
    limits = c("A", "B", "C", "D"),labels = c(
      "A" = "At D",
      "B" = "At R",
      "C" = "Vf D",
      "D" = "Vf R")) +
  scale_fill_manual(  values = c("A" = "black", "B" = "#fa6b5c", "C" = "black", "D" = "#fa6b5c")) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    strip.text = element_text(face = "bold"),
    legend.position = "none") +
  labs( x = "", y = "Log Intensity",title="Methyl indole 
   acetate")+ good_theme2e +ylim(2,7)


dfadenosine<- read.csv(file.choose(), header = TRUE)
Plotadenosine<- ggplot(dfadenosine , aes(x = Letter, y = Fold.change, fill = Letter)) +
  geom_boxplot(outlier.shape = NA, alpha = 0.6, color = "black") +
  geom_jitter(width = 0.15, size = 3, alpha = 0.8, fill = "white", color = "black", shape = 21, stroke = 0.7) +
  stat_compare_means( method = "t.test",  label = "p.format", size = 5, comparisons = list(c("A", "B")), 
                      label.y = 7) +
  stat_compare_means(method = "t.test", label = "p.format", size = 5,comparisons = list(c("C", "D")), 
                     label.y = 7) +
  scale_x_discrete(
    limits = c("A", "B", "C", "D"),labels = c(
      "A" = "At D",
      "B" = "At R",
      "C" = "Vf D",
      "D" = "Vf R")) +
  scale_fill_manual(  values = c("A" = "black", "B" = "#fa6b5c", "C" = "black", "D" = "#fa6b5c")) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    strip.text = element_text(face = "bold"),
    legend.position = "none") +
  labs( x = "", y = "Log Intensity",title="5-methylthio-
    adenosine")+ good_theme2e +ylim(5,8)


dfFumaricacid<- read.csv(file.choose(), header = TRUE)
PlotFumaricacid<- ggplot(dfFumaricacid , aes(x = Letter, y = Fold.change, fill = Letter)) +
  geom_boxplot(outlier.shape = NA, alpha = 0.6, color = "black") +
  geom_jitter(width = 0.15, size = 3, alpha = 0.8, fill = "white", color = "black", shape = 21, stroke = 0.7) +
  stat_compare_means( method = "t.test",  label = "p.format", size = 5, comparisons = list(c("A", "B")), 
                      label.y = 22) +
  stat_compare_means(method = "t.test", label = "p.format", size = 5,comparisons = list(c("C", "D")), 
                     label.y = 13) +
  scale_x_discrete(
    limits = c("A", "B", "C", "D"),labels = c(
      "A" = "At D",
      "B" = "At R",
      "C" = "Vf D",
      "D" = "Vf R")) +
  scale_fill_manual(  values = c("A" = "black", "B" = "#fa6b5c", "C" = "black", "D" = "#fa6b5c")) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    strip.text = element_text(face = "bold"),
    legend.position = "none") +
  labs( x = "", y = "Log Intensity",title="Fumaric acid")+ good_theme2e +ylim(5,25)




grid.arrange(Plottheronic,PlotMethylindoleacetate,Plotadenosine,Plotmalic,PlotFumaricacid,Plotglucose,Plotsucrose, nrow=2)






###########################################plot 3#################################################################################3

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

# Display plot
plot_3A


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

# Display plot
plot_3B

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

# Display plot
plot_3C


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

# Display plot
plot_3D

data3E<-read.csv(choose.files(),header = TRUE)
good_theme1<-theme(legend.position =  c(0.3,.6),,
                   plot.tag =  element_text(colour='black',size=22,face='bold'),
                   plot.tag.position = c(0.02,.98),
                   axis.text.x = element_text(colour="black",size=16,face="bold"),
                   axis.text.y = element_text(colour="black",size=16,face="bold"),
                   axis.title = element_text(colour="black",size=18,face="bold"),
                   plot.title = element_text(hjust = 0.5,colour="black",size=22,face="plain"),
                   panel.background = element_rect(fill = 'white'),
                   axis.line = element_line(colour = "black", size = 1))

plot_3E<-ggplot(data3E, aes(shape=Group, y=gsw, x=Time,color=Group))+
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
  
  
  geom_text(x=68, y=0.124, label="*",col="Black", size=5, fontface="bold")+
  geom_text(x=70, y=0.128, label="*",col="Black", size=5, fontface="bold")+
  geom_text(x=72, y=0.132, label="*",col="Black", size=5, fontface="bold")+
  geom_text(x=74, y=0.134, label="*",col="Black", size=5, fontface="bold")+
  geom_text(x=76, y=0.137, label="*",col="Black", size=5, fontface="bold")+
  geom_text(x=78, y=0.141, label="*",col="Black", size=5, fontface="bold")+
  geom_text(x=80, y=0.144, label="*",col="Black", size=5, fontface="bold")+
  
  geom_text(x=66, y=0.052, label="********************",col="Black", size=5, fontface="bold")+
  geom_text(x=59, y=0.032, label="******************************",col="Black", size=5, fontface="bold")


## Fig 3F  Separate Boxplots for Glucose, Fructose, and Sucrose
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

plot_3F <- grid.arrange(
  textGrob("e", x = 0.02, y = 0.6, gp = gpar(fontsize = 22, fontface = "bold")),
  arrangeGrob(plot_glucose, plot_fructose, plot_sucrose, ncol = 3),
  nrow = 2,
  heights = c(0.05, 0.6)
)

grid.arrange(plot_3A,plot_3B,plot_3C,plot_3D,plot_3F,plot_3E, nrow=3)



####################Figure 4 ########################################################3
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
  geom_text(x = 2, y = 1.5, label = "**", col = "Black", size = 6, fontface = "bold") +
  geom_text(x = 3.2, y = 1.7, label = "*", col = "Black", size = 6, fontface = "bold") +
  geom_text(x = 2.8, y = 2.1, label = "**", col = "Black", size = 6, fontface = "bold") +
  geom_text(x = 4.22, y = 1.7, label = "*", col = "Black", size = 6, fontface = "bold") +
  geom_text(x = 3.8, y = 2.55, label = "***", col = "Black", size = 6, fontface = "bold") +
  geom_text(x = 4.6, y = 3.5, label = "Red light", col = "red", size = 7, fontface = "bold") +
  geom_text(x = 4.75, y = 1.3, label = "*", col = "Black", size = 6, fontface = "bold") +
  geom_text(x = 5.24, y = 1.3, label = "*", col = "Black", size = 6, fontface = "bold") +
  geom_segment(aes(x = 3.7, y = 2.7, xend = 4.3, yend = 2.7), size = 0.5, col = "black") +
  geom_text(x = 3.95, y = 2.9, label = "0.0012", col = "Black", size = 5) +
  geom_segment(aes(x = 2.7, y = 2.4, xend = 3.3, yend = 2.4), size = 0.5, col = "black") +
  geom_text(x = 3, y = 2.6, label = "0.1", col = "Black", size = 5) +
  geom_segment(aes(x = 4.7, y = 1.5, xend = 5.3, yend = 1.5), size = 0.5, col = "black") +
  geom_text(x = 5, y = 1.7, label = "0.42", col = "Black", size = 5) +
  geom_segment(aes(x = 5.7, y = 1.3, xend = 6.3, yend = 1.3), size = 0.5, col = "black") +
  geom_text(x = 6, y = 1.5, label = "0.33", col = "Black", size = 5) +
  geom_segment(aes(x = 6.7, y = 0.9, xend = 7.3, yend = 0.9), size = 0.5, col = "black") +
  geom_text(x = 7, y = 1.1, label = "0.36", col = "Black", size = 5)




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
  
  geom_text(x=1.85, y=1.17, label="**",col="Black", size=6, fontface="bold")+
  geom_text(x=2.85, y=1.7, label="***",col="Black", size=6, fontface="bold")+
  geom_text(x=3.15, y=1.2, label="**",col="Black", size=6, fontface="bold")+

  
  geom_segment(aes(x = 1.7, y = 1.3, xend =2.3, yend = 1.3),size=0.5,col="black")+
  geom_segment(aes(x = 2.7, y = 1.85, xend =3.3, yend = 1.85),size=0.5,col="black")+
  geom_segment(aes(x = 3.7, y = 0.45, xend =4.3, yend = 0.45),size=0.5,col="black")+
  
  geom_text(x=1.95, y=1.5, label="0.003",col="Black", size=5)+
  geom_text(x=2.95, y=1.98, label="0.004",col="Black", size=5)+
  geom_text(x=3.95, y=0.54, label="0.69",col="Black", size=5)


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
  geom_segment(aes(x = 1.7, y = 1.65, xend =2.2, yend = 1.65),size=0.5,col="black")+
  geom_segment(aes(x = 2.7, y = 1.95, xend =3.3, yend = 1.95),size=0.5,col="black")+
  geom_segment(aes(x = 3.7, y = 2.2, xend =4.3, yend = 2.2),size=0.5,col="black")+
  geom_segment(aes(x = 4.7, y = 1.2, xend =5.3, yend = 1.2),size=0.5,col="black")+
  geom_segment(aes(x = 5.7, y = 1, xend =6.3, yend = 1),size=0.5,col="black")+
  geom_segment(aes(x = 6.7, y = 0.79, xend =7.3, yend = 0.79),size=0.5,col="black")+
  
  geom_text(x=1.9, y=1.8, label="ns",col="Black", size=5)+
  geom_text(x=2.95, y=2.1, label="ns",col="Black", size=5)+
  geom_text(x=3.95, y=2.35, label="ns",col="Black", size=5)+
  geom_text(x=4.95, y=1.35, label="ns",col="Black", size=5)+
  geom_text(x=5.95, y=1.15, label="ns",col="Black", size=5)+
  geom_text(x=6.95, y=0.95, label="ns",col="Black", size=5)+

  geom_text(x=1.82, y=1.3, label="*",col="Black", size=6, fontface="bold")+
  geom_text(x=2.1, y=1.5, label="*",col="Black", size=6, fontface="bold")+
  geom_text(x=2.85, y=1.8, label="*",col="Black", size=6, fontface="bold")+
  geom_text(x=3.2, y=1.7, label="*",col="Black", size=6, fontface="bold")+
  
  geom_text(x=3.85, y=2.05, label="*",col="Black", size=6, fontface="bold")+
  geom_text(x=4.15, y=2, label="*",col="Black", size=6, fontface="bold")+
  
  
  theme(legend.position="top")


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
  geom_segment(aes(x = 1.7, y = 1.25, xend =2.3, yend = 1.25),size=0.5,col="black")+
  geom_segment(aes(x = 2.7, y = 2.15, xend =3.3, yend = 2.15),size=0.5,col="black")+
  geom_segment(aes(x = 3.7, y = 0.95, xend =4.3, yend = 0.95),size=0.5,col="black")+
  geom_text(x=1.95, y=1.4, label="ns",col="Black", size=5)+
  geom_text(x=2.95, y=2.3, label="ns",col="Black", size=5)+
  geom_text(x=3.95, y=1.15, label="ns",col="Black", size=5)+
  geom_text(x=1.85, y=1.1, label="*",col="Black", size=6, fontface="bold")+
  geom_text(x=2.15, y=1, label="*",col="Black", size=6, fontface="bold")+
  geom_text(x=2.85, y=1.95, label="*",col="Black", size=6, fontface="bold")+
  geom_text(x=3.12, y=1.95, label="*",col="Black", size=6, fontface="bold")

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
  
  
  geom_segment(aes(x = 1.7, y = 1.4, xend =2.3, yend = 1.4),size=0.5,col="black")+
  geom_segment(aes(x = 2.7, y = 2.05, xend =3.3, yend = 2.05),size=0.5,col="black")+
  geom_segment(aes(x = 3.7, y = 1.2, xend =4.3, yend = 1.2),size=0.5,col="black")+
  
  geom_text(x=1.95, y=1.55, label="ns",col="Black", size=5)+
  geom_text(x=2.95, y=2.2, label="ns",col="Black", size=5)+
  geom_text(x=3.95, y=1.35, label="ns",col="Black", size=5)+
  
  geom_text(x=2.13, y=1.25, label="*",col="Black", size=6, fontface="bold")+
  geom_text(x=1.85, y=1.25, label="*",col="Black", size=6, fontface="bold")+
  geom_text(x=3.15, y=1.95, label="*",col="Black", size=6, fontface="bold")+
  geom_text(x=2.85, y=1.95, label="*",col="Black", size=6, fontface="bold")+
  theme(legend.position="top")+
annotate("text", x = 1.2, y = 2.6, label = "Anova, p = 4e-6", size = 5)





grid.arrange(plot_4A,plot_4B,plot_4C,plot_4D,plot_4E,plot_4F, nrow=3)


#########################Figure 5 



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
  
  
  geom_segment(aes(x = 1.7, y = 1.2, xend =2.2, yend = 1.2),size=0.5,col="black")+
  geom_segment(aes(x = 2.7, y = 1.6, xend =3.3, yend = 1.6),size=0.5,col="black")+
  geom_segment(aes(x = 3.7, y = 1.8, xend =4.3, yend = 1.8),size=0.5,col="black")+
  geom_segment(aes(x = 4.7, y = 1.15, xend =5.3, yend = 1.15),size=0.5,col="black")+
  geom_segment(aes(x = 5.7, y = 0.95, xend =6.3, yend = 0.95),size=0.5,col="black")+
  geom_segment(aes(x = 6.7, y = 0.95, xend =7.3, yend = 0.95),size=0.5,col="black")+
  
  geom_text(x=1.9, y=1.35, label="0.03",col="Black", size=5)+
  geom_text(x=2.95, y=1.75, label="0.02",col="Black", size=5)+
  geom_text(x=3.95, y=1.95, label="0.03",col="Black", size=5)+
  geom_text(x=4.95, y=1.3, label="0.46",col="Black", size=5)+
  geom_text(x=5.95, y=1.1, label="0.83",col="Black", size=5)+
  geom_text(x=6.95, y=1.1, label="0.42",col="Black", size=5)+
  
  geom_text(x=1.83, y=1.05, label="**",col="Black", size=6, fontface="bold")+
  geom_text(x=2.8, y=1.45, label="***",col="Black", size=6, fontface="bold")+
  geom_text(x=3.2, y=0.85, label="**",col="Black", size=6, fontface="bold")+
  
  geom_text(x=3.83, y=1.68, label="***",col="Black", size=6, fontface="bold")+
  geom_text(x=4.2, y=1.1, label="**",col="Black", size=6, fontface="bold")+
  
  geom_text(x=4.81, y=1, label="*",col="Black", size=6, fontface="bold")+
  geom_text(x=5.22, y=1, label="*",col="Black", size=6, fontface="bold")+
  geom_text(x=6.22, y=0.7, label="*",col="Black", size=6, fontface="bold")+
  annotate("text", x = 2, y = 2.3, label = "Anova: p = 7.6e-7", size = 5, family = "Times New Roman")+
  theme(legend.position="top")

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

# Brackets comparing Mannitol (center) to Sucrose (right)
bracket_data1 <- data.frame(
  x_start = c(1, 2),              # Mannitol positions
  x_end   = c(1 + 0.2, 2 + 0.2),  # Sucrose positions
  y       = c(2.0, 3.4),          # line height
  label   = c("0.12", "0.02"),
  x_text  = c(1 + 0.1, 2 + 0.1),  # midpoint for text
  y_text  = c(2.05, 3.45)
)

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



data5D<-read.csv(choose.files(),header = TRUE)

bracket_data <- data.frame(
  x = c(1, 2, 3),
  y = c(2.4, 3.5, 3.7),
  label = c("0.09", "0.02", "0.41")
)


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
  
  # Annotated segments and texts
  geom_segment(aes(x = 1.5, y = 2.5, xend = 7.5, yend = 2.5), size = 1, col = "red") +
  ylim(0, 2.8) +
  geom_text(x = 4.6, y = 2.8, label = "Red light", col = "red", size = 7, fontface = "bold") +
  geom_text(x = 2.18, y = 1.95, label = "***", col = "Black", size = 6, fontface = "bold") +
  geom_text(x = 1.82, y = 1.2, label = "**", col = "Black", size = 6, fontface = "bold") +
  geom_text(x = 2.78, y = 1.65, label = "***", col = "Black", size = 6, fontface = "bold") +
  geom_text(x = 3.25, y = 1.65, label = "***", col = "Black", size = 6, fontface = "bold") +
  geom_text(x = 3.75, y = 1.9, label = "***", col = "Black", size = 6, fontface = "bold") +
  geom_text(x = 4.25, y = 1.95, label = "***", col = "Black", size = 6, fontface = "bold") +
  geom_text(x = 5.17, y = 1.05, label = "**", col = "Black", size = 6, fontface = "bold") +
  geom_text(x = 4.83, y = 0.95, label = "*", col = "Black", size = 6, fontface = "bold") +
  geom_text(x = 6.17, y = 0.98, label = "*", col = "Black", size = 6, fontface = "bold") +
  
  geom_segment(aes(x = 1.7, y = 2.1, xend = 2.3, yend = 2.1), size = 0.5, col = "black") +
  geom_text(x = 1.95, y = 2.3, label = "0.009", col = "Black", size = 5) +
  
  geom_segment(aes(x = 2.7, y = 1.8, xend = 3.3, yend = 1.8), size = 0.5, col = "black") +
  geom_text(x = 2.95, y = 2, label = "ns", col = "Black", size = 5) +
  
  geom_segment(aes(x = 3.7, y = 2.1, xend = 4.3, yend = 2.1), size = 0.5, col = "black") +
  geom_text(x = 3.95, y = 2.3, label = "ns", col = "Black", size = 5) +
  
  geom_segment(aes(x = 4.7, y = 1.2, xend = 5.3, yend = 1.2), size = 0.5, col = "black") +
  geom_text(x = 5, y = 1.4, label = "ns", col = "Black", size = 5) +
  
  geom_segment(aes(x = 5.7, y = 1.1, xend = 6.3, yend = 1.1), size = 0.5, col = "black") +
  geom_text(x = 6, y = 1.3, label = "ns", col = "Black", size = 5) +
  
  geom_segment(aes(x = 6.7, y = 0.5, xend = 7.3, yend = 0.5), size = 0.5, col = "black") +
  geom_text(x = 7, y = 0.7, label = "ns", col = "Black", size = 5)+
  annotate("text", x = 2, y = 2.7, label = "Anova, p = 5.5e-12", size = 5)


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
  
  
  geom_text(x=1.85, y=1.3, label="*",col="Black", size=6, fontface="bold")+
  geom_text(x=2.1, y=2.5, label="**",col="Black", size=6, fontface="bold")+
  
  geom_text(x=2.8, y=2, label="**",col="Black", size=6, fontface="bold")+
  geom_text(x=3.2, y=2.2, label="***",col="Black", size=6, fontface="bold")+
  
  
  geom_text(x=3.75, y=2.4, label="***",col="Black", size=6, fontface="bold")+
  geom_text(x=4.25, y=2.2, label="***",col="Black", size=6, fontface="bold")+
  
  geom_text(x=4.75, y=1.25, label="**",col="Black", size=6, fontface="bold")+
  geom_text(x=5.2, y=1.8, label="*",col="Black", size=6, fontface="bold")+
  
  geom_text(x=5.75, y=0.83, label="",col="Black", size=6, fontface="bold")+
  geom_text(x=6.24, y=1.86, label="",col="Black", size=6, fontface="bold")+
  
  
  geom_segment(aes(x = 0.6, y = 1, xend =1.3, yend = 1),size=1,col="black")+
  geom_text(x=0.95, y=1.2, label="0.016",col="Black", size=5)+
  
  geom_segment(aes(x = 1.6, y = 2.7, xend =2.3, yend = 2.7),size=1,col="black")+
  geom_text(x=1.95, y=2.9, label="0.013",col="Black", size=5)+
  
  geom_segment(aes(x = 2.6, y = 2.7, xend =3.3, yend = 2.7),size=1,col="black")+
  geom_text(x=2.95, y=2.9, label="0.19",col="Black", size=5)+
  
  geom_segment(aes(x = 3.6, y = 2.7, xend =4.3, yend = 2.7),size=1,col="black")+
  geom_text(x=3.95, y=2.9, label="0.08",col="Black", size=5)+
  
  geom_segment(aes(x = 4.6, y = 2, xend =5.3, yend = 2),size=1,col="black")+
  geom_text(x=4.95, y=2.2, label="0.16",col="Black", size=5)+
  
  geom_segment(aes(x = 5.6, y = 2, xend =6.3, yend = 2),size=1,col="black")+
  geom_text(x=6, y=2.2, label="0.058",col="Black", size=5)+
  
  geom_segment(aes(x = 6.6, y = 1.2, xend =7.3, yend = 1.2),size=1,col="black")+
  geom_text(x=7, y=1.4, label="0.054",col="Black", size=5)+
  
  theme(legend.position="top")+
  annotate("text", x = 2, y = 4, label = "Anova, p = 2e-7", size = 5)


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
  geom_text(x=47, y=0.06, label="**************************",col="black", size=5, fontface="bold")
  
  
geom_text(x=43, y=0.17, label="*****************************",col="blue", size=6, fontface="bold")+
geom_text(x=46, y=0.16, label="************************",col="green", size=6, fontface="bold")+
geom_text(x=44, y=0.15, label="*****************************",col="gray", size=6, fontface="bold")

grid.arrange(plot6A, plot6B, plot6C,  nrow=3, heights=c(1, 1, 1))



##########extended dtata
dataE1<-read.csv(choose.files(),header = TRUE)

plotE1 <- library(ggplot2)
library(ggpubr)

library(ggplot2)
library(ggpubr)

plotE1 <- ggplot(dataE1, aes(x = factor(Time), y = Avg_Aperture, fill = Sucrose)) +
  # Boxplot and jitter
  geom_boxplot(position = position_dodge(0.75), width = 0.6, alpha = 0.8, outlier.shape = NA) +
  geom_jitter(position = position_jitterdodge(jitter.width = 0.1, dodge.width = 0.75),
              shape = 21, size = 2.5, stroke = 0.4, color = "black", alpha = 0.8) +
  
  # Fill colors
  scale_fill_manual(values = c("Control" = "green", "Sucrose" = "blue")) +
  
  # Axis and label
  labs(
    title = "",
    x = "Time (minutes)",
    y = expression(paste(italic('V. faba'), " Stomatal Aperture (", mu*m, ")")),
    tag = 'a'
  ) +
  
  # Statistical test results (closer to boxes)
  stat_compare_means(aes(group = Sucrose),
                     method = "t.test",
                     label = "p.signif",
                     size = 5,
                     label.y = 5) +
  
  # Red line (bottom) from x = 1 to x = 7 (Time = 0 to 180)
  annotate("segment", x = 1, xend = 7, y = 6, yend = 6, colour = "red", size = 1.5) +
  annotate("text", x = 4, y = 6.5, label = "Red light", color = "red", size = 5, fontface = "bold") +
  
  # Black line (bottom) only under Time = 210 (x = 8)
  annotate("segment", x = 7, xend = 8, y = 6, yend = 6, colour = "black", size = 1.5) +
  annotate("text", x = 7.5, y = 6.5, label = "Dark", color = "black", size = 5, fontface = "bold") +
  
  # Black line (bottom) only under Time = 210 (x = 8)
  annotate("segment", x = 0, xend = 1, y = 6, yend = 6, colour = "black", size = 1.5) +
  annotate("text", x = 0.6, y = 6.5, label = "Dark", color = "black", size = 5, fontface = "bold") +
  
  # Set y-limits to allow room for text and keep plots visible
  ylim(0, 6.5) +
  
  # Theme
  theme(
    legend.title = element_blank(),
    legend.text = element_text(colour = "black", size = 14, family = "Arial"),
    plot.margin = margin(t = 20, r = 10, b = 40, l = 10)  # extend bottom for annotation space
  ) +
  good_theme5C


################## extanded data
data<-read.csv(choose.files(),header = TRUE)


library(ggplot2)
library(ggplot2)
library(ggplot2)
library(ggpubr)

# Convert Species names to R expressions for italic facet labels
data$Species <- ifelse(
  data$Species == "A. thaliana", "italic('A. thaliana')",
  ifelse(data$Species == "V. faba", "italic('V. faba')", data$Species)
)

# Define custom theme
good_themeE1 <- theme(
  legend.position = 'none',
  plot.tag = element_text(colour = 'black', size = 16, face = 'plain'),
  plot.tag.position = c(0.02, .98),
  axis.text.x = element_text(colour = "black", size = 12, face = "plain"),
  axis.text.y = element_text(colour = "black", size = 16, face = "plain"),
  axis.title = element_text(colour = "black", size = 14, face = "plain"),
  plot.title = element_text(hjust = 0.5, colour = "black", size = 16, face = "bold"),
  strip.text = element_text(size = 12),
  panel.background = element_rect(fill = 'white'),
  axis.line = element_line(colour = "black", size = 1)
)

# Define the pairwise comparisons you want
my_comparisons <- list(c("Dark", "0 mM"), c("Dark", "1 mM"),c("0 mM", "1 mM"))

# Plot
ggplot(data = data, aes(x = Treatment, y = Value, fill = Treatment)) +
  geom_boxplot(width = 0.5, alpha = 0.8, outlier.shape = NA) +
  geom_jitter(width = 0.15, size = 3, alpha = 0.8,
              fill = "white", color = "black", shape = 21, stroke = 0.7) +
  scale_fill_manual(values = c("Dark" = "black", "0 mM" = "red", "1 mM" = "red")) +
  labs(
    y = expression(paste(italic(""), " Stomatal Aperture (µm)")),
    x = "",
    title = ""
  ) +
  ylim(0, 9) +
  scale_x_discrete(limits = c("Dark", "0 mM", "1 mM")) +
  facet_grid(
    Species ~ Metabolite,
    labeller = labeller(Species = label_parsed, Metabolite = label_value)
  ) +
  good_theme +
  stat_compare_means(
    method = "anova",
    label.x = 1,
    label.y = 8.7,
    size = 5
  ) +
  theme(legend.position = "none")+
  stat_compare_means(
    comparisons = my_comparisons,
    label = "p.signif",
    method = "t.test",
    ref.group = "Dark",
    size = 5,
    label.y = c(6, 7,8)
  )  +theme(axis.text.x = element_text(face = "italic", angle = 45, hjust = 1))
########################################################################
good_theme<-theme(legend.position =  c(0.8,.9),
                  plot.tag =  element_text(colour='black',size=22,face='bold'),
                  plot.tag.position = c(0.02,.98),
                  axis.text.x = element_text(colour="black",size=16,face="bold"),
                  axis.text.y = element_text(colour="black",size=16,face="bold"),
                  axis.title = element_text(colour="black",size=18,face="bold"),
                  plot.title = element_text(hjust = 0.5,colour="black",size=16,face="plain"),
                  panel.background = element_rect(fill = 'white'),
                  axis.line = element_line(colour = "black", size = 1))

data1E<-read.csv(choose.files(),header = TRUE)

plot_E1A<-ggplot(data1E, aes(fill=Letter, y=Aperture, x=Letter))  +facet_wrap(~Group)+ theme(strip.background = element_rect(colour="black", fill="white",   size=2, linetype="solid"))+theme(strip.text.x = element_text(size = 14))+
  geom_boxplot(width = 0.5, alpha = 0.8, outlier.shape = NA) +
  geom_jitter(width = 0.15, size = 3, alpha = 0.8, fill = "white", color = "black", shape = 21, stroke = 0.7)+
  labs(y=expression(paste(italic('V. faba')," Stomatal  Aperture "( mu*m))), x="", title=expression(paste(italic('V. faba to V. faba '))),tag='a')+
  scale_x_discrete(limit = c("A","B","C"),labels=c("A" = "Mock","B" = "Dark-AF", "C"="Red light-AF "))+
  good_theme+
  scale_fill_manual(breaks=c("A","B","C"),values=c("gray","black","red","black", "black","red"), labels=c("A" = "Mock","B" = "Dark-AF", "C"="Red light-AF "))+
  
  good_theme+theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  theme(legend.text = element_text(colour="black", size=16 ))+
  theme(legend.position = "none")+
  stat_compare_means(method = "anova", label.x=1,label.y = 9,size=5)+      # Add global p-value
  stat_compare_means(method = "t.test", label = "p.format",label.y = 6.4,size=5, comparisons=list(c("A","B")))+
  stat_compare_means(method = "t.test", label = "p.format",label.y = 7.3,size=5, comparisons=list(c("A","C")))+
  stat_compare_means(method = "t.test", label = "p.format",label.y = 8.1,size=5, comparisons=list(c("B","C")))


dataE1B<-read.csv(choose.files(),header = TRUE)


plot_E1B<-ggplot(dataE1B, aes(fill=Group, y=Aperture, x=Group)) + 
  geom_boxplot(width = 0.5, alpha = 0.8, outlier.shape = NA) +
  geom_jitter(width = 0.15, size = 3, alpha = 0.8, fill = "white", color = "black", shape = 21, stroke = 0.7)+
  labs(y=expression(paste(italic('V. faba')," Stomatal  Aperture " ( mu*m))), x="", title='',tag='b')+
  scale_x_discrete(limit = c("A","C","B","D","E","F","G","H","I"),labels=c("A"="Initial-Dark","B" = "Mock Control", "C"="Original","D"="Flowthrough","E"="E-(5% MeOH)","F"="E-25 MeOH","G"="E-50 MeOH","H"="E-75 MeOH","I"="E-100 MeOH"))+
  good_theme+
  scale_fill_manual(breaks=c("A","B","C","D","E","F","G","H","I"),values=c("black","red","Red","red", "red","red","red","red","red","red","red"),labels=c("A"="Initial-Dark","B" = "Mock Control", "C"="Original","D"="Flowthrough","E"="E-(5% MeOH)","F"="E-25 MeOH","G"="E-50 MeOH","H"="E-75 MeOH","I"="E-100 MeOH"))+
  good_theme+theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  theme(legend.text = element_text(colour="black", size=16 ))+
  theme(legend.position = "none")+
  stat_compare_means(method = "anova", label.x=1,label.y = 7,size=5)+      # Add global p-value
  stat_compare_means(label = "p.signif", method = "t.test", ref.group = "C",size=6,label.y = c(3, 3.7,4.9,4.8,5.2,4.5,4.4,4,3.7))+
  
  stat_compare_means(method = "t.test", label = "p.format",label.y = 5.5,size=4, comparisons=list(c("C","B")))+
  stat_compare_means(method = "t.test", label = "p.format",label.y = 6.1,size=4, comparisons=list(c("C","D")))+
  stat_compare_means(method = "t.test", label = "p.format",label.y = 6.7,size=4, comparisons=list(c("C","E")))+
  stat_compare_means(method = "t.test", label = "p.format",label.y = 7.3,size=4, comparisons=list(c("C","F")))+
  stat_compare_means(method = "t.test", label = "p.format",label.y = 7.9,size=4, comparisons=list(c("C","G")))+
  stat_compare_means(method = "t.test", label = "p.format",label.y = 8.6,size=4, comparisons=list(c("C","H")))+
  stat_compare_means(method = "t.test", label = "p.format",label.y = 9.1,size=4, comparisons=list(c("C","I")))+
  ylim(0,10)

good_theme<-theme(legend.position =  c(0.15,.75),
                  plot.tag =  element_text(colour='black',size=22,face='bold'),
                  plot.tag.position = c(0.02,.98),
                  axis.text.x = element_text(colour="black",size=16,face="bold"),
                  axis.text.y = element_text(colour="black",size=16,face="bold"),
                  axis.title = element_text(colour="black",size=18,face="bold"),
                  plot.title = element_text(hjust = 0.5,colour="black",size=22,face="plain"),
                  panel.background = element_rect(fill = 'white'),
                  axis.line = element_line(colour = "black", size = 1))


dataE1C<-read.csv(choose.files(),header = TRUE)



#############################3
library(ggplot2)
library(ggpubr)
library(dplyr)
library(tidyr)

# Filter and prepare data
data_sub <- data %>%
  filter(Treatment %in% c("0 mM", "1 mM")) %>%
  group_by(Species, Metabolite, Treatment) %>%
  mutate(Replicate = row_number()) %>%
  ungroup()

# Pivot wider for paired t-tests
wide_data <- data_sub %>%
  pivot_wider(names_from = Treatment, values_from = Value)

# Filter to valid pairs
wide_data_filtered <- wide_data %>%
  filter(!is.na(`0 mM`) & !is.na(`1 mM`))

# Perform paired t-tests
ttest_df <- wide_data_filtered %>%
  group_by(Species, Metabolite) %>%
  summarise(
    p_value = t.test(`0 mM`, `1 mM`, paired = TRUE)$p.value,
    .groups = "drop"
  ) %>%
  mutate(
    group1 = "0 mM",
    group2 = "1 mM",
    y.position = 8.5,
    p_label = paste0("p = ", signif(p_value, 2))
  )

# Now plot using full data
ggplot(data = data, aes(x = Treatment, y = Value, fill = Treatment)) +
  geom_boxplot(width = 0.5, alpha = 0.8, outlier.shape = NA) +
  geom_jitter(width = 0.15, size = 3, alpha = 0.8,
              fill = "white", color = "black", shape = 21, stroke = 0.7) +
  stat_pvalue_manual(
    data = ttest_df,
    label = "p_label",
    xmin = "group1", xmax = "group2",
    y.position = "y.position",
    tip.length = 0.01,
    size = 5
  ) +
  scale_fill_manual(values = c("Dark" = "black", "0 mM" = "red", "1 mM" = "red")) +
  labs(
    y = expression(paste(italic(""), " Stomatal Aperture (µm)")),
    x = "",
    title = ""
  ) +
  ylim(0, 9) +
  scale_x_discrete(limits = c("Dark", "0 mM", "1 mM")) +
  facet_grid(Species ~ Metabolite) +
  good_theme +
  theme(axis.text.x = element_text(face = "italic", angle = 45, hjust = 1))

#####################################################################################3Assuming 'dataos' is your data frame and has the necessary columns

plot_E1C <- ggplot(dataE1C, aes(y = Osmotic, x = Letter, fill = Letter)) + 
  geom_boxplot(width = 0.5, alpha = 0.8, outlier.shape = NA) +
  geom_jitter(width = 0.15, size = 3, alpha = 0.8, fill = "white", color = "black", shape = 21, stroke = 0.7)+
  labs(y = expression(paste('Osmotic concentration'['']*' (mmol '['']*' '^''*'kg'^'-1'*')')), 
       x = " ", 
       title = expression(paste(italic(''), ' ')), 
       tag = 'c') +
  scale_x_discrete(limits = c("A","B","C","D","E","F","G","H"), 
                   labels = c("A" = "Mock Control", "B" = "Original", "C" = "Flowthrough", 
                              "D" = "E-(5% MeOH)", "E" = "E-25 MeOH", "F" = "E-50 MeOH", 
                              "G" = "E-75 MeOH", "H" = "E-100 MeOH")) +
  scale_fill_manual(values = c("A" = "red", "B" = "red", "C" = "red", "D" = "red", 
                               "E" = "red", "F" = "red", "G" = "red", "H" = "red"),
                    labels = c("A" = "Mock Control", "B" = "Original", "C" = "Flowthrough", 
                               "D" = "E-(5% MeOH)", "E" = "E-25 MeOH", "F" = "E-50 MeOH", 
                               "G" = "E-75 MeOH", "H" = "E-100 MeOH")) +
  good_theme +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), 
        legend.text = element_text(colour = "black", size = 16),
        legend.position = "none") +
  stat_compare_means(label = "p.signif", method = "t.test", ref.group = "B", size = 6, 
                     label.y = c(200, 202.7, 400.9, 40.8, 50.2, 40.5, 40.4, 40, 30.7)) +
  ylim(0,100)



dataE1D<-read.csv(choose.files(),header = TRUE)

plot_E1D<-ggplot(dataE1D, aes(fill=Group, y=Aperture, x=group)) + 
  geom_boxplot(width = 0.5, alpha = 0.8, outlier.shape = NA) +
  geom_jitter(width = 0.15, size = 3, alpha = 0.8, fill = "white", color = "black", shape = 21, stroke = 0.7)+
  labs(y=expression(paste(italic('V. faba')," Stomatal  Aperture " ( mu*m))), x="", title='',tag='d')+
  scale_x_discrete(limit = c("MC","F01","F02","F03","F04","F05","F06","F07","F08"),labels=c("MC"="Mock","F01" = "5-6 min", "F02"="6-6.5 min","F03"="6.5-7.5 min","F04"="7.5-8 min","F05"="8-10 min","F06"="10-14 min","F07"="14-18 min","F08"="18-20 min"))+
  good_theme+
  scale_fill_manual(breaks=c("MC","F01","F02","F03","F04","F05","F06","F07","F08"),values=c("red","red","Red","red", "red","red","red","red","red","red","red"), labels=c("MC"="Mock","F01" = "5-6 min", "F02"="6-6.5 min","F03"="6.5-7.5 min","F04"="7.5-8 min","F05"="8-10 min","F06"="10-14 min","F07"="14-18 min","F08"="18-20 min"))+
  good_theme+theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  theme(legend.text = element_text(colour="black", size=16 ))+
  theme(legend.position = "none")+
  stat_compare_means(method = "anova", label.x=1,label.y = 7,size=5)+      # Add global p-value
  stat_compare_means(label = "p.signif", method = "t.test", ref.group = "MC",size=6,label.y = c(5,5,5,5,5,5,5,5))+
stat_compare_means(method = "t.test", label = "p.format",label.y = 7.5,size=5, comparisons=list(c("MC","F01")))+
  stat_compare_means(method = "t.test", label = "p.format",label.y = 8.0,size=5, comparisons=list(c("MC","F02")))+
  stat_compare_means(method = "t.test", label = "p.format",label.y = 8.5,size=5, comparisons=list(c("MC","F03")))+
  stat_compare_means(method = "t.test", label = "p.format",label.y = 9,size=5, comparisons=list(c("MC","F04")))+
  stat_compare_means(method = "t.test", label = "p.format",label.y = 9.5,size=5, comparisons=list(c("MC","F05")))+
  stat_compare_means(method = "t.test", label = "p.format",label.y = 10,size=5, comparisons=list(c("MC","FO5")))+
  stat_compare_means(method = "t.test", label = "p.format",label.y = 10.5,size=5, comparisons=list(c("MC","F06")))+
  stat_compare_means(method = "t.test", label = "p.format",label.y = 11,size=5, comparisons=list(c("MC","F07")))+
  stat_compare_means(method = "t.test", label = "p.format",label.y = 11.5,size=5, comparisons=list(c("MC","F08")))


dataE1E<-read.csv(choose.files(),header = TRUE)

plot_E1E<-ggplot(dataE1E, aes(fill=Group, y=Aperture, x=Group)) + 
  geom_boxplot(width = 0.5, alpha = 0.8, outlier.shape = NA) +
  geom_jitter(width = 0.15, size = 3, alpha = 0.8, fill = "white", color = "black", shape = 21, stroke = 0.7)+
  labs(y=expression(paste(italic('V. faba')," Stomatal  Aperture " ( mu*m))), x="", title='',tag='e')+
  scale_x_discrete(limit = c("MC","F01","F02","F03","F04","F05","F06","F07","F08"),labels=c("MC"="Mock","F01" = "5-6.5 min", "F02"="6.5-7 min","F03"="7-8 min","F04"="8-9 min","F05"="9-11 min","F06"="11-14 min","F07"="14-17 min","F08"="17-20 min"))+
  good_theme+
  scale_fill_manual(breaks=c("MC","F01","F02","F03","F04","F05","F06","F07","F08"),values=c("red","red","Red","red", "red","red","red","red","red","red","red"),labels=c("MC"="Mock","F01" = "5-6.5 min", "F02"="6.5-7 min","F03"="7-8 min","F04"="8-9 min","F05"="9-11 min","F06"="11-14 min","F07"="14-17 min","F08"="17-20 min"))+
  good_theme+theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  theme(legend.text = element_text(colour="black", size=16 ))+
  theme(legend.position = "none")+
  stat_compare_means(method = "anova", label.x=1,label.y = 7,size=5)+      # Add global p-value
  stat_compare_means(label = "p.signif", method = "t.test", ref.group = "MC",size=6,label.y = c(5,5,5,5,5,5,5,5))+
  stat_compare_means(method = "t.test", label = "p.format",label.y = 5,size=5, comparisons=list(c("MC","F01")))+
  stat_compare_means(method = "t.test", label = "p.format",label.y = 5.5,size=5, comparisons=list(c("MC","F02")))+
  stat_compare_means(method = "t.test", label = "p.format",label.y = 6,size=5, comparisons=list(c("MC","F03")))+
  stat_compare_means(method = "t.test", label = "p.format",label.y = 6.5,size=5, comparisons=list(c("MC","F04")))+
  stat_compare_means(method = "t.test", label = "p.format",label.y = 7,size=5, comparisons=list(c("MC","F05")))+
  stat_compare_means(method = "t.test", label = "p.format",label.y = 7.5,size=5, comparisons=list(c("MC","FO5")))+
  stat_compare_means(method = "t.test", label = "p.format",label.y = 8,size=5, comparisons=list(c("MC","F06")))+
  stat_compare_means(method = "t.test", label = "p.format",label.y = 8.5,size=5, comparisons=list(c("MC","F07")))+
  stat_compare_means(method = "t.test", label = "p.format",label.y = 9,size=5, comparisons=list(c("MC","F08")))




grid.arrange(plot_E1A,plot_E1B,plot_E1C,plot_E1D,plot_E1E,nrow=3)

###############Figure E3



###time course
good_themee3a<-theme(legend.position =  c(0.15,.75),
                  plot.tag =  element_text(colour='black',size=22,face='bold'),
                  plot.tag.position = c(0.02,.98),
                  axis.text.x = element_text(colour="black",size=16,face="bold"),
                  axis.text.y = element_text(colour="black",size=16,face="bold"),
                  axis.title = element_text(colour="black",size=18,face="bold"),
                  plot.title = element_text(hjust = 0.5,colour="black",size=16,face="plain"),
                  panel.background = element_rect(fill = 'white'),
                  axis.line = element_line(colour = "black", size = 1))


dataE3a <- read.csv(file.choose(), header = TRUE)

dataE3a <- read.csv(file.choose(), header = TRUE)



# Plot data with boxplot and jitter
plotE3A <- ggplot(dataE3a, aes(x = factor(Time), y = Avg_Aperture, fill = Sucrose)) +
  geom_boxplot(position = position_dodge(0.75), width = 0.6, alpha = 0.8, outlier.shape = NA) +
  geom_jitter(position = position_jitterdodge(jitter.width = 0.1, dodge.width = 0.75),
              shape = 21, size = 2.5, stroke = 0.4, color = "black", alpha = 0.8) +
  scale_fill_manual(values = c("Control" = "green", "Sucrose" = "blue")) +
  labs(x = "Time (min)", 
       y = expression(paste(italic('V. faba')," Stomatal Aperture (", mu,m,")")), 
       title = "", tag = 'c') +
  ylim(0, 5.9) +
  theme(legend.title=element_blank())+
  theme(legend.text = element_text(colour="black", size=14 ))+  
  good_themee3a+
  annotate("text", x = "0",   y = 3, label = "0.624", size = 4) +
  annotate("segment", x = 0.7, xend = 1.3, y = 2.7, yend = 2.7, color = "black", size = 0.5) +
  
  annotate("text", x = "30",  y = 3.8, label = "0.399", size = 4) +
  annotate("segment", x = 1.7, xend = 2.3, y = 3.5, yend = 3.5, color = "black", size = 0.5) +
  
  annotate("text", x = "60",  y = 3.8, label = "0.014", size = 4) +
  annotate("segment", x = 2.7, xend = 3.3, y = 3.5, yend = 3.5, color = "black", size = 0.5) +
  
  annotate("text", x = "90",  y = 5.3, label = "0.003", size = 4) +
  annotate("segment", x = 3.7, xend = 4.3, y = 5, yend = 5, color = "black", size = 0.5) +
  
  annotate("text", x = "120", y = 5.3, label = "0.001", size = 4) +
  annotate("segment", x = 4.7, xend = 5.3, y = 5, yend = 5, color = "black", size = 0.5) +
  
  annotate("text", x = "150", y = 5.3, label = "0.042", size = 4) +
  annotate("segment", x = 5.7, xend = 6.3, y = 5, yend = 5, color = "black", size = 0.5) +
  
  annotate("text", x = "180", y = 5.3, label = "0.009", size = 4) +
  annotate("segment", x = 6.7, xend = 7.3, y = 5, yend = 5, color = "black", size = 0.5) +
  
  annotate("text", x = "210", y = 3.7, label = "0.052", size = 4) +
  annotate("segment", x = 7.7, xend = 8.3, y = 3.4, yend = 3.4, color = "black", size = 0.5)+
annotate("text", x = 1.7, y = 5.4, label = "Anova, p = 1.3e-2", size = 5)+
  
  geom_segment(aes(x = 1.3, y = 5.6, xend = 7.5, yend = 5.6),size=2,color='red')+
  
  geom_text(x=4, y=5.85, label="Red light",col="red", size=6, fontface="bold")+
  
  geom_segment(aes(x = 0,  y = 5.6, xend = 1.3,  yend = 5.6),size=2,color='black')+
  
  geom_text(x=0.6, y=5.85, label="Dark",col="Black", size=6, fontface="bold")+
  
geom_segment(aes(x = 7.5,  y = 5.6, xend = 8.5,  yend = 5.6),size=2,color='black')+
  
  geom_text(x=8, y=5.85, label="Dark",col="Black", size=6, fontface="bold")

  


dataE3b <- read.csv(file.choose(), header = TRUE)


good_themeE3b<-theme(legend.position =  c(0.25,.75),
                   plot.tag =  element_text(colour='black',size=22,face='bold'),
                   plot.tag.position = c(0.02,.98),
                   axis.text.x = element_text(colour="black",size=16,face="bold"),
                   axis.text.y = element_text(colour="black",size=16,face="bold"),
                   axis.title = element_text(colour="black",size=18,face="bold"),
                   plot.title = element_text(hjust = 0.5,colour="black",size=22,face="plain"),
                   panel.background = element_rect(fill = 'white'),
                   axis.line = element_line(colour = "black", size = 1))



# Plot
plot_E3B<-ggplot(dataE3b, aes(shape=Group, y=gsw, x=Time,color=Group))+
  stat_summary(geom = "point", fun = mean, position = "dodge",size=5) +
  stat_summary(geom = "errorbar", fun.data = mean_se)+
  labs(y=expression(paste( italic('V. faba')  ,   '  g'['sw']*' (mol H'['2']*'O m'^'2'*'s'^'-1'*')')), x='Time (min)',tag='d')+
  good_themeE3b+
  scale_shape_manual(breaks=c('a','b','c'),values=c(17,16,15),labels=c("a"='1 mM Dark',"b" = "0 mM Red light","c" = "1 mM Red light"))+
  scale_color_manual(breaks=c('a','b','c'),values=c("black","red","pink"),labels=c("a"='1 mM Dark',"b" = "0 mM Red light","c" = "1 mM Red light"))+
  theme(legend.title=element_blank())+
  theme(legend.text = element_text(colour="black", size=14 ))+                  
  ylim(0,0.23)+
  xlim(1,80)+
  geom_segment(aes(x = 10, y = 0.22, xend = 80, yend = 0.22),size=2,color='red')+
  
  geom_text(x=45, y=0.23, label="Red light",col="red", size=6, fontface="bold")+
  
  geom_segment(aes(x = 2,  y = 0.22, xend = 10,  yend = 0.22),size=2,color='black')+
  
  geom_text(x=2.1, y=0.23, label="Dark",col="Black", size=6, fontface="bold")+
  
  geom_text(x=52, y=0.125, label="*",col="Black", size=5, fontface="bold")+
  geom_text(x=54, y=0.13, label="*",col="Black", size=5, fontface="bold")+
  geom_text(x=56, y=0.135, label="*",col="Black", size=5, fontface="bold")+
  geom_text(x=54, y=0.13, label="*",col="Black", size=5, fontface="bold")+
  geom_text(x=56, y=0.135, label="*",col="Black", size=5, fontface="bold")+
  geom_text(x=58, y=0.14, label="*",col="Black", size=5, fontface="bold")+
  geom_text(x=60, y=0.145, label="*",col="Black", size=5, fontface="bold")+
  geom_text(x=62, y=0.15, label="*",col="Black", size=5, fontface="bold")+
  geom_text(x=64, y=0.154, label="*",col="Black", size=5, fontface="bold")+
  geom_text(x=66, y=0.16, label="*",col="Black", size=5, fontface="bold")+
geom_text(x=68, y=0.164, label="*",col="Black", size=5, fontface="bold")+
  geom_text(x=70, y=0.168, label="*",col="Black", size=5, fontface="bold")+
  geom_text(x=72, y=0.172, label="*",col="Black", size=5, fontface="bold")+
  geom_text(x=74, y=0.176, label="*",col="Black", size=5, fontface="bold")+
  geom_text(x=76, y=0.18, label="*",col="Black", size=5, fontface="bold")+
  geom_text(x=78, y=0.184, label="*",col="Black", size=5, fontface="bold")+
  geom_text(x=80, y=0.188, label="*",col="Black", size=5, fontface="bold")





##################Figure E3c

good_theme3e <- theme(legend.position = "non",
                    plot.tag = element_text(colour = 'black', size = 22, face = 'bold'),
                    plot.tag.position = c(0.02, .98),
                    axis.text.x = element_text(colour = "black", size = 16, face = "bold", angle = 45, hjust = 1),
                    axis.text.y = element_text(colour = "black", size = 16, face = "bold"),
                    axis.title = element_text(colour = "black", size = 18, face = "bold"),
                    plot.title = element_text(hjust = 0.5, colour = "black", size = 22, face = "plain"),
                    panel.background = element_rect(fill = 'white'),
                    axis.line = element_line(colour = "black", size = 1),
                    legend.title = element_blank(),
                    legend.text = element_text(colour = "black", size = 16),
                    text = element_text(family = ""))
dataE3C<-read.csv(choose.files(),header = TRUE)

plot_E3C<-ggplot(dataE3C, aes(fill=Letter, y=Aperture, x=Letter))+ 
  geom_boxplot(width = 0.5, alpha = 0.8, outlier.shape = NA) +
  geom_jitter(width = 0.15, size = 3, alpha = 0.8, fill = "white", color = "black", shape = 21, stroke = 0.7)+
  labs(y=expression(paste(italic('V. faba')," Stomatal  Aperture  "  ( mu*m))), x="",title='Glucose',tag='a')+
  scale_x_discrete(limit = c("A","B","C","D"),labels=c("A" = "Dark", "B"="0 mM","C"="1 mM","D"="100 mM"))+
  scale_fill_manual(breaks=c("A","B","C","D"),values=c("black","red","red", "red"), labels=c("A" = "Dark", "B"="0 mM","C"="1 mM","D"="100 mM"))+
  scale_color_manual(breaks=c("A","B","C","D"),values=c("black","black","black","black","black","black","black","black"), labels=c("A" = "Dark", "B"="0 mM","C"="1 mM","D"="100 mM"))+
 theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  theme(legend.text = element_text(colour="black", size=16 ))+
  theme(legend.position = "none")+
  geom_segment(aes(x = 1.5,y = 4.4, xend = 4.5, yend = 4.4),size=1,col='red')+
  ylim(0,5)+good_theme3e+
  geom_text(x=2.6, y=4.7, label="Red light",col="red", size=7, fontface="bold")+
  stat_compare_means(method = "anova", label.x=0.9,label.y = 4.9,size=5)+      # Add global p-value
  stat_compare_means(label = "p.signif", method = "t.test", ref.group = "A",size=6,label.y = c(2, 1.9,3.6,2.55,2.1,0.9))+
  stat_compare_means(method = "t.test", label = "p.format",label.y = 3.5,size=5, comparisons=list(c("B","C")))+
  stat_compare_means(method = "t.test", label = "p.format",label.y = 3.8,size=5, comparisons=list(c("B","D")))


dataE3d<-read.csv(choose.files(),header = TRUE)

plot_E3d<-ggplot(dataE3d, aes(fill=Letter, y=aperture, x=Letter)) + 
  geom_boxplot(width = 0.5, alpha = 0.8, outlier.shape = NA) +
  geom_jitter(width = 0.15, size = 3, alpha = 0.8, fill = "white", color = "black", shape = 21, stroke = 0.7)+
  labs(y=expression(paste(italic('A.thaliana')," Stomatal  Aperture "( mu*m))), x="",title='Glucose',tag='b')+
  scale_x_discrete(limit = c("A","B","C","D"),labels=c("A" = "Dark", "B"="0 mM","C"="1 mM","D"="100 mM"))+
  scale_fill_manual(breaks=c("A","B","C","D"),values=c("black","red","red", "red"), labels=c("A" = "Dark", "B"="0 mM","C"="1 mM","D"="100 mM"))+
  scale_color_manual(breaks=c("A","B","C","D"),values=c("black","black","black","black","black","black","black","black"), labels=c("A" = "Dark", "B"="0 mM","C"="1 mM","D"="100 mM"))+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  theme(legend.text = element_text(colour="black", size=16 ))+
  theme(legend.position = "none")+
  geom_segment(aes(x = 1.5,y = 4.4, xend = 4.5, yend = 4.4),size=1,col='red')+
  ylim(0,5)+good_theme3e+
  geom_text(x=2.6, y=4.7, label="Red light",col="red", size=7, fontface="bold")+
  stat_compare_means(method = "anova", label.x=0.9,label.y = 4.9,size=5)+      # Add global p-value
  stat_compare_means(label = "p.signif", method = "t.test", ref.group = "A",size=6,label.y = c(2.2, 1.6,3.1,2 ))+
  stat_compare_means(method = "t.test", label = "p.format",label.y = 3.5,size=5, comparisons=list(c("B","C")))+
  stat_compare_means(method = "t.test", label = "p.format",label.y = 3.8,size=5, comparisons=list(c("B","D")))















################phosphorilation 30 mM 

library(ggplot2)

library(ggplot2)

# Load data
dataE3e<-read.csv(choose.files(),header = TRUE)
# Clean Treatment names
dataE3e$Treatment <- trimws(dataE3e$Treatment)

# Explicitly define Treatment as factor
dataE3e$Treatment <- factor(dataE3e$Treatment, levels = c("Mannitol", "Sucrose"))

# Bracket data with explicit p-values
bracket_dataE <- data.frame(
  time_x = c(1, 2, 3, 4, 5),
  y = c(1.15, 1.91, 2.39, 3.40, 3.54),
  label = c("0.12", " 0.04", "0.009", "  0.001", "0.017")
)

# Define theme
good_themeE6 <- theme(
  legend.position = c(0.15, .75),
  plot.tag = element_text(colour='black', size=22, face='bold'),
  plot.tag.position = c(0.02, .98),
  axis.text.x = element_text(colour="black", size=16, face="bold"),
  axis.text.y = element_text(colour="black", size=16, face="bold"),
  axis.title = element_text(colour="black", size=18, face="bold"),
  plot.title = element_text(hjust = 0.5, colour="black", size=22, face="plain"),
  panel.background = element_rect(fill = 'white'),
  axis.line = element_line(colour = "black", size = 1)
)

# Plot
plot_E3e <- ggplot(dataE3e, aes(x = factor(Time), y = Value, fill = Treatment)) +
  geom_boxplot(position = position_dodge(0.75), width = 0.6, alpha = 0.8, outlier.shape = NA) +
  geom_jitter(position = position_jitterdodge(jitter.width = 0.1, dodge.width = 0.75),
              shape = 21, size = 2.5, stroke = 0.4, color = "black", alpha = 0.8) +
  scale_fill_manual(values = c("Mannitol" = "green", "Sucrose" = "blue")) +
  labs(title = "", x = "Time (min)", y = "Phosphorylation level", tag = 'e') +
  geom_segment(data = bracket_dataE,
               aes(x = time_x - 0.2, xend = time_x + 0.2, y = y - 0.1, yend = y - 0.1),
               inherit.aes = FALSE, color = "black", size = 0.6) +
  geom_text(data = bracket_dataE,
            aes(x = time_x, y = y, label = label),
            inherit.aes = FALSE, size = 4.5, fontface = "bold") +
  good_themeE6 +
  theme(legend.title = element_blank(),
        legend.text = element_text(colour = "black", size = 14))


grid.arrange(plot_E3C,plot_E3d,plotE3A,plot_E3B,plot_E3e,nrow=3)

##############Fig E4 b
library(dplyr)

data4EB1<-read.csv(choose.files(),header = TRUE)
# Summarize data by group
summary_data <- data4EB1 %>%
  group_by(Letter) %>%
  summarise(
    mean_current = mean(Current, na.rm = TRUE),
    se = sd(Current, na.rm = TRUE) / sqrt(n())
  )

# Bar plot with jittered raw points
plot_E4b30 <- ggplot() +
  # Bars for means
  geom_bar(data = summary_data, aes(x = Letter, y = mean_current, fill = Letter),
           stat = "identity", width = 0.5, alpha = 0.8) +
  # Error bars
  geom_errorbar(data = summary_data, aes(x = Letter, ymin = mean_current - se, ymax = mean_current + se),
                width = 0.2) +
  # Jittered individual points
  geom_jitter(data = data4EB1, aes(x = Letter, y = Current),
              width = 0.15, size = 3, alpha = 0.8, fill = "white", color = "black", shape = 21, stroke = 0.7) +
  labs(
    y = expression("Current (" * mu * "A)"),
    x = "",
    title = expression("30 mM NaNO"[3]),
    tag = 'b'
  ) +
  scale_x_discrete(
    limits = c("A", "B", "C"),
    labels = c("A" = "Control", "B" = "1 mM", "C" = "3 mM"),
    position = "top"
  ) +
  scale_fill_manual(
    breaks = c("A", "B", "C"),
    values = c("black", "#fa6b5c", "blue"),
    labels = c("Control", "1 mM Sucrose", "3 mM Sucrose")
  ) +
  ylim(-22, 0) +
  good_theme +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 0),
    legend.text = element_text(colour = "black", size = 16),
    legend.position = "none"
  )


data4EB2<-read.csv(choose.files(),header = TRUE)
# Summarize data by group
summary_data2 <- data4EB2 %>%
  group_by(Letter) %>%
  summarise(
    mean_current = mean(Current, na.rm = TRUE),
    se = sd(Current, na.rm = TRUE) / sqrt(n())
  )

# Bar plot with jittered raw points
plot_E4b100 <- ggplot() +
  # Bars for means
  geom_bar(data = summary_data2, aes(x = Letter, y = mean_current, fill = Letter),
           stat = "identity", width = 0.5, alpha = 0.8) +
  # Error bars
  geom_errorbar(data = summary_data2, aes(x = Letter, ymin = mean_current - se, ymax = mean_current + se),
                width = 0.2) +
  # Jittered individual points
  geom_jitter(data = data4EB2, aes(x = Letter, y = Current),
              width = 0.15, size = 3, alpha = 0.8, fill = "white", color = "black", shape = 21, stroke = 0.7) +
  labs(
    y = expression("Current (" * mu * "A)"),
    x = "",
    title = expression("100 mM NaNO"[3]),
    tag = ''
  ) +
  scale_x_discrete(
    limits = c("A", "B", "C"),
    labels = c("A" = "Control", "B" = "1 mM", "C" = "3 mM"),
    position = "top"
  ) +
  scale_fill_manual(
    breaks = c("A", "B", "C"),
    values = c("#fa6b5c", "#fa6b5c", "#fa6b5c"),
    labels = c("Control", "1 mM Sucrose", "3 mM Sucrose")
  ) +
  ylim(-2, 0) +
  good_theme +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 0),
    legend.text = element_text(colour = "black", size = 16),
    legend.position = "none"
  )



grid.arrange(plot_E4b30,plot_E4b100,nrow=1)
library(ggplot2)
library(dplyr)

# Load data
dataE4b100 <- read.csv(choose.files(), header = TRUE)

# Summarize data by group
summary_data3 <- dataE4b100 %>%
  group_by(Letter) %>%
  summarise(
    mean_current = mean(Current, na.rm = TRUE),
    se = sd(Current, na.rm = TRUE) / sqrt(n())
  )

# Plot
plot_E4b100 <- ggplot() +
  # Bars for means
  geom_bar(data = summary_data3, aes(x = Letter, y = mean_current, fill = Letter),
           stat = "identity", width = 0.5, alpha = 0.8) +
  # Error bars
  geom_errorbar(data = summary_data3, aes(x = Letter, ymin = mean_current - se, ymax = mean_current + se),
                width = 0.2) +
  # Jittered individual points
  geom_jitter(data = dataE4b100, aes(x = Letter, y = Current),
              width = 0.15, size = 3, alpha = 0.8,
              fill = "white", color = "black", shape = 21, stroke = 0.7) +
  labs(
    y = expression("Current (" * mu * "A)"),
    x = "",
    tag = ''
  ) +
  scale_x_discrete(
    limits = c("A", "B"),
    labels = c("A" = expression(H[2]*O), "B" = "1 mM"),
    position = "top"
  ) +
  scale_fill_manual(
    breaks = c("A", "B"),
    values = c("black", "#fa6b5c"),
    labels = c("H2O", "1 mM Sucrose")
  ) +
  ylim(-1.8, 0) +
  good_theme +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 0),
    legend.text = element_text(colour = "black", size = 16),
    legend.position = "none"
  )
# Print the plot
print(plot_E4b100)



####Fig E6


dataE6 <- read.csv(file.choose(), header = TRUE)

good_themeE6<-theme(legend.position =  c(0.15,.75),
                     plot.tag =  element_text(colour='black',size=22,face='bold'),
                     plot.tag.position = c(0.02,.98),
                     axis.text.x = element_text(colour="black",size=16,face="bold"),
                     axis.text.y = element_text(colour="black",size=16,face="bold"),
                     axis.title = element_text(colour="black",size=18,face="bold"),
                     plot.title = element_text(hjust = 0.5,colour="black",size=22,face="plain"),
                     panel.background = element_rect(fill = 'white'),
                     axis.line = element_line(colour = "black", size = 1))

# Plot using ggplot2
ggplot(dataE6, aes(x = `Wavelength.nm.`, y = normalized_PFD)) +
  geom_line(color = "red", linewidth = 2.4) +
  labs(x = "Wavelength (nm)", y = "Normalized intensity (a.u.)",
       title = "",
       subtitle = "") +good_themeE6+ylim(0.0002,1)




