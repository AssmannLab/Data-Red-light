
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


