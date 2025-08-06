

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
