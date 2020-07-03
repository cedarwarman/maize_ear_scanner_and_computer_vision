###### ImageJ hand quantification & traditional computer vision approach ######
# Here we compare hand counts of scanned ears in ImageJ to manually counting 
# kernels on ears without scanning (Figure 3).

library(tidyverse)


###### Comparing hand-counts to ImageJ scanned image hand-counts (Figure 3C) ######
# Importing the data
imagej_validation <- read.table(file="./data/imagej_hand_counts.tsv",
                                sep = '\t',
                                header = TRUE)

# Plotting
ggplot(imagej_validation, aes(x = percent_GFP_hand, y = percent_GFP_imagej)) +
  geom_abline(slope = 1, intercept = 0, linetype = 'dashed', size = 1, color = '#4D4D4D') +
  geom_point(alpha = 1, size = 3, shape = 21, stroke = 1, color = 'black', fill = 'orange') +
  coord_fixed(xlim = c(20, 60), ylim = c(20, 60)) +
  labs(title = 'Percent fluorescent kernels', x = "Manual counts on ear", y = "Manual annotations ImageJ") +
  theme_bw() +
  theme(axis.title = element_text(size = 18, face = 'bold'),
        axis.text = element_text(size = 18, face = 'bold'),
        plot.title = element_text(size = 23, face = 'bold', hjust = 0.5, margin = margin(0, 0, 10, 0)),
        axis.title.x = element_text(margin = margin(10, 0, 0, 0)),
        axis.title.y = element_text(margin = margin(0, 10, 0, 0)),
        axis.line = element_line(size = 0, color = '#4D4D4D'),
        axis.ticks = element_line(size = 0.75, color = '#4D4D4D'),
        axis.ticks.length = unit(4, 'pt'),
        plot.margin = margin(0.5, 0.5, 0.5, 0.5, 'cm'),
        panel.border = element_rect(color = '#4D4D4D', size = 2, fill = NA),
        panel.grid.major.y = element_line(size = 0.75),
        panel.grid.minor.y = element_line(size = 0.5),
        panel.grid.major.x = element_line(size = 0.75),
        panel.grid.minor.x = element_line(size = 0.5),
        legend.position = 'none')

ggsave(filename = './plots/imagej_hand_counts.png', 
       device = 'png', 
       width = 5,
       height = 5, 
       dpi = 400,
       units = 'in')

