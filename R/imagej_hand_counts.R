###### ImageJ hand quantification & traditional computer vision approach ######
# Here we compare hand counts of scanned ears in ImageJ to manually counting 
# kernels on ears without scanning (Figure 3).

library(tidyverse)
library(xml2)

###### Generating xml example plot (Figure 3B) ######
# A function to get useful coordinates out of ImageJ Cell Counter xml output
xml_to_coordinates <- function(input_data_path) {
  input_xml_data <- read_xml(input_data_path)
  input_xml_as_list <- as_list(input_xml_data) # Not very elegant, but should work
  
  input_data_type_1 <- input_xml_as_list[[1]][[2]][[2]] # These are GFP kernels as long as the person annotating using type "1" for GFP
  input_data_type_1 <- input_data_type_1[-1] # Removing the first sub-list which is just a number string of the data type
  input_data_type_2 <- input_xml_as_list[[1]][[2]][[3]] # These are non-GFP kernels
  input_data_type_2 <- input_data_type_2[-1]
  
  # Making the dataframes with columns for type, x coordinate, and y coordinate
  type_1_df <- data.frame("type" = rep("GFP", length(input_data_type_1)),
                          "x" = as.numeric(unlist(sapply(input_data_type_1, "[[", 1))), 
                          "y" = as.numeric(unlist(sapply(input_data_type_1, "[[", 2))))
  
  type_2_df <- data.frame("type" = rep("WT", length(input_data_type_2)),
                          "x" = as.numeric(unlist(sapply(input_data_type_2, "[[", 1))), 
                          "y" = as.numeric(unlist(sapply(input_data_type_2, "[[", 2))))
  final_df <- rbind(type_1_df, type_2_df)
  
  return(final_df)
  
}

# Getting the example xml file as a dataframe
example_xml <- xml_to_coordinates("./data/X400x533A-8m3.xml")

# Cropping the dataframe to the figure ear image detail size
example_xml <- example_xml[example_xml$x > 280 & 
                           example_xml$x < 1210 &
                           example_xml$y > 130 &
                           example_xml$y < 655, ]

# Plotting
ggplot(example_xml, aes(x = x, y = y, fill = type)) +
  geom_point(alpha = 1, size = 4, shape = 21, stroke = 1, color = 'black') +
  scale_fill_manual(values = c("green", "#614973")) +
  coord_fixed(ratio = 1) +
  scale_x_continuous(name = seq(400, 1200, 200), breaks = seq(400, 1200, 200)) +
  scale_y_reverse() +
  theme_bw() +
  theme(axis.title = element_blank(),
        axis.text = element_text(size = 18, face = 'bold'),
        plot.title = element_text(size = 24, face = 'bold', margin = margin(0, 0, 10, 0)),
        axis.line = element_line(size = 0, color = '#4D4D4D'),
        axis.ticks = element_line(size = 0.75, color = '#4D4D4D'),
        axis.ticks.length = unit(4, 'pt'),
        plot.margin = margin(0.5, 0.5, 0.5, 0.5, 'cm'),
        panel.border = element_rect(color = '#4D4D4D', size = 2, fill = NA),
        panel.grid.major.y = element_line(size = 0.75),
        #panel.grid.minor.y = element_line(size = 0.5),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.x = element_line(size = 0.75),
        #panel.grid.minor.x = element_line(size = 0.5),
        panel.grid.minor.x = element_blank(),
        legend.position = 'none')

ggsave(filename = './plots/xml_file_plot_example.png', 
       device = 'png', 
       width = 7,
       height = 4, 
       dpi = 400,
       units = 'in')

###### Comparing hand-counts to ImageJ scanned image hand-counts (Figure 3C) ######
# Importing the data
imagej_validation <- read.table(file="./data/imagej_hand_counts.tsv",
                                sep = '\t',
                                header = TRUE)

# Plotting
ggplot(imagej_validation, aes(x = percent_GFP_imagej, y = percent_GFP_hand)) +
  geom_abline(slope = 1, intercept = 0, linetype = 'dashed', size = 1, color = '#4D4D4D') +
  geom_point(alpha = 1, size = 3, shape = 21, stroke = 1, color = 'black', fill = 'orange') +
  coord_fixed(xlim = c(20, 60), ylim = c(20, 60)) +
  labs(title = 'Percent GFP', x = "ImageJ manual", y = "Hand-counted") +
  theme_bw() +
  theme(axis.title = element_text(size = 20, face = 'bold'),
        axis.text = element_text(size = 18, face = 'bold'),
        plot.title = element_text(size = 24, face = 'bold', margin = margin(0, 0, 10, 0)),
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
