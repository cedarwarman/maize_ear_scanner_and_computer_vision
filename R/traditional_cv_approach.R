###### Traditional computer vision approach ######
# Here we quantify a traditional computer vision approach, implemented in 
# scikit-image, used to count kernels in scanned ear images (Supplemental 
# Figure 1). 

library(tidyverse)
library(xml2)

###### Cropping validation set to match scikit-image predictions ######
# This script will "crop" the validation xml files so they're directly 
# comparable to scikit-image output. scikit-image is currently set to crop 
# 15% off each side of the image before quantificaion. I'll use that 
# percentage to crop the validation xml files.

### Kernel cropping function
# Function that takes in xml data (as read in by xml2) and outputs a cropped 
# total count of GFP and WT kernels, plus the image name.
kernel_cropper <- function(input_xml_data) {
  input_xml_as_list <- as_list(input_xml_data) # Not very elegant, but should work
  
  # Image name
  input_image_name <- sub('\\..{3}$', '', as.character(input_xml_as_list[[1]][[1]][[1]]))
  
  # Kernel data
  input_data_type_1 <- input_xml_as_list[[1]][[2]][[2]] # These are the fluorescent kernels
  input_data_type_1 <- input_data_type_1[-1] # Removing the first sub-list, not needed here
  input_data_type_2 <- input_xml_as_list[[1]][[2]][[3]] # These are non-fluorescent kernels
  input_data_type_2 <- input_data_type_2[-1]
  
  # Making the dataframes with columns for type, x coordinate, and y coordinate
  type_1_df <- data.frame("type" = rep("GFP", length(input_data_type_1)),
                          "x" = as.numeric(unlist(sapply(input_data_type_1, "[[", 1))), 
                          "y" = as.numeric(unlist(sapply(input_data_type_1, "[[", 2))))
  
  type_2_df <- data.frame("type" = rep("WT", length(input_data_type_2)),
                          "x" = as.numeric(unlist(sapply(input_data_type_2, "[[", 1))), 
                          "y" = as.numeric(unlist(sapply(input_data_type_2, "[[", 2))))
  coord_df <- rbind(type_1_df, type_2_df)
  
  # Here I'll crop the kernels. Images dimensions are 1920x746
  percent_crop = 0.15
  left_crop <- 1920 * percent_crop
  right_crop <- 1920 - (1920 * percent_crop)
  cropped_df <- coord_df[coord_df$x >= left_crop & 
                           coord_df$x <= right_crop, ]
  
  # Counting the total GFP and WT kernels in the cropped dataframe.
  GFP_kernel_count <- nrow(cropped_df[cropped_df$type == "GFP", ])
  WT_kernel_count <- nrow(cropped_df[cropped_df$type == "WT", ])
  output <- list(input_image_name, GFP_kernel_count, WT_kernel_count)
  return(output)
}


# ###### Running the cropping on the test data ######
# ### UPDATE THIS WITH NEW PATH AND NEW XMLS
# setwd("/Users/CiderBones/Desktop/Laboratory/computer_vision/sk_kernel_counter_batch_data/fluorescent_no_color_images_and_xmls")
# filenames <- list.files(pattern="*.xml", full.names=TRUE)
# 
# # Generates a list of the "raw" imported xml files
# input_list <- lapply(filenames, read_xml)
# 
# # Applying the function to the input data and organizing it into a dataframe
# kernel_count_output <- lapply(input_list, kernel_cropper)
# kernel_count_output <- lapply(kernel_count_output, unlist)
# cropped_df <- as.data.frame(do.call(rbind, kernel_count_output))
# 
# ### UPDATE THIS WITH NEW DATA AND FILE STRUCTURE
# # Writes the df to a file, because doing the counting is pretty slow
# write.table(cropped_df, file = "/Users/CiderBones/Desktop/Laboratory/r_projects/computer_vision/data/cropped_xml_df_2.txt", sep = "\t", row.names = FALSE, quote = FALSE)
# 

###### Importing and processing the data (start here) ######
hand_counts_cropped <- read.table(file = "/Users/CiderBones/Desktop/Laboratory/r_projects/computer_vision/data/cropped_xml_df_2.txt", 
                                  sep = "\t",
                                  header = TRUE)
colnames(hand_counts_cropped) <- c("image_name", "GFP_hand", "wt_hand")

sk_image_counts <- read.table(file = "/Users/CiderBones/Desktop/Laboratory/r_projects/computer_vision/data/sk_kernel_counter_batch_output.tsv",
                              sep = "\t",
                              header = FALSE)
colnames(sk_image_counts) <- c("image_name", "GFP_skimage", "wt_skimage")

### Generating GFP percentages
plot_df <- full_join(hand_counts_cropped, sk_image_counts)
plot_df <- plot_df[complete.cases(plot_df), ] ### MIGHT BE ABLE TO TAKE THIS OUT WITH NEW DATA

plot_df <- plot_df %>% mutate(percent_GFP_hand = 100 * (GFP_hand / (GFP_hand + wt_hand)),
                       percent_GFP_skimage = 100 * (GFP_skimage / (GFP_skimage + wt_skimage)))


###### Plotting ######
### DELETE THIS
setwd("/Users/CiderBones/Desktop/Laboratory/r_projects/maize_scanner_cv_paper")

### Fluorescent
ggplot(plot_df, aes(x = GFP_hand, y = GFP_skimage)) +
  geom_abline(slope = 1, intercept = 0, linetype = 'dashed', size = 1, color = '#4D4D4D') +
  geom_point(alpha = 1, size = 2, shape = 21, stroke = 1, color = 'black', fill = '#98f542') +
  coord_fixed(xlim = c(0, 350), ylim = c(0, 350)) +
  labs(title = 'Fluorescent kernels', x = 'Manual count', y = 'scikit-image') +
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

ggsave(filename = './plots/scikit-image_GFP.png', 
       device = 'png', 
       width = 5,
       height = 5, 
       dpi = 400,
       units = 'in')


### Nonfluorescent
ggplot(plot_df, aes(x = wt_hand, y = wt_skimage)) +
  geom_abline(slope = 1, intercept = 0, linetype = 'dashed', size = 1, color = '#4D4D4D') +
  geom_point(alpha = 1, size = 2, shape = 21, stroke = 1, color = 'black', fill = '#614973') +
  coord_fixed(xlim = c(0, 350), ylim = c(0, 350)) +
  labs(title = 'Nonfluorescent kernels', x = 'Manual count', y = 'scikit-image') +
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

ggsave(filename = './plots/scikit-image_wt.png', 
       device = 'png', 
       width = 5,
       height = 5, 
       dpi = 400,
       units = 'in')


### Percent GFP
ggplot(plot_df, aes(x = percent_GFP_hand, y = percent_GFP_skimage)) +
  geom_abline(slope = 1, intercept = 0, linetype = 'dashed', size = 1, color = '#4D4D4D') +
  geom_point(alpha = 1, size = 2, shape = 21, stroke = 1, color = 'black', fill = 'orange') +
  coord_fixed(xlim = c(0, 100), ylim = c(0, 100)) +
  labs(title = 'Percent fluorescent', x = 'Manual count', y = 'scikit-image') +
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

ggsave(filename = './plots/scikit-image_percent.png', 
       device = 'png', 
       width = 5,
       height = 5, 
       dpi = 400,
       units = 'in')


###### Stats ######
### Function to get r-squared values
do_stats <- function(name, input_df) {
  fluorescent_lm <- lm(GFP_hand ~ GFP_skimage, data = input_df)
  fluorescent_r_squared <- summary(fluorescent_lm)$adj.r.squared
  
  nonfluorescent_lm <- lm(wt_hand ~ wt_skimage, data = input_df)
  nonfluorescent_r_squared <- summary(nonfluorescent_lm)$adj.r.squared
  
  percent_fluorescent_lm <- lm(percent_GFP_hand ~ percent_GFP_skimage, data = input_df)
  percent_fluorescent_r_squared <- summary(percent_fluorescent_lm)$adj.r.squared
  
  output_df <- data.frame(name = rep(name, 3),
                          r_squared_category = c("fluorescent", "nonfluorescent", "percent_fluorescent"),
                          r_squared = c(fluorescent_r_squared, nonfluorescent_r_squared, percent_fluorescent_r_squared))
  
  return(output_df)
}

# Running the regressions
stats_results <- do_stats("scikit-image", plot_df)
