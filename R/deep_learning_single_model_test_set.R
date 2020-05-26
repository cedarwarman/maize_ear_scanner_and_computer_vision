###### Deep learning model test set ######
# Here we test the effectiveness of our deep learning model on 2018 and 2019 
# field season test sets, based on a single model trained on images from both
# years. For validation, we used hand-counted total kernel numbers.

library(tidyverse)


###### Importing the data ######

# Hand-counted validation data
hand_counted <- read.table(file="./data/test_set_hand_counts.tsv",
                           sep = '\t',
                           header = TRUE)
hand_counted$image_name <- as.character(hand_counted$image_name)

# Function to import and tidy the data
import_tensorflow_summary <- function(import_path, hand_counted_df) {
  library(tidyverse)
  tensorflow_output <- read.table(file=import_path,
                                  sep = '\t',
                                  header = FALSE)
  colnames(tensorflow_output) <- c("image_name", "GFP_tf", "wt_tf")
  tensorflow_output$image_name <- as.character(tensorflow_output$image_name)
  
  df <- full_join(hand_counted_df, tensorflow_output)
  df <- df[complete.cases(df), ]
  
  df <- df %>% mutate(percent_GFP_hand = GFP_hand / (GFP_hand + wt_hand),
                      percent_GFP_tf = GFP_tf / (GFP_tf + wt_tf))
  
  return(df)
}

results <- import_tensorflow_summary("./data/test_set_single_model_predictions_2018_2019_summary.tsv", hand_counted)


###### Plotting the results (Supplemental Figure 2) ######
### Fluorescent
ggplot(results, aes(x = GFP_hand, y = GFP_tf)) +
  geom_abline(slope = 1, intercept = 0, linetype = 'dashed', size = 1, color = '#4D4D4D') +
  geom_point(alpha = 1, size = 2, shape = 21, stroke = 1, color = 'black', fill = '#98f542') +
  coord_fixed(xlim = c(0, 350), ylim = c(0, 350)) +
  labs(title = 'Fluorescent kernels', x = 'Manual count', y = 'Tensorflow') +
  theme_bw() +
  theme(axis.title = element_text(size = 20, face = 'bold'),
        axis.text = element_text(size = 18, face = 'bold'),
        plot.title = element_text(size = 22, face = 'bold', hjust = 0.5, margin = margin(0, 0, 10, 0)),
        axis.title.x = element_text(margin = margin(10, 0, 0, 0)),
        axis.title.y = element_text(margin = margin(0, 10, 0, 0)),
        axis.line = element_line(size = 0, color = '#4D4D4D'),
        axis.ticks = element_line(size = 0.75, color = '#4D4D4D'),
        axis.ticks.length = unit(4, 'pt'),
        plot.margin = margin(0.5, 0.8, 0.5, 0.3, 'cm'),
        panel.border = element_rect(color = '#4D4D4D', size = 2, fill = NA),
        panel.grid.major.y = element_line(size = 0.75),
        panel.grid.minor.y = element_line(size = 0.5),
        panel.grid.major.x = element_line(size = 0.75),
        panel.grid.minor.x = element_line(size = 0.5),
        legend.position = 'none')

ggsave(filename = './plots/tensorflow_GFP_single_model.png', 
       device = 'png', 
       width = 5,
       height = 5, 
       dpi = 400,
       units = 'in')

### Nonfluorescent
ggplot(results, aes(x = wt_hand, y = wt_tf)) +
  geom_abline(slope = 1, intercept = 0, linetype = 'dashed', size = 1, color = '#4D4D4D') +
  geom_point(alpha = 1, size = 2, shape = 21, stroke = 1, color = 'black', fill = '#614973') +
  coord_fixed(xlim = c(0, 350), ylim = c(0, 350)) +
  labs(title = 'Non-fluorescent kernels', x = 'Manual count', y = 'Tensorflow') +
  theme_bw() +
  theme(axis.title = element_text(size = 20, face = 'bold'),
        axis.text = element_text(size = 18, face = 'bold'),
        plot.title = element_text(size = 22, face = 'bold', hjust = 0.5, margin = margin(0, 0, 10, 0)),
        axis.title.x = element_text(margin = margin(10, 0, 0, 0)),
        axis.title.y = element_text(margin = margin(0, 10, 0, 0)),
        axis.line = element_line(size = 0, color = '#4D4D4D'),
        axis.ticks = element_line(size = 0.75, color = '#4D4D4D'),
        axis.ticks.length = unit(4, 'pt'),
        plot.margin = margin(0.5, 0.8, 0.5, 0.3, 'cm'),
        panel.border = element_rect(color = '#4D4D4D', size = 2, fill = NA),
        panel.grid.major.y = element_line(size = 0.75),
        panel.grid.minor.y = element_line(size = 0.5),
        panel.grid.major.x = element_line(size = 0.75),
        panel.grid.minor.x = element_line(size = 0.5),
        legend.position = 'none')

ggsave(filename = './plots/tensorflow_wt_single_model.png', 
       device = 'png', 
       width = 5,
       height = 5, 
       dpi = 400,
       units = 'in')

### Percent GFP
ggplot(results, aes(x = 100 * percent_GFP_hand, y = 100 * percent_GFP_tf)) +
  geom_abline(slope = 1, intercept = 0, linetype = 'dashed', size = 1, color = '#4D4D4D') +
  geom_point(alpha = 1, size = 2, shape = 21, stroke = 1, color = 'black', fill = 'orange') +
  coord_fixed(xlim = c(0, 62), ylim = c(0, 62)) +
  labs(title = 'Percent fluorescent', x = 'Manual count', y = 'Tensorflow') +
  theme_bw() +
  theme(axis.title = element_text(size = 20, face = 'bold'),
        axis.text = element_text(size = 18, face = 'bold'),
        plot.title = element_text(size = 22, face = 'bold', hjust = 0.5, margin = margin(0, 0, 10, 0)),
        axis.title.x = element_text(margin = margin(10, 0, 0, 0)),
        axis.title.y = element_text(margin = margin(0, 10, 0, 0)),
        axis.line = element_line(size = 0, color = '#4D4D4D'),
        axis.ticks = element_line(size = 0.75, color = '#4D4D4D'),
        axis.ticks.length = unit(4, 'pt'),
        plot.margin = margin(0.5, 0.8, 0.5, 0.3, 'cm'),
        panel.border = element_rect(color = '#4D4D4D', size = 2, fill = NA),
        panel.grid.major.y = element_line(size = 0.75),
        panel.grid.minor.y = element_line(size = 0.5),
        panel.grid.major.x = element_line(size = 0.75),
        panel.grid.minor.x = element_line(size = 0.5),
        legend.position = 'none')

ggsave(filename = './plots/tensorflow_percent_single_model.png', 
       device = 'png', 
       width = 5,
       height = 5, 
       dpi = 400,
       units = 'in')


###### Linear regression ######
do_stats <- function(name, input_df) {
  fluorescent_lm <- lm(GFP_hand ~ GFP_tf, data = input_df)
  fluorescent_r_squared <- summary(fluorescent_lm)$adj.r.squared
  
  nonfluorescent_lm <- lm(wt_hand ~ wt_tf, data = input_df)
  nonfluorescent_r_squared <- summary(nonfluorescent_lm)$adj.r.squared
  
  percent_GFP_lm <- lm(percent_GFP_hand ~ percent_GFP_tf, data = input_df)
  percent_GFP_r_squared <- summary(percent_GFP_lm)$adj.r.squared
  
  output_df <- data.frame(name = rep(name, 3),
                          r_squared_category = c("fluorescent", "nonfluorescent", "percent_GFP"),
                          r_squared = c(fluorescent_r_squared, nonfluorescent_r_squared, percent_GFP_r_squared))
  
  return(output_df)
}

stats_results <- do_stats("Tensorflow single model", results)


###### Mean absolute deviation ######
# Getting the mean number of kernels per ear
mean_total_kernels <- mean(results$GFP_hand + results$wt_hand)

# Mean absolute deviations
mean_abs_dev_fluorescent <- mean(abs(results$GFP_hand - results$GFP_tf))
mean_abs_dev_wt <- mean(abs(results$wt_hand - results$wt_tf))
mean_abs_dev_percent <- mean(abs(results$percent_GFP_hand - results$percent_GFP_tf))

