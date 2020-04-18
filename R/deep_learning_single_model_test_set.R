###### Deep learning model test set ######
# Here we test the effectiveness of our deep learning model on 2018 and 2019 
# field season test sets, based on a single model trained on images from both
# years. For validation, we used hand-counted total kernel numbers.

library(tidyverse)


###### Importing the data ######

# Hand-counted validation data
hand_counted <- read.table(file="./data/hand_counts.tsv",
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

results_2018 <- import_tensorflow_summary("./data/test_set_model_predictions_2018_summary.tsv", hand_counted)
results_2019 <- import_tensorflow_summary("./data/test_set_model_predictions_2019_summary.tsv", hand_counted)


###### Plotting the results (Figure 5B) ######
### 2018 Fluorescent
ggplot(results_2018, aes(x = GFP_hand, y = GFP_tf)) +
  geom_abline(slope = 1, intercept = 0, linetype = 'dashed', size = 1, color = '#4D4D4D') +
  geom_point(alpha = 1, size = 2, shape = 21, stroke = 1, color = 'black', fill = '#98f542') +
  coord_fixed(xlim = c(0, 350), ylim = c(0, 350)) +
  labs(title = 'Fluorescent kernels 2018', x = 'Manual count', y = 'Tensorflow') +
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

ggsave(filename = './plots/tensorflow_GFP_2018.png', 
       device = 'png', 
       width = 5,
       height = 5, 
       dpi = 400,
       units = 'in')

### 2018 Nonfluorescent
ggplot(results_2018, aes(x = wt_hand, y = wt_tf)) +
  geom_abline(slope = 1, intercept = 0, linetype = 'dashed', size = 1, color = '#4D4D4D') +
  geom_point(alpha = 1, size = 2, shape = 21, stroke = 1, color = 'black', fill = '#614973') +
  coord_fixed(xlim = c(0, 350), ylim = c(0, 350)) +
  labs(title = 'Nonfluorescent kernels 2018', x = 'Manual count', y = 'Tensorflow') +
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

ggsave(filename = './plots/tensorflow_wt_2018.png', 
       device = 'png', 
       width = 5,
       height = 5, 
       dpi = 400,
       units = 'in')

### 2018 Percent GFP
ggplot(results_2018, aes(x = 100 * percent_GFP_hand, y = 100 * percent_GFP_tf)) +
  geom_abline(slope = 1, intercept = 0, linetype = 'dashed', size = 1, color = '#4D4D4D') +
  geom_point(alpha = 1, size = 2, shape = 21, stroke = 1, color = 'black', fill = 'orange') +
  coord_fixed(xlim = c(0, 62), ylim = c(0, 62)) +
  labs(title = 'Percent fluorescent 2018', x = 'Manual count', y = 'Tensorflow') +
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

ggsave(filename = './plots/tensorflow_percent_2018.png', 
       device = 'png', 
       width = 5,
       height = 5, 
       dpi = 400,
       units = 'in')

### 2019 Fluorescent
ggplot(results_2019, aes(x = GFP_hand, y = GFP_tf)) +
  geom_abline(slope = 1, intercept = 0, linetype = 'dashed', size = 1, color = '#4D4D4D') +
  geom_point(alpha = 1, size = 2, shape = 21, stroke = 1, color = 'black', fill = '#98f542') +
  coord_fixed(xlim = c(0, 350), ylim = c(0, 350)) +
  labs(title = 'Fluorescent kernels 2019', x = 'Manual count', y = 'Tensorflow') +
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

ggsave(filename = './plots/tensorflow_GFP_2019.png', 
       device = 'png', 
       width = 5,
       height = 5, 
       dpi = 400,
       units = 'in')

### 2019 Nonfluorescent
ggplot(results_2019, aes(x = wt_hand, y = wt_tf)) +
  geom_abline(slope = 1, intercept = 0, linetype = 'dashed', size = 1, color = '#4D4D4D') +
  geom_point(alpha = 1, size = 2, shape = 21, stroke = 1, color = 'black', fill = '#614973') +
  coord_fixed(xlim = c(0, 350), ylim = c(0, 350)) +
  labs(title = 'Nonfluorescent kernels 2019', x = 'Manual count', y = 'Tensorflow') +
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

ggsave(filename = './plots/tensorflow_wt_2019.png', 
       device = 'png', 
       width = 5,
       height = 5, 
       dpi = 400,
       units = 'in')

### 2019 Percent GFP
ggplot(results_2019, aes(x = 100 * percent_GFP_hand, y = 100 * percent_GFP_tf)) +
  geom_abline(slope = 1, intercept = 0, linetype = 'dashed', size = 1, color = '#4D4D4D') +
  geom_point(alpha = 1, size = 2, shape = 21, stroke = 1, color = 'black', fill = 'orange') +
  coord_fixed(xlim = c(0, 62), ylim = c(0, 62)) +
  labs(title = 'Percent fluorescent 2019', x = 'Manual count', y = 'Tensorflow') +
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

ggsave(filename = './plots/tensorflow_percent_2019.png', 
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

stats_results_2018 <- do_stats("Tensorflow 2018", results_2018)
stats_results_2019 <- do_stats("Tensorflow 2019", results_2019)

stats <- rbind(stats_results_2018, stats_results_2019)


###### Mean absolute deviation ######
# Getting the mean number of kernels per ear
mean_total_kernels <- mean(results$GFP_hand + results$wt_hand)

mean_abs_dev_fluorescent <- mean(abs(results$GFP_hand - results$GFP_tf))
mean_abs_dev_wt <- mean(abs(results$wt_hand - results$wt_tf))
mean_abs_dev_percent <- mean(abs(results$percent_GFP_hand - results$percent_GFP_tf))




