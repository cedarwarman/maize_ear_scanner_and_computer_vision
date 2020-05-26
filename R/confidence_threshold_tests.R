###### Confidence threshold tests ######
# Here we empirically determine the optimal confidence threshold for bounding 
# bounding box outputs, based on the test image set (Supplemental Figure 2).

library(tidyverse)

###### Importing the data ######
# Hand counted validation data
hand_validations <- read.table(file="./data/test_set_hand_counts.tsv",
                               sep = '\t',
                               header = TRUE)
hand_validations$image_name <- as.character(hand_validations$image_name)

# Combined model predictions
model_predictions_2018_2019 <- read.table(file="./data/test_set_single_model_predictions_2018_2019_detailed.tsv",
                                     sep = '\t',
                                     header = TRUE)
### CHANGE THIS ON THE INPUT TSV FILE THEN DELETE THIS STEP
colnames(model_predictions_2018_2019)[7] <- "image_name"
model_predictions_2018_2019$image_name <- as.character(model_predictions_2018_2019$image_name)

model_predictions_2018_2019 <- model_predictions_2018_2019[model_predictions_2018_2019$score != 0, ]

# 2018 model predictions
model_predictions_2018 <- read.table(file="./data/test_set_two_models_predictions_2018_detailed.tsv",
                                sep = '\t',
                                header = TRUE)
### CHANGE THIS ON THE INPUT TSV FILE THEN DELETE THIS STEP
colnames(model_predictions_2018)[7] <- "image_name"
model_predictions_2018$image_name <- as.character(model_predictions_2018$image_name)

# There's some rows with zeros (probably resulting from some bad code 
# when exporting the data)
### DELETE THESE IN FINAL SET
model_predictions_2018 <- model_predictions_2018[model_predictions_2018$score != 0, ]


# 2019 model predictions
model_predictions_2019 <- read.table(file="./data/test_set_two_models_predictions_2019_detailed.tsv",
                                     sep = '\t',
                                     header = TRUE)
### CHANGE THIS ON THE INPUT TSV FILE THEN DELETE THIS STEP
colnames(model_predictions_2019)[7] <- "image_name"
model_predictions_2019$image_name <- as.character(model_predictions_2019$image_name)

# There's some rows with zeros (probably resulting from some bad code 
# when exporting the data)
### DELETE THESE IN FINAL SET
model_predictions_2019 <- model_predictions_2019[model_predictions_2019$score != 0, ]


###### Summarizing fluorescent/non-fluorescent R-squared values at different confidence thresholds ######
### Function for fluorescent kernels
get_fluor_r_squared <- function(input_df, hand_counts) {
  # Sub function that calculates the R-squared for a single threshold
  get_single_fluor_r_squared <- function(threshold, input_df, hand_counts) {
    df <- input_df %>% 
      group_by(image_name, class) %>%
      filter(score > threshold) %>%
      filter(class == 1) %>%
      summarize(count = n())
    
    df <- inner_join(df, hand_counts)
    df <- df[complete.cases(df), ]
    
    # Returns 0 when there are no bounding boxes that satisfy a threshold
    if (nrow(df) == 0) {
      output <- data.frame("threshold" = threshold, "r_squared" = 0)
      return(output)
      
      # Runs the regression if bounding boxes are present
    } else {
      regression_r <- summary(lm(count ~ GFP_hand, data = df))$adj.r.squared
      output <- data.frame("threshold" = threshold, "r_squared" = regression_r)
      return(output)
    }
  }
  
  # This sets up a list, which will become a dataframe after it's populated by 
  # the following for loop
  df_list_fluor <- list()
  i = 1
  for (x in seq(0.01, 1, by=0.01)) {
    print(x)
    output <- get_single_fluor_r_squared(x, input_df, hand_counts)
    df_list_fluor[[i]] <- output
    i = i + 1
  }
  
  fluor_r_squared <- bind_rows(df_list_fluor)
  return(fluor_r_squared)
}

### Function for nonluorescent kernels
get_nonfluor_r_squared <- function(input_df, hand_counts) {
  # Sub function that calculates the R-squared for a single threshold
  get_single_nonfluor_r_squared <- function(threshold, input_df, hand_counts) {
    df <- input_df %>% 
      group_by(image_name, class) %>%
      filter(score > threshold) %>%
      filter(class == 2) %>%
      summarize(count = n())
    
    df <- inner_join(df, hand_counts)
    df <- df[complete.cases(df), ]
    
    # Returns 0 when there are no bounding boxes that satisfy a threshold
    if (nrow(df) == 0) {
      output <- data.frame("threshold" = threshold, "r_squared" = 0)
      return(output)
      
      # Runs the regression if bounding boxes are present
    } else {
      regression_r <- summary(lm(count ~ wt_hand, data = df))$adj.r.squared
      output <- data.frame("threshold" = threshold, "r_squared" = regression_r)
      return(output)
    }
  }
  
  # This sets up a list, which will become a dataframe after it's populated by 
  # the following for loop
  df_list_nonfluor <- list()
  i = 1
  for (x in seq(0.01, 1, by=0.01)) {
    print(x)
    output <- get_single_nonfluor_r_squared(x, input_df, hand_counts)
    df_list_nonfluor[[i]] <- output
    i = i + 1
  }
  
  nonfluor_r_squared <- bind_rows(df_list_nonfluor)
  return(nonfluor_r_squared)
}

### Calculating fluorescent R-squared values
fluor_r_squared_2018_2019 <- get_fluor_r_squared(model_predictions_2018_2019, hand_validations)
fluor_r_squared_2018 <- get_fluor_r_squared(model_predictions_2018, hand_validations)
fluor_r_squared_2019 <- get_fluor_r_squared(model_predictions_2019, hand_validations)


### Calculating nonfluorescent R-squared values
nonfluor_r_squared_2018_2019 <- get_nonfluor_r_squared(model_predictions_2018_2019, hand_validations)
nonfluor_r_squared_2018 <- get_nonfluor_r_squared(model_predictions_2018, hand_validations)
nonfluor_r_squared_2019 <- get_nonfluor_r_squared(model_predictions_2019, hand_validations)


###### Calculating the optimal confidence thresholds ######
# I will define the optimal confidence threshold as the confidence level that 
# maximizes the combined fluorescent and non-fluorescent R-squared.
joined_df_2018_2019 <- data.frame(threshold = fluor_r_squared_2018_2019$threshold,
                             fluor_r_squared = fluor_r_squared_2018_2019$r_squared,
                             nonfluor_r_squared = nonfluor_r_squared_2018_2019$r_squared)
joined_df_2018_2019$sum_r_squared <- joined_df_2018_2019$fluor_r_squared + joined_df_2018_2019$nonfluor_r_squared
joined_df_2018_2019 <- joined_df_2018_2019[complete.cases(joined_df_2018_2019), ]
max_r_squared_2018_2019 <- joined_df_2018_2019$threshold[joined_df_2018_2019$sum_r_squared == max(joined_df_2018_2019$sum_r_squared)]

joined_df_2018 <- data.frame(threshold = fluor_r_squared_2018$threshold,
                             fluor_r_squared = fluor_r_squared_2018$r_squared,
                             nonfluor_r_squared = nonfluor_r_squared_2018$r_squared)
joined_df_2018$sum_r_squared <- joined_df_2018$fluor_r_squared + joined_df_2018$nonfluor_r_squared
joined_df_2018 <- joined_df_2018[complete.cases(joined_df_2018), ]
max_r_squared_2018 <- joined_df_2018$threshold[joined_df_2018$sum_r_squared == max(joined_df_2018$sum_r_squared)]

joined_df_2019 <- data.frame(threshold = fluor_r_squared_2019$threshold,
                             fluor_r_squared = fluor_r_squared_2019$r_squared,
                             nonfluor_r_squared = nonfluor_r_squared_2019$r_squared)
joined_df_2019$sum_r_squared <- joined_df_2019$fluor_r_squared + joined_df_2019$nonfluor_r_squared
joined_df_2019 <- joined_df_2019[complete.cases(joined_df_2019), ]
max_r_squared_2019 <- joined_df_2019$threshold[joined_df_2019$sum_r_squared == max(joined_df_2019$sum_r_squared)]


###### Plotting (Supplemental Figure 2) ######
# Function to make the data tidy for plotting
tidy_data_for_plot <- function(input_df) {
  to_be_tidied <- input_df[ , 1:3]
  colnames(to_be_tidied) <- c("Threshold", "Fluorescent", "Non-fluorescent")
  tidy_joined_df <- gather(to_be_tidied, "Class", "R_squared", 2:3)
  return(tidy_joined_df)
}

# Plotting 2018/2019 data
plot_2018_2019 <- tidy_data_for_plot(joined_df_2018_2019)

ggplot(plot_2018_2019, aes(x = Threshold, y = R_squared, group = Class)) +
  geom_point(aes(shape = Class), size = 2) +
  scale_shape_manual(values = c(19, 4)) +
  geom_vline(xintercept = max_r_squared_2018_2019, linetype = 'dashed', size = 1) +
  coord_fixed(xlim = c(0, 1), ylim = c(0, 1)) +
  labs(title = 'Optimal confidence threshold 2018/2019', x = 'Threshold', y = 'Adj. R-squared') +
  theme_bw() +
  theme(axis.title = element_text(size = 24, face = 'bold'),
        axis.text = element_text(size = 18, face = 'bold'),
        plot.title = element_text(hjust = 0.5, size = 28, face = 'bold', margin = margin(0, 0, 10, 0)),
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

ggsave(filename = './plots/optimal_confidence_thresholds_2018_2019.png',
       device = 'png',
       width = 9,
       height = 8,
       dpi = 400,
       units = 'in')

# Plotting 2018 data
plot_2018 <- tidy_data_for_plot(joined_df_2018)

ggplot(plot_2018, aes(x = Threshold, y = R_squared, group = Class)) +
  geom_point(aes(shape = Class), size = 2) +
  scale_shape_manual(values = c(19, 4)) +
  geom_vline(xintercept = max_r_squared_2018, linetype = 'dashed', size = 1) +
  coord_fixed(xlim = c(0, 1), ylim = c(0, 1)) +
  labs(title = 'Optimal confidence threshold 2018', x = 'Threshold', y = 'Adj. R-squared') +
  theme_bw() +
  theme(axis.title = element_text(size = 24, face = 'bold'),
        axis.text = element_text(size = 18, face = 'bold'),
        plot.title = element_text(size = 28, face = 'bold', margin = margin(0, 0, 10, 0)),
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

ggsave(filename = './plots/optimal_confidence_thresholds_2018.png',
       device = 'png',
       width = 9,
       height = 8,
       dpi = 400,
       units = 'in')

# Plotting 2019 data
plot_2019 <- tidy_data_for_plot(joined_df_2019)

ggplot(plot_2019, aes(x = Threshold, y = R_squared, group = Class)) +
  geom_point(aes(shape = Class), size = 2) +
  scale_shape_manual(values = c(19, 4)) +
  geom_vline(xintercept = max_r_squared_2019, linetype = 'dashed', size = 1) +
  coord_fixed(xlim = c(0, 1), ylim = c(0, 1)) +
  labs(title = 'Optimal confidence threshold 2019', x = 'Threshold', y = 'Adj. R-squared') +
  theme_bw() +
  theme(axis.title = element_text(size = 24, face = 'bold'),
        axis.text = element_text(size = 18, face = 'bold'),
        plot.title = element_text(size = 28, face = 'bold', margin = margin(0, 0, 10, 0)),
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
        legend.position = 'right',
        legend.text = element_text(size = 16, face = 'bold'),
        legend.title = element_text(size = 18, face = 'bold'))

ggsave(filename = './plots/optimal_confidence_thresholds_2019.png',
       device = 'png',
       width = 10,
       height = 8,
       dpi = 400,
       units = 'in')
