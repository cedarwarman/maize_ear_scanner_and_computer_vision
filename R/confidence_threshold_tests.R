###### Confidence threshold tests ######
# Here we empirically determine the optimal confidence threshold for bounding 
# bounding box outputs (Supplemental Figure 2)

library(tidyverse)

###### Importing the data ######
# Hand counted validation data
hand_validations <- read.table(file="./data/test_set_hand_validations.tsv",
                               sep = '\t',
                               header = TRUE)
hand_validations$image_name <- as.character(hand_validations$image_name)
### ADD X TO THE NAMES HERE, THEN DON'T DELETE IT ON THE MODEL PREDICTIONS

# Model predictions
model_predictions <- read.table(file="./data/test_set_model_predictions.tsv",
                                sep = '\t',
                                header = TRUE)
### CHANGE THIS ON THE INPUT TSV FILE THEN DELETE THIS STEP
colnames(model_predictions)[7] <- "image_name"
model_predictions$image_name <- as.character(model_predictions$image_name)
### CHANGE THIS ON THE INPUT TSV FILE THEN DELETE THIS STEP
model_predictions$image_name <- str_remove(model_predictions$image_name, 'X')

# There's some rows with zeros (probably resulting from some bad code 
# when exporting the data)
### DELETE THESE IN FINAL SET
model_predictions <- model_predictions[model_predictions$score != 0, ]


###### Summarizing fluorescent/nonfluorescent R-squared values at different confidence thresholds ######
### Fluorescent kernels
get_fluor_r_squared <- function(threshold, input_df, hand_counts) {
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
  output <- get_fluor_r_squared(x, model_predictions, hand_validations)
  df_list_fluor[[i]] <- output
  i = i + 1
}

fluor_r_squared <- bind_rows(df_list_fluor)

### Nonfluorescent kernels
get_nonfluor_r_squared <- function(threshold, input_df, hand_counts) {
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
  output <- get_nonfluor_r_squared(x, model_predictions, hand_validations)
  df_list_nonfluor[[i]] <- output
  i = i + 1
}

nonfluor_r_squared <- bind_rows(df_list_nonfluor)


###### Calculating the optimal confidence threshold ######
# I will define the optimal confidence threshold as the confidence level that 
# maximizes the combined fluorescent and non-fluorescent R-squared.
joined_df <- data.frame(threshold = fluor_r_squared$threshold, 
                        fluor_r_squared = fluor_r_squared$r_squared, 
                        nonfluor_r_squared = nonfluor_r_squared$r_squared)
joined_df$sum_r_squared <- joined_df$fluor_r_squared + joined_df$nonfluor_r_squared
max_r_squared <- joined_df$threshold[joined_df$sum_r_squared == max(joined_df$sum_r_squared)]

###### Plotting ######
# Making the data tidy
to_be_tidied <- joined_df[ , 1:3]
colnames(to_be_tidied) <- c("Threshold", "Fluorescent", "Non-fluorescent")
tidy_joined_df <- gather(to_be_tidied, "Class", "R_squared", 2:3)

ggplot(tidy_joined_df, aes(x = Threshold, y = R_squared, group = Class)) +
  geom_point(aes(shape = Class), size = 2) +
  scale_shape_manual(values = c(19, 4)) +
  geom_vline(xintercept = max_r_squared, linetype = 'dashed', size = 1) +
  coord_fixed(xlim = c(0, 1), ylim = c(0, 1)) +
  labs(title = 'Optimal confidence threshold', x = 'Threshold', y = 'Adj. R-squared') +
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

ggsave(filename = './plots/optimal_confidence_thresholds.png',
       device = 'png',
       width = 10,
       height = 8,
       dpi = 400,
       units = 'in')
