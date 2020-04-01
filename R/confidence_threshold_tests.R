library(tidyverse)

#### Importing the data ######

# Hand_counted data
hand_counted <- read.table(file="./data/hand_count_summary.tsv",
                           sep = '\t',
                           header = TRUE)
hand_counted$image_name <- as.character(hand_counted$image_name)

# Detailed tensorflow output
input_df <- read.table(file="/Users/CiderBones/Desktop/Laboratory/r_projects/computer_vision/data/detailed_output_723.tsv",
                       sep = '\t',
                       header = TRUE)

colnames(input_df)[7] <- "image_name"
input_df$image_name <- as.character(input_df$image_name)
input_df$image_name <- str_remove(input_df$image_name, 'X')


# There's some rows with zeros (probably resulting from some bad code 
# when exporting the data)
input_df <- input_df[input_df$score != 0, ]


###### Summarizing the number of fluorescent / nonfluorescent at different confidence thresholds ######
### Functionalized
get_fluor_r_squared <- function(threshold, input_df, hand_counts) {
  df <- input_df %>% 
    group_by(image_name, class) %>%
    filter(score > threshold) %>%
    filter(class == 1) %>%
    summarize(count = n())
  
  df <- inner_join(df, hand_counts)
  df <- df[complete.cases(df), ]
  
  regression_r <- summary(lm(count ~ GFP_hand, data = df))$adj.r.squared
  
  output <- data.frame("threshold" = threshold, "r_squared" = regression_r)
  return(output)
}

# output <- get_fluor_r_squared(0.15, input_df, hand_counted)

df_list <- list()
i = 1

for (x in seq(0.01, 0.98, by=0.01)) {
  print(x)
  output <- get_fluor_r_squared(x, input_df, hand_counted)
  df_list[[i]] <- output
  i = i + 1
}

plot_df <- bind_rows(df_list)

###### Plotting ######
ggplot(plot_df, aes(x = threshold, y = r_squared)) +
  # geom_abline(slope = 1, intercept = 0, linetype = 'dashed', size = 1, color = '#4D4D4D') +
  geom_point(alpha = 1, size = 5, shape = 21, stroke = 1, color = 'black', fill = 'turquoise') +
  # geom_smooth(method = lm, color = "red") +
  # coord_fixed(xlim = c(0.4, 0.6), ylim = c(0.4, 0.6)) +
  labs(title = 'Confidence thresholds', x = 'Threshold', y = 'Adj. R Squared') +
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

ggsave(filename = './plots/confidence_thresholds.png',
       device = 'png',
       width = 5,
       height = 5,
       dpi = 400,
       units = 'in')



# Here's the newer version, with fluorescent and non-fluorescent 

#### Importing the data (2019_994 version) ######

# Hand_counted data
hand_counted <- read.table(file="./data/hand_count_2019.tsv",
                           sep = '\t',
                           header = TRUE)
hand_counted$image_name <- as.character(hand_counted$image_name)

# Detailed tensorflow output
input_df <- read.table(file="/Users/CiderBones/Desktop/Laboratory/r_projects/computer_vision/data/detailed_output_2019_994.tsv",
                       sep = '\t',
                       header = TRUE)

colnames(input_df)[7] <- "image_name"
input_df$image_name <- as.character(input_df$image_name)
#input_df$image_name <- str_remove(input_df$image_name, 'Y')


# There's some rows with zeros (probably resulting from some bad code 
# when exporting the data)
input_df <- input_df[input_df$score != 0, ]


###### Summarizing the number of fluorescent / nonfluorescent at different confidence thresholds ######
### Functionalized
get_fluor_r_squared <- function(threshold, input_df, hand_counts) {
  df <- input_df %>% 
    group_by(image_name, class) %>%
    filter(score > threshold) %>%
    filter(class == 1) %>%
    summarize(count = n())
  
  df <- inner_join(df, hand_counts)
  df <- df[complete.cases(df), ]
  
  regression_r <- summary(lm(count ~ GFP_hand, data = df))$adj.r.squared
  
  output <- data.frame("threshold" = threshold, "r_squared" = regression_r)
  return(output)
}

# output <- get_fluor_r_squared(0.15, input_df, hand_counted)

df_list <- list()
i = 1

for (x in seq(0.01, 0.98, by=0.01)) {
  print(x)
  output <- get_fluor_r_squared(x, input_df, hand_counted)
  df_list[[i]] <- output
  i = i + 1
}

plot_df <- bind_rows(df_list)

### Nonfluorescent
get_nonfluor_r_squared <- function(threshold, input_df, hand_counts) {
  df <- input_df %>% 
    group_by(image_name, class) %>%
    filter(score > threshold) %>%
    filter(class == 2) %>%
    summarize(count = n())
  
  df <- inner_join(df, hand_counts)
  df <- df[complete.cases(df), ]
  
  regression_r <- summary(lm(count ~ wt_hand, data = df))$adj.r.squared
  
  output <- data.frame("threshold" = threshold, "r_squared" = regression_r)
  return(output)
}

# output <- get_fluor_r_squared(0.15, input_df, hand_counted)

df_list_nonfluor <- list()
i = 1

for (x in seq(0.01, 0.98, by=0.01)) {
  print(x)
  output <- get_nonfluor_r_squared(x, input_df, hand_counted)
  df_list_nonfluor[[i]] <- output
  i = i + 1
}

plot_df_nonfluor <- bind_rows(df_list_nonfluor)



###### Plotting ######
ggplot(plot_df_nonfluor, aes(x = threshold, y = r_squared)) +
  # geom_abline(slope = 1, intercept = 0, linetype = 'dashed', size = 1, color = '#4D4D4D') +
  geom_point(alpha = 1, size = 3, shape = 21, stroke = 1, color = 'black', fill = 'turquoise') +
  # geom_smooth(method = lm, color = "red") +
  # coord_fixed(xlim = c(0.4, 0.6), ylim = c(0.4, 0.6)) +
  labs(title = 'Confidence thresholds', x = 'Threshold', y = 'Adj. R Squared') +
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

ggsave(filename = './plots/confidence_thresholds_994_nonfluor.png',
       device = 'png',
       width = 5,
       height = 5,
       dpi = 400,
       units = 'in')


### Playing around with the two outputs, trying to find the best confidence interval
plot_df_nonfluor <- plot_df_nonfluor[1:96, ]
joined_df <- data.frame(threshold = plot_df$threshold, fluor_r_squared = plot_df$r_squared, nonfluor_r_squared = plot_df_nonfluor$r_squared)
joined_df$test_col <- joined_df$fluor_r_squared + joined_df$nonfluor_r_squared

###### Plotting fluorescent and nonfluorescent on same plot ######
# Making the data tidy
to_be_tidied <- joined_df[ , 1:3]
colnames(to_be_tidied) <- c("Threshold", "Fluorescent", "Non-fluorescent")
tidy_joined_df <- gather(to_be_tidied, "Class", "R_squared", 2:3)

# Getting the value for the line at max r-squared
max_r_squared <- joined_df$threshold[joined_df$test_col == max(joined_df$test_col)]

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
