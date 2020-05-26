###### Deep learning model larger test case ######
# Here we apply 2018 and 2019 individually trained deep learning models on a 
# larger set of data from field trails. 

library(tidyverse)

###### Importing data ######
# 2018 hand counts
hand_counts_2018_male <- read.table(file = "./data/hand_counts_transcriptome_set_2018_male.tsv",
                                    header = TRUE,
                                    sep = '\t')
hand_counts_2018_female <- read.table(file = "./data/hand_counts_transcriptome_set_2018_female.tsv",
                                      header = TRUE,
                                      sep = '\t')

# Tensorflow model predictions
tensorflow_counts <- read.table(file = "./data/transcriptome_set_tensorflow_predictions.tsv",
                                header = TRUE,
                                sep = '\t')

# Subsetting out 2018 Tensorflow predictions
tensorflow_counts_2018 <- tensorflow_counts[tensorflow_counts$year == '2018', ]

# Metadata
metadata <- read.table(file = "./data/transcriptome_metadata.tsv",
                       header = TRUE,
                       sep = '\t')


###### Generalized linear model function ######
do_glm <- function(counts, category_string, group_string) {
  glm_output <- glm(cbind(number_of_GFP_kernels, number_of_WT_kernels) ~
                      as.factor(allele),
                    data = counts,
                    family = quasibinomial(link = "logit"))
  print(summary(glm_output))
  
  # Getting p-values for each line using a quasi-likelihood test (comparing to 
  # Mendelian, 50% transmission)
  S = vcov(glm_output)
  p_value = coef(summary(glm_output))[1,4]
  
  for (i in 2:nrow(coef(summary(glm_output)))){
    beta = coef(summary(glm_output))[1,1] + coef(summary(glm_output))[i,1]
    sigma = sqrt(sum(S[c(1,i),c(1,i)]))
    p_value = c(p_value, 2*(1-pnorm(abs(beta/sigma))))
  }
  names(p_value) = sort(unique(counts$allele))
  
  # Multiple testing correction for the p-values using Benjamini-Hochberg method
  p_value_adj = p.adjust(p_value, method = "BH")
  p_results = data.frame(raw_p = p_value, adjusted_p = p_value_adj)
  
  # Tidying
  p_results <- rownames_to_column(p_results, "allele")
  p_results$category <- category_string
  p_results$group <- group_string
  
  # Getting the percent transmission
  transmission <- counts %>% group_by(allele) %>%
    summarize(percent_transmission = sum(number_of_GFP_kernels) /
                (sum(number_of_GFP_kernels) + sum(number_of_WT_kernels)))
  transmission$allele <- as.character(transmission$allele)
  p_results <- full_join(p_results, transmission)
  
  # Adding factor for p-value significance
  p_value_threshold <- 0.05
  p_results$glm_adj_p_sig <- NA
  p_results$glm_adj_p_sig[p_results$adjusted_p >= p_value_threshold] <- 0
  p_results$glm_adj_p_sig[p_results$adjusted_p < p_value_threshold] <- 1
  
  return(p_results)
}


###### GLM for 2018 hand counts ######
glm_2018_hand_mp <- do_glm(hand_counts_2018_male[hand_counts_2018_male$category == "vegetative_cell_high", ], "2018_hand", "mp")
glm_2018_hand_sc <- do_glm(hand_counts_2018_male[hand_counts_2018_male$category == "sperm_cell_high", ], "2018_hand", "sc")
glm_2018_hand_seedling <- do_glm(hand_counts_2018_male[hand_counts_2018_male$category == "seedling_only", ], "2018_hand", "seedling")
glm_2018_hand_females <- do_glm(hand_counts_2018_female, "2018_hand", 'females')

# Combining them all into one 
glm_2018_hand <- rbind(glm_2018_hand_mp, glm_2018_hand_sc, glm_2018_hand_seedling, glm_2018_hand_females)

# Adding expression levels for plotting
glm_2018_hand <- left_join(glm_2018_hand, unique(metadata[, c(3:4)]))

# Adding allele category
glm_2018_hand$allele_category <- "2018_hand"


###### GLM for Tensorflow model predictions (2018 only) ######
glm_model_counts_2018_mp <- do_glm(tensorflow_counts_2018[tensorflow_counts_2018$cross_type == "male" & 
                                                          tensorflow_counts_2018$category == "vegetative_cell_high", ], "tensorflow_counts_2018", "mp")
glm_model_counts_2018_sc <- do_glm(tensorflow_counts_2018[tensorflow_counts_2018$cross_type == "male" & 
                                                          tensorflow_counts_2018$category == "sperm_cell_high", ], "tensorflow_counts_2018", "sc")
glm_model_counts_2018_seedling <- do_glm(tensorflow_counts_2018[tensorflow_counts_2018$cross_type == "male" & 
                                                                tensorflow_counts_2018$category == "seedling_only", ], "tensorflow_counts_2018", "seedling")
glm_model_counts_2018_females <- do_glm(tensorflow_counts_2018[tensorflow_counts_2018$cross_type == "female", ], "tensorflow_counts_2018", "females")

# Combining them all into one 
glm_model_counts_2018 <- rbind(glm_model_counts_2018_mp, glm_model_counts_2018_sc, glm_model_counts_2018_seedling, glm_model_counts_2018_females)

# Adding expression levels and allele group for plotting
glm_model_counts_2018 <- left_join(glm_model_counts_2018, unique(metadata[, c(3:5)]))


###### GLM for Tensorflow model predictions (2018 + 2019) ######
glm_model_counts_mp <- do_glm(tensorflow_counts[tensorflow_counts$cross_type == "male" & 
                                                tensorflow_counts$category == "vegetative_cell_high", ], "tensorflow_counts", "mp")
glm_model_counts_sc <- do_glm(tensorflow_counts[tensorflow_counts$cross_type == "male" & 
                                                tensorflow_counts$category == "sperm_cell_high", ], "tensorflow_counts", "sc")
glm_model_counts_seedling <- do_glm(tensorflow_counts[tensorflow_counts$cross_type == "male" & 
                                                      tensorflow_counts$category == "seedling_only", ], "tensorflow_counts", "seedling")
glm_model_counts_females <- do_glm(tensorflow_counts[tensorflow_counts$cross_type == "female", ], "tensorflow_counts", "females")

# Combining them all into one 
glm_model_counts <- rbind(glm_model_counts_mp, glm_model_counts_sc, glm_model_counts_seedling, glm_model_counts_females)

# Adding expression levels and allele group for plotting
glm_model_counts <- left_join(glm_model_counts, unique(metadata[, c(3:5)]))


###### GLM for 2018 hand counts vs 2018 Tensorflow predictions with year as a factor ######
# Here we test whether hand counts and Tensorflow predictions are different 
# in any systemic way by using hand counts vs model predictions as a factor
# in a GLM.

# First, setting up the dataset:
hand_female <- hand_counts_2018_female[ , 2:5]
hand_female$cross_type <- 'female' 
hand_female$count_method <- 'hand'

hand_male <- hand_counts_2018_male[ , 2:5]
hand_male$cross_type <- 'male' 
hand_male$count_method <- 'hand'

tf_counts <- tensorflow_counts_2018[ , c(4, 5, 9, 10, 7)]
tf_counts$count_method <- 'tensorflow'

hand_vs_model_counts <- rbind(hand_female, hand_male, tf_counts)

# Making a GLM function 
do_glm_with_count_method <- function(counts) {
  glm_output <- glm(cbind(number_of_GFP_kernels, number_of_WT_kernels) ~
                    as.factor(allele) + count_method,
                    data = counts,
                    family = quasibinomial(link = "logit"))
  print(summary(glm_output))
}

# Running the GLMs
do_glm_with_count_method(hand_vs_model_counts)
do_glm_with_count_method(hand_vs_model_counts[hand_vs_model_counts$cross_type == 'female', ])
do_glm_with_count_method(hand_vs_model_counts[hand_vs_model_counts$cross_type == 'male' &
                                                hand_vs_model_counts$category == 'seedling_only', ])
do_glm_with_count_method(hand_vs_model_counts[hand_vs_model_counts$cross_type == 'male' &
                                                hand_vs_model_counts$category == 'vegetative_cell_high', ])
do_glm_with_count_method(hand_vs_model_counts[hand_vs_model_counts$cross_type == 'male' &
                                                hand_vs_model_counts$category == 'sperm_cell_high', ])


###### Combining hand counts and CV counts for plotting ######
# 2018 hand coutns vs 2018 Tensorflow predictions
plot_df_2018 <- rbind(glm_2018_hand, glm_model_counts_2018)
plot_df_2018 <- plot_df_2018[plot_df_2018$allele %in% unique(glm_model_counts_2018$allele), ]

# 2018 hand counts vs 2018+2019 Tensorflow predictions
plot_df <- rbind(glm_2018_hand, glm_model_counts)


###### Plotting 2018 Hand counts vs 2018 Tensorflow predictions ######
### All females
color_vec <- c("#2d2d2d", "#F6B8FF") # Black and pink
ggplot(data = plot_df_2018[plot_df_2018$group == "females", ], 
       aes(x = log2(FPKM), 
           y = 100 * percent_transmission, 
           fill = factor(glm_adj_p_sig))) +
  geom_hline(yintercept = 50, size = 1, linetype = "dashed") +
  geom_line(aes(group = allele), color = "black", size = 0.8) +
  geom_point(aes(shape = allele_category), size = 3.5, stroke = 1, color = 'black') +
  scale_shape_manual(values = c(21, 23)) +
  scale_y_continuous(breaks = c(20, 25, 30, 35, 40, 45, 50, 55, 60),
                     labels = c("20", "25", "30", "35", "40", "45", "50", "55", "60"),
                     limits = c(20, 60)) +
  scale_x_continuous(breaks = seq(5, 15, by = 2.5),
                     labels = as.character(seq(5, 15, by = 2.5)),
                     limits = c(4.5, 15)) +
  labs(title = "Female transmission",
       x = "log2(FPKM)",
       y = "% GFP transmission") +
  scale_fill_manual(values = color_vec,
                    name = "Significance",
                    breaks = c("0", "1"),
                    labels = c(" p > 0.05", " p < 0.05")) +
  theme_bw() +
  theme(plot.title = element_text(size = 32, face = "bold", margin = margin(0, 0, 12, 0)),
        axis.title = element_text(size = 22, face = "bold"),
        axis.text = element_text(size = 18, face = "bold"),
        axis.title.x = element_text(margin = margin(10, 0, 0, 0)),
        axis.title.y = element_text(margin = margin(0, 10, 0, 0)),
        axis.line = element_line(size = 0, color = "#4D4D4D"),
        axis.ticks = element_line(size = 1, color = "#4D4D4D"),
        axis.ticks.length = unit(4, "pt"),
        plot.margin = margin(0.1, 0.1, 0.1, 0.1, "in"),
        panel.border = element_rect(color = "#4D4D4D", size = 2, fill = NA),
        panel.grid.major.y = element_line(size = 1),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        legend.position = "none")

ggsave(filename = './plots/larger_transcriptome_set_2018_females.png',
       device = 'png',
       width = 12,
       height = 6,
       dpi = 400,
       units = 'in')

### All males
color_vec <- c("#2d2d2d", "#F6B8FF") # Black and pink
ggplot(data = plot_df_2018[plot_df_2018$group == "seedling" | 
                             plot_df_2018$group == "mp" |
                             plot_df_2018$group == "sc", ], 
       aes(x = log2(FPKM), 
           y = 100 * percent_transmission, 
           fill = factor(glm_adj_p_sig))) +
  geom_hline(yintercept = 50, size = 1, linetype = "dashed") +
  geom_line(aes(group = allele), color = "black", size = 0.8) +
  geom_point(aes(shape = allele_category), size = 3.5, stroke = 1, color = 'black') +
  scale_shape_manual(values = c(21, 23)) +
  scale_y_continuous(breaks = c(20, 25, 30, 35, 40, 45, 50, 55, 60),
                     labels = c("20", "25", "30", "35", "40", "45", "50", "55", "60"),
                     limits = c(20, 60)) +
  scale_x_continuous(breaks = seq(5, 15, by = 2.5),
                     labels = as.character(seq(5, 15, by = 2.5)),
                     limits = c(4.5, 15)) +
  labs(title = "Male transmission",
       x = "log2(FPKM)",
       y = "% GFP transmission") +
  scale_fill_manual(values = color_vec,
                    name = "Significance",
                    breaks = c("0", "1"),
                    labels = c(" p > 0.05", " p < 0.05")) +
  theme_bw() +
  theme(plot.title = element_text(size = 32, face = "bold", margin = margin(0, 0, 12, 0)),
        axis.title = element_text(size = 22, face = "bold"),
        axis.text = element_text(size = 18, face = "bold"),
        axis.title.x = element_text(margin = margin(10, 0, 0, 0)),
        axis.title.y = element_text(margin = margin(0, 10, 0, 0)),
        axis.line = element_line(size = 0, color = "#4D4D4D"),
        axis.ticks = element_line(size = 1, color = "#4D4D4D"),
        axis.ticks.length = unit(4, "pt"),
        plot.margin = margin(0.1, 0.1, 0.1, 0.1, "in"),
        panel.border = element_rect(color = "#4D4D4D", size = 2, fill = NA),
        panel.grid.major.y = element_line(size = 1),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        legend.position = "none")

ggsave(filename = './plots/larger_transcriptome_set_2018_males.png',
       device = 'png',
       width = 12,
       height = 6,
       dpi = 400,
       units = 'in')


###### Plotting 2018 Hand counts vs 2018+2019 Tensorflow predictions ######
### All females
color_vec <- c("#2d2d2d", "#F6B8FF") # Black and pink
ggplot(data = plot_df[plot_df$group == "females", ], 
       aes(x = log2(FPKM), 
           y = 100 * percent_transmission, 
           fill = factor(glm_adj_p_sig))) +
  geom_hline(yintercept = 50, size = 1, linetype = "dashed") +
  geom_line(aes(group = allele), color = "black", size = 0.8) +
  geom_point(aes(shape = allele_category), size = 3.5, stroke = 1, color = 'black') +
  scale_shape_manual(values = c(21, 24, 23)) +
  scale_y_continuous(breaks = c(20, 25, 30, 35, 40, 45, 50, 55, 60),
                     labels = c("20", "25", "30", "35", "40", "45", "50", "55", "60"),
                     limits = c(20, 60)) +
  scale_x_continuous(breaks = seq(5, 15, by = 2.5),
                     labels = as.character(seq(5, 15, by = 2.5)),
                     limits = c(4.5, 15)) +
  labs(title = "Female transmission",
       x = "log2(FPKM)",
       y = "% GFP transmission") +
  scale_fill_manual(values = color_vec,
                    name = "Significance",
                    breaks = c("0", "1"),
                    labels = c(" p > 0.05", " p < 0.05")) +
  theme_bw() +
  theme(plot.title = element_text(size = 32, face = "bold", margin = margin(0, 0, 12, 0)),
        axis.title = element_text(size = 22, face = "bold"),
        axis.text = element_text(size = 18, face = "bold"),
        axis.title.x = element_text(margin = margin(10, 0, 0, 0)),
        axis.title.y = element_text(margin = margin(0, 10, 0, 0)),
        axis.line = element_line(size = 0, color = "#4D4D4D"),
        axis.ticks = element_line(size = 1, color = "#4D4D4D"),
        axis.ticks.length = unit(4, "pt"),
        plot.margin = margin(0.1, 0.1, 0.1, 0.1, "in"),
        panel.border = element_rect(color = "#4D4D4D", size = 2, fill = NA),
        panel.grid.major.y = element_line(size = 1),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        legend.position = "none")

ggsave(filename = './plots/larger_transcriptome_set_females.png',
       device = 'png',
       width = 12,
       height = 6,
       dpi = 400,
       units = 'in')

### All males
color_vec <- c("#2d2d2d", "#F6B8FF") # Black and pink
ggplot(data = plot_df[plot_df$group == "seedling" | 
                        plot_df$group == "mp" |
                        plot_df$group == "sc", ], 
       aes(x = log2(FPKM), 
           y = 100 * percent_transmission, 
           fill = factor(glm_adj_p_sig))) +
  geom_hline(yintercept = 50, size = 1, linetype = "dashed") +
  geom_line(aes(group = allele), color = "black", size = 0.8) +
  geom_point(aes(shape = allele_category), size = 3.5, stroke = 1, color = 'black') +
  scale_shape_manual(values = c(21, 24, 23)) +
  scale_y_continuous(breaks = c(20, 25, 30, 35, 40, 45, 50, 55, 60),
                     labels = c("20", "25", "30", "35", "40", "45", "50", "55", "60"),
                     limits = c(20, 60)) +
  scale_x_continuous(breaks = seq(5, 15, by = 2.5),
                     labels = as.character(seq(5, 15, by = 2.5)),
                     limits = c(4.5, 15)) +
  labs(title = "Male transmission",
       x = "log2(FPKM)",
       y = "% GFP transmission") +
  scale_fill_manual(values = color_vec,
                    name = "Significance",
                    breaks = c("0", "1"),
                    labels = c(" p > 0.05", " p < 0.05")) +
  theme_bw() +
  theme(plot.title = element_text(size = 32, face = "bold", margin = margin(0, 0, 12, 0)),
        axis.title = element_text(size = 22, face = "bold"),
        axis.text = element_text(size = 18, face = "bold"),
        axis.title.x = element_text(margin = margin(10, 0, 0, 0)),
        axis.title.y = element_text(margin = margin(0, 10, 0, 0)),
        axis.line = element_line(size = 0, color = "#4D4D4D"),
        axis.ticks = element_line(size = 1, color = "#4D4D4D"),
        axis.ticks.length = unit(4, "pt"),
        plot.margin = margin(0.1, 0.1, 0.1, 0.1, "in"),
        panel.border = element_rect(color = "#4D4D4D", size = 2, fill = NA),
        panel.grid.major.y = element_line(size = 1),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        legend.position = "none")

ggsave(filename = './plots/larger_transcriptome_set_males.png',
       device = 'png',
       width = 12,
       height = 6,
       dpi = 400,
       units = 'in')


###### Creating Supplemental Table 1 ######
# Here we organize the Tensorflow model predictions for output as Supplemental Table 1
# 2018:
export_2018 <- tensorflow_counts[tensorflow_counts$year == '2018' |
                                   tensorflow_counts$year == '2018_additional_alleles', ]
export_2018$PCR_confirmed <- "TRUE"
no_pcr_2018 <- c("416", "428", "436", "460", "473", "476", "477", "528")
export_2018$PCR_confirmed[export_2018$family %in% no_pcr_2018] <- "FALSE"
export_2018$allele <- paste('tdsg', export_2018$allele, sep = '')
write.table(export_2018, file = "./Data/table_S1_2018_export.tsv", row.names = F, quote = F, sep = '\t')

# 2019:
export_2019 <- tensorflow_counts[tensorflow_counts$year == '2019' |
                                   tensorflow_counts$year == '2019_additional_alleles', ]
export_2019$PCR_confirmed <- "TRUE"
no_pcr_2019 <- c("200", "201", "202", "203", "204", "205", "206", "15")
export_2019$PCR_confirmed[export_2019$family %in% no_pcr_2019] <- "FALSE"
export_2019$allele <- paste('tdsg', export_2019$allele, sep = '')
write.table(export_2019, file = "./Data/table_S1_2019_export.tsv", row.names = F, quote = F, sep = '\t')

# Summary
export_summary <- plot_df[plot_df$category == "tensorflow_counts", ]
export_summary <- export_summary[ , -c(4, 9)]
export_summary$allele <- paste('tdsg', export_summary$allele, sep = '')
PCR_status_key <- unique(rbind(export_2018[ , c(10, 12)], export_2019[ , c(10, 12)]))
export_summary <- left_join(export_summary, PCR_status_key)
write.table(export_summary, file = "./Data/table_S1_summary_export.tsv", row.names = F, quote = F, sep = '\t')




