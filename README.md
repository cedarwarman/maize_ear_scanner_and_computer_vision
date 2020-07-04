# Maize scanner and computer vision paper R methods and plotting
Cedar Warman

## Overview
This git repository contains the code (organized in an RStudio project) used to analyze the data produced in the study "A cost-effective maize ear phenotyping platform enables rapid categorization and quantification of kernels", as well as to generate plots for the majority of the figures.

## How to run
First, open the R project (.Rproj file). This project contains R scripts that cover the various analyses performed in the paper. R scripts can be run in any order.

## Contents
### confidence_threshold_tests.R
This script contains code used to determine the optimal confidence thresholds for computer vision models. 

### deep_learning_individual_models_test_set.R
This script examines the accuracy of deep learning models trained individually on two field seasons.

### deep_learning_model_larger_test_set.R
This script looks at a broad application of the deep learning models described in the paper.

### deep_learning_single_model_test_set.R
This script examines the accuracy of a deep learning model trained on two field seasons together.

### imagej_hand_counts.R
This script compares hand counts on the ear to manual annotations of ear images using ImageJ.

### traditional_cv_approach.R
This script examines the accuracy of a traditional computer vision approach for quantifying maize kernels.