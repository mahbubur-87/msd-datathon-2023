library(data.table)
library(ggplot2)
library(waffle)
library(GGally)
library(patchwork)
#library(tidyverse)
library(viridis)
library(fmsb)
library(grid)
library(gridExtra)

# baloon plot

setwd("/home/mahbubur/mahbub-dev/berliner_hochschule_for_technik_berlin/msd_datathon_2023/data")
df_failed_treatments_for_investigation <- read.csv("failed_treatments_2017-2020_investigation.csv")

# Research Question 1.1
# Identify what states are neglecting types of substance abuse programs or consistently having failed treatments.
top_5_states <- c('New Jersey, New York, Pennsylvania', 'California', 'New York', 'Michigan', 'Connecticut')
df_failed_treatments_states <- df_failed_treatments_for_investigation[df_failed_treatments_for_investigation$statename %in% top_5_states, ]

# Research Question 1.2
# Identify what regions are neglecting types of substance abuse programs or consistently having failed treatments.
top_3_regions <- c(1, 2, 4)
df_failed_treatments_regions <- df_failed_treatments_for_investigation[df_failed_treatments_for_investigation$REGION %in% top_3_regions, ]


# Research Question 1.3
# Identify what statistical areas are neglecting types of substance abuse programs or consistently having failed treatments.
top_5_statistical_areas <- c(35620, 31080, 14460, 19820, 33460)
df_failed_treatments_areas <- df_failed_treatments_for_investigation[df_failed_treatments_for_investigation$CBSA %in% top_5_statistical_areas, ]

# PCA chart (Scree Plot and Cumulative Sum Plot)
## Function visualize_pca:
### Description:
#This function helps to visualize the scree plot and the cumulative proportion variable explained to select the number of principle components. 

### Arguments:
#title       : string, the title to explain briefly about the chart. 

#pca_obj     : principle component object that is returned by the "prcomp" function.

visualize_pca <- function(title, caption, pca_obj) {
  num_pc <- length(pca_obj$sdev)
  pca_df <- data.frame(PC=1:num_pc,
                       var_explained=(pca_obj$sdev)^2/sum((pca_obj$sdev)^2))
  pca_df$cumsum_var_explained <- cumsum(pca_df$var_explained)
  
  scree_plot <- ggplot(pca_df) + 
    labs(title = title,
         caption = caption[1]) + 
    aes(x = PC, y = var_explained, group=1) +
    geom_point(size = 4) +
    ylab("Prop. Variance Explained") + 
    geom_line() + 
    theme(
      plot.caption = element_text(hjust = 0.5, size = 12, face = "bold")
    )
  
  cumsum_plot <- ggplot(pca_df) + 
    labs(caption = caption[2]) +
    aes(x = PC, y = cumsum_var_explained, group=1) +
    geom_point(size = 4) +
    ylab("Cumulative Prop. Variance Explained") + 
    geom_line() + 
    theme(
      plot.caption = element_text(hjust = 0.5, size = 12, face = "bold")
    )
  
  scree_plot + cumsum_plot
}

## Function visualize_influencial_features_on_pca:
### Description:
#This function helps to see which explanatory variables have greater influence on the main principle components. These main principle components are chosen from the previous the scree plot and the cumulative proportion variable explained chart. Also another purpose is to see the cluster formation among the explanatory variables to measures which explanatory variables are close and correlated to each others.

### Arguments:
#title       : string, the title to explain briefly about the chart.

#pca_obj     : principle component object that is returned by the "prcomp" function.

#num_pc      : integer, the number of the main principle components.

# features that contribute to the classification
# explanatory variables that are influencial and correlated to each other
visualize_influencial_features_on_pca <- function(title, caption, pca_obj, num_pc) {
  df_pca_loadings <- data.frame(pca_obj$rotation)
  df_pca_loadings$features <- row.names(df_pca_loadings)
  
  ggplot(df_pca_loadings) +
    labs(title = title,
         caption = caption) +
    aes(x = PC1, y = PC2,
        label = features, color = features ) + 
    geom_point() + 
    geom_text(size=3) + 
    theme(
      plot.caption = element_text(hjust = 0.5, size = 12, face = "bold")
    )
}

## Function visualize_pca_analysis:
### Description:
#This function helps to see the principle components on whole data set by group wise, so that it can easily visualize the dependency of different groups on the main principle components.

### Arguments:
#title       : string, the title to explain briefly about the chart. 

#pca_obj     : principle component object that is returned by the "prcomp" function.
visualize_pca_analysis <- function(title, caption, pca_obj, group) {
  df_pca_analysis <- data.frame(pca_obj$x)
  df_pca_analysis$group <- group
  
  # plot PC1 and PC2 values by group
  ggplot(df_pca_analysis) +
    labs(title = title,
         caption = caption) +
    aes(x = PC1, y = PC2, color = group) +
    geom_point() + 
    theme(
      plot.caption = element_text(hjust = 0.5, size = 12, face = "bold")
    )
}

# Research Question 1.4
# Can you demonstrate a data driven understanding of why this is occurring for top 5 states
pca_result_states <- prcomp(df_failed_treatments_states[-1], scale.=TRUE)
summary(pca_result_states)

visualize_pca(title = 'PCA for Top 5 States Failed Substance Abuse Treatments', 
              caption = c("Figure 1.4.1: Scree Plot",
                          "Figure 1.4.2: Cumulative Sum Plot"), 
              pca_obj = pca_result_states)

visualize_influencial_features_on_pca(title = "Influencial Features on PCA", 
                                      caption = "Figure 1.4.3: Score Plot PC1 vs PC2",
                                      pca_obj = pca_result_states, num_pc = 4)

visualize_pca_analysis(title = "PC Analysis",
                       caption = "Figure 1.4.4: PC Score Plot: PC1 vs PC2: data points by States",
                       pca_obj = pca_result_states,
                       group = df_failed_treatments_states$statename)

# Research Question 1.5
# Can you demonstrate a data driven understanding of why this is occurring for top 3 regions
pca_result_regions <- prcomp(df_failed_treatments_regions[-1], scale.=TRUE)
summary(pca_result_regions)

visualize_pca(title = 'PCA for Top 3 Regions Failed Substance Abuse Treatments', 
              caption = c("Figure 1.5.1: Scree Plot",
                          "Figure 1.5.2: Cumulative Sum Plot"), 
              pca_obj = pca_result_regions)

visualize_influencial_features_on_pca(title = "Influencial Features on PCA", 
                                      caption = "Figure 1.5.3: Score Plot PC1 vs PC2",
                                      pca_obj = pca_result_regions, num_pc = 4)

visualize_pca_analysis(title = "PC Analysis",
                       caption = "Figure 1.5.4: PC Score Plot: PC1 vs PC2: data points by Regions",
                       pca_obj = pca_result_regions,
                       group = df_failed_treatments_regions$REGION)

# Research Question 1.6
# Can you demonstrate a data driven understanding of why this is occurring for top 5 statistical areas
pca_result_areas <- prcomp(df_failed_treatments_areas[-1], scale.=TRUE)
summary(pca_result_areas)

visualize_pca(title = 'PCA for Top 5 Statistical Areas Failed Substance Abuse Treatments', 
              caption = c("Figure 1.6.1: Scree Plot",
                          "Figure 1.6.2: Cumulative Sum Plot"), 
              pca_obj = pca_result_areas)

visualize_influencial_features_on_pca(title = "Influencial Features on PCA", 
                                      caption = "Figure 1.6.3: Score Plot PC1 vs PC2",
                                      pca_obj = pca_result_areas, num_pc = 4)

visualize_pca_analysis(title = "PC Analysis",
                       caption = "Figure 1.6.4: PC Score Plot: PC1 vs PC2: data points by Statistical Areas",
                       pca_obj = pca_result_areas,
                       group = df_failed_treatments_areas$CBSA)


