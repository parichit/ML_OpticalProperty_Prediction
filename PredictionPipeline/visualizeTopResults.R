library(ggplot2)
library(dplyr)
library(gridExtra)
library(grid)

source("plotResults.R")

# Set plot label and size parameters
plot_title_size = 13
subplot_title_size = 10
axis_label_size = 11
axis_tick_size = 9
legend_size = 1
legend_title_color = "Black"


drawPlots <- function(trainData, testData, num_top_models, out_dir, out_file) {
  
  outFileName = ""
  outFilePath = file.path(out_dir, out_file)

  
  select_topN <- function(someData, num_top_models){
    
    temp = as.vector(unique(someData$Model)[1:num_top_models])
    someData <- someData[someData$Model %in% temp, ]
    return(someData)
  }
  
  # Process raw data
  process_data <- function(rawData, type_data, num_top_models){
    
    rawData$Model <- factor(rawData$Model)
    
    rawData$RMSE <- as.numeric(rawData$RMSE)
    rawData$Rsquared <- as.numeric(rawData$Rsquared)
    rawData$MAE <- as.numeric(rawData$MAE)
    
    DataRMSE <- rawData[, c(1, 4)]
    DataRSQ <- rawData[, c(2, 4)]
    DataMAE <- rawData[, c(3, 4)]
    
    DataRMSE <- DataRMSE %>% arrange(RMSE)
    DataRSQ <- DataRSQ %>% arrange(-Rsquared)
    DataMAE <- DataMAE %>% arrange(MAE)
    
    # Select top models for visualization
    if (type_data == "train"){
      DataRMSE <- select_topN(DataRMSE, num_top_models)
      DataRSQ <- select_topN(DataRSQ, num_top_models)
      DataMAE <- select_topN(DataMAE, num_top_models)
    }
    else if (type_data == "test"){
      DataRMSE <- DataRMSE[!duplicated(DataRMSE$Model), ]
      DataRMSE <- DataRMSE[1:num_top_models, ]
      
      DataRSQ <- DataRSQ[!duplicated(DataRSQ$Model), ]
      DataRSQ <- DataRSQ[1:num_top_models, ]
      
      DataMAE <- DataMAE[!duplicated(DataMAE$Model), ]
      DataMAE <- DataMAE[1:num_top_models, ]
    }
    return(list(DataRMSE, DataRSQ, DataMAE))
  }
  
  
  out <- process_data(trainData, "train", num_top_models)
  trainDataRMSE = out[[1]]
  trainDataRSQ = out[[2]]
  trainDataMAE = out[[3]]
  
  
  out <- process_data(testData, "test", num_top_models)
  testDataRMSE = out[[1]]
  testDataRSQ = out[[2]]
  testDataMAE = out[[3]]
  
  
  # source("plotResults.R")
  out <- plot_on_grid(trainDataRMSE, testDataRMSE, "(A) RMSE of regression models on validation data", 
                      "Boxplots for top models", "(D) Average error on test data", 
                      "RMSE values for top models", "",
                      "RMSE (Root mean squared error)", 1, subplot_title_size, axis_label_size, axis_tick_size)
  
  p1 <- out[[1]]
  p2 <- out[[2]]
  

  out <- plot_on_grid(trainDataMAE, testDataMAE, "(B) MAE of regression models on validation data", 
                      "Boxplots for top models", "(E) Average error on test data", 
                      "MAE values for top models", "",
                      "MAE", 1, subplot_title_size, axis_label_size, axis_tick_size)
  p3 <- out[[1]]
  p4 <- out[[2]]
  
  
  out <- plot_on_grid(trainDataRSQ, testDataRSQ, "(C) RSquared of regression models on validation data", 
                      "Boxplots for top models", "(F) Cor-relation between ground-truth and predictions measured via R-Squared", 
                      "RSquared values for top models", "Rsquared",
                      "RSquared", 1, subplot_title_size, axis_label_size, axis_tick_size)
  
  p5 <- out[[1]]
  p6 <- out[[2]]
  

  title = paste("Prediction performance of top models")
  grid_title <- textGrob(title, gp = gpar(fontsize = plot_title_size, fontface = 'bold'))
  
  FinalPlot <- grid.arrange(grid_title, arrangeGrob(p1, p2, p3, p4, nrow=2, ncol=2), nrow=2, heights=c(0.5, 10))
  
  # Save the plot on disk
  ggsave(file.path(outFilePath), FinalPlot, dpi=360, height=16,
         width=14, units="in")
  
}


# trainData <- read.csv2(file = "/Users/schmuck/Library/CloudStorage/OneDrive-IndianaUniversity/PhD/ComprehensiveSportsAnalytics/NBA_DEF/train_results.csv",
#                        stringsAsFactors = FALSE, sep=",")
# 
# testData <- read.csv2(file = "/Users/schmuck/Library/CloudStorage/OneDrive-IndianaUniversity/PhD/ComprehensiveSportsAnalytics/NBA_DEF/test_results.csv",
#                       stringsAsFactors = FALSE, sep=",")
# 
# out_dir = "/Users/schmuck/Library/CloudStorage/OneDrive-IndianaUniversity/PhD/ComprehensiveSportsAnalytics/NBA_DEF/"
# drawPlots(trainData, testData, 20, out_dir, "DEF.png")





















       
