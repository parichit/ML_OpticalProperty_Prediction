library(ggplot2)
library(magrittr)
library(dplyr)
library(grid)
library(gridExtra)

source("plotAllResults.R")

# Set plot label and size parameters
plot_title_size = 12
subplot_title_size = 9
axis_label_size = 9
axis_tick_size = 6.5
legend_size = 1
legend_title_color = "Black"
typeData = "real"


drawAllPlots <- function(trainData, testData, out_dir, out_file) {
  

  # Process raw data
  process_data <- function(rawData){
    
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
      
        return(list(DataRMSE, DataRSQ, DataMAE))
  }
  
  
  out <- process_data(trainData)
  trainDataRMSE = out[[1]]
  trainDataRSQ = out[[2]]
  trainDataMAE = out[[3]]

  out <- process_data(testData)
  testDataRMSE = out[[1]]
  testDataRSQ = out[[2]]
  testDataMAE = out[[3]]
  
  # source("plotAllResults.R")
  out <- plot_individual_results(trainDataRMSE, testDataRMSE, "Spread of RMSE for all models on training data", 
                   "Prediction error on test data", "RMSE", 1, subplot_title_size, axis_tick_size)
  
  p1 <- out[[1]]
  p2 <- out[[2]]
  
  title = "Visualization of RMSE for all models"
  x_axis_label = "RMSE (Root Mean Squared Error)"
  y_axis_label = "Model"
  
  grid_title <- textGrob(title, gp = gpar(fontsize = plot_title_size, fontface = 'bold'))
  grid_x_label <- textGrob(y_axis_label, hjust = 0.5, vjust=-2.5, gp = gpar(fontsize = axis_label_size, fontface = 'bold'))
  grid_y_label <- textGrob(x_axis_label, rot=90, vjust = 1.2, gp = gpar(fontsize = axis_label_size, fontface = 'bold'))
  
  combined_plot <- grid.arrange(grid_title, arrangeGrob(p1, p2, nrow=2), nrow=2, left=grid_y_label, bottom=grid_x_label, heights=c(0.5, 10))
  

  ggsave(file.path(out_dir, "AllRMSE.png"), combined_plot, dpi=360, height=10,
                width=9, units="in")
  
  
  
  # out <- plot_individual_results(trainDataRSQ, testDataRSQ, "Spread of RSquared for all models on training data", 
  #                 "Prediction error on test data", "Rsquared", 1, subplot_title_size, axis_tick_size)
  # 
  # p1 <- out[[1]]
  # p2 <- out[[2]]
  # 
  # title = "Visualization of RSquared for all models"
  # x_axis_label = "Rsquared"
  # y_axis_label = "Model"
  # 
  # grid_title <- textGrob(title, gp = gpar(fontsize = plot_title_size, fontface = 'bold'))
  # grid_x_label <- textGrob(y_axis_label, hjust = 0.5, vjust=-2.5, gp = gpar(fontsize = axis_label_size, fontface = 'bold'))
  # grid_y_label <- textGrob(x_axis_label, rot=90, vjust = 1.2, gp = gpar(fontsize = axis_label_size, fontface = 'bold'))
  
  
  # combined_plot <- grid.arrange(grid_title, arrangeGrob(p1, p2, nrow=2), nrow=2, left=grid_y_label, bottom=grid_x_label, heights=c(0.5, 10))
  # 
  # ggsave(file.path(out_dir, "AllRSQ.png"), combined_plot, dpi=360, height=10,
  #        width=9, units="in")

  
  
  out <- plot_individual_results(trainDataMAE, testDataMAE, "Spread of MAE for all models on training data", 
                  "Prediction error on test data", "MAE", 1, subplot_title_size, axis_tick_size)
  
  p1 <- out[[1]]
  p2 <- out[[2]]
  
  title = "Visualization of MAE for all models"
  x_axis_label = "MAE"
  y_axis_label = "Model"
  
  grid_title <- textGrob(title, gp = gpar(fontsize = plot_title_size, fontface = 'bold'))
  grid_y_label <- textGrob(y_axis_label, rot = 90, vjust = 1.2, gp = gpar(fontsize = axis_label_size, fontface = 'bold'))
  grid_x_label <- textGrob(x_axis_label, hjust = 0.5, vjust=-0.5, gp = gpar(fontsize = axis_label_size, fontface = 'bold'))
  
  combined_plot <- grid.arrange(grid_title, arrangeGrob(p1, p2, nrow=2), nrow=2, left=grid_y_label, bottom=grid_x_label, heights=c(0.5, 10))
  
  ggsave(file.path(out_dir, "AllMAE.png"), combined_plot, dpi=360, height=10,
         width=9, units="in")
  

}



# trainData <- read.csv2(file = "/Users/schmuck/Library/CloudStorage/OneDrive-IndianaUniversity/PhD/ComprehensiveSportsAnalytics/NBA_DEF/train_results.csv",
#                        stringsAsFactors = FALSE, sep=",")
# 
# testData <- read.csv2(file = "/Users/schmuck/Library/CloudStorage/OneDrive-IndianaUniversity/PhD/ComprehensiveSportsAnalytics/NBA_DEF/test_results.csv",
#                       stringsAsFactors = FALSE, sep=",")
# 
# 
# out_dir = "/Users/schmuck/Library/CloudStorage/OneDrive-IndianaUniversity/PhD/ComprehensiveSportsAnalytics/NBA_DEF/"
# drawAllPlots(trainData, testData, out_dir, "DEF")








