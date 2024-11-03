# load packages
require(caret)
require(magrittr)
require(tidyverse)
require(caretEnsemble)
require(ggthemes)
require(ggplot2)
require(xtable)
require(tictoc)
require(R.utils)
require(dplyr)


runModels <- function(selected_Models, train_data, test_data, time_limit, number, repeats, num_top_models,
                      type_pred, output_path, train_out_file, test_out_file, stat_file, model_time_file_name){
  
  train_out = ""
  test_out = ""
  failCounter = 0;
  
  allresultsDF = data.frame()
  
  output_path = out_dir
  
  
  set.seed(6)
  control <- trainControl(method = "repeatedcv", 
                          number = number, 
                          repeats = repeats, 
                          savePredictions = "final",
                          index = createResample(train_data$target, number*repeats), 
                          allowParallel = FALSE)
  

    train_out = file.path(output_path, train_out_file)
    test_out = file.path(output_path, test_out_file)
    res_stats = file.path(output_path, stat_file)
  
  
  selected_Models = availableModels
  
  if (num_top_models != 0){
    selected_Models = selected_Models[(length(selected_Models)-num_top_models):length(selected_Models)]
  }
  
  
  
  for(i in 1:length(selected_Models))
  
    {
    
    print(paste("Running", selected_Models[i]))

        tic()
        result <- tryCatch(
        {
          
          withTimeout(
            {
              train(target~., data=train_data, method=selected_Models[i],
                    trControl = control)
            }, timeout = time_limit)
        },

        TimeoutException = function(ex) {
          result = "timeout"
          print(paste("Timeout for ", selected_Models[i]))
        },
        
        error = function(err){
          print(err)
          result =  "error"
        }
      )
 
    endTime <- toc()

    if (length(result) > 1){
      
      resultsDF <- as.data.frame(result$resample)
      resultsDF <- resultsDF[, -ncol(resultsDF)]
      resultsDF["Model"] = rep(selected_Models[i], nrow(resultsDF))
      
      allresultsDF <- rbind(allresultsDF, resultsDF)
      
      # Save the results on training data in file
      if (!file.exists(train_out)){
        write.table(resultsDF, file=train_out, row.names = FALSE, sep = "," , quote = FALSE)
      }
      else{
        write.table(resultsDF, file=train_out, row.names = FALSE, sep = "," , append = TRUE, quote = FALSE, 
                    col.names = FALSE)
      }
      
      # Predict the results on test data
      testPredictions <- predict(result, test_data)
      
      # Calculate the RMSE between predictions and actual test data
      resultsTestDF = data.frame(t(data.matrix(postResample(as.numeric(unlist(testPredictions)),  test_data$target))))

      
      resultsTestDF["Model"] = selected_Models[i]
      
      if (!file.exists(test_out)){
        write.table(resultsTestDF, file=test_out, row.names = FALSE, sep= ",", quote = FALSE)
      }
      else{
        write.table(resultsTestDF, file=test_out, row.names = FALSE, append = TRUE, sep = ",", quote = FALSE, 
                    col.names = FALSE)
      }
      
      
      TotalTime = endTime$toc = endTime$tic
      
      if (TotalTime > 0) {

        temp = data.frame(Model=selected_Models[i], Time1=TotalTime, Time2=(TotalTime/60))

            if (!file.exists(model_time_file_name)){
              write.table(temp, file=model_time_file_name, row.names = FALSE, sep= ",", quote = FALSE)
            }
            else{
              write.table(temp, file=model_time_file_name, row.names = FALSE, append = TRUE, sep = ",", quote = FALSE, 
                          col.names = FALSE)
            }
      }
      
      
      
    } else if (length(result) == 1){
      failCounter = failCounter + 1
    }

    # Record Time utilization of the model
    TotalTime = endTime$toc - endTime$tic
    if (length(result) > 1){
      comm = "success"
    } else if(length(result) == 1){
      comm = result
    }
    temp = data.frame(Model=selected_Models[i], Time1=TotalTime, Time2=(TotalTime/60), Status=comm)
    if (!file.exists(model_time_file_name)){
        write.table(temp, file=model_time_file_name, row.names = FALSE, sep= ",", quote = FALSE)
    } else{
      write.table(temp, file=model_time_file_name, row.names = FALSE, append = TRUE, sep = ",", quote = FALSE, 
                    col.names = FALSE)
      }

  }
  
  if (length(failCounter) == length(selected_Models)){
    print("None of the models completed successfully.")
  
  } else if(length(failCounter) < length(selected_Models)){
    
    
    # Save summary statistics for k-fold cross-validation 
    mae_stat <- as.data.frame(allresultsDF %>% dplyr::select(MAE, Model) %>% group_by(Model) %>%
                                summarize(min = min(MAE), max = max(MAE), median = median(MAE), 
                                          mean = mean(MAE), sd = sd(MAE)) %>%
                                arrange(-mean) %>% mutate_if(is.numeric, function(x) {round(x, 3)}))
    
    rmse_stat <- as.data.frame(allresultsDF %>% dplyr::select(RMSE, Model) %>% group_by(Model) %>%
                                 summarize(min = min(RMSE), max = max(RMSE), median = median(RMSE), 
                                           mean = mean(RMSE), sd = sd(RMSE)) %>% arrange(-mean) %>%
                                 mutate_if(is.numeric, function(x) {round(x, 3)}))
    
    rsquared_stat <- as.data.frame(allresultsDF %>% dplyr::select(Rsquared, Model) %>% group_by(Model) %>%
                                     summarize(min = min(Rsquared), max = max(Rsquared), median = median(Rsquared), 
                                               mean = mean(Rsquared), sd = sd(Rsquared)) %>% arrange(-mean) %>%
                                     mutate_if(is.numeric, function(x) {round(x, 3)}))
    
    
    mae_stat <- mae_stat[order(mae_stat$max, mae_stat$min, mae_stat$sd),]
    rmse_stat <- rmse_stat[order(rmse_stat$max, rmse_stat$min, rmse_stat$sd),]
    rsquared_stat <- rsquared_stat[order(-rsquared_stat$min, rsquared_stat$sd, 
                                         -rsquared_stat$max, -rsquared_stat$mean, 
                                         -rsquared_stat$median),]
    
    result_statistics <- cbind(rmse_stat, mae_stat, rsquared_stat)
    write.table(result_statistics, file=res_stats, row.names = FALSE, sep= ",", quote = FALSE)
  }
  
}

