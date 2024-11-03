suppressMessages(library("caretEnsemble"))

path = getwd()
setwd(file.path(path))
base_path = dirname(path)


run_ensemble <- function(train_data, number, repeats, best_models){
  
  set.seed(41259)
  control <- trainControl(method = "repeatedcv", 
                          number = number, 
                          repeats = repeats, 
                          savePredictions = "final",
                          index = createResample(train_data$target, number*repeats), 
                          allowParallel = FALSE)
  
  
  model_list <- caretList(target~., data=train_data, trControl=control, methodList=best_models)
  return(model_list)
}


# args = commandArgs(trailingOnly=TRUE)
# 
# wch_data = args[1]
# input_file_name = args[2]



# source("DataIO.R")
# 
# out <- load_data(file.path(base_path, "data", input_file_name))
# train_data <- out[[1]]
# test_data <- out[[2]]
# 
# 
# number <- 5
# repeats <- 5
# 
# total_data = rbind(train_data, test_data)


##################
# DEF ensemble
##################

if (wch_data == "DEF"){
  
  best_models = c("cubist")
  
  model_list <- run_ensemble(total_data, number, repeats, best_models)
  mdl_ensemble <- caretEnsemble(model_list)
  saveRDS(mdl_ensemble, "DEF_mdl.rds")
  
}




##################
#  MVP ensemble
##################

if (wch_mdl == "MVP"){
  
  best_models = c("cubist")
  
  model_list <- run_ensemble(total_data, number, repeats, best_models)
  mdl_ensemble <- caretEnsemble(model_list)
  saveRDS(mdl_ensemble, "MVP_mdl.rds")
  
}



##################
#  IMP ensemble
##################

if (wch_mdl == "IMP"){
  
  best_models = c("pcaNNet")
  
  model_list <- run_ensemble(total_data, number, repeats, best_models)
  mdl_ensemble <- caretEnsemble(model_list)
  saveRDS(mdl_ensemble, "IMP_mdl.rds")
}




##################
#  Rookie ensemble
##################


if (wch_data == "Rookie"){
  
  best_models = c("mlpML", "cubist")
  
  model_list <- run_ensemble(total_data, number, repeats, best_models)
  mdl_ensemble <- caretEnsemble(model_list)
  saveRDS(mdl_ensemble, "Rookie_mdl.rds")
  
}


# mdl <- readRDS("/Users/schmuck/Library/CloudStorage/OneDrive-IndianaUniversity/PhD/ComprehensiveSportsAnalytics/best_models/DEF_mdl.rds")
# res <- predict(mdl, test_data)



