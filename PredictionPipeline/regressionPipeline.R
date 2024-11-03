  path = getwd()
  setwd(file.path(path))
  base_path = dirname(path)


  # args = commandArgs(trailingOnly=TRUE)

  # if (length(args) < 4){
  #   print("Please mention which prediction you want to make?")
  #   print("Choices: real, imaginary")
  #   stop(exiting)
  # }

  # already_running = args[1]
  # result_dir_name = args[2]
  # input_file_name = args[3]
  # model_time_file_name = args[4]
  
  
  already_running = "no"
  result_dir_name = "test_results_123"
  input_file_name = "dos_total_pure_R8.csv"
  model_time_file_name = "model.csv"
  
  out_dir = file.path(path, result_dir_name)
  

  if ( (dir.exists(out_dir)) && (already_running == "no") ){
    time_stamp = format(Sys.time(), "%m_%d_%H-%m-%S")
    new_name = paste(out_dir, "_old_", time_stamp, sep="")
    file.rename(out_dir, new_name)
    dir.create(out_dir)
  } else if ( !(dir.exists(out_dir)) && (already_running == "no") ){
    dir.create(out_dir)
  }
  
  
  print(paste("Started execution on:", Sys.time()))
  
  
  print("###################################")
  print("1 Install packages")
  print("###################################")
  
  
  source("InstallMissingPackages.R")
  source("visualizeTopResults.R")
  source("visualizeAllResults.R")
  
  
  availableModels = install_missing_packages(file.path("modelList.txt"))
  
  
  print("###################################")
  print("2 READ/COMBINE DATA")
  print("###################################")

  source("DataIO.R")
  
  out <- load_data(file.path(path, "data", input_file_name))
  train_data <- out[[1]]
  test_data <- out[[2]]


  source("runModels.R")

  # Set parameters
  time_limit = 1000
  number <- 5
  repeats <- 3
  num_top_models = 3

  
  print("###################################")
  print("3 RUN MODELS FOR REGRESSION")
  print("###################################")
  
  train_out_file = "train_results.csv"
  test_out_file = "test_results.csv"
  stat_file = "Model_stat.csv"
  plot_file = "TopResults.png"
  
  trainFilePath = file.path(out_dir, train_out_file)
  testFilePath = file.path(out_dir, test_out_file)
  
  runModels(availableModels, train_data, test_data, time_limit, number, repeats, 
            num_top_models, typePred, out_dir, train_out_file, 
            test_out_file, stat_file, model_time_file_name)

  trainFilePath = file.path(out_dir, train_out_file)
  testFilePath = file.path(out_dir, test_out_file)
  
  if (file.exists(trainFilePath) && (file.exists(testFilePath))){
    
    trainData <- read.csv2(file = trainFilePath, stringsAsFactors = FALSE, sep=",")
    testData <- read.csv2(file = testFilePath, stringsAsFactors = FALSE, sep=",")
    
    # Plot top models
    drawPlots(trainData, testData, num_top_models, out_dir, plot_file)
    
    # Plot all models
    drawAllPlots(trainData, testData, out_dir, plot_file)
    
  } else{
    print("Results could not be located! Please check if the run was completed.")
  }


print(paste("Completed execution on:", Sys.time()))

