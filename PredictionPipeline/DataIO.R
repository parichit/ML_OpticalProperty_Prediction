require("caret")
require("readxl")
require("stringr")

load_data <- function(base_data_path){

  
read_data <- function(base_data_path){

  Inputdata <- read.csv2(base_data_path, sep="", stringsAsFactors = FALSE)
  
  Inputdata <- apply(Inputdata, 2, as.numeric)
  Inputdata <- as.data.frame(Inputdata)

  target = Inputdata[, 2]
  Inputdata <- Inputdata[, -ncol(Inputdata)]
  Inputdata <- cbind(Inputdata, target=target)
  
  # Select the valid range of eV and corresponding dos
  Inputdata <- Inputdata[Inputdata[, 1] <= 2, ]
  Inputdata <- Inputdata[Inputdata[, 1] >= -10, ]
  
  colnames(Inputdata) <- c("ev", "target")
  Inputdata <- as.data.frame(Inputdata)

  return(Inputdata)
}


Inputdata <- read_data(base_data_path)


# Create training and test data
set.seed(1)
train_indices <- createDataPartition(y = Inputdata$target, p = 0.80, list = FALSE)
training_data <- Inputdata[train_indices, ]
test_data <- Inputdata[-train_indices, ]

print("Data read-in successfully")
print(paste("Rows:", nrow(Inputdata), " Cols:", ncol(Inputdata)))

out = list("train_data" = training_data, "test_data" = test_data)

return(out)

}