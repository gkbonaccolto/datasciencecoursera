pollutantmean<- function(directory, pollutant, id = 1:332) {
  # "directory" is a charter vector of lenght 1 indicating the location of csv file
  directory<- c("rprog_data_specdata/specdata")
  
  # "pollutant" is a charter vector of length 1 indicating the name of the pollutant  for wich we 
  # calculate the mean; either  "sulfate" or "nitrate"
  # id is an integer vector indicating the monitor of ID number tpo be used
  
  # Return the mean of the pollutant across all the monitporing list
  # in the "id" vector (ignoring NA value)
  # do not round the result
  means<- c()
  
  for (monitor in id) {
    path<- paste(getwd(), "/", directory, "/", sprintf("%03d", monitor), ".csv", sep = "")
    monitor_data<- read.csv(path)
    interested_data<- monitor_data[pollutant]
    means<- c(means, interested_data[!is.na(interested_data)])
  }
  means(means)
}