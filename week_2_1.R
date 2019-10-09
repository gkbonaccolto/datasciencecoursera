pollutantmean<- function(directory, pollutant, id = 1:332) {
  # "directory" is a charter vector of lenght 1 indicating the location of csv file
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
  mean(means)
}
complete<- function(directory, id = 1:332) {
  results<- data.frame(id=numeric(0), nobs=numeric(0))
  for (monitor in id) {
      path<- paste(getwd(), "/", directory, "/", sprintf("%03d", monitor), ".csv", sep = "")
      monitor_data<- read.csv(path)
      interested_data<- monitor_data[(!is.na(monitor_data$sulfate)), ]
      interested_data<- interested_data[(!is.na(interested_data$nitrate)), ]
      nobs<- nrow(interested_data)
      results<- rbind(results, data.frame(id=monitor, nobs= nobs))
  }
  results
}
corr<- function(directory, threshold=0){
    # "threshold is a numerici vector of lenght 1 indicating the number of completely observed observation (on all variables)
    # required  to compute the correlation beteen nit and sul; the default value is 0
    cor_results<- numeric(0)
    complete_cases<- complete(directory)
    complete_cases<- complete_cases[complete_cases$nobs>= threshold, ]
      if(nrow(complete_cases>0)){
          for(monitor in complete_cases$id){
        path<- paste(getwd(), "/", directory, "/", sprintf("%03d", monitor), ".csv", sep = "")
        monitor_data<- read.csv(path)
        interested_data<- monitor_data[(!is.na(monitor_data$sulfate)), ]
        interested_data<- interested_data[(!is.na(interested_data$nitrate)), ]
        sulfate_data<- interested_data["sulfate"]
        nitrate_data<- interested_data["nitrate"]
        cor_results<-c(cor_results, cor(sulfate_data, nitrate_data))
        }
}
cor_result
}