library(readr)

get_sector_info <- function(name, csv_data){
  # get the value of the second column for the given name
  demand = csv_data[csv_data$name == name, 3]
  capacity = csv_data[csv_data$name == name, 2]
  return(c(demand, capacity))
}

get_sector_color <- function(name, csv_data){
  demand_capacity = get_sector_info(name, csv_data)
  demand = demand_capacity[[1]]
  capacity = demand_capacity[[2]]
  
  if (demand > capacity){
    return("red")
  } else if (demand >= 0.85 * capacity){
    return("yellow")
  } else {
    return("blue")
  }
}