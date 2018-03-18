library(mongolite)
library(jsonlite)

HOST = "localhost"
PORT = 27017
DB_NAME = "ODPRT"

# Get 50k
connect_50k <- function(){
  con <- mongo("publications", url=paste("mongodb://", HOST, ":", PORT, "/", DB_NAME, sep=""))
  con
}

# Get Staff Listing
connect_staff <- function(){
  con <- mongo("staff", url=paste("mongodb://", HOST, ":", PORT, "/", DB_NAME, sep=""))
  con
}

grant_visualisation_function0 <- function(researcherName){
  options(stringsAsFactors = FALSE)
  pub_qry <- paste0('{ "NUS Researcher" : "', researcherName,'"}')
  data_50k_filtered <- connect_50k()$find(pub_qry) #filter by researcher name
  staff_qry <- paste0('{ "NUS_Researcher" : "', researcherName,'"}')
  data_staff_filtered <- connect_staff()$find(staff_qry) #filter by researcher name
  total_numPub <- nrow(data_50k_filtered)
  
  
  total_numCitation <- sum(data_50k_filtered$Citation)
  
  # take 1st row; leave the rest (duplicates)
  data_staff_filtered = data_staff_filtered[1,]
  
  h_index <- data_staff_filtered$`H-index`
  job_title <- data_staff_filtered$`Job Title`
  faculty <- data_staff_filtered$Faculty
  department <- data_staff_filtered$Department
  output <- t(c(researcherName, job_title, faculty, department, h_index, total_numPub, total_numCitation))
  output <- as.data.frame(output, stringasfactors = FALSE)
  names(output) <- c("name", "title", "faculty", "department", "hindex", "totalDocument", "totalCitation")
  output <- list(output)
  final_output <- data.frame()
  final_output[1,1] <- list(output)
  
  names(final_output) <- c(researcherName)
  final_output <- toJSON(final_output, pretty=TRUE)
  print(data_staff_filtered)
  print(list(output))
  print(final_output)
  return(final_output)
}