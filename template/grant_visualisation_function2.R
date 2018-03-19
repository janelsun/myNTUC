library(mongolite)
library(readxl)
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

# Get Grants
connect_grants <- function(){
  con <- mongo("grants", url=paste("mongodb://", HOST, ":", PORT, "/", DB_NAME, sep=""))
  con
}

#preprocess grants dataset
grant_visualisation_function2 <- function() {
  data_grants <- connect_grants()$find()
  data_grants$`Status (0-Not successful, 1-Successful, 2-Withdrawn, 3-Not Eligible, 4-Pending/Unknown, 5-Rejected, )`[data_grants$`Status (0-Not successful, 1-Successful, 2-Withdrawn, 3-Not Eligible, 4-Pending/Unknown, 5-Rejected, )` >1] <- 0
  data_grants[,16] <- NULL #remove the empty column, not sure why its there in the first place
  
  #preprocess 50k dataset
  data_50k <- connect_50k()$find()
  data_50k$Year <- as.Date(data_50k$Year, format="%d/%m/%Y")
  data_50k$Year <- format(as.Date(as.Date(data_50k$Year, format="%Y/%m/%d")),"%Y") #format the Year column to only show Year
  
  #obtain staff listing dataset
  data_staff <- connect_staff()$find()
  #keep unique values if there are duplciates
  data_staff <- unique(data_staff)
  output_front <- data.frame()
  
  researcherNames <- as.data.frame(data_staff$NUS_Researcher) #get all names of nus researcher via staff listing
  faculties <- as.data.frame(data_staff$Faculty) #get all faculty
  for (i in 1:nrow(researcherNames)) {
    researcherName <- researcherNames[i,]
    faculty <- faculties[i,]
    data_50k_filtered <- data_50k[(data_50k$`NUS Researcher` == researcherName) & (data_50k$Faculty == faculty),]    #filter the data by researcher
    row.names(data_50k_filtered) <- NULL
    data_grants_filtered  <- data_grants[(data_grants$`PI Name` == researcherName),] #filter grants data by researcher
    researcherName <- as.data.frame(researcherName)
    names(researcherName) <- "NUS_Researcher"
    if (nrow(data_50k_filtered) > 0) {
      year_freq <- as.data.frame(table(data_50k_filtered$Year)) #use number of year popping up as the number of publications in a year
      if (nrow(data_grants_filtered) > 0) {
        data_grants_filtered <- data_grants_filtered[order(-data_grants_filtered$Year, -(data_grants_filtered$`Status (0-Not successful, 1-Successful, 2-Withdrawn, 3-Not Eligible, 4-Pending/Unknown, 5-Rejected, )`)),] #sort by year in ascending order and success/failure in descending order
        grant_first_application_year <- data_grants_filtered$Year[1] #Year of first application
        grant_status <- data_grants_filtered$`Status (0-Not successful, 1-Successful, 2-Withdrawn, 3-Not Eligible, 4-Pending/Unknown, 5-Rejected, )`[1] #success or failure for the researcher 
        grant_status <- as.data.frame(grant_status)
        names(grant_status) <- "status"
      } else { #these researchers have not applied for grants before
        grant_status <- data.frame()
        grant_status[1,1] <- -1
        names(grant_status) <- "status"
        grant_first_application_year <- 0
      }
      if (nrow(year_freq) > 0) {
        names(year_freq) <- c("year", "number")
        year_data <- as.data.frame(as.character(year_freq$year[1]), stringsAsFactors = FALSE)  #this value to be used as "startYear"
        names(year_data) <- "startYear"
        first_publication_year <- as.integer(year_data$startYear[1])
        if (grant_first_application_year > 0) {
          before_years <- grant_first_application_year - first_publication_year #number of years before obtaining grant
          current_year <- as.integer(format(Sys.Date(), "%Y")) #get current year
          after_years <- current_year - grant_first_application_year #number of years after obtaining grant
          before_pubs <- nrow(data_50k_filtered[(data_50k_filtered$Year < grant_first_application_year),]) #number of publications before obtaining first grant
          after_pubs <- nrow(data_50k_filtered[(data_50k_filtered$Year > grant_first_application_year),]) #number of publications after obtaining first grant
          if (before_years == 0 || after_years == 0) {
            improvement <- 0
          } else {
            improvement <- (after_pubs/after_years - before_pubs/before_years)
          }
          improvement <- as.data.frame(improvement)
          names(improvement) <- "improvement"
        } else {
          improvement <- 0
          improvement <- as.data.frame(improvement)
          names(improvement) <- "improvement"
        }
      } else {
        year_data <- data.frame()
        year_data[1,1] <- "Not available"
        names(year_data) <- "startYear"
      }
    } else {
      year_data <- data.frame()
      year_data[1,1] <- "Not available"
      names(year_data) <- "startYear"
      grant_status <- data.frame()
      grant_status[1,1] <- -1
      names(grant_status) <- "status"
      improvement <- 0
      improvement <- as.data.frame(improvement)
      names(improvement) <- "improvement"
    }
    first_half <- cbind(grant_status, year_data, improvement)
    output_front <- rbind(output_front, first_half)
  }
  
  data_staff <- cbind(data_staff, output_front)
  data_staff <- data_staff[!(data_staff$status == -1),] #only keep those who have applied for grants before
  data_staff$startYear <- as.numeric(as.character(data_staff$startYear))
  faculties <- as.data.frame(table(data_staff$Faculty), stringsAsFactors = FALSE) #get remaining faculties out
  names(faculties)[1] <- "faculties"
  headers <- c("ARTS & SOC SC", "BUSINESS", "COMPUTING", "DENTISTRY", "DESIGN & ENVIRONMENT",
               "ENGINEERING", "LAW", "SCIENCE", "SSHSPH", "USP", "YALE-NUS COLLEGE",
               "YONG LOO LIN SCH OF MEDICINE", "YONG SIEW TOH CONSV OF MUSIC")
  final_output <- as.data.frame(matrix(,ncol=13,nrow=1))
  names(final_output) <- headers
  
  for (i in 1:nrow(faculties)) {
    faculty = faculties$faculties[i]
    holder <- data.frame() #to hold the data for each faculty
    data_staff_filtered <- data_staff[(data_staff$Faculty == faculty),] #filter by faculty
    for (j in 1:nrow(data_staff_filtered)) {
      name <- data_staff_filtered$NUS_Researcher[j] #get the researcher name
      title <- data_staff_filtered$`Job Title`[j] #get the researcher's title
      status <- data_staff_filtered$status[j] #get the researcher's application status
      startYear <- data_staff_filtered$startYear[j] #get the researcher's first publication year
      improvement <- data_staff_filtered$improvement[j] #get the researcher's improvement
      h_index <- data_staff_filtered$`H-index`[j]
      entity <- data.frame()
      entity[j,1] <- name
      entity[j,2] <- title
      entity[j,3] <- status
      entity[j,4] <- startYear
      entity[j,5] <- improvement
      entity[j,6] <- h_index
      names(entity) <- c("name", "title", "status", "startYear", "improvement", "h_index")
      holder <- rbind(holder, entity)
    }
    holder <- na.omit(holder)
    row.names(holder) <- NULL
    final_output[[faculty]] <- list(holder)
  }
  final_output <- toJSON(final_output, pretty=TRUE) #to json format
  return(final_output)
}
