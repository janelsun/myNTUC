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

# Get Grants
connect_grants <- function(){
  con <- mongo("grants", url=paste("mongodb://", HOST, ":", PORT, "/", DB_NAME, sep=""))
  con
}

grant_visualisation_function1 <- function(researcherName){
  pub_qry <- paste0('{ "NUS Researcher" : "', researcherName,'"}')
  #loadData("", pub_qry)
  data_50k_filtered <- connect_50k()$find(pub_qry) #filter by researcher name
  # remove duplicates
  data_50k_filtered <- data_50k_filtered[!duplicated(data_50k_filtered[c('EID')]),]
  if (nrow(data_50k_filtered) == 0) { #researcher cannot be found in the dataset
    output_front <- data.frame()
    output_front[1,1] <- "NULL"
    names(output_front) <- c(researcherName)
    final_output <- toJSON(output_front, pretty=TRUE)
  } else {
    data_50k_filtered$Year <- as.Date(data_50k_filtered$Year, format="%d/%m/%Y")
    data_50k_filtered$Year <- format(as.Date(as.Date(data_50k_filtered$Year, format="%Y/%m/%d")),"%Y") #format the Year column to only show Year
    
    year_freq <- as.data.frame(table(data_50k_filtered$Year)) #use number of year popping up as the number of publications in a year
    names(year_freq) <- c("year", "number")
    
    name <- c(researcherName)
    n <- max(length(name), length(year_freq[1]))
    length(name) <- n
    length(year_freq[1]) <- n
    year_data <- data.frame(year_freq$year)
    maxPub <- max(year_freq$number) #maximum number of publications in a year across all years
    maxPub <- as.data.frame(maxPub)
    names(maxPub) <- "maxPub"
    
    holder <- data.frame() #to hold all rows of lists of publications
    for (i in 1:nrow(year_freq)) {
      year <- year_freq$year[i]
      data_50k_filtered_year <- data_50k_filtered[data_50k_filtered$Year == year,] #filter data by year
      titles <- data_50k_filtered_year$Title #all titles for a particular year
      titles <- na.omit(titles)
      titles <- list(titles)
      numCitation <- data_50k_filtered_year$Citation #number of citations for each paper in a particular year
      numCitation <- na.omit(numCitation)
      numCitation <- list(numCitation)
      holder[i,1] <- list(titles) #append
      holder[i,2] <- list(numCitation) #append
    }
    #bind year and publication data and correctly label header
    holder <- as.data.frame(cbind(year_data, holder))
    names(holder) <- c("year", "pubList", "numCitation")
    
    grants_qry <- paste0('{ "PI Name" : "', researcherName,'"}')
    data_grants_filtered <- connect_grants()$find(grants_qry) #filter by researcher name
    if (nrow(data_grants_filtered) == 0) { #researcher cannot be found in the dataset
      output_front <- data.frame()
      output_front[1,1] <- "NULL"
      names(output_front) <- c(researcherName)
      final_output <- toJSON(output_front, pretty=TRUE)
    } else {
      data_grants_filtered$`Status (0-Not successful, 1-Successful, 2-Withdrawn, 3-Not Eligible, 4-Pending/Unknown, 5-Rejected, )`[data_grants_filtered$`Status (0-Not successful, 1-Successful, 2-Withdrawn, 3-Not Eligible, 4-Pending/Unknown, 5-Rejected, )` >1] <- 0
      data_grants_filtered[,16] <- NULL #remove the empty column, not sure why its there in the first place
      data_grants_filtered <- data_grants_filtered[order(-data_grants_filtered$Year, -(data_grants_filtered$`Status (0-Not successful, 1-Successful, 2-Withdrawn, 3-Not Eligible, 4-Pending/Unknown, 5-Rejected, )`)),] #sort by year in ascending order
      
      #only take first entry because data is sorted
      grant_year <- data_grants_filtered$Year[1] #year of application
      grant <- data_grants_filtered$`Status (0-Not successful, 1-Successful, 2-Withdrawn, 3-Not Eligible, 4-Pending/Unknown, 5-Rejected, )`[1] #success or failure
      grantor <- data_grants_filtered$Grantor #grantor of the grant
      programme <- data_grants_filtered$Programme #programme of the grant
      proposalTitle <- data_grants_filtered$`Proposal Title` #title of grant
      directCostApproved <- data_grants_filtered$`Direct Cost Approved` #directCostApproved
      grants <- cbind(grantor, programme, proposalTitle, directCostApproved, grant, grant_year) #bind everything grant application year
      
      #combine and convert to json format
      output_front <- data.frame()
      output_front <- cbind(year_freq[2], year_freq[1])
      output_front <- merge(data.frame(output_front, row.names=NULL), data.frame(maxPub, row.names=NULL), by = 0, all = TRUE)[-1]
      output_front <- merge(data.frame(output_front, row.names=NULL), data.frame(name, row.names=NULL), by = 0, all = TRUE)[-1]
      output_front <- merge(data.frame(output_front, row.names=NULL), data.frame(grants, row.names=NULL), by = 0, all = TRUE)[-1]
      output_front <- merge(data.frame(output_front), data.frame(holder), by = "year")
      output_front$year <- as.Date(as.character(output_front$year), "%Y")
      output_front$year <- format(as.Date(output_front$year, format="%Y/%m/%d"), "%Y")
      output_front$year <- as.integer(output_front$year)
      final_output <- data.frame()
      output_front <- list(output_front)
      final_output[1,1] <- list(output_front)
      names(final_output) <- c(researcherName)
      final_output <- toJSON(final_output, pretty=TRUE)
    }
    return (final_output)
  } 
}