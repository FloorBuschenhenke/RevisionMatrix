library(tidyverse)
library(rio)
library(XML)
#require(lubridate)

###------------ LOAD LOG DATA ----------------------------------------------####
files_ga <- list.files(path = "data_in/",
                          pattern = "*GA.xml", recursive = TRUE, full.names = TRUE)



# Function to load General XML files and combine them into one dataframe
load_logs <- function(filenames) {
  do.call(bind_rows, lapply(filenames, function(filename) {
    print(filename)
    data <- xmlParse(filename)
    events <- xmlToDataFrame(data, nodes = getNodeSet(data, "//session/event" ), 
                             stringsAsFactors = FALSE)
    
    ## add revision analysis (if exported from inputlog)
    if("RevisionInfo" %in% names(events)){
      revisions <-  xmlToDataFrame(data, 
                                   nodes = getNodeSet(data,
                                                      "//session/event/RevisionInfo" ), 
                                   stringsAsFactors = FALSE)
      events <- cbind(events, revisions) %>%
        select(-RevisionInfo)
    }else{
    }
    
    sessioninfo <- as.data.frame(
      do.call(rbind, xpathApply(data, "//entry", xmlAttrs)),
      stringsAsFactors = FALSE) %>%
      spread("name", "value")
    
    # rename variable names for different versions of Inputlog
    # 1. all variable names lower case & remove whitespacing surronding names
    names(sessioninfo) <- tolower(names(sessioninfo))
    names(sessioninfo) <- trimws(names(sessioninfo))
    # 2. all variable names to English
    if("taak" %in% names(sessioninfo)){
      sessioninfo <- sessioninfo %>% 
        rename(task = taak)
    }else{
    }
    
    all <- cbind(events, sessioninfo)  
  }))
}



logs <- load_logs(files_ga)
data_all <- bind_rows(logs)



### nietrelevante kolommen eruit halen
data_all_2 <- select(data_all, -age, -experience, -gender, -group, -'restricted logging', -session, -'text language')


### kolom logfile heeft bestandsnaam, hieruit alleen de IDnummer bewaren en als sessienummer-kolom opslaan
data_all_3 <- data_all_2%>%
  mutate(session_number = gsub(".*_(\\d{1,3}).*", "\\1", logfile))%>%
  select(-logfile,-analysiscreationdate)
  
#head(data_all_3)                                
                                
#names(data_all_3)

write.csv(data_all_3, "data/data_all_JeanMarie.csv")


