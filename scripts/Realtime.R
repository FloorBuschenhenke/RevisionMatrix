### session id 1 ###


# adding a column to revision matrix with 'real' start and endtimes bases on GA
library(tidyverse)
library(lubridate)
# first set start and endtimes to 0 at the start of the session
matrix1 <- read.csv("data/revisionmatrix.csv")
names(matrix1)

matrix2 <- matrix1 %>%
  filter(session_number == "1")%>%
  mutate(starttijdschoon = start_time - first(start_time),
          eindtijdschoon = (end_time - start_time) + starttijdschoon)

#view(matrix2)



# add real session start clock time and date 
# change for each session! 
matrix3 <- matrix2 %>%
  mutate(startdatumtijd = ymd_hms("2020-08-20 12:04:13"))

#view(matrix3)

# convert event start and endtimes to datetime format 

matrix4 <- matrix3 %>%
  mutate(starttijdklok1 = seconds_to_period(starttijdschoon/1000))
  
#view(matrix4)

matrix5 <- matrix4 %>%
  mutate(starttijdklok2 = startdatumtijd + starttijdklok1)

#view(matrix5)

### format aanpassen (platte string van maken om seq-element in TEI te matchen)

matrix6 <- matrix5 %>%
  mutate(seq_element_starttime1 = as.character(starttijdklok2))

matrix6$seq_element_starttime2 <- gsub('[-: ]', '', matrix6$seq_element_starttime1)

#view(matrix6)
names(matrix6)
#clean, remove helper columns
matrix7 <- matrix6 %>%
  select(-startdatumtijd, -starttijdklok1, -starttijdklok2, -seq_element_starttime1)%>%
  rename (seq_element = seq_element_starttime2)

view(matrix7)

write.csv(matrix7, "data/meijen_revmatrix_sessionid_1.csv", row.names = FALSE)







