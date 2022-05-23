


# adding a column to General Analysis files with 'real' start and endtimes bases on GA
## because sometimes the revision matrix (with returns eg) doesn't give the start time for each datapoint in the tei-xml


library(tidyverse)
library(lubridate)

matrix1 <- read.csv("data/data_all_JeanMarie.csv")
#names(matrix1)


### session id 1 #### 
matrix2 <- matrix1 %>%
  filter(session_number == "1")%>%
  mutate(starttijdschoon = startTime - first(startTime),
          eindtijdschoon = (endTime - startTime) + starttijdschoon)

#view(matrix2)



# add real session start clock time and date 
# change for each session! 
matrix3 <- matrix2 %>%
  mutate(startdatumtijd = ymd_hms("2020-08-04 12:04:13"))

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
#names(matrix6)
#clean, remove helper columns
matrix7 <- matrix6 %>%
  select(-startdatumtijd, -starttijdklok1, -starttijdklok2, -seq_element_starttime1)%>%
  rename (seq_element = seq_element_starttime2)%>%
  relocate(seq_element, .before = id)
  

view(matrix7)

write.csv(matrix7, "data/meijen_GA_sessionid_1.csv", row.names = FALSE)

### vanaf hier nog niet aangepast naar GA!####
### session id 4 ####

# first set start and endtimes to 0 at the start of the session
matrix1 <- read.csv("data/revisionmatrix.csv")
names(matrix1)

matrix2 <- matrix1 %>%
  filter(session_number == "4")%>%
  mutate(starttijdschoon = start_time - first(start_time),
         eindtijdschoon = (end_time - start_time) + starttijdschoon)

#view(matrix2)



# add real session start clock time and date 
# change for each session! 
matrix3 <- matrix2 %>%
  mutate(startdatumtijd = ymd_hms("2020-08-04 13:05:15"))

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
  rename (seq_element = seq_element_starttime2)%>%
  relocate(seq_element, .before = start_id_GA)%>%
  relocate(start_position, .before = start_time_rel)%>%
  relocate(end_position, .after = start_position)

#view(matrix7)

write.csv(matrix7, "data/meijen_revmatrix_sessionid_4.csv", row.names = FALSE)





### session id 7 ####

# first set start and endtimes to 0 at the start of the session
matrix1 <- read.csv("data/revisionmatrix.csv")
names(matrix1)

matrix2 <- matrix1 %>%
  filter(session_number == "7")%>%
  mutate(starttijdschoon = start_time - first(start_time),
         eindtijdschoon = (end_time - start_time) + starttijdschoon)

#view(matrix2)



# add real session start clock time and date 
# change for each session! 
matrix3 <- matrix2 %>%
  mutate(startdatumtijd = ymd_hms("2020-08-04 21:28:18"))

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
  rename (seq_element = seq_element_starttime2)%>%
  relocate(seq_element, .before = start_id_GA)%>%
  relocate(start_position, .before = start_time_rel)%>%
  relocate(end_position, .after = start_position)

#view(matrix7)

write.csv(matrix7, "data/meijen_revmatrix_sessionid_7.csv", row.names = FALSE)




### session id 8 ####

# first set start and endtimes to 0 at the start of the session
matrix1 <- read.csv("data/revisionmatrix.csv")
names(matrix1)

matrix2 <- matrix1 %>%
  filter(session_number == "8")%>%
  mutate(starttijdschoon = start_time - first(start_time),
         eindtijdschoon = (end_time - start_time) + starttijdschoon)

#view(matrix2)



# add real session start clock time and date 
# change for each session! 
matrix3 <- matrix2 %>%
  mutate(startdatumtijd = ymd_hms("2020-08-05 14:09:48"))

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
  rename (seq_element = seq_element_starttime2)%>%
  relocate(seq_element, .before = start_id_GA)%>%
  relocate(start_position, .before = start_time_rel)%>%
  relocate(end_position, .after = start_position)

#view(matrix7)

write.csv(matrix7, "data/meijen_revmatrix_sessionid_8.csv", row.names = FALSE)

### session id 10 ####

# first set start and endtimes to 0 at the start of the session
matrix1 <- read.csv("data/revisionmatrix.csv")
names(matrix1)

matrix2 <- matrix1 %>%
  filter(session_number == "10")%>%
  mutate(starttijdschoon = start_time - first(start_time),
         eindtijdschoon = (end_time - start_time) + starttijdschoon)

#view(matrix2)



# add real session start clock time and date 
# change for each session! 
matrix3 <- matrix2 %>%
  mutate(startdatumtijd = ymd_hms("2020-08-18 17:15:56"))

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
  rename (seq_element = seq_element_starttime2)%>%
  relocate(seq_element, .before = start_id_GA)%>%
  relocate(start_position, .before = start_time_rel)%>%
  relocate(end_position, .after = start_position)

#view(matrix7)

write.csv(matrix7, "data/meijen_revmatrix_sessionid_10.csv", row.names = FALSE)



### session id 11 ####

# first set start and endtimes to 0 at the start of the session
matrix1 <- read.csv("data/revisionmatrix.csv")
names(matrix1)

matrix2 <- matrix1 %>%
  filter(session_number == "11")%>%
  mutate(starttijdschoon = start_time - first(start_time),
         eindtijdschoon = (end_time - start_time) + starttijdschoon)

#view(matrix2)



# add real session start clock time and date 
# change for each session! 
matrix3 <- matrix2 %>%
  mutate(startdatumtijd = ymd_hms("2020-08-19 10:45:04"))

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
  rename (seq_element = seq_element_starttime2)%>%
  relocate(seq_element, .before = start_id_GA)%>%
  relocate(start_position, .before = start_time_rel)%>%
  relocate(end_position, .after = start_position)

#view(matrix7)

write.csv(matrix7, "data/meijen_revmatrix_sessionid_11.csv", row.names = FALSE)
