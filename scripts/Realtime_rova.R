


# adding a column to revision matrix with 'real' start and endtimes bases on GA

library(tidyverse)
library(lubridate)

matrix1 <- read.csv("data/revisionmatrix.csv")
#names(matrix1)


### session id 0 #### 
matrix2 <- matrix1 %>%
  filter(session_number == "0")%>%
  mutate(starttijdschoon = start_time - first(start_time),
          eindtijdschoon = (end_time - start_time) + starttijdschoon)

#view(matrix2)



# add real session start clock time and date 
# change for each session! 
matrix3 <- matrix2 %>%
  mutate(startdatumtijd = ymd_hms("2020-12-03 09:28:55"))

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
  relocate(seq_element, .before = start_id_GA)%>%
  relocate(start_position, .before = start_time_rel)%>%
  relocate(end_position, .after = start_position)

view(matrix7)

write.csv(matrix7, "data/rova_revmatrix_sessionid_0.csv", row.names = FALSE)


### session id 1 ####

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
  mutate(startdatumtijd = ymd_hms("2020-12-07 17:57:30"))

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

write.csv(matrix7, "data/rova_revmatrix_sessionid_1.csv", row.names = FALSE)





### session id 2 ####

# first set start and endtimes to 0 at the start of the session
matrix1 <- read.csv("data/revisionmatrix.csv")
names(matrix1)

matrix2 <- matrix1 %>%
  filter(session_number == "2")%>%
  mutate(starttijdschoon = start_time - first(start_time),
         eindtijdschoon = (end_time - start_time) + starttijdschoon)

#view(matrix2)



# add real session start clock time and date 
# change for each session! 
matrix3 <- matrix2 %>%
  mutate(startdatumtijd = ymd_hms("2020-12-08 11:21:56"))

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

write.csv(matrix7, "data/rova_revmatrix_sessionid_2.csv", row.names = FALSE)




### session id 3 ####

# first set start and endtimes to 0 at the start of the session
matrix1 <- read.csv("data/revisionmatrix.csv")
names(matrix1)

matrix2 <- matrix1 %>%
  filter(session_number == "3")%>%
  mutate(starttijdschoon = start_time - first(start_time),
         eindtijdschoon = (end_time - start_time) + starttijdschoon)

#view(matrix2)



# add real session start clock time and date 
# change for each session! 
matrix3 <- matrix2 %>%
  mutate(startdatumtijd = ymd_hms("2020-12-08 14:05:49"))

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

write.csv(matrix7, "data/rova_revmatrix_sessionid_3.csv", row.names = FALSE)

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
  mutate(startdatumtijd = ymd_hms("2020-12-09 12:29:38"))

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

write.csv(matrix7, "data/rova_revmatrix_sessionid_4.csv", row.names = FALSE)



### session id 5 ####

# first set start and endtimes to 0 at the start of the session
matrix1 <- read.csv("data/revisionmatrix.csv")
names(matrix1)

matrix2 <- matrix1 %>%
  filter(session_number == "5")%>%
  mutate(starttijdschoon = start_time - first(start_time),
         eindtijdschoon = (end_time - start_time) + starttijdschoon)

#view(matrix2)



# add real session start clock time and date 
# change for each session! 
matrix3 <- matrix2 %>%
  mutate(startdatumtijd = ymd_hms("2020-12-09 12:56:40"))

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

write.csv(matrix7, "data/rova_revmatrix_sessionid_5.csv", row.names = FALSE)


### session id 6 ####

# first set start and endtimes to 0 at the start of the session
matrix1 <- read.csv("data/revisionmatrix.csv")
names(matrix1)

matrix2 <- matrix1 %>%
  filter(session_number == "6")%>%
  mutate(starttijdschoon = start_time - first(start_time),
         eindtijdschoon = (end_time - start_time) + starttijdschoon)

#view(matrix2)



# add real session start clock time and date 
# change for each session! 
matrix3 <- matrix2 %>%
  mutate(startdatumtijd = ymd_hms("2020-12-09 20:20:25"))

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

write.csv(matrix7, "data/rova_revmatrix_sessionid_6.csv", row.names = FALSE)

### session id 7 ####

# first set start and endtimes to 0 at the start of the session
matrix1 <- read.csv("data/revisionmatrix.csv")
names(matrix1)

matrix2 <- matrix1 %>%
  filter(session_number == "6")%>%
  mutate(starttijdschoon = start_time - first(start_time),
         eindtijdschoon = (end_time - start_time) + starttijdschoon)

#view(matrix2)



# add real session start clock time and date 
# change for each session! 
matrix3 <- matrix2 %>%
  mutate(startdatumtijd = ymd_hms("2020-12-10 17:06:12"))

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

write.csv(matrix7, "data/rova_revmatrix_sessionid_7.csv", row.names = FALSE)
