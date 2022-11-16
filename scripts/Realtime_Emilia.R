
## deze is voor Emilia

# adding a column to revision matrix with 'real' start and endtimes bases on GA

library(tidyverse)
library(lubridate)

## TODO change this into a function (and enter session number, date and time)

matrix1 <- read.csv("data/revisionmatrix_Emilia.csv")
#names(matrix1)


### session id 1 #### 
matrix2 <- matrix1 %>%
  filter(session_number == "1")%>%
  mutate(starttijdschoon = start_time - first(start_time),
          eindtijdschoon = (end_time - start_time) + starttijdschoon)

#view(matrix2)



# add real session start clock time and date 
# change for each session! 
matrix3 <- matrix2 %>%
  mutate(startdatumtijd = ymd_hms("2020-11-23 13:39:52"))

# view(matrix3)

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

# view(matrix6)
#names(matrix6)
#clean, remove helper columns
matrix7 <- matrix6 %>%
  select(-startdatumtijd, -starttijdklok1, -starttijdklok2, -seq_element_starttime1)%>%
  rename (seq_element = seq_element_starttime2)%>%
  relocate(seq_element, .before = start_id_GA)%>%
  relocate(start_position, .before = start_time_rel)%>%
  relocate(end_position, .after = start_position)

view(matrix7)

write.csv(matrix7, "data/emilia_revmatrix_sessionid_1.csv", row.names = FALSE)


### session id 2 ####

# first set start and endtimes to 0 at the start of the session
matrix1 <- read.csv("data/revisionmatrix_Emilia.csv")
names(matrix1)

matrix2 <- matrix1 %>%
  filter(session_number == "2")%>%
  mutate(starttijdschoon = start_time - first(start_time),
         eindtijdschoon = (end_time - start_time) + starttijdschoon)

#view(matrix2)

# 25-11-20 14:57:47.113

# add real session start clock time and date 
# change for each session! 
matrix3 <- matrix2 %>%
  mutate(startdatumtijd = ymd_hms("2020-11-25 14:57:47"))

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

write.csv(matrix7, "data/emilia_revmatrix_sessionid_2.csv", row.names = FALSE)





### session id 3 emilia ####

# first set start and endtimes to 0 at the start of the session
matrix1 <- read.csv("data/revisionmatrix_Emilia.csv")
names(matrix1)

matrix2 <- matrix1 %>%
  filter(session_number == "3")%>%
  mutate(starttijdschoon = start_time - first(start_time),
         eindtijdschoon = (end_time - start_time) + starttijdschoon)

#view(matrix2)



# add real session start clock time and date 
# change for each session! 

matrix3 <- matrix2 %>%
  mutate(startdatumtijd = ymd_hms("2020-11-25 17:17:12"))

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

write.csv(matrix7, "data/emilia_revmatrix_sessionid_3.csv", row.names = FALSE)




### session id 4 ####

# first set start and endtimes to 0 at the start of the session
matrix1 <- read.csv("data/revisionmatrix_Emilia.csv")
names(matrix1)

matrix2 <- matrix1 %>%
  filter(session_number == "4")%>%
  mutate(starttijdschoon = start_time - first(start_time),
         eindtijdschoon = (end_time - start_time) + starttijdschoon)

#view(matrix2)



# add real session start clock time and date 
# change for each session! 

matrix3 <- matrix2 %>%
  mutate(startdatumtijd = ymd_hms("2020-11-30 16:30:29"))

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

write.csv(matrix7, "data/emilia_revmatrix_sessionid_4.csv", row.names = FALSE)





### session id 5 ####

# first set start and endtimes to 0 at the start of the session
matrix1 <- read.csv("data/revisionmatrix_Emilia.csv")
names(matrix1)


matrix2 <- matrix1 %>%
  filter(session_number == "5")%>%
  mutate(starttijdschoon = start_time - first(start_time),
         eindtijdschoon = (end_time - start_time) + starttijdschoon)

#view(matrix2)




# add real session start clock time and date 
# change for each session! 
matrix3 <- matrix2 %>%
  mutate(startdatumtijd = ymd_hms("2020-12-08 11:48:04"))

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

write.csv(matrix7, "data/emilia_revmatrix_sessionid_5.csv", row.names = FALSE)


### session id 6 ####

# first set start and endtimes to 0 at the start of the session
matrix1 <- read.csv("data/revisionmatrix_Emilia.csv")
names(matrix1)

matrix2 <- matrix1 %>%
  filter(session_number == "6")%>%
  mutate(starttijdschoon = start_time - first(start_time),
         eindtijdschoon = (end_time - start_time) + starttijdschoon)

#view(matrix2)


# add real session start clock time and date 
# change for each session! 
matrix3 <- matrix2 %>%
  mutate(startdatumtijd = ymd_hms("2020-12-08 13:44:24"))

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

write.csv(matrix7, "data/emilia_revmatrix_sessionid_6.csv", row.names = FALSE)



### session id 7 ####

# first set start and endtimes to 0 at the start of the session
matrix1 <- read.csv("data/revisionmatrix_Emilia.csv")
names(matrix1)

matrix2 <- matrix1 %>%
  filter(session_number == "7")%>%
  mutate(starttijdschoon = start_time - first(start_time),
         eindtijdschoon = (end_time - start_time) + starttijdschoon)

#view(matrix2)



# add real session start clock time and date 
# change for each session! 
matrix3 <- matrix2 %>%
  mutate(startdatumtijd = ymd_hms("2020-12-09 11:06:53"))

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
# names(matrix6)
#clean, remove helper columns
matrix7 <- matrix6 %>%
  select(-startdatumtijd, -starttijdklok1, -starttijdklok2, -seq_element_starttime1)%>%
  rename (seq_element = seq_element_starttime2)%>%
  relocate(seq_element, .before = start_id_GA)%>%
  relocate(start_position, .before = start_time_rel)%>%
  relocate(end_position, .after = start_position)

#view(matrix7)

write.csv(matrix7, "data/emilia_revmatrix_sessionid_7.csv", row.names = FALSE)



### session id 8 ####

# first set start and endtimes to 0 at the start of the session
matrix1 <- read.csv("data/revisionmatrix_Emilia.csv")
names(matrix1)

matrix2 <- matrix1 %>%
  filter(session_number == "8")%>%
  mutate(starttijdschoon = start_time - first(start_time),
         eindtijdschoon = (end_time - start_time) + starttijdschoon)

#view(matrix2)




# add real session start clock time and date 
# change for each session! 
matrix3 <- matrix2 %>%
  mutate(startdatumtijd = ymd_hms("2020-12-10 13:37:38"))

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
# names(matrix6)
#clean, remove helper columns
matrix7 <- matrix6 %>%
  select(-startdatumtijd, -starttijdklok1, -starttijdklok2, -seq_element_starttime1)%>%
  rename (seq_element = seq_element_starttime2)%>%
  relocate(seq_element, .before = start_id_GA)%>%
  relocate(start_position, .before = start_time_rel)%>%
  relocate(end_position, .after = start_position)

#view(matrix7)

write.csv(matrix7, "data/emilia_revmatrix_sessionid_8.csv", row.names = FALSE)



### session id 11 ####

# first set start and endtimes to 0 at the start of the session
matrix1 <- read.csv("data/revisionmatrix_Emilia.csv")
# names(matrix1)

matrix2 <- matrix1 %>%
  filter(session_number == "11")%>%
  mutate(starttijdschoon = start_time - first(start_time),
         eindtijdschoon = (end_time - start_time) + starttijdschoon)

#view(matrix2)




# add real session start clock time and date 
# change for each session! 
matrix3 <- matrix2 %>%
  mutate(startdatumtijd = ymd_hms("2020-12-11 10:07:33"))

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
# names(matrix6)
#clean, remove helper columns
matrix7 <- matrix6 %>%
  select(-startdatumtijd, -starttijdklok1, -starttijdklok2, -seq_element_starttime1)%>%
  rename (seq_element = seq_element_starttime2)%>%
  relocate(seq_element, .before = start_id_GA)%>%
  relocate(start_position, .before = start_time_rel)%>%
  relocate(end_position, .after = start_position)

#view(matrix7)

write.csv(matrix7, "data/emilia_revmatrix_sessionid_11.csv", row.names = FALSE)


### session id 12 ####

# first set start and endtimes to 0 at the start of the session
matrix1 <- read.csv("data/revisionmatrix_Emilia.csv")
# names(matrix1)

matrix2 <- matrix1 %>%
  filter(session_number == "12")%>%
  mutate(starttijdschoon = start_time - first(start_time),
         eindtijdschoon = (end_time - start_time) + starttijdschoon)

#view(matrix2)


# add real session start clock time and date 
# change for each session! 
matrix3 <- matrix2 %>%
  mutate(startdatumtijd = ymd_hms("2020-12-11 14:22:45"))

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
# names(matrix6)
#clean, remove helper columns
matrix7 <- matrix6 %>%
  select(-startdatumtijd, -starttijdklok1, -starttijdklok2, -seq_element_starttime1)%>%
  rename (seq_element = seq_element_starttime2)%>%
  relocate(seq_element, .before = start_id_GA)%>%
  relocate(start_position, .before = start_time_rel)%>%
  relocate(end_position, .after = start_position)

#view(matrix7)

write.csv(matrix7, "data/emilia_revmatrix_sessionid_12.csv", row.names = FALSE)


### session id 14 ####

# first set start and endtimes to 0 at the start of the session
matrix1 <- read.csv("data/revisionmatrix_Emilia.csv")
# names(matrix1)

matrix2 <- matrix1 %>%
  filter(session_number == "14")%>%
  mutate(starttijdschoon = start_time - first(start_time),
         eindtijdschoon = (end_time - start_time) + starttijdschoon)

#view(matrix2)



# add real session start clock time and date 
# change for each session! 
matrix3 <- matrix2 %>%
  mutate(startdatumtijd = ymd_hms("2020-12-15 13:33:16"))

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
# names(matrix6)
#clean, remove helper columns
matrix7 <- matrix6 %>%
  select(-startdatumtijd, -starttijdklok1, -starttijdklok2, -seq_element_starttime1)%>%
  rename (seq_element = seq_element_starttime2)%>%
  relocate(seq_element, .before = start_id_GA)%>%
  relocate(start_position, .before = start_time_rel)%>%
  relocate(end_position, .after = start_position)

#view(matrix7)

write.csv(matrix7, "data/emilia_revmatrix_sessionid_14.csv", row.names = FALSE)

### session id 15 ####

# first set start and endtimes to 0 at the start of the session
matrix1 <- read.csv("data/revisionmatrix_Emilia.csv")
# names(matrix1)

matrix2 <- matrix1 %>%
  filter(session_number == "15")%>%
  mutate(starttijdschoon = start_time - first(start_time),
         eindtijdschoon = (end_time - start_time) + starttijdschoon)

#view(matrix2)


# add real session start clock time and date 
# change for each session! 
matrix3 <- matrix2 %>%
  mutate(startdatumtijd = ymd_hms("2020-12-19 16:16:34"))

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
# names(matrix6)
#clean, remove helper columns
matrix7 <- matrix6 %>%
  select(-startdatumtijd, -starttijdklok1, -starttijdklok2, -seq_element_starttime1)%>%
  rename (seq_element = seq_element_starttime2)%>%
  relocate(seq_element, .before = start_id_GA)%>%
  relocate(start_position, .before = start_time_rel)%>%
  relocate(end_position, .after = start_position)

#view(matrix7)

write.csv(matrix7, "data/emilia_revmatrix_sessionid_15.csv", row.names = FALSE)

### session id 16 ####

# first set start and endtimes to 0 at the start of the session
matrix1 <- read.csv("data/revisionmatrix_Emilia.csv")
# names(matrix1)

matrix2 <- matrix1 %>%
  filter(session_number == "16")%>%
  mutate(starttijdschoon = start_time - first(start_time),
         eindtijdschoon = (end_time - start_time) + starttijdschoon)

#view(matrix2)


# add real session start clock time and date 
# change for each session! 
matrix3 <- matrix2 %>%
  mutate(startdatumtijd = ymd_hms("2020-12-23 16:43:06"))

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
# names(matrix6)
#clean, remove helper columns
matrix7 <- matrix6 %>%
  select(-startdatumtijd, -starttijdklok1, -starttijdklok2, -seq_element_starttime1)%>%
  rename (seq_element = seq_element_starttime2)%>%
  relocate(seq_element, .before = start_id_GA)%>%
  relocate(start_position, .before = start_time_rel)%>%
  relocate(end_position, .after = start_position)

#view(matrix7)

write.csv(matrix7, "data/emilia_revmatrix_sessionid_16.csv", row.names = FALSE)

### sessie 17 ####

# first set start and endtimes to 0 at the start of the session
matrix1 <- read.csv("data/revisionmatrix_Emilia.csv")
# names(matrix1)

matrix2 <- matrix1 %>%
  filter(session_number == "17")%>%
  mutate(starttijdschoon = start_time - first(start_time),
         eindtijdschoon = (end_time - start_time) + starttijdschoon)

#view(matrix2)



# add real session start clock time and date 
# change for each session! 
matrix3 <- matrix2 %>%
  mutate(startdatumtijd = ymd_hms("2020-12-31 10:42:27"))

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
# names(matrix6)
#clean, remove helper columns
matrix7 <- matrix6 %>%
  select(-startdatumtijd, -starttijdklok1, -starttijdklok2, -seq_element_starttime1)%>%
  rename (seq_element = seq_element_starttime2)%>%
  relocate(seq_element, .before = start_id_GA)%>%
  relocate(start_position, .before = start_time_rel)%>%
  relocate(end_position, .after = start_position)

#view(matrix7)

write.csv(matrix7, "data/emilia_revmatrix_sessionid_17.csv", row.names = FALSE)


### session id 18 ####

# first set start and endtimes to 0 at the start of the session
matrix1 <- read.csv("data/revisionmatrix_Emilia.csv")
# names(matrix1)

matrix2 <- matrix1 %>%
  filter(session_number == "18")%>%
  mutate(starttijdschoon = start_time - first(start_time),
         eindtijdschoon = (end_time - start_time) + starttijdschoon)

#view(matrix2)



# add real session start clock time and date 
# change for each session! 
matrix3 <- matrix2 %>%
  mutate(startdatumtijd = ymd_hms("2021-01-06 09:53:35"))

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
# names(matrix6)
#clean, remove helper columns
matrix7 <- matrix6 %>%
  select(-startdatumtijd, -starttijdklok1, -starttijdklok2, -seq_element_starttime1)%>%
  rename (seq_element = seq_element_starttime2)%>%
  relocate(seq_element, .before = start_id_GA)%>%
  relocate(start_position, .before = start_time_rel)%>%
  relocate(end_position, .after = start_position)

#view(matrix7)

write.csv(matrix7, "data/emilia_revmatrix_sessionid_18.csv", row.names = FALSE)


### session id 19 ####

# first set start and endtimes to 0 at the start of the session
matrix1 <- read.csv("data/revisionmatrix_Emilia.csv")
# names(matrix1)

matrix2 <- matrix1 %>%
  filter(session_number == "19")%>%
  mutate(starttijdschoon = start_time - first(start_time),
         eindtijdschoon = (end_time - start_time) + starttijdschoon)

#view(matrix2)



# add real session start clock time and date 
# change for each session! 
matrix3 <- matrix2 %>%
  mutate(startdatumtijd = ymd_hms("2021-01-06 15:29:02"))

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
# names(matrix6)
#clean, remove helper columns
matrix7 <- matrix6 %>%
  select(-startdatumtijd, -starttijdklok1, -starttijdklok2, -seq_element_starttime1)%>%
  rename (seq_element = seq_element_starttime2)%>%
  relocate(seq_element, .before = start_id_GA)%>%
  relocate(start_position, .before = start_time_rel)%>%
  relocate(end_position, .after = start_position)

#view(matrix7)

write.csv(matrix7, "data/emilia_revmatrix_sessionid_19.csv", row.names = FALSE)


### session id 20 ####

# first set start and endtimes to 0 at the start of the session
matrix1 <- read.csv("data/revisionmatrix_Emilia.csv")
# names(matrix1)

matrix2 <- matrix1 %>%
  filter(session_number == "20")%>%
  mutate(starttijdschoon = start_time - first(start_time),
         eindtijdschoon = (end_time - start_time) + starttijdschoon)

#view(matrix2)



# add real session start clock time and date 
# change for each session! 
matrix3 <- matrix2 %>%
  mutate(startdatumtijd = ymd_hms("2021-01-10 14:08:27"))

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
# names(matrix6)
#clean, remove helper columns
matrix7 <- matrix6 %>%
  select(-startdatumtijd, -starttijdklok1, -starttijdklok2, -seq_element_starttime1)%>%
  rename (seq_element = seq_element_starttime2)%>%
  relocate(seq_element, .before = start_id_GA)%>%
  relocate(start_position, .before = start_time_rel)%>%
  relocate(end_position, .after = start_position)

#view(matrix7)

write.csv(matrix7, "data/emilia_revmatrix_sessionid_20.csv", row.names = FALSE)



### session id 21 ####

# first set start and endtimes to 0 at the start of the session
matrix1 <- read.csv("data/revisionmatrix_Emilia.csv")
# names(matrix1)

matrix2 <- matrix1 %>%
  filter(session_number == "21")%>%
  mutate(starttijdschoon = start_time - first(start_time),
         eindtijdschoon = (end_time - start_time) + starttijdschoon)

#view(matrix2)


# add real session start clock time and date 
# change for each session! 
matrix3 <- matrix2 %>%
  mutate(startdatumtijd = ymd_hms("2021-01-10 16:28:45"))

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
# names(matrix6)
#clean, remove helper columns
matrix7 <- matrix6 %>%
  select(-startdatumtijd, -starttijdklok1, -starttijdklok2, -seq_element_starttime1)%>%
  rename (seq_element = seq_element_starttime2)%>%
  relocate(seq_element, .before = start_id_GA)%>%
  relocate(start_position, .before = start_time_rel)%>%
  relocate(end_position, .after = start_position)

#view(matrix7)

write.csv(matrix7, "data/emilia_revmatrix_sessionid_21.csv", row.names = FALSE)



