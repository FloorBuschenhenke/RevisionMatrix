

sum_jump <- data_add %>%
  left_join(jump_add, by = c("participant", "session_number", "jump_number"))

#glimpse(sum_jump)


# remove first part of the session (not really a jump) 
sum_jumpB <- sum_jump %>%
  filter(jump_number != 1)



sum_jumpC <- sum_jumpB%>%
  group_by(participant, session_number, jump_number)%>%
  summarize(
    charactercount = sum(char_count > 0),
    start_id_GA = first(id), 
    output = first(outputs),
    event_type = first(jump_type),
    start_time_rel = first(startTime)/first(endTime_session),
    initial_pause = first(pauseTime),
    start_position = first(positionFull),
    end_position = last(positionFull),
    start_position_rel = start_position/first(doclengthFull),
    end_position_rel = end_position/last(doclengthFull),
    start_location = first(prev_loc),
    end_location = last(next_loc),
    start_eventtype = first(type),
    end_eventtype = last(type),
    start_time = first(startTime),
    end_time = last(endTime),
    doclength = first(doclengthFull),
       ## 0 vervangen door NA
    start_position = ifelse(start_position == 0, NA, start_position),
    start_position_rel = ifelse(start_position_rel == 0, NA, start_position_rel))
    
   


view(head(sum_jumpC))

names(sum_jumpC)


### inkorten tabel 


sum_jump2 <- sum_jumpC %>%
  select( -start_location, -end_location, -start_eventtype, -end_eventtype)

#view(sum_jump2)

write.csv(sum_jump2, 'data/chunks.csv', row.names = F)

# revision matrix

# voor na AddInfo_chunks

chunks <- read.csv("data/chunks.csv")

#view(chunks)

chunks_clean <- chunks %>%
  filter(event_type != "focus" & event_type != "jump")

view(chunks_clean)

write.csv(chunks_clean, "data/revisionmatrix.csv", row.names = FALSE)



