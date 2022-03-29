## oude scriptjes

##############
## Calculate characteristics for JUMPS and typing events
sum_jump <- data_add %>%
  left_join(jump_add, by = c("participant", "session_number", "jump_number"))




# remove first part of the session (not really a jump) 
sum_jumpB <- sum_jump %>%
  filter(jump_number != 1)

#names(sum_jumpB)

#sum_jumpC <- sum_jumpB%>%
# group_by(participant, session_number, jump_number)%>%
# summarize (charactercount = sum(char_count > 0))

# view(sum_jumpC)

## er zitten nog hele grote/lange dingen in.. 
# geprobeerd: charactercount = sum(char_count, na.rm = T)
# ook geprobeerd: (charactercount = sum(char_count)) zelfde resultaat
# ik snap niet waarom de gekozen oplossing wel werkt en de andere niet


sum_jumpC <- sum_jumpB%>%
  group_by(participant, session_number, jump_number)%>%
  summarize(
    charactercount = sum(char_count > 0),
    start_id_GA = first(id), 
    intervalholi2 = first(intervalholi),
    chrononumber = first(chrononumber),
    event_type = first(jump_type),
    start_time_rel = first(Starttijd2)/first(endTime_session),
    jump_pause = first(pauseTime),
    start_position = first(positionFull),
    end_position = last(positionFull),
    start_position_rel = start_position/first(doclengthFull),
    end_position_rel = end_position/last(doclengthFull),
    start_position_edge = first(doclengthFull)-start_position,
    end_position_edge = last(doclengthFull)-end_position,
    start_location = first(prev_loc),
    end_location = last(next_loc),
    start_eventtype = first(type),
    end_eventtype = last(type),
    start_time = first(Starttijd2),
    end_time = last(Eindtijd2),
    doclength = first(doclengthFull),
    # counts of types of events within jump
    n_events = n(),
    n_scroll_movements = sum(output == "Scroll"),
    n_selections = sum(type == "replacement"),
    n_arrowkeys = sum(keytype == "arrow_key"))%>%
  mutate(   
    jump_duration = end_time - start_time,
    ## 0 vervangen door NA
    start_position = ifelse(start_position == 0, NA, start_position),
    start_position_rel = ifelse(start_position_rel == 0, NA, start_position_rel),
    start_position_edge = ifelse(start_position_edge == 0, NA, start_position_edge),
    jump_size_chars = end_position - start_position,
    
    # slopes (delta characters/ delta time)
    jump_slope = (jump_size_chars)/jump_duration,
    #converting all jump sizes into positive values for adding up later
    jump_size_charsPlus = ifelse(jump_size_chars < 0, jump_size_chars *-1, jump_size_chars),
    # adding jump size relative to document size at that moment
    jump_size_rel = jump_size_charsPlus/doclength
    
  ) 


#view(head(sum_jumpC))





### inkorten tabel 

#names(sum_jump)

sum_jump2 <- sum_jumpC %>%
  select( -jump_pause, start_position_edge, end_position_edge)

#view(sum_jump2)

write.csv(sum_jump2, 'data/outputjumpevents_Tom.csv', row.names = F)


