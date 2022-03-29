

# libraries
#library(tidyverse)

# read data

alledata <- read.csv("data_out_timefix/timefixeddata.csv", stringsAsFactors = F)


pauzesEruit <- alledata %>%
  mutate(pauze = ifelse(participant == lag(participant)
                        & session_number == lag(session_number)
                        # 5 minuten = 3000000 milliseconden
                        & startTime_Fixed > lag(endTime_Fixed) + 300000, "pauze", "clear"))



## code loslaten op alle sessies met 1 pauze erin.

alleendepauzes <- pauzesEruit %>%
  filter(pauze == "pauze")

countpauzespersessie <- alleendepauzes %>%
  group_by(participant, session_number)%>%
  count(pauze)%>%
  rename( Npauzes = n)%>%
  select(-pauze)

  view(countpauzespersessie)
## 82 sessies (van de 167 ongeveer) waarin pauzes zitten
  
  maar1pauze <- pauzesEruit %>%
    left_join(countpauzespersessie)%>%
    filter(Npauzes == 1)
 
  # ong 19.0000 rijen van de 80.0000 totaal 
  ## en 39 sessies van de 82 
  
  colnames(maar1pauze)
  head(maar1pauze)
  
  ## daar moet dus nog een extra filtering overheen >>
  restvandata <- pauzesEruit%>%
    left_join(countpauzespersessie)%>%
    filter(Npauzes != 1)
## / einde
  
 
  
  
  
  ## hieronder de filterfunctie
  df2 <- maar1pauze %>% 
    group_by(participant, session_number) %>% 
      arrange(id, .by_group = TRUE) %>% 
    mutate(gap = startTime_Fixed - lag(endTime_Fixed),
           
           spell = cumsum(c(1, ifelse(diff(startTime_Fixed)>300000, 1, 0))),
           spell_start_raw = ifelse(is.na(gap), startTime_Fixed, 
                                    ifelse(lag(spell)!= spell, lag(endTime_Fixed), -12345 )),
           
           spell_end_raw = ifelse(is.na(gap), endTime_Fixed, 
                                  ifelse(lag(spell)!= spell, endTime_Fixed - gap, -12345 ))) %>%
    group_by(participant, session_number, spell) %>% 
    mutate(
      spell_start = ifelse(
        
        spell_start_raw == -12345, startTime_Fixed-first(gap),
        spell_start_raw),
      spell_end = ifelse(spell_end_raw == -12345, 
                         endTime_Fixed-first(gap), spell_end_raw))%>%
    mutate(spell_start = ifelse(is.na(spell_start), startTime_Fixed, spell_start),
           spell_end = ifelse(is.na(spell_end), endTime_Fixed, spell_end)) 
  
  
  # clean up auxiliary variables
  df2 <- df2 %>% 
      ungroup()%>%
    select( -gap, -spell, -spell_start_raw, -spell_end_raw, -Npauzes) 
  

  
  
  
  Blad <- df2%>%
    filter(participant == 'Bladergroen')
  
  view(Blad)
  
  write.csv(df2, 'pauzegefilterd1pauzepersessie.csv')
  
  ## weer re-groupen! op participant en session_number 
  # voor verdere bewerking

  
#### nu met sessies met 2 pauzes ##
  ## twee keer door de code 'halen'
  
 
  pauzes2 <- restvandata%>%
    filter(Npauzes == 2)

  
  
  
  ## hieronder de filterfunctie
  df3 <- pauzes2 %>% 
    group_by(participant, session_number) %>% 
    arrange(id, .by_group = TRUE) %>% 
    mutate(gap = startTime_Fixed - lag(endTime_Fixed),
           
           spell = cumsum(c(1, ifelse(diff(startTime_Fixed)>300000, 1, 0))),
           spell_start_raw = ifelse(is.na(gap), startTime_Fixed, 
                                    ifelse(lag(spell)!= spell, lag(endTime_Fixed), -12345 )),
           
           spell_end_raw = ifelse(is.na(gap), endTime_Fixed, 
                                  ifelse(lag(spell)!= spell, endTime_Fixed - gap, -12345 ))) %>%
    group_by(participant, session_number, spell) %>% 
    mutate(
      spell_start = ifelse(
        
        spell_start_raw == -12345, startTime_Fixed-first(gap),
        spell_start_raw),
      spell_end = ifelse(spell_end_raw == -12345, 
                         endTime_Fixed-first(gap), spell_end_raw))%>%
    mutate(spell_start = ifelse(is.na(spell_start), startTime_Fixed, spell_start),
           spell_end = ifelse(is.na(spell_end), endTime_Fixed, spell_end)) 
  
  
  # clean up auxiliary variables
  df3 <- df3 %>% 
    mutate(startTime_Fixed = spell_start,
           endTime_Fixed = spell_end)%>%
    ungroup()%>%
    select( -gap, -spell, -spell_start_raw, -spell_end_raw) 
  
  ## en dan nogmaals bovenstaande filter uitvoeren (hier geplakt)
  
  df4 <- df3 %>% 
    group_by(participant, session_number) %>% 
    arrange(id, .by_group = TRUE) %>% 
    mutate(gap = startTime_Fixed - lag(endTime_Fixed),
           
           spell = cumsum(c(1, ifelse(diff(startTime_Fixed)>300000, 1, 0))),
           spell_start_raw = ifelse(is.na(gap), startTime_Fixed, 
                                    ifelse(lag(spell)!= spell, lag(endTime_Fixed), -12345 )),
           
           spell_end_raw = ifelse(is.na(gap), endTime_Fixed, 
                                  ifelse(lag(spell)!= spell, endTime_Fixed - gap, -12345 ))) %>%
    group_by(participant, session_number, spell) %>% 
    mutate(
      spell_start = ifelse(
        
        spell_start_raw == -12345, startTime_Fixed-first(gap),
        spell_start_raw),
      spell_end = ifelse(spell_end_raw == -12345, 
                         endTime_Fixed-first(gap), spell_end_raw))%>%
    mutate(spell_start = ifelse(is.na(spell_start), startTime_Fixed, spell_start),
           spell_end = ifelse(is.na(spell_end), endTime_Fixed, spell_end)) 
  
  
  df4 <- df4 %>%
    ungroup()%>%
    select( -gap, -spell, -spell_start_raw, -spell_end_raw) 
  
  write.csv(df4, '2pauzespersessiegefilterd.csv', row.names = F)
  
  head(df4)
  
  
  #### nu met sessies met 3 pauzes ##
  ## drie keer door de code 'halen'
  
  
  pauzes3 <- restvandata%>%
    filter(Npauzes == 3)
  
  
  
  
  ## hieronder de filterfunctie
  df5 <- pauzes3 %>% 
    group_by(participant, session_number) %>% 
    arrange(id, .by_group = TRUE) %>% 
    mutate(gap = startTime_Fixed - lag(endTime_Fixed),
           
           spell = cumsum(c(1, ifelse(diff(startTime_Fixed)>300000, 1, 0))),
           spell_start_raw = ifelse(is.na(gap), startTime_Fixed, 
                                    ifelse(lag(spell)!= spell, lag(endTime_Fixed), -12345 )),
           
           spell_end_raw = ifelse(is.na(gap), endTime_Fixed, 
                                  ifelse(lag(spell)!= spell, endTime_Fixed - gap, -12345 ))) %>%
    group_by(participant, session_number, spell) %>% 
    mutate(
      spell_start = ifelse(
        
        spell_start_raw == -12345, startTime_Fixed-first(gap),
        spell_start_raw),
      spell_end = ifelse(spell_end_raw == -12345, 
                         endTime_Fixed-first(gap), spell_end_raw))%>%
    mutate(spell_start = ifelse(is.na(spell_start), startTime_Fixed, spell_start),
           spell_end = ifelse(is.na(spell_end), endTime_Fixed, spell_end)) 
  
  
  # clean up auxiliary variables
  df5 <- df5 %>% 
    mutate(startTime_Fixed = spell_start,
           endTime_Fixed = spell_end)%>%
    ungroup()%>%
    select( -gap, -spell, -spell_start_raw, -spell_end_raw) 
  
  ## en dan 2e keer bovenstaande filter uitvoeren (hier geplakt)
  
  df6 <- df5 %>% 
    group_by(participant, session_number) %>% 
    arrange(id, .by_group = TRUE) %>% 
    mutate(gap = startTime_Fixed - lag(endTime_Fixed),
           
           spell = cumsum(c(1, ifelse(diff(startTime_Fixed)>300000, 1, 0))),
           spell_start_raw = ifelse(is.na(gap), startTime_Fixed, 
                                    ifelse(lag(spell)!= spell, lag(endTime_Fixed), -12345 )),
           
           spell_end_raw = ifelse(is.na(gap), endTime_Fixed, 
                                  ifelse(lag(spell)!= spell, endTime_Fixed - gap, -12345 ))) %>%
    group_by(participant, session_number, spell) %>% 
    mutate(
      spell_start = ifelse(
        
        spell_start_raw == -12345, startTime_Fixed-first(gap),
        spell_start_raw),
      spell_end = ifelse(spell_end_raw == -12345, 
                         endTime_Fixed-first(gap), spell_end_raw))%>%
    mutate(spell_start = ifelse(is.na(spell_start), startTime_Fixed, spell_start),
           spell_end = ifelse(is.na(spell_end), endTime_Fixed, spell_end)) 
  
  
  df6 <- df6 %>%
    ungroup()%>%
    mutate(startTime_Fixed = spell_start,
           endTime_Fixed = spell_end)%>%
       select( -gap, -spell, -spell_start_raw, -spell_end_raw) 
  
  ## derde en laatste keer deze groep sessies door het filter halen
  
  
  df7 <- df6 %>% 
    group_by(participant, session_number) %>% 
    arrange(id, .by_group = TRUE) %>% 
    mutate(gap = startTime_Fixed - lag(endTime_Fixed),
           
           spell = cumsum(c(1, ifelse(diff(startTime_Fixed)>300000, 1, 0))),
           spell_start_raw = ifelse(is.na(gap), startTime_Fixed, 
                                    ifelse(lag(spell)!= spell, lag(endTime_Fixed), -12345 )),
           
           spell_end_raw = ifelse(is.na(gap), endTime_Fixed, 
                                  ifelse(lag(spell)!= spell, endTime_Fixed - gap, -12345 ))) %>%
    group_by(participant, session_number, spell) %>% 
    mutate(
      spell_start = ifelse(
        
        spell_start_raw == -12345, startTime_Fixed-first(gap),
        spell_start_raw),
      spell_end = ifelse(spell_end_raw == -12345, 
                         endTime_Fixed-first(gap), spell_end_raw))%>%
    mutate(spell_start = ifelse(is.na(spell_start), startTime_Fixed, spell_start),
           spell_end = ifelse(is.na(spell_end), endTime_Fixed, spell_end)) 
  
  
  df7 <- df7 %>%
    ungroup()%>%
    select( -gap, -spell, -spell_start_raw, -spell_end_raw) 
  
  write.csv(df7, '3pauzespersessiegefilterd.csv', row.names = F)

  
  ### alle sessies met 4 pauzes   
  
 
  
  pauzes4 <- restvandata%>%
    filter(Npauzes == 4)
  
  
  
  
  ## hieronder de filterfunctie
  df5 <- pauzes4 %>% 
    group_by(participant, session_number) %>% 
    arrange(id, .by_group = TRUE) %>% 
    mutate(gap = startTime_Fixed - lag(endTime_Fixed),
           
           spell = cumsum(c(1, ifelse(diff(startTime_Fixed)>300000, 1, 0))),
           spell_start_raw = ifelse(is.na(gap), startTime_Fixed, 
                                    ifelse(lag(spell)!= spell, lag(endTime_Fixed), -12345 )),
           
           spell_end_raw = ifelse(is.na(gap), endTime_Fixed, 
                                  ifelse(lag(spell)!= spell, endTime_Fixed - gap, -12345 ))) %>%
    group_by(participant, session_number, spell) %>% 
    mutate(
      spell_start = ifelse(
        
        spell_start_raw == -12345, startTime_Fixed-first(gap),
        spell_start_raw),
      spell_end = ifelse(spell_end_raw == -12345, 
                         endTime_Fixed-first(gap), spell_end_raw))%>%
    mutate(spell_start = ifelse(is.na(spell_start), startTime_Fixed, spell_start),
           spell_end = ifelse(is.na(spell_end), endTime_Fixed, spell_end)) 
  
  
  # clean up auxiliary variables
  df5 <- df5 %>% 
    mutate(startTime_Fixed = spell_start,
           endTime_Fixed = spell_end)%>%
    ungroup()%>%
    select( -gap, -spell, -spell_start_raw, -spell_end_raw) 
  
  ## en dan 2e keer bovenstaande filter uitvoeren (hier geplakt)
  
  df6 <- df5 %>% 
    group_by(participant, session_number) %>% 
    arrange(id, .by_group = TRUE) %>% 
    mutate(gap = startTime_Fixed - lag(endTime_Fixed),
           
           spell = cumsum(c(1, ifelse(diff(startTime_Fixed)>300000, 1, 0))),
           spell_start_raw = ifelse(is.na(gap), startTime_Fixed, 
                                    ifelse(lag(spell)!= spell, lag(endTime_Fixed), -12345 )),
           
           spell_end_raw = ifelse(is.na(gap), endTime_Fixed, 
                                  ifelse(lag(spell)!= spell, endTime_Fixed - gap, -12345 ))) %>%
    group_by(participant, session_number, spell) %>% 
    mutate(
      spell_start = ifelse(
        
        spell_start_raw == -12345, startTime_Fixed-first(gap),
        spell_start_raw),
      spell_end = ifelse(spell_end_raw == -12345, 
                         endTime_Fixed-first(gap), spell_end_raw))%>%
    mutate(spell_start = ifelse(is.na(spell_start), startTime_Fixed, spell_start),
           spell_end = ifelse(is.na(spell_end), endTime_Fixed, spell_end)) 
  
  
  df6 <- df6 %>%
    mutate(startTime_Fixed = spell_start,
           endTime_Fixed = spell_end)%>%
    ungroup()%>%
    select( -gap, -spell, -spell_start_raw, -spell_end_raw) 
  
  ## derde keer deze groep sessies door het filter halen
  
  
  df7 <- df6 %>% 
    group_by(participant, session_number) %>% 
    arrange(id, .by_group = TRUE) %>% 
    mutate(gap = startTime_Fixed - lag(endTime_Fixed),
           
           spell = cumsum(c(1, ifelse(diff(startTime_Fixed)>300000, 1, 0))),
           spell_start_raw = ifelse(is.na(gap), startTime_Fixed, 
                                    ifelse(lag(spell)!= spell, lag(endTime_Fixed), -12345 )),
           
           spell_end_raw = ifelse(is.na(gap), endTime_Fixed, 
                                  ifelse(lag(spell)!= spell, endTime_Fixed - gap, -12345 ))) %>%
    group_by(participant, session_number, spell) %>% 
    mutate(
      spell_start = ifelse(
        
        spell_start_raw == -12345, startTime_Fixed-first(gap),
        spell_start_raw),
      spell_end = ifelse(spell_end_raw == -12345, 
                         endTime_Fixed-first(gap), spell_end_raw))%>%
    mutate(spell_start = ifelse(is.na(spell_start), startTime_Fixed, spell_start),
           spell_end = ifelse(is.na(spell_end), endTime_Fixed, spell_end)) 
  
  
  df7 <- df7 %>%
    mutate(startTime_Fixed = spell_start,
           endTime_Fixed = spell_end)%>%
    ungroup()%>%
    select( -gap, -spell, -spell_start_raw, -spell_end_raw) 
  
  
  ## vierde en laatste keer door filter
  
  df8 <- df7 %>% 
    group_by(participant, session_number) %>% 
    arrange(id, .by_group = TRUE) %>% 
    mutate(gap = startTime_Fixed - lag(endTime_Fixed),
           
           spell = cumsum(c(1, ifelse(diff(startTime_Fixed)>300000, 1, 0))),
           spell_start_raw = ifelse(is.na(gap), startTime_Fixed, 
                                    ifelse(lag(spell)!= spell, lag(endTime_Fixed), -12345 )),
           
           spell_end_raw = ifelse(is.na(gap), endTime_Fixed, 
                                  ifelse(lag(spell)!= spell, endTime_Fixed - gap, -12345 ))) %>%
    group_by(participant, session_number, spell) %>% 
    mutate(
      spell_start = ifelse(
        
        spell_start_raw == -12345, startTime_Fixed-first(gap),
        spell_start_raw),
      spell_end = ifelse(spell_end_raw == -12345, 
                         endTime_Fixed-first(gap), spell_end_raw))%>%
    mutate(spell_start = ifelse(is.na(spell_start), startTime_Fixed, spell_start),
           spell_end = ifelse(is.na(spell_end), endTime_Fixed, spell_end)) 
  
  
  df8 <- df8 %>%
    ungroup()%>%
    select( -gap, -spell, -spell_start_raw, -spell_end_raw)
  
  
  
  
  write.csv(df8, '4pauzespersessiegefilterd.csv', row.names = F)
  
  
  
  
  ### alle sessies met 5 pauzes   
  
  
  
  pauzes5 <- restvandata%>%
    filter(Npauzes == 5)
  
  
  
  
  ## hieronder de filterfunctie
  df5 <- pauzes5 %>% 
    group_by(participant, session_number) %>% 
    arrange(id, .by_group = TRUE) %>% 
    mutate(gap = startTime_Fixed - lag(endTime_Fixed),
           
           spell = cumsum(c(1, ifelse(diff(startTime_Fixed)>300000, 1, 0))),
           spell_start_raw = ifelse(is.na(gap), startTime_Fixed, 
                                    ifelse(lag(spell)!= spell, lag(endTime_Fixed), -12345 )),
           
           spell_end_raw = ifelse(is.na(gap), endTime_Fixed, 
                                  ifelse(lag(spell)!= spell, endTime_Fixed - gap, -12345 ))) %>%
    group_by(participant, session_number, spell) %>% 
    mutate(
      spell_start = ifelse(
        
        spell_start_raw == -12345, startTime_Fixed-first(gap),
        spell_start_raw),
      spell_end = ifelse(spell_end_raw == -12345, 
                         endTime_Fixed-first(gap), spell_end_raw))%>%
    mutate(spell_start = ifelse(is.na(spell_start), startTime_Fixed, spell_start),
           spell_end = ifelse(is.na(spell_end), endTime_Fixed, spell_end)) 
  
  
  # clean up auxiliary variables
  df5 <- df5 %>% 
    mutate(startTime_Fixed = spell_start,
           endTime_Fixed = spell_end)%>%
    ungroup()%>%
    select( -gap, -spell, -spell_start_raw, -spell_end_raw) 
  
  ## en dan 2e keer bovenstaande filter uitvoeren (hier geplakt)
  
  df6 <- df5 %>% 
    group_by(participant, session_number) %>% 
    arrange(id, .by_group = TRUE) %>% 
    mutate(gap = startTime_Fixed - lag(endTime_Fixed),
           
           spell = cumsum(c(1, ifelse(diff(startTime_Fixed)>300000, 1, 0))),
           spell_start_raw = ifelse(is.na(gap), startTime_Fixed, 
                                    ifelse(lag(spell)!= spell, lag(endTime_Fixed), -12345 )),
           
           spell_end_raw = ifelse(is.na(gap), endTime_Fixed, 
                                  ifelse(lag(spell)!= spell, endTime_Fixed - gap, -12345 ))) %>%
    group_by(participant, session_number, spell) %>% 
    mutate(
      spell_start = ifelse(
        
        spell_start_raw == -12345, startTime_Fixed-first(gap),
        spell_start_raw),
      spell_end = ifelse(spell_end_raw == -12345, 
                         endTime_Fixed-first(gap), spell_end_raw))%>%
    mutate(spell_start = ifelse(is.na(spell_start), startTime_Fixed, spell_start),
           spell_end = ifelse(is.na(spell_end), endTime_Fixed, spell_end)) 
  
  
  df6 <- df6 %>%
    mutate(startTime_Fixed = spell_start,
           endTime_Fixed = spell_end)%>%
    ungroup()%>%
    select( -gap, -spell, -spell_start_raw, -spell_end_raw) 
  
  ## derde keer deze groep sessies door het filter halen
  
  
  df7 <- df6 %>% 
    group_by(participant, session_number) %>% 
    arrange(id, .by_group = TRUE) %>% 
    mutate(gap = startTime_Fixed - lag(endTime_Fixed),
           
           spell = cumsum(c(1, ifelse(diff(startTime_Fixed)>300000, 1, 0))),
           spell_start_raw = ifelse(is.na(gap), startTime_Fixed, 
                                    ifelse(lag(spell)!= spell, lag(endTime_Fixed), -12345 )),
           
           spell_end_raw = ifelse(is.na(gap), endTime_Fixed, 
                                  ifelse(lag(spell)!= spell, endTime_Fixed - gap, -12345 ))) %>%
    group_by(participant, session_number, spell) %>% 
    mutate(
      spell_start = ifelse(
        
        spell_start_raw == -12345, startTime_Fixed-first(gap),
        spell_start_raw),
      spell_end = ifelse(spell_end_raw == -12345, 
                         endTime_Fixed-first(gap), spell_end_raw))%>%
    mutate(spell_start = ifelse(is.na(spell_start), startTime_Fixed, spell_start),
           spell_end = ifelse(is.na(spell_end), endTime_Fixed, spell_end)) 
  
  
  df7 <- df7 %>%
    mutate(startTime_Fixed = spell_start,
           endTime_Fixed = spell_end)%>%
    ungroup()%>%
    select( -gap, -spell, -spell_start_raw, -spell_end_raw) 
  
  ## vierde keer
  
  
  
  df8 <- df7 %>% 
    group_by(participant, session_number) %>% 
    arrange(id, .by_group = TRUE) %>% 
    mutate(gap = startTime_Fixed - lag(endTime_Fixed),
           
           spell = cumsum(c(1, ifelse(diff(startTime_Fixed)>300000, 1, 0))),
           spell_start_raw = ifelse(is.na(gap), startTime_Fixed, 
                                    ifelse(lag(spell)!= spell, lag(endTime_Fixed), -12345 )),
           
           spell_end_raw = ifelse(is.na(gap), endTime_Fixed, 
                                  ifelse(lag(spell)!= spell, endTime_Fixed - gap, -12345 ))) %>%
    group_by(participant, session_number, spell) %>% 
    mutate(
      spell_start = ifelse(
        
        spell_start_raw == -12345, startTime_Fixed-first(gap),
        spell_start_raw),
      spell_end = ifelse(spell_end_raw == -12345, 
                         endTime_Fixed-first(gap), spell_end_raw))%>%
    mutate(spell_start = ifelse(is.na(spell_start), startTime_Fixed, spell_start),
           spell_end = ifelse(is.na(spell_end), endTime_Fixed, spell_end)) 
  
  
  df8 <- df8 %>%
    mutate(startTime_Fixed = spell_start,
           endTime_Fixed = spell_end)%>%
    ungroup()%>%
    select( -gap, -spell, -spell_start_raw, -spell_end_raw) 
  
  ## vijfde en laatste keer door filter
  
  df9 <- df8 %>% 
    group_by(participant, session_number) %>% 
    arrange(id, .by_group = TRUE) %>% 
    mutate(gap = startTime_Fixed - lag(endTime_Fixed),
           
           spell = cumsum(c(1, ifelse(diff(startTime_Fixed)>300000, 1, 0))),
           spell_start_raw = ifelse(is.na(gap), startTime_Fixed, 
                                    ifelse(lag(spell)!= spell, lag(endTime_Fixed), -12345 )),
           
           spell_end_raw = ifelse(is.na(gap), endTime_Fixed, 
                                  ifelse(lag(spell)!= spell, endTime_Fixed - gap, -12345 ))) %>%
    group_by(participant, session_number, spell) %>% 
    mutate(
      spell_start = ifelse(
        
        spell_start_raw == -12345, startTime_Fixed-first(gap),
        spell_start_raw),
      spell_end = ifelse(spell_end_raw == -12345, 
                         endTime_Fixed-first(gap), spell_end_raw))%>%
    mutate(spell_start = ifelse(is.na(spell_start), startTime_Fixed, spell_start),
           spell_end = ifelse(is.na(spell_end), endTime_Fixed, spell_end)) 
  
  
  df9 <- df9 %>%
    ungroup()%>%
    select( -gap, -spell, -spell_start_raw, -spell_end_raw)
  
  
  write.csv(df9, "5pauzespersessiegefilterd.csv")

  
  ######### sessies met 6 pauzes ####
  
  
  
  pauzes6 <- restvandata%>%
    filter(Npauzes == 6)
  
  
  
  
  ## hieronder de filterfunctie
  df5 <- pauzes6 %>% 
    group_by(participant, session_number) %>% 
    arrange(id, .by_group = TRUE) %>% 
    mutate(gap = startTime_Fixed - lag(endTime_Fixed),
           
           spell = cumsum(c(1, ifelse(diff(startTime_Fixed)>300000, 1, 0))),
           spell_start_raw = ifelse(is.na(gap), startTime_Fixed, 
                                    ifelse(lag(spell)!= spell, lag(endTime_Fixed), -12345 )),
           
           spell_end_raw = ifelse(is.na(gap), endTime_Fixed, 
                                  ifelse(lag(spell)!= spell, endTime_Fixed - gap, -12345 ))) %>%
    group_by(participant, session_number, spell) %>% 
    mutate(
      spell_start = ifelse(
        
        spell_start_raw == -12345, startTime_Fixed-first(gap),
        spell_start_raw),
      spell_end = ifelse(spell_end_raw == -12345, 
                         endTime_Fixed-first(gap), spell_end_raw))%>%
    mutate(spell_start = ifelse(is.na(spell_start), startTime_Fixed, spell_start),
           spell_end = ifelse(is.na(spell_end), endTime_Fixed, spell_end)) 
  
  
  # clean up auxiliary variables
  df5 <- df5 %>% 
    mutate(startTime_Fixed = spell_start,
           endTime_Fixed = spell_end)%>%
    ungroup()%>%
    select( -gap, -spell, -spell_start_raw, -spell_end_raw) 
  
  ## en dan 2e keer bovenstaande filter uitvoeren (hier geplakt)
  
  df6 <- df5 %>% 
    group_by(participant, session_number) %>% 
    arrange(id, .by_group = TRUE) %>% 
    mutate(gap = startTime_Fixed - lag(endTime_Fixed),
           
           spell = cumsum(c(1, ifelse(diff(startTime_Fixed)>300000, 1, 0))),
           spell_start_raw = ifelse(is.na(gap), startTime_Fixed, 
                                    ifelse(lag(spell)!= spell, lag(endTime_Fixed), -12345 )),
           
           spell_end_raw = ifelse(is.na(gap), endTime_Fixed, 
                                  ifelse(lag(spell)!= spell, endTime_Fixed - gap, -12345 ))) %>%
    group_by(participant, session_number, spell) %>% 
    mutate(
      spell_start = ifelse(
        
        spell_start_raw == -12345, startTime_Fixed-first(gap),
        spell_start_raw),
      spell_end = ifelse(spell_end_raw == -12345, 
                         endTime_Fixed-first(gap), spell_end_raw))%>%
    mutate(spell_start = ifelse(is.na(spell_start), startTime_Fixed, spell_start),
           spell_end = ifelse(is.na(spell_end), endTime_Fixed, spell_end)) 
  
  
  df6 <- df6 %>%
    mutate(startTime_Fixed = spell_start,
           endTime_Fixed = spell_end)%>%
    ungroup()%>%
    select( -gap, -spell, -spell_start_raw, -spell_end_raw) 
  
  ## derde keer deze groep sessies door het filter halen
  
  
  df7 <- df6 %>% 
    group_by(participant, session_number) %>% 
    arrange(id, .by_group = TRUE) %>% 
    mutate(gap = startTime_Fixed - lag(endTime_Fixed),
           
           spell = cumsum(c(1, ifelse(diff(startTime_Fixed)>300000, 1, 0))),
           spell_start_raw = ifelse(is.na(gap), startTime_Fixed, 
                                    ifelse(lag(spell)!= spell, lag(endTime_Fixed), -12345 )),
           
           spell_end_raw = ifelse(is.na(gap), endTime_Fixed, 
                                  ifelse(lag(spell)!= spell, endTime_Fixed - gap, -12345 ))) %>%
    group_by(participant, session_number, spell) %>% 
    mutate(
      spell_start = ifelse(
        
        spell_start_raw == -12345, startTime_Fixed-first(gap),
        spell_start_raw),
      spell_end = ifelse(spell_end_raw == -12345, 
                         endTime_Fixed-first(gap), spell_end_raw))%>%
    mutate(spell_start = ifelse(is.na(spell_start), startTime_Fixed, spell_start),
           spell_end = ifelse(is.na(spell_end), endTime_Fixed, spell_end)) 
  
  
  df7 <- df7 %>%
    mutate(startTime_Fixed = spell_start,
           endTime_Fixed = spell_end)%>%
    ungroup()%>%
    select( -gap, -spell, -spell_start_raw, -spell_end_raw) 
  
  ## vierde keer
  
  
  
  df8 <- df7 %>% 
    group_by(participant, session_number) %>% 
    arrange(id, .by_group = TRUE) %>% 
    mutate(gap = startTime_Fixed - lag(endTime_Fixed),
           
           spell = cumsum(c(1, ifelse(diff(startTime_Fixed)>300000, 1, 0))),
           spell_start_raw = ifelse(is.na(gap), startTime_Fixed, 
                                    ifelse(lag(spell)!= spell, lag(endTime_Fixed), -12345 )),
           
           spell_end_raw = ifelse(is.na(gap), endTime_Fixed, 
                                  ifelse(lag(spell)!= spell, endTime_Fixed - gap, -12345 ))) %>%
    group_by(participant, session_number, spell) %>% 
    mutate(
      spell_start = ifelse(
        
        spell_start_raw == -12345, startTime_Fixed-first(gap),
        spell_start_raw),
      spell_end = ifelse(spell_end_raw == -12345, 
                         endTime_Fixed-first(gap), spell_end_raw))%>%
    mutate(spell_start = ifelse(is.na(spell_start), startTime_Fixed, spell_start),
           spell_end = ifelse(is.na(spell_end), endTime_Fixed, spell_end)) 
  
  
  df8 <- df8 %>%
    mutate(startTime_Fixed = spell_start,
           endTime_Fixed = spell_end)%>%
    ungroup()%>%
    select( -gap, -spell, -spell_start_raw, -spell_end_raw) 
  
  
  
  ### vijfde keer door het filter 
  
  df9 <- df8 %>% 
    group_by(participant, session_number) %>% 
    arrange(id, .by_group = TRUE) %>% 
    mutate(gap = startTime_Fixed - lag(endTime_Fixed),
           
           spell = cumsum(c(1, ifelse(diff(startTime_Fixed)>300000, 1, 0))),
           spell_start_raw = ifelse(is.na(gap), startTime_Fixed, 
                                    ifelse(lag(spell)!= spell, lag(endTime_Fixed), -12345 )),
           
           spell_end_raw = ifelse(is.na(gap), endTime_Fixed, 
                                  ifelse(lag(spell)!= spell, endTime_Fixed - gap, -12345 ))) %>%
    group_by(participant, session_number, spell) %>% 
    mutate(
      spell_start = ifelse(
        
        spell_start_raw == -12345, startTime_Fixed-first(gap),
        spell_start_raw),
      spell_end = ifelse(spell_end_raw == -12345, 
                         endTime_Fixed-first(gap), spell_end_raw))%>%
    mutate(spell_start = ifelse(is.na(spell_start), startTime_Fixed, spell_start),
           spell_end = ifelse(is.na(spell_end), endTime_Fixed, spell_end)) 
  
  
  # clean up auxiliary variables
  df9 <- df9 %>% 
    mutate(startTime_Fixed = spell_start,
           endTime_Fixed = spell_end)%>%
    ungroup()%>%
    select( -gap, -spell, -spell_start_raw, -spell_end_raw) 
  
  
  
  ## zesde en laatste keer door filter
  
  df9 <- df9 %>% 
    group_by(participant, session_number) %>% 
    arrange(id, .by_group = TRUE) %>% 
    mutate(gap = startTime_Fixed - lag(endTime_Fixed),
           
           spell = cumsum(c(1, ifelse(diff(startTime_Fixed)>300000, 1, 0))),
           spell_start_raw = ifelse(is.na(gap), startTime_Fixed, 
                                    ifelse(lag(spell)!= spell, lag(endTime_Fixed), -12345 )),
           
           spell_end_raw = ifelse(is.na(gap), endTime_Fixed, 
                                  ifelse(lag(spell)!= spell, endTime_Fixed - gap, -12345 ))) %>%
    group_by(participant, session_number, spell) %>% 
    mutate(
      spell_start = ifelse(
        
        spell_start_raw == -12345, startTime_Fixed-first(gap),
        spell_start_raw),
      spell_end = ifelse(spell_end_raw == -12345, 
                         endTime_Fixed-first(gap), spell_end_raw))%>%
    mutate(spell_start = ifelse(is.na(spell_start), startTime_Fixed, spell_start),
           spell_end = ifelse(is.na(spell_end), endTime_Fixed, spell_end)) 
  
  
  df10 <- df9 %>%
    ungroup()%>%
    select( -gap, -spell, -spell_start_raw, -spell_end_raw)
  
  
  write.csv(df10, "6pauzespersessiegefilterd.csv")

  
  
  ####### voor 8 pauzes ( er waren geen sessies met 7 pauzes vandaar)    
  
  
  
  
  
  
  pauzes8 <- restvandata%>%
    filter(Npauzes == 8)
  
  
  
  
  ## hieronder de filterfunctie
  df5 <- pauzes8 %>% 
    group_by(participant, session_number) %>% 
    arrange(id, .by_group = TRUE) %>% 
    mutate(gap = startTime_Fixed - lag(endTime_Fixed),
           
           spell = cumsum(c(1, ifelse(diff(startTime_Fixed)>300000, 1, 0))),
           spell_start_raw = ifelse(is.na(gap), startTime_Fixed, 
                                    ifelse(lag(spell)!= spell, lag(endTime_Fixed), -12345 )),
           
           spell_end_raw = ifelse(is.na(gap), endTime_Fixed, 
                                  ifelse(lag(spell)!= spell, endTime_Fixed - gap, -12345 ))) %>%
    group_by(participant, session_number, spell) %>% 
    mutate(
      spell_start = ifelse(
        
        spell_start_raw == -12345, startTime_Fixed-first(gap),
        spell_start_raw),
      spell_end = ifelse(spell_end_raw == -12345, 
                         endTime_Fixed-first(gap), spell_end_raw))%>%
    mutate(spell_start = ifelse(is.na(spell_start), startTime_Fixed, spell_start),
           spell_end = ifelse(is.na(spell_end), endTime_Fixed, spell_end)) 
  
  
  # clean up auxiliary variables
  df5 <- df5 %>% 
    mutate(startTime_Fixed = spell_start,
           endTime_Fixed = spell_end)%>%
    ungroup()%>%
    select( -gap, -spell, -spell_start_raw, -spell_end_raw) 
  
  ## en dan 2e keer bovenstaande filter uitvoeren (hier geplakt)
  
  df6 <- df5 %>% 
    group_by(participant, session_number) %>% 
    arrange(id, .by_group = TRUE) %>% 
    mutate(gap = startTime_Fixed - lag(endTime_Fixed),
           
           spell = cumsum(c(1, ifelse(diff(startTime_Fixed)>300000, 1, 0))),
           spell_start_raw = ifelse(is.na(gap), startTime_Fixed, 
                                    ifelse(lag(spell)!= spell, lag(endTime_Fixed), -12345 )),
           
           spell_end_raw = ifelse(is.na(gap), endTime_Fixed, 
                                  ifelse(lag(spell)!= spell, endTime_Fixed - gap, -12345 ))) %>%
    group_by(participant, session_number, spell) %>% 
    mutate(
      spell_start = ifelse(
        
        spell_start_raw == -12345, startTime_Fixed-first(gap),
        spell_start_raw),
      spell_end = ifelse(spell_end_raw == -12345, 
                         endTime_Fixed-first(gap), spell_end_raw))%>%
    mutate(spell_start = ifelse(is.na(spell_start), startTime_Fixed, spell_start),
           spell_end = ifelse(is.na(spell_end), endTime_Fixed, spell_end)) 
  
  
  df6 <- df6 %>%
    mutate(startTime_Fixed = spell_start,
           endTime_Fixed = spell_end)%>%
    ungroup()%>%
    select( -gap, -spell, -spell_start_raw, -spell_end_raw) 
  
  ## derde keer deze groep sessies door het filter halen
  
  
  df7 <- df6 %>% 
    group_by(participant, session_number) %>% 
    arrange(id, .by_group = TRUE) %>% 
    mutate(gap = startTime_Fixed - lag(endTime_Fixed),
           
           spell = cumsum(c(1, ifelse(diff(startTime_Fixed)>300000, 1, 0))),
           spell_start_raw = ifelse(is.na(gap), startTime_Fixed, 
                                    ifelse(lag(spell)!= spell, lag(endTime_Fixed), -12345 )),
           
           spell_end_raw = ifelse(is.na(gap), endTime_Fixed, 
                                  ifelse(lag(spell)!= spell, endTime_Fixed - gap, -12345 ))) %>%
    group_by(participant, session_number, spell) %>% 
    mutate(
      spell_start = ifelse(
        
        spell_start_raw == -12345, startTime_Fixed-first(gap),
        spell_start_raw),
      spell_end = ifelse(spell_end_raw == -12345, 
                         endTime_Fixed-first(gap), spell_end_raw))%>%
    mutate(spell_start = ifelse(is.na(spell_start), startTime_Fixed, spell_start),
           spell_end = ifelse(is.na(spell_end), endTime_Fixed, spell_end)) 
  
  
  df7 <- df7 %>%
    mutate(startTime_Fixed = spell_start,
           endTime_Fixed = spell_end)%>%
    ungroup()%>%
    select( -gap, -spell, -spell_start_raw, -spell_end_raw) 
  
  ## vierde keer
  
  
  
  df8 <- df7 %>% 
    group_by(participant, session_number) %>% 
    arrange(id, .by_group = TRUE) %>% 
    mutate(gap = startTime_Fixed - lag(endTime_Fixed),
           
           spell = cumsum(c(1, ifelse(diff(startTime_Fixed)>300000, 1, 0))),
           spell_start_raw = ifelse(is.na(gap), startTime_Fixed, 
                                    ifelse(lag(spell)!= spell, lag(endTime_Fixed), -12345 )),
           
           spell_end_raw = ifelse(is.na(gap), endTime_Fixed, 
                                  ifelse(lag(spell)!= spell, endTime_Fixed - gap, -12345 ))) %>%
    group_by(participant, session_number, spell) %>% 
    mutate(
      spell_start = ifelse(
        
        spell_start_raw == -12345, startTime_Fixed-first(gap),
        spell_start_raw),
      spell_end = ifelse(spell_end_raw == -12345, 
                         endTime_Fixed-first(gap), spell_end_raw))%>%
    mutate(spell_start = ifelse(is.na(spell_start), startTime_Fixed, spell_start),
           spell_end = ifelse(is.na(spell_end), endTime_Fixed, spell_end)) 
  
  
  df8 <- df8 %>%
    mutate(startTime_Fixed = spell_start,
           endTime_Fixed = spell_end)%>%
    ungroup()%>%
    select( -gap, -spell, -spell_start_raw, -spell_end_raw) 
  
  
  
  ### vijfde keer door het filter 
  
  df9 <- df8 %>% 
    group_by(participant, session_number) %>% 
    arrange(id, .by_group = TRUE) %>% 
    mutate(gap = startTime_Fixed - lag(endTime_Fixed),
           
           spell = cumsum(c(1, ifelse(diff(startTime_Fixed)>300000, 1, 0))),
           spell_start_raw = ifelse(is.na(gap), startTime_Fixed, 
                                    ifelse(lag(spell)!= spell, lag(endTime_Fixed), -12345 )),
           
           spell_end_raw = ifelse(is.na(gap), endTime_Fixed, 
                                  ifelse(lag(spell)!= spell, endTime_Fixed - gap, -12345 ))) %>%
    group_by(participant, session_number, spell) %>% 
    mutate(
      spell_start = ifelse(
        
        spell_start_raw == -12345, startTime_Fixed-first(gap),
        spell_start_raw),
      spell_end = ifelse(spell_end_raw == -12345, 
                         endTime_Fixed-first(gap), spell_end_raw))%>%
    mutate(spell_start = ifelse(is.na(spell_start), startTime_Fixed, spell_start),
           spell_end = ifelse(is.na(spell_end), endTime_Fixed, spell_end)) 
  
  
  # clean up auxiliary variables
  df9 <- df9 %>% 
    mutate(startTime_Fixed = spell_start,
           endTime_Fixed = spell_end)%>%
    ungroup()%>%
    select( -gap, -spell, -spell_start_raw, -spell_end_raw) 
  
  ## zesde keer
  
  df10 <- df9 %>% 
    group_by(participant, session_number) %>% 
    arrange(id, .by_group = TRUE) %>% 
    mutate(gap = startTime_Fixed - lag(endTime_Fixed),
           
           spell = cumsum(c(1, ifelse(diff(startTime_Fixed)>300000, 1, 0))),
           spell_start_raw = ifelse(is.na(gap), startTime_Fixed, 
                                    ifelse(lag(spell)!= spell, lag(endTime_Fixed), -12345 )),
           
           spell_end_raw = ifelse(is.na(gap), endTime_Fixed, 
                                  ifelse(lag(spell)!= spell, endTime_Fixed - gap, -12345 ))) %>%
    group_by(participant, session_number, spell) %>% 
    mutate(
      spell_start = ifelse(
        
        spell_start_raw == -12345, startTime_Fixed-first(gap),
        spell_start_raw),
      spell_end = ifelse(spell_end_raw == -12345, 
                         endTime_Fixed-first(gap), spell_end_raw))%>%
    mutate(spell_start = ifelse(is.na(spell_start), startTime_Fixed, spell_start),
           spell_end = ifelse(is.na(spell_end), endTime_Fixed, spell_end)) 
  
  
  # clean up auxiliary variables
  df10 <- df10 %>% 
    mutate(startTime_Fixed = spell_start,
           endTime_Fixed = spell_end)%>%
    ungroup()%>%
    select( -gap, -spell, -spell_start_raw, -spell_end_raw) 
  
  
  ## zevende keer door filter
  
  df11 <- df10 %>% 
    group_by(participant, session_number) %>% 
    arrange(id, .by_group = TRUE) %>% 
    mutate(gap = startTime_Fixed - lag(endTime_Fixed),
           
           spell = cumsum(c(1, ifelse(diff(startTime_Fixed)>300000, 1, 0))),
           spell_start_raw = ifelse(is.na(gap), startTime_Fixed, 
                                    ifelse(lag(spell)!= spell, lag(endTime_Fixed), -12345 )),
           
           spell_end_raw = ifelse(is.na(gap), endTime_Fixed, 
                                  ifelse(lag(spell)!= spell, endTime_Fixed - gap, -12345 ))) %>%
    group_by(participant, session_number, spell) %>% 
    mutate(
      spell_start = ifelse(
        
        spell_start_raw == -12345, startTime_Fixed-first(gap),
        spell_start_raw),
      spell_end = ifelse(spell_end_raw == -12345, 
                         endTime_Fixed-first(gap), spell_end_raw))%>%
    mutate(spell_start = ifelse(is.na(spell_start), startTime_Fixed, spell_start),
           spell_end = ifelse(is.na(spell_end), endTime_Fixed, spell_end)) 
  
  
  df11 <- df11 %>% 
    mutate(startTime_Fixed = spell_start,
           endTime_Fixed = spell_end)%>%
    ungroup()%>%
    select( -gap, -spell, -spell_start_raw, -spell_end_raw) 
  
  
  
  ### achtste en laatste keer
  
  
    df12 <- df11 %>% 
    group_by(participant, session_number) %>% 
    arrange(id, .by_group = TRUE) %>% 
    mutate(gap = startTime_Fixed - lag(endTime_Fixed),
           
           spell = cumsum(c(1, ifelse(diff(startTime_Fixed)>300000, 1, 0))),
           spell_start_raw = ifelse(is.na(gap), startTime_Fixed, 
                                    ifelse(lag(spell)!= spell, lag(endTime_Fixed), -12345 )),
           
           spell_end_raw = ifelse(is.na(gap), endTime_Fixed, 
                                  ifelse(lag(spell)!= spell, endTime_Fixed - gap, -12345 ))) %>%
    group_by(participant, session_number, spell) %>% 
    mutate(
      spell_start = ifelse(
        
        spell_start_raw == -12345, startTime_Fixed-first(gap),
        spell_start_raw),
      spell_end = ifelse(spell_end_raw == -12345, 
                         endTime_Fixed-first(gap), spell_end_raw))%>%
    mutate(spell_start = ifelse(is.na(spell_start), startTime_Fixed, spell_start),
           spell_end = ifelse(is.na(spell_end), endTime_Fixed, spell_end)) 
  
  
  df12 <- df12 %>% 
       ungroup()%>%
    select( -gap, -spell, -spell_start_raw, -spell_end_raw) 
  
  
  
  
  write.csv(df12, "8pauzespersessiegefilterd.csv")

  
  ########### sessie (1tje) met 10 pauzes ( we hadden geen van 9)  
  
  
  
  
  
  
  pauzes10 <- restvandata%>%
    filter(Npauzes == 10)
  
  
  
  
  ## hieronder de filterfunctie
  df5 <- pauzes10 %>% 
    group_by(participant, session_number) %>% 
    arrange(id, .by_group = TRUE) %>% 
    mutate(gap = startTime_Fixed - lag(endTime_Fixed),
           
           spell = cumsum(c(1, ifelse(diff(startTime_Fixed)>300000, 1, 0))),
           spell_start_raw = ifelse(is.na(gap), startTime_Fixed, 
                                    ifelse(lag(spell)!= spell, lag(endTime_Fixed), -12345 )),
           
           spell_end_raw = ifelse(is.na(gap), endTime_Fixed, 
                                  ifelse(lag(spell)!= spell, endTime_Fixed - gap, -12345 ))) %>%
    group_by(participant, session_number, spell) %>% 
    mutate(
      spell_start = ifelse(
        
        spell_start_raw == -12345, startTime_Fixed-first(gap),
        spell_start_raw),
      spell_end = ifelse(spell_end_raw == -12345, 
                         endTime_Fixed-first(gap), spell_end_raw))%>%
    mutate(spell_start = ifelse(is.na(spell_start), startTime_Fixed, spell_start),
           spell_end = ifelse(is.na(spell_end), endTime_Fixed, spell_end)) 
  
  
  # clean up auxiliary variables
  df5 <- df5 %>% 
    mutate(startTime_Fixed = spell_start,
           endTime_Fixed = spell_end)%>%
    ungroup()%>%
    select( -gap, -spell, -spell_start_raw, -spell_end_raw) 
  
  ## en dan 2e keer bovenstaande filter uitvoeren (hier geplakt)
  
  df6 <- df5 %>% 
    group_by(participant, session_number) %>% 
    arrange(id, .by_group = TRUE) %>% 
    mutate(gap = startTime_Fixed - lag(endTime_Fixed),
           
           spell = cumsum(c(1, ifelse(diff(startTime_Fixed)>300000, 1, 0))),
           spell_start_raw = ifelse(is.na(gap), startTime_Fixed, 
                                    ifelse(lag(spell)!= spell, lag(endTime_Fixed), -12345 )),
           
           spell_end_raw = ifelse(is.na(gap), endTime_Fixed, 
                                  ifelse(lag(spell)!= spell, endTime_Fixed - gap, -12345 ))) %>%
    group_by(participant, session_number, spell) %>% 
    mutate(
      spell_start = ifelse(
        
        spell_start_raw == -12345, startTime_Fixed-first(gap),
        spell_start_raw),
      spell_end = ifelse(spell_end_raw == -12345, 
                         endTime_Fixed-first(gap), spell_end_raw))%>%
    mutate(spell_start = ifelse(is.na(spell_start), startTime_Fixed, spell_start),
           spell_end = ifelse(is.na(spell_end), endTime_Fixed, spell_end)) 
  
  
  df6 <- df6 %>%
    mutate(startTime_Fixed = spell_start,
           endTime_Fixed = spell_end)%>%
    ungroup()%>%
    select( -gap, -spell, -spell_start_raw, -spell_end_raw) 
  
  ## derde keer deze groep sessies door het filter halen
  
  
  df7 <- df6 %>% 
    group_by(participant, session_number) %>% 
    arrange(id, .by_group = TRUE) %>% 
    mutate(gap = startTime_Fixed - lag(endTime_Fixed),
           
           spell = cumsum(c(1, ifelse(diff(startTime_Fixed)>300000, 1, 0))),
           spell_start_raw = ifelse(is.na(gap), startTime_Fixed, 
                                    ifelse(lag(spell)!= spell, lag(endTime_Fixed), -12345 )),
           
           spell_end_raw = ifelse(is.na(gap), endTime_Fixed, 
                                  ifelse(lag(spell)!= spell, endTime_Fixed - gap, -12345 ))) %>%
    group_by(participant, session_number, spell) %>% 
    mutate(
      spell_start = ifelse(
        
        spell_start_raw == -12345, startTime_Fixed-first(gap),
        spell_start_raw),
      spell_end = ifelse(spell_end_raw == -12345, 
                         endTime_Fixed-first(gap), spell_end_raw))%>%
    mutate(spell_start = ifelse(is.na(spell_start), startTime_Fixed, spell_start),
           spell_end = ifelse(is.na(spell_end), endTime_Fixed, spell_end)) 
  
  
  df7 <- df7 %>%
    mutate(startTime_Fixed = spell_start,
           endTime_Fixed = spell_end)%>%
    ungroup()%>%
    select( -gap, -spell, -spell_start_raw, -spell_end_raw) 
  
  ## vierde keer
  
  
  
  df8 <- df7 %>% 
    group_by(participant, session_number) %>% 
    arrange(id, .by_group = TRUE) %>% 
    mutate(gap = startTime_Fixed - lag(endTime_Fixed),
           
           spell = cumsum(c(1, ifelse(diff(startTime_Fixed)>300000, 1, 0))),
           spell_start_raw = ifelse(is.na(gap), startTime_Fixed, 
                                    ifelse(lag(spell)!= spell, lag(endTime_Fixed), -12345 )),
           
           spell_end_raw = ifelse(is.na(gap), endTime_Fixed, 
                                  ifelse(lag(spell)!= spell, endTime_Fixed - gap, -12345 ))) %>%
    group_by(participant, session_number, spell) %>% 
    mutate(
      spell_start = ifelse(
        
        spell_start_raw == -12345, startTime_Fixed-first(gap),
        spell_start_raw),
      spell_end = ifelse(spell_end_raw == -12345, 
                         endTime_Fixed-first(gap), spell_end_raw))%>%
    mutate(spell_start = ifelse(is.na(spell_start), startTime_Fixed, spell_start),
           spell_end = ifelse(is.na(spell_end), endTime_Fixed, spell_end)) 
  
  
  df8 <- df8 %>%
    mutate(startTime_Fixed = spell_start,
           endTime_Fixed = spell_end)%>%
    ungroup()%>%
    select( -gap, -spell, -spell_start_raw, -spell_end_raw) 
  
  
  
  ### vijfde keer door het filter 
  
  df9 <- df8 %>% 
    group_by(participant, session_number) %>% 
    arrange(id, .by_group = TRUE) %>% 
    mutate(gap = startTime_Fixed - lag(endTime_Fixed),
           
           spell = cumsum(c(1, ifelse(diff(startTime_Fixed)>300000, 1, 0))),
           spell_start_raw = ifelse(is.na(gap), startTime_Fixed, 
                                    ifelse(lag(spell)!= spell, lag(endTime_Fixed), -12345 )),
           
           spell_end_raw = ifelse(is.na(gap), endTime_Fixed, 
                                  ifelse(lag(spell)!= spell, endTime_Fixed - gap, -12345 ))) %>%
    group_by(participant, session_number, spell) %>% 
    mutate(
      spell_start = ifelse(
        
        spell_start_raw == -12345, startTime_Fixed-first(gap),
        spell_start_raw),
      spell_end = ifelse(spell_end_raw == -12345, 
                         endTime_Fixed-first(gap), spell_end_raw))%>%
    mutate(spell_start = ifelse(is.na(spell_start), startTime_Fixed, spell_start),
           spell_end = ifelse(is.na(spell_end), endTime_Fixed, spell_end)) 
  
  
  # clean up auxiliary variables
  df9 <- df9 %>% 
    mutate(startTime_Fixed = spell_start,
           endTime_Fixed = spell_end)%>%
    ungroup()%>%
    select( -gap, -spell, -spell_start_raw, -spell_end_raw) 
  
  ## zesde keer
  
  df10 <- df9 %>% 
    group_by(participant, session_number) %>% 
    arrange(id, .by_group = TRUE) %>% 
    mutate(gap = startTime_Fixed - lag(endTime_Fixed),
           
           spell = cumsum(c(1, ifelse(diff(startTime_Fixed)>300000, 1, 0))),
           spell_start_raw = ifelse(is.na(gap), startTime_Fixed, 
                                    ifelse(lag(spell)!= spell, lag(endTime_Fixed), -12345 )),
           
           spell_end_raw = ifelse(is.na(gap), endTime_Fixed, 
                                  ifelse(lag(spell)!= spell, endTime_Fixed - gap, -12345 ))) %>%
    group_by(participant, session_number, spell) %>% 
    mutate(
      spell_start = ifelse(
        
        spell_start_raw == -12345, startTime_Fixed-first(gap),
        spell_start_raw),
      spell_end = ifelse(spell_end_raw == -12345, 
                         endTime_Fixed-first(gap), spell_end_raw))%>%
    mutate(spell_start = ifelse(is.na(spell_start), startTime_Fixed, spell_start),
           spell_end = ifelse(is.na(spell_end), endTime_Fixed, spell_end)) 
  
  
  # clean up auxiliary variables
  df10 <- df10 %>% 
    mutate(startTime_Fixed = spell_start,
           endTime_Fixed = spell_end)%>%
    ungroup()%>%
    select( -gap, -spell, -spell_start_raw, -spell_end_raw) 
  
  
  ## zevende keer door filter
  
  df11 <- df10 %>% 
    group_by(participant, session_number) %>% 
    arrange(id, .by_group = TRUE) %>% 
    mutate(gap = startTime_Fixed - lag(endTime_Fixed),
           
           spell = cumsum(c(1, ifelse(diff(startTime_Fixed)>300000, 1, 0))),
           spell_start_raw = ifelse(is.na(gap), startTime_Fixed, 
                                    ifelse(lag(spell)!= spell, lag(endTime_Fixed), -12345 )),
           
           spell_end_raw = ifelse(is.na(gap), endTime_Fixed, 
                                  ifelse(lag(spell)!= spell, endTime_Fixed - gap, -12345 ))) %>%
    group_by(participant, session_number, spell) %>% 
    mutate(
      spell_start = ifelse(
        
        spell_start_raw == -12345, startTime_Fixed-first(gap),
        spell_start_raw),
      spell_end = ifelse(spell_end_raw == -12345, 
                         endTime_Fixed-first(gap), spell_end_raw))%>%
    mutate(spell_start = ifelse(is.na(spell_start), startTime_Fixed, spell_start),
           spell_end = ifelse(is.na(spell_end), endTime_Fixed, spell_end)) 
  
  
  df11 <- df11 %>% 
    mutate(startTime_Fixed = spell_start,
           endTime_Fixed = spell_end)%>%
    ungroup()%>%
    select( -gap, -spell, -spell_start_raw, -spell_end_raw) 
  
  
  
  ### achtste 
  
  
  df12 <- df11 %>% 
    group_by(participant, session_number) %>% 
    arrange(id, .by_group = TRUE) %>% 
    mutate(gap = startTime_Fixed - lag(endTime_Fixed),
           
           spell = cumsum(c(1, ifelse(diff(startTime_Fixed)>300000, 1, 0))),
           spell_start_raw = ifelse(is.na(gap), startTime_Fixed, 
                                    ifelse(lag(spell)!= spell, lag(endTime_Fixed), -12345 )),
           
           spell_end_raw = ifelse(is.na(gap), endTime_Fixed, 
                                  ifelse(lag(spell)!= spell, endTime_Fixed - gap, -12345 ))) %>%
    group_by(participant, session_number, spell) %>% 
    mutate(
      spell_start = ifelse(
        
        spell_start_raw == -12345, startTime_Fixed-first(gap),
        spell_start_raw),
      spell_end = ifelse(spell_end_raw == -12345, 
                         endTime_Fixed-first(gap), spell_end_raw))%>%
    mutate(spell_start = ifelse(is.na(spell_start), startTime_Fixed, spell_start),
           spell_end = ifelse(is.na(spell_end), endTime_Fixed, spell_end)) 
  
  
  df12 <- df12 %>% 
    ungroup()%>%
    mutate(startTime_Fixed = spell_start,
           endTime_Fixed = spell_end)%>%
    select( -gap, -spell, -spell_start_raw, -spell_end_raw) 
  
  
  ### negende keer 
  
  df13 <- df12 %>% 
    group_by(participant, session_number) %>% 
    arrange(id, .by_group = TRUE) %>% 
    mutate(gap = startTime_Fixed - lag(endTime_Fixed),
           
           spell = cumsum(c(1, ifelse(diff(startTime_Fixed)>300000, 1, 0))),
           spell_start_raw = ifelse(is.na(gap), startTime_Fixed, 
                                    ifelse(lag(spell)!= spell, lag(endTime_Fixed), -12345 )),
           
           spell_end_raw = ifelse(is.na(gap), endTime_Fixed, 
                                  ifelse(lag(spell)!= spell, endTime_Fixed - gap, -12345 ))) %>%
    group_by(participant, session_number, spell) %>% 
    mutate(
      spell_start = ifelse(
        
        spell_start_raw == -12345, startTime_Fixed-first(gap),
        spell_start_raw),
      spell_end = ifelse(spell_end_raw == -12345, 
                         endTime_Fixed-first(gap), spell_end_raw))%>%
    mutate(spell_start = ifelse(is.na(spell_start), startTime_Fixed, spell_start),
           spell_end = ifelse(is.na(spell_end), endTime_Fixed, spell_end)) 
  
  
  df13 <- df13 %>% 
    ungroup()%>%
    mutate(startTime_Fixed = spell_start,
           endTime_Fixed = spell_end)%>%
    select( -gap, -spell, -spell_start_raw, -spell_end_raw) 
  
  
  ## 10e en laatste keer
  
  df14 <- df13 %>% 
    group_by(participant, session_number) %>% 
    arrange(id, .by_group = TRUE) %>% 
    mutate(gap = startTime_Fixed - lag(endTime_Fixed),
           
           spell = cumsum(c(1, ifelse(diff(startTime_Fixed)>300000, 1, 0))),
           spell_start_raw = ifelse(is.na(gap), startTime_Fixed, 
                                    ifelse(lag(spell)!= spell, lag(endTime_Fixed), -12345 )),
           
           spell_end_raw = ifelse(is.na(gap), endTime_Fixed, 
                                  ifelse(lag(spell)!= spell, endTime_Fixed - gap, -12345 ))) %>%
    group_by(participant, session_number, spell) %>% 
    mutate(
      spell_start = ifelse(
        
        spell_start_raw == -12345, startTime_Fixed-first(gap),
        spell_start_raw),
      spell_end = ifelse(spell_end_raw == -12345, 
                         endTime_Fixed-first(gap), spell_end_raw))%>%
    mutate(spell_start = ifelse(is.na(spell_start), startTime_Fixed, spell_start),
           spell_end = ifelse(is.na(spell_end), endTime_Fixed, spell_end)) 
  
  
  df14 <- df14 %>% 
    ungroup()%>%
      select( -gap, -spell, -spell_start_raw, -spell_end_raw) 
  
  
  
  write.csv(df14, "10pauzespersessiegefilterd.csv")
  
  
  ############### alles opgeschoond, nu de tabellen weer samenvoegen ##
  
  ## geen pauzes 
  
  countpauzespersessie
  
  allesessies <- pauzesEruit%>%
    group_by(participant, session_number)%>%
    summarise( lines = n())
  
  view(allesessies)
  
  a1 <- select(allesessies, -lines)
  a2 <- select(countpauzespersessie, -Npauzes)
 
  ## dplyr Basically, setdiff(bigFrame, smallFrame) gets you the extra records in the first table. 
  
   a3 <-  setdiff(a1, a2)
  
  zonderpauzes <- a3 %>%
    mutate (pauzestatus = 'zonderpauzes')
  
  
  metpauzes <- a2 %>%
    mutate(pauzestatus = 'metpauzes')
  
  view(zonderpauzes)
  view(metpauzes)
  
  #merge deed het niet..
  
 lijstje <- rbind(metpauzes,zonderpauzes)
  

 lijsjegrouped <- group_by(lijstje, participant, session_number)
 view(lijsjegrouped)
  
  grotetabel <- pauzesEruit%>%
    group_by(participant, session_number)%>%
   left_join(lijsjegrouped)
  
  sessieszonderpauzes <- grotetabel%>%
    filter(pauzestatus == 'zonderpauzes')%>%
    mutate(spell_start = startTime_Fixed,
           spell_end = endTime_Fixed)
  
  write.csv(sessieszonderpauzes, 'sessieszonderpauzes.csv')
  
  
  pauze1 <- read.csv('pauzegefilterd1pauzepersessie.csv', stringsAsFactors = F)
  pauze2 <- read.csv('2pauzespersessiegefilterd.csv', stringsAsFactors = F)
  pauze3 <- read.csv('3pauzespersessiegefilterd.csv', stringsAsFactors = F)
  pauze4 <- read.csv('4pauzespersessiegefilterd.csv', stringsAsFactors = F)
  pauze5 <- read.csv('5pauzespersessiegefilterd.csv', stringsAsFactors = F)
  pauze6 <- read.csv('6pauzespersessiegefilterd.csv', stringsAsFactors = F)
  pauze8 <- read.csv('8pauzespersessiegefilterd.csv', stringsAsFactors = F)
  pauze10 <- read.csv('10pauzespersessiegefilterd.csv', stringsAsFactors = F)
  
  pauzefilterdata <- rbind(sessieszonderpauzes,pauze1, pauze2, pauze3, pauze4, pauze5, pauze6, pauze8, pauze10)
  
  pauzefilterdata <- pauzefilterdata%>%
    select( -pauzestatus, -X.1, -Npauzes)
  
  write.csv(pauzefilterdata, 'pauzefiltereddata.csv', row.names = F)

 

 summary(pauzefilterdata)  
  
 checkje <- pauzefilterdata %>%
   filter(participant == 'marie', session_number == 1)

 view(checkje)  
 