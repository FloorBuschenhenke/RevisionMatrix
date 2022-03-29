# testdata Floor
# Author: richard.zijdeman@iisg.nl
# Date: Apr 14, 2021
# Last change: -

# setup
#setwd("~/Downloads/") # change to directory where your .csv file resides

# libraries
library(tidyverse)

# testdatafloor3.csv is met een tweede pauze in de eerste sessie 
# die wordt nog 'overgeslagen' door de code
# testdatafloor2 is met 1 pauze per sessie, daar werkt de code al helemaal ok voor de
# aanpassing van de starttijd

# testdatafloor 4 is met een langere tweede pauze in de eerste sessie 


# read data
df <- read_delim("langepauzesFilter/testdatafloor4.csv", ";", na="na")

# clean data
df <- rename(df, rowID = X1)

# creating 'corrected' start times to annihilate 'breaks'

df3 <- df %>% 
  group_by(participant) %>% 
  select(-duur_van_tussentijd_vorige_event_dit_event, -starttime_zoalshijzoumoeten)%>%
  arrange(id, .by_group = TRUE) %>% 
  mutate(gap = starttime_nu - lag(endtime),
        
  spell = cumsum(c(1, ifelse(diff(starttime_nu)>2000, 1, 0))),
  spell_start_raw = ifelse(is.na(gap), starttime_nu, 
                                  ifelse(lag(spell)!= spell, lag(endtime), -12345 )),

  spell_end_raw = ifelse(is.na(gap), endtime, 
                         ifelse(lag(spell)!= spell, endtime - gap, -12345 ))) %>%
 group_by(participant, spell) %>% 
 mutate(
   spell_start = ifelse(
   
     spell_start_raw == -12345, starttime_nu-first(gap),
                             spell_start_raw),
        spell_end = ifelse(spell_end_raw == -12345, 
                          endtime-first(gap), spell_end_raw))%>%
  mutate(spell_start = ifelse(is.na(spell_start), starttime_nu, spell_start),
         spell_end = ifelse(is.na(spell_end), endtime, spell_end)) 
   
view(df3)

### hoe benaderen bij meer dan 1 pauze? (zelfde circus nog eens?)



df4 <- df3%>%
  mutate(starttime_nu = spell_start,
         endtime = spell_end)%>%
  ungroup()%>%
  select(-gap, -spell, -spell_start_raw, -spell_end_raw, -spell_start, -spell_end) 
view(df4)

df5 <- df4 %>% 
  group_by(participant) %>% 
  arrange(id, .by_group = TRUE) %>% 
  mutate(gap = starttime_nu - lag(endtime),
         
         spell = cumsum(c(1, ifelse(diff(starttime_nu)>2000, 1, 0))),
         spell_start_raw = ifelse(is.na(gap), starttime_nu, 
                                  ifelse(lag(spell)!= spell, lag(endtime), -12345 )),
         
         spell_end_raw = ifelse(is.na(gap), endtime, 
                                ifelse(lag(spell)!= spell, endtime - gap, -12345 ))) %>%
  group_by(participant, spell) %>% 
  mutate(
    spell_start = ifelse(
      
      spell_start_raw == -12345, starttime_nu-first(gap),
      spell_start_raw),
    spell_end = ifelse(spell_end_raw == -12345, 
                       endtime-first(gap), spell_end_raw))%>%
  mutate(spell_start = ifelse(is.na(spell_start), starttime_nu, spell_start),
         spell_end = ifelse(is.na(spell_end), endtime, spell_end)) 

view(df5)


## ja, dat werkt voor de tweede pauze ! 
#lelijk, maar swah









### rare loops doen het niet in mutate


df4 <- ungroup(df3)
 df4$spell_start <- ifelse(spell == 3 & spell != lag(spell), lag(spell_end), spell_start)

view(df4)

df5 <- df4 %>%
  mutate(spell_end = ifelse(spell == 3 & spell != lag(spell), (endtime-starttime_nu)+spell_start, spell_end))

df6 <- df5 %>%
mutate(spell_start = ifelse(spell == lag(spell) & spell == 3, lag(spell_end)+gap, spell_start))

view(df6)

df7 <- df6 %>%
mutate(spell_end = ifelse(spell == lag(spell) & spell == 3, (endtime-starttime_nu)+spell_start, spell_end))

 
  view(df7)     
 
  
  
    

# clean up auxiliary variables
 df3 <- df3 %>% 
   ungroup()%>%
   select(-gap, -spell, -spell_start_raw, -spell_end_raw, -spell_start, spell_end) 


##uitprobeersels
 ### hoe benaderen bij meer dan 1 pauze? (zelfde circus nog eens?)
 
 df4 <- ungroup(df3) %>%
   mutate(spell_start = ifelse(spell == 3 & spell != lag(spell), lag(spell_end), spell_start))
 
 df5 <- df4 %>%
   mutate(spell_end = ifelse(spell == 3 & spell != lag(spell), (endtime-starttime_nu)+spell_start, spell_end))
 
 df6 <- df5 %>%
   mutate(spell_start = ifelse(spell == lag(spell) & spell == 3, lag(spell_end)+gap, spell_start))
 
 view(df6)
 
 df7 <- df6 %>%
   mutate(spell_end = ifelse(spell == lag(spell) & spell == 3, (endtime-starttime_nu)+spell_start, spell_end))
 
 #  spell_end = ifelse(spell == 3 & spell == lag(spell), (endtime-starttime_nu)+spell_start, spell_end))
 
 
 #okee, dit werkt. even testen met langere 2e pauze (meer dan 2 regels)
 # pakt nog niet de 3e regel van spell 3 mee.. #
 
 view(df4)
 
 ##zelfde probleem, pakt alleen eerste regel
 df5 <- df4 %>%
   filter(spell == 3) %>%
   mutate(spell_start = ifelse(spell==lag(spell),lag(spell_end)+gap, spell_start))
 
 view(df5)  

