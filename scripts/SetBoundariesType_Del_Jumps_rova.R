# load packages
library(tidyverse)

# no scientific notion
options(scipen = 999)

##########----------- Load data -----------------------------------------#######

## zoek en vervang de naam van de schrijver om het op een andere schrijver te draaien
# alles consistent gelabeld nl.
## 



all_dataX <- read.csv("data/data_all_Rova.csv", stringsAsFactors = F)
#colnames(all_dataX)

#deze input heeft row names (kolom X.1)is niet de bedoeling
all_data <- all_dataX %>%
  select(-X)

##########------------ Add info -----------------------------------------#######
arrow_keys <- c("UP", "DOWN", "LEFT", "RIGHT", "END", "HOME", "PAGE_DOWN",
                "PAGE_UP", "PRIOR", "NEXT")


# check all possible keystrokes 
keys <- all_data %>% filter(type =="keyboard")
keyst <- data.frame(output = unique(keys$output), 
                    stringsAsFactors = F) 

keyst_add <- keyst %>%
  mutate(length = nchar(as.character(output)),
         ##### deze zou de LCTRL als function key moeten labelen want length > 3
         # klopt, doet ie
         keytype = ifelse((length < 3 & output != "UP") | 
                         grepl("OEM_", output), "visible_char",
                ifelse(output %in% c("SPACE", "RETURN", "TAB"),
                       "whitespace",
                       # hier ook prior en next toegevoegd
                ifelse(grepl("UP|DOWN|LEFT|RIGHT|END|HOME|PRIOR|NEXT", output),
                       "arrow_key",
                ifelse(output %in% c("BACK", "DELETE"),
                       "delete_key",
                "function_key")))))


 #view(keyst_add)
 
 ##### hier met startTime en endTime - op ruwe GA's, pauzes zijn er niet
## uitgefilterd

 
# add boundaries for non-linearity
data_add <- all_data %>%
  # remove keystrokes outside doc (e.g., save as XXX)
  filter(!(type == "keyboard" & is.na(positionFull))) %>%
  
  left_join(keyst_add) %>%
  
  # calculate per session/file separately
  group_by(participant, session_number) %>%
  mutate(jump_start = ifelse(
    row_number() == 1 |
    #1)	When a typist moves from typing a character to a mouse event 
    #   (click, movement, scroll, selection), or vice versa.
    (type == "keyboard" & 
       (keytype %in% c("visible_char", "whitespace") | 
          output == "CAPS LOCK" |
                    (keytype == "function_key" & 
                     pauseLocationFull == "COMBINATION KEY"))  &
      (lag(type) == "mouse" | type == "replacement")) | 
      ((type == "mouse" | lag(type) == "replacement")  &
      lag(type) == "keyboard" & 
         (lag(keytype) %in% c("visible_char", "whitespace")) |
        output == "CAPS LOCK") |
    #2) When a typists moves from an insertion to another event, or vice versa.
    (lag(type) %in% c("insert") & 
       type != lag(type) |
      (type %in% c("insert")  &
         lag(type) != type)) |
    #3)	When a typist moves from typing a character to typing an arrow key, 
    #   or vice versa.
      (type == "keyboard" & 
         (keytype %in% c("visible_char", "whitespace") |
            output == "CAPS LOCK" |
            # dus LCTRL + S is een typing event hier... dat is hoe we het opgezet hebben
            (pauseLocationFull == "COMBINATION KEY")) &
         # hier bij arrowkey type in keytype verandert
         lag(type) == "keyboard" & lag(keytype) == "arrow_key") | 
      (type == "keyboard" & keytype == "arrow_key"
       & (lag(type) == "keyboard" & 
            # keytype ipv type in regel hieronder
         lag(keytype) %in% c("visible_char", "whitespace"))  | 
         lag(type) == "replacement")  |
      #deze toegevoegd 
               lag(keytype == "function_key" & pauseLocationFull == "COMBINATION KEY")|
    #4)	When a typists moves from a keystroke or mouse event to a 
    #   delete/backspace keypress, or vice versa.
      (type %in% c("mouse","keyboard", "insert") & 
         (is.na(keytype) | keytype != "delete_key") &
         lag(type) == "keyboard" & lag(keytype) == "delete_key") | 
      (type == "keyboard" & keytype == "delete_key"
       & lag(type) %in% c("mouse","keyboard", "insert") & 
         !lag(output) %in% c("DELETE", "BACK")) |  
    #5)	When a typist moves from one mode of deletion to another (e.g., from 
    #   delete key to backspace key press).
      (keytype == "delete_key" & lag(keytype) == "delete_key" & 
      (output) != lag(output)) |
    #6)	When a typist moves from the main text to a different source 
    #   (e.g., online dictionary)
      (lag(type) %in% c("focus") & 
         type != lag(type)) |
         (type %in% c("focus") & 
            type != lead(type)) , 1, 0),
    
    # set to zero if selection is directly followed by insert/delete 
    # (series of deletions count as one event)
    #delete - replacement
    jump_start = ifelse((type == "replacement" &
                            lag(output) == "DELETE" & 
                            startTime == lag(startTime) &
                            endTime == lag(endTime))
                         | (lag(type) == "replacement" &
                              output == "DELETE" &
                                lead(type) == "replacement"  & 
                          lag(output,2) == "DELETE" ) |
                          (type == "replacement" &
                          lag(type) == "replacement") |
                          is.na(jump_start), 0, jump_start),
    # create count number of linear event
    jump_number = ifelse(jump_start == 1,
                       cumsum(jump_start == 1 |
                                row_number() == 1), NA),
      prev_loc = lag(pauseLocationFull),
      next_loc = lead(pauseLocationFull),
      endTime_session = max(endTime)
    ) %>%
  fill(jump_number) 
  

## in die kolom de 0 vervangen door NA > 
data_add$charProduction[data_add$charProduction == 0] <- NA  

#view(head(data_add))

write.csv(data_add, "data/data_add.csv", row.names = F)

#names(data_add)
##########------------ summarize jump events --------------------------#######


#glimpse(data_add)

# summary statistics for each jump event

jump_add <- data_add %>%
  group_by(participant, session_number, jump_number) %>%
  # nested ifelsen deden het niet, dan maar zo
  # als er een function key staat, vervang door niks, bij SPACE etc vervangen door spatie
  mutate (output2 = ifelse(keytype == "function_key", "", output),
          output3 = ifelse(keytype == "whitespace", " ", output2))%>%
  summarize(
    ### 
    outputs = paste(output3, collapse = ""),
    char_count = length(which(keytype == "visible_char" | keytype == "whitespace")),
    action_types = paste(unique(type), collapse = ", "),
    key_types = paste(unique(keytype), collapse = ", ") ) %>%
  mutate(
    jump_type = ifelse(grepl("delete_key", key_types), "delete",
                       ifelse(grepl("focus", action_types), "focus",
                              ifelse(action_types == "insert", "insert",
                                     ifelse(action_types %in% c("keyboard", "keyboard, replacement") &
                                              grepl("visible_char|whitespace", key_types), "typing",
                                            "jump"))))
  )

#view(head(jump_add, 100))
#names(jump_add)
write.csv(jump_add, "data/jump_add.csv", row.names = F)

#view(jump_add)

