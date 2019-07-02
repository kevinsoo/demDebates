# Load libraries
library(textreadr)
library(tidyverse)

# Load transcripts
# I've cleaned out lines with (CROSSTALK) in a text editor
deb1 <- "DemDebate1_Night1.txt" %>%
   read_document()
deb2 <- "DemDebate1_Night2.txt" %>%
   read_document()

# Get into tibbles for cleaning
df1 <- tibble(tmp = deb1,
              start = ifelse(str_detect(tmp, ":") == TRUE, 1, 0),
              Debate = "Night 1")
df2 <- tibble(tmp = deb2,
             start = ifelse(str_detect(tmp, ":") == TRUE, 1, 0),
             Debate = "Night 2")

# Check if lines indicating names are correct
# df1 %>%
#       filter(start == 1) %>%
#       group_by(tmp) %>%
#       count() %>%
#       arrange(n) %>% View
# df2 %>%
#    filter(start == 1) %>%
#    group_by(tmp) %>%
#    count() %>%
#    arrange(n) %>% View

# Group utterances into single row, get name of speaker into column
df1 <- df1 %>% 
   mutate(ID = cumsum(start)) %>% 
   group_by(ID) %>% 
   mutate(Speaker = str_to_title(head(tmp, 1)),
          Speaker = str_remove(Speaker, ":"),
          Speaker = str_remove(Speaker, "\\("),
          Speaker = str_remove(Speaker, "\\)")) %>% 
   filter(start == 0) %>% 
   select(-start) %>% 
   group_by(Debate, ID, Speaker) %>% 
   summarise(Speech = str_c(tmp, collapse = " ")) %>% 
   arrange(ID)
df2 <- df2 %>% 
   mutate(ID = cumsum(start)) %>% 
   group_by(ID) %>% 
   mutate(Speaker = str_to_title(head(tmp, 1)),
          Speaker = str_remove(Speaker, ":"),
          Speaker = str_remove(Speaker, "\\("),
          Speaker = str_remove(Speaker, "\\)")) %>% 
   filter(start == 0) %>% 
   select(-start) %>% 
   group_by(Debate, ID, Speaker) %>% 
   summarise(Speech = str_c(tmp, collapse = " ")) %>% 
   arrange(ID)

# Combine data across debates
demDeb <- df1 %>% 
   bind_rows(df2) %>% 
   mutate(Speaker = ifelse(Speaker == "O'rourke", "O'Rourke", Speaker),
          Type = ifelse(Speaker %in% c("Diaz-Balart", "Guthrie", "Holt", "Maddow", "Todd"), "Moderator", Speaker)) %>% 
   select(Debate, ID, Type, Speaker, Speech)

# Save data
save(demDeb, file = "demDebate1_2019-06-26_27.Rda")
