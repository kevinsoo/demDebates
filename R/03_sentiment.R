# Load libraries
library(tidyverse)
library(tidytext)
library(widyr)
library(tidyr)
library(ggrepel)
library(cowplot)

# Load data
load(file = "demDebate1_2019-06-26_27.Rda")

# Get data in tidy format
tidyDem <- demDeb %>% 
      filter(!Type %in% c("Moderator", "Unknown", "Announcer")) %>% 
      mutate(Top = ifelse(Speaker %in% c("Biden", "Sanders", "Warren", "Harris", "Buttigieg", "O'Rourke", "Booker", "Klobuchar"), Speaker, "Rest of field")) %>% 
      unnest_tokens(word, Speech) %>% 
      anti_join(stop_words, by = "word")

# Peek at most common words
tidyDem %>% 
      count(word, sort = TRUE)

# Word counts over time
# demDeb %>% 
#       filter(!Type %in% c("Moderator", "Unknown", "Announcer")) %>% 
#       mutate(Top = ifelse(Speaker %in% c("Biden", "Sanders", "Warren", "Harris", "Buttigieg", "O'Rourke", "Booker", "Klobuchar"), Speaker, "Rest of field")) %>%
#       filter(Top != "Rest of field") %>% 
#       group_by(Debate) %>% 
#       mutate(n = row_number()) %>% 
#       ungroup() %>% 
#       unnest_tokens(word, Speech) %>% 
#       group_by(Debate, Speaker, Top, n) %>% 
#       count(name = "Words") %>% 
#       ungroup() %>% 
#       group_by(Speaker) %>% 
#       mutate(Total = cumsum(Words)) %>% 
#       ggplot(aes(x = n, y = Total)) +
#       geom_path() +
#       facet_grid(~ Debate, scales = "free_x")

# Pos vs. Neg sentiment
demSen <- tidyDem %>%
      inner_join(get_sentiments("bing")) %>%
      count(Speaker, Top, sentiment) %>%
      spread(sentiment, n, fill = 0) %>%
      mutate(sentiment = positive - negative)

# Plot
demSen %>% 
      mutate(Words = negative + positive,
             label = ifelse(sentiment == 11, "Klobuchar", 
                            ifelse(sentiment == 9, "Harris",
                                   ifelse(sentiment == -15, "Harris", NA)))) %>%
      group_by(Debate) %>% 
      mutate(n = row_number()) %>% 
      ggplot(aes(x = n, y = sentiment)) +
      geom_hline(yintercept = 0, linetype = "dashed") +
      geom_point(alpha = .8, aes(color = sentiment)) +
      geom_text_repel(aes(label = label), size = 3) +
      facet_grid(~ Debate, scales = "free_x") +
      stat_smooth(color = "dodgerblue2") +
      scale_color_viridis_c(guide = FALSE) +
      theme_minimal() +
      theme(axis.text.x = element_blank()) +
      labs(x = NULL, y = "Sentiment", 
           title = "Speech sentiment at the Dem Debates (June 2019)",
           caption = "\nNote: Each observation represents a speaking turn")

# Outliers
(demDeb %>% filter(ID == 282, Speaker == "Harris"))$Speech
(demDeb %>% filter(ID == 20, Speaker == "Harris"))$Speech
(demDeb %>% filter(ID == 386, Speaker == "Klobuchar"))$Speech

# Continuous sentiment
demSen <- tidyDem %>%
      inner_join(get_sentiments("afinn"))

# Plot
n1 <- demSen %>% 
      filter(Top != "Rest of field", Debate == "Night 1") %>% 
      group_by(Top) %>% 
      mutate(n = row_number(),
             Speaker = factor(Speaker, levels = c("Warren", "O'Rourke", "Booker", "Klobuchar"))) %>% 
      ggplot(aes(x = n, y = score)) +
      facet_grid(~ Speaker, scales = "free_x") +
      geom_hline(yintercept = 0, linetype = "dashed") +
      geom_point(alpha = .7, aes(color = score)) +
      stat_smooth(color = "dodgerblue2") +
      scale_color_viridis_c(guide = FALSE) +
      theme_minimal() +
      theme(axis.text.x = element_blank()) +
      scale_y_continuous(limits = c(-4, 4)) +
      labs(x = NULL, y = "Sentiment", 
           title = "Speech sentiment for top candidates",
           subtitle = "Night 1")
n2 <- demSen %>% 
      filter(Top != "Rest of field", Debate == "Night 2") %>% 
      group_by(Top) %>% 
      mutate(n = row_number(),
             Speaker = factor(Speaker, levels = c("Biden", "Sanders", "Harris", "Buttigieg"))) %>% 
      ggplot(aes(x = n, y = score)) +
      facet_grid(~ Speaker, scales = "free_x") +
      geom_hline(yintercept = 0, linetype = "dashed") +
      geom_point(alpha = .7, aes(color = score)) +
      stat_smooth(color = "dodgerblue2") +
      scale_color_viridis_c(guide = FALSE) +
      theme_minimal() +
      theme(axis.text.x = element_blank()) +
      scale_y_continuous(limits = c(-4, 4)) +
      labs(x = NULL, y = "Sentiment", 
           title = NULL,
           caption = "\nNote: Each observation represents a word",
           subtitle = "Night 2")

plot_grid(n1, n2, nrow = 2)
