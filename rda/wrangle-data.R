library(tidyverse)
library(dslabs)
data("polls_us_election_2016")
head(results_us_election_2016)

Polls_init <- read.csv("data/president_polls.csv")
poll_filter <- Polls_init %>% filter(candidate_name %in% c("Donald Trump","Joseph R. Biden Jr.") & (fte_grade %in% c("A+", "A", "A-", "B+") | is.na(fte_grade))) %>% 
  select("question_id","state","pollster","fte_grade","sample_size","population","start_date","end_date","candidate_name","pct") %>%
  group_by(question_id)
head(poll2020)
trump2020 <- poll_filter %>% rowid_to_column() %>%spread(candidate_name , pct) %>% select( -rowid , -`Joseph R. Biden Jr.`) %>% filter(!is.na(`Donald Trump`))
joe_biden2020  <- poll_filter%>% rowid_to_column()%>%spread(candidate_name , pct) %>% filter(!is.na(`Joseph R. Biden Jr.`))%>% select( question_id , `Joseph R. Biden Jr.`) 

poll_2020 <- merge(trump2020 , joe_biden2020 , by = "question_id")
poll2020 <- poll_2020 %>% mutate(spread = `Joseph R. Biden Jr.`/100 -`Donald Trump`/100 ) 

save(poll2020 , file = "rda/poll2020.rda")

-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

presidential_poll <- read.csv("data/president_polls2.csv")
state_polls <- presidential_poll %>%filter(candidate_name %in% c("Donald Trump","Joseph R. Biden Jr.") & (!fte_grade %in% c("F") | is.na(fte_grade))) %>% 
  select("question_id","state","pollster","fte_grade","sample_size","population","start_date","end_date","candidate_name","pct") %>%
  group_by(question_id)

trump <- state_polls %>% rowid_to_column() %>%spread(candidate_name , pct) %>% select( -rowid , -`Joseph R. Biden Jr.`) %>% filter(!is.na(`Donald Trump`))
joe_biden <-state_polls %>% rowid_to_column()%>%spread(candidate_name , pct) %>% filter(!is.na(`Joseph R. Biden Jr.`))%>% select( question_id , `Joseph R. Biden Jr.`) 

state_polls2020 <- merge(trump , joe_biden , by = "question_id")
state_polls2020 <- state_polls2020 %>% mutate(spread = `Joseph R. Biden Jr.`/100 -`Donald Trump`/100 ) %>% ungroup()

state_polls2020 <- state_polls2020 %>% group_by(state) %>% summarise(avg = mean(spread) , 
                                                                     sd = sd(spread) , n = n() ) %>% mutate(state = as.character(state))

results_us_election_2016 <- results_us_election_2016 %>% select(electoral_votes , state)
head(results_us_election_2016)
state_polls2020$state[state_polls2020$state == "Nebraska CD-2"] = "Nebraska"
state_polls2020 <- merge(state_polls2020 , results_us_election_2016 , by= "state" )
state_polls2020 <- state_polls2020 %>% mutate( sd = ifelse(is.na(sd) ,
                                                           median(state_polls2020$sd , na.rm = TRUE) ,
                                                           sd))

save(state_polls2020 , file = "rda/state_polls2020.rda")

