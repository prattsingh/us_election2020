library(tidyverse)
Polls_2020 <- read.csv("data/president_polls.csv")
poll2020 <- Polls_2020 %>% filter(candidate_name %in% c("Donald Trump","Joseph R. Biden Jr.") & (fte_grade %in% c("A+", "A", "A-", "B+") | is.na(fte_grade))) %>% 
  select("question_id","state","pollster","fte_grade","sample_size","population","start_date","end_date","candidate_name","pct") %>%
  group_by(question_id)
head(poll2020)
trump2020 <- poll2020 %>% rowid_to_column() %>%spread(candidate_name , pct) %>% select( -rowid , -`Joseph R. Biden Jr.`) %>% filter(!is.na(`Donald Trump`))
joe_biden2020 <-poll2020 <- poll2020 %>% rowid_to_column()%>%spread(candidate_name , pct) %>% filter(!is.na(`Joseph R. Biden Jr.`))%>% select( question_id , `Joseph R. Biden Jr.`) 

poll2020 <- merge(trump2020 , joe_biden2020 , by = "question_id")
poll2020 <- poll2020 %>% mutate(spread = `Joseph R. Biden Jr.`/100 -`Donald Trump`/100 ) 

save(poll2020 , file = "rda/poll2020.rda")
