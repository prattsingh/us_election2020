library(tidyverse)
Polls_2020 <- read.csv("/home/pratyush/Downloads/president_polls.csv")
poll2020 <- Polls_2020 %>% filter(candidate_name %in% c("Donald Trump","Joseph R. Biden Jr.") & (fte_grade %in% c("A+", "A", "A-", "B+") | is.na(fte_grade))) %>% 
  select("question_id","state","pollster","fte_grade","sample_size","population","start_date","end_date","candidate_name","pct") %>%
  group_by(question_id)
head(poll2020)
trump2020 <- poll2020 %>% rowid_to_column() %>%spread(candidate_name , pct) %>% select( -rowid , -`Joseph R. Biden Jr.`) %>% filter(!is.na(`Donald Trump`))
joe_biden2020 <-poll2020 <- poll2020 %>% rowid_to_column()%>%spread(candidate_name , pct) %>% filter(!is.na(`Joseph R. Biden Jr.`))%>% select( question_id , `Joseph R. Biden Jr.`) 

poll2020 <- merge(trump2020 , joe_biden2020 , by = "question_id")

poll2020 <- poll2020 %>% mutate(spread = `Joseph R. Biden Jr.`/100 -`Donald Trump`/100 ) 
results <- poll2020 %>%   summarise(avg = sum(spread*sample_size)/sum(sample_size))
p_hat <-(results$avg + 1 )/2
spread <- round(results$avg*100,1)
sd <- sd(poll2020$spread)/sqrt(length(poll2020$spread))
results$avg + c(-1.96,1.96)*sd

z <- qt(0.975 , nrow(poll2020) - 1)
results$avg + c(-z , z)*sd
(p_mean + c(-z,z)*p_se)

mu <- 0.0
tau <- 0.035
sigma_b <- 0.03
sigma <- sqrt(sd^2 + sigma_b^2)
B <-  sigma^2 /(sigma^2 + tau^2)
p_se <- sqrt(1 /(1/sigma^2 + 1/tau^2))
p_mean <- B*mu + (1-B)*results$avg
(p_mean + c(-1.96,1.96)*p_se)
1 - pnorm(0 , p_mean,p_se)
p_mean
p_se