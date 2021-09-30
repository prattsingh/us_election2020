load("rda/state_polls2020.rda")
load("rda/result2016.rda")
head(state_polls2020)
head(results)

results <- results  %>% mutate(electoral_votes = electoral_votes.x) %>%  select(state , avg , sd , n , electoral_votes) 
Diff = setdiff(results$state , state_polls2020$state)
results_diff <- results %>% filter( state %in% Diff)

state_polls2020 <- state_polls2020 %>% rbind( results_diff) %>% filter(!state == "Alaska")

mu <- 0.0
tau <- 0.02
sigma_b <- 0.03

election_day <- replicate(10000 , {
  state_polls2020 %>% mutate(sigma = sqrt((sd^2)/n + sigma_b^2 ) ,
                     B = sigma^2 /(sigma^2 + tau^2) ,
                     posterior_mean = B*mu + (1 -B)*avg ,
                     posterior_sd = sqrt(1 /(1/sigma^2 + 1/tau^2)),
                     simulation = rnorm(length(posterior_mean), posterior_mean , posterior_sd),
                     Biden = ifelse(simulation >0 , electoral_votes , 0)) %>%
    summarise(Biden = sum(Biden)) %>% .$Biden +7
})

mean(election_day > 269)

