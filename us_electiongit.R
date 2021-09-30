
load( "rda/poll2020.rda")
results <- poll2020 %>%   summarise(avg = sum(spread*sample_size)/sum(sample_size))
p_hat <-(results$avg + 1 )/2
spread <- round(results$avg*100,1)
sd <- sd(poll2020$spread)/sqrt(length(poll2020$spread))
results$avg + c(-qnorm(0.965),qnorm(0.965))*sd
spread
z <- qt(0.975 , nrow(poll2020) - 1)
results$avg + c(-z , z)*sd


mu <- 0.0
tau <- 0.035
sigma_b <- 0.03
sigma <- sqrt(sd^2 + sigma_b^2)
B <-  sigma^2 /(sigma^2 + tau^2)
p_se <- sqrt(1 /(1/sigma^2 + 1/tau^2))
p_mean <- B*mu + (1-B)*results$avg
((p_mean + c(-qnorm(0.965),qnorm(0.965))*p_se) )
1 - pnorm(0 , p_mean,p_se)
spread_biased <- round(p_mean*100 , 1)
spread_biased
p_se

(p_mean + c(-z,z)*p_se)

spread_data <- tibble(stat = c("Biased" , "Unbiased") , avg = c(results$avg , p_mean) ,
                      se = c(sd , p_se))
spread_data %>% ggplot() + geom_pointrange(aes(stat , avg ,
                                                              ymin = avg - qnorm(0.965)*se ,
                                                              ymax = avg +qnorm(0.965)*se , 
                                                              color = stat )) + theme_dark()
ggsave("figs/spread_plot.png")

