# setup
library(tidyverse)
library(tidymodels)
library(GGally)
library(knitr)
library(patchwork)
library(viridis)
library(ggfortify)
library(kableExtra)

music <- read_csv("slides/data/musicdata.csv")
music %>%
  filter(id %in% c(1, 43)) %>%
  group_by(id) %>%
  slice(1:3) %>%
  select(id, diary, perform_type, na, gender, instrument)

music <- music %>%
  mutate(orchestra = if_else(instrument == "orchestral instrument", 1, 0), 
         large_ensemble = if_else(perform_type == "Large Ensemble", 1,0))

ols <- lm(na ~ orchestra + large_ensemble + orchestra * large_ensemble, 
          data = music) 
tidy(ols) 


### Univariate EDA



### Bivariat EDA


### Example for obs 22

music %>%
  filter(id == 22) %>%
  select(id, diary, perform_type, instrument, na) %>%
  slice(1:3, 13:15) 




### Fitting to all 37 musicians

model_stats <- tibble(slopes = rep(0,37), 
                      intercepts = rep(0,37), 
                      r.squared = rep(0, 37))


ids <- music %>% distinct(id) %>% pull()

# counter to keep track of row number to store model_stats

count <- 1

for(i in ids){
  level_one_model <- music %>%
    filter(id == i) %>%
    lm(na ~ large_ensemble, data = .)
  
  level_one_model_tidy <- tidy(level_one_model)
  
  
  model_stats$slopes[count] <- level_one_model_tidy$estimate[2]
  model_stats$intercepts[count] <- level_one_model_tidy$estimate[1]
  model_stats$r.squared[count] <- glance(level_one_model)$r.squared
  
  count = count + 1
}

p1 <- ggplot(data = model_stats, aes(x = intercepts)) + 
  geom_histogram(fill = "steelblue", color = "black", binwidth = 2) + 
  labs(x = "Fitted intercepts", 
       title  = "Intercepts", 
       subtitle = "from 37 musicians")

p2 <- ggplot(data = model_stats, aes(x = slopes)) + 
  geom_histogram(fill = "steelblue", color = "black", binwidth = 2) + 
  labs(x = "Fitted Slopes", 
       title  = "Slopes", 
       subtitle = "from 37 musicians")

p1 + p2
  
  