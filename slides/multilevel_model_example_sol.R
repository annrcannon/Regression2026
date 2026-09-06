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

# code for histograms on slide 13

p1 <- ggplot(data = music, aes(x = na)) + 
  geom_histogram(fill = "steelblue", color = "black", binwidth = 2) + 
  labs(x = "Individual negative affect", 
       title = "Negative affect scores")

p2 <- music %>%
  group_by(id) %>%
  summarise(mean_na = mean(na)) %>%
  ggplot(aes(x = mean_na)) + 
  geom_histogram(fill = "steelblue", color = "black", binwidth = 2) + 
  labs(x = "Mean negative affect", 
       title = "Mean negative affect scores")

p1 + p2



### Bivariate EDA

#1. make a single scatterplot of the negative affect versus the number of previous
#performances (previous) using the individual observations.  Use geom_smooth() to 
#add a linear regression line to the plot

ggplot(data=music, aes(x=previous, y=na))+
  geom_point()+
  geom_smooth(method="lm")

#2. Make separate scatterplots of na versus previous for each musician (id). 
# Use a lattice plot for this.  Add geom_smooth to add a linear regression line
# to each plot

ggplot(music,aes(x=previous, y=na))+
  geom_point()+
  facet_wrap(~id)+
  geom_smooth(method="lm")

##ASK:  How are the plots similar?  How do they differ?
##ASK:  What are some advantages of each plot, what are some disadvantages


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

ggplot(model_stats, aes(x = r.squared)) + 
  geom_histogram(color = "white", binwidth = 0.05) +
  labs(x = "", 
       title = "Fitted R-squared values", 
       subtitle = "for 37 musicians")
  

### Level 2 models

# Make a Level Two data set

musicians <- music |>
  distinct(id, orchestra) |>
  bind_cols(model_stats)

# Model for intercepts

a <- lm(intercepts ~ orchestra, data = musicians) 
tidy(a) |>
  kable(digits = 3)

# Model for slopes

b <- lm(slopes ~ orchestra, data = musicians) 
tidy(b) |>
  kable(digits = 3)
