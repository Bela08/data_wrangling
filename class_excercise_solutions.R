library(tidyverse)
data("starwars")
starwars
# sex, birth year, species and height and add height in meters
starwars %>% 
  select(sex,birth_year,species,height) %>% 
  mutate(height_m = height*0.01)

# What is the most common eye color
starwars %>% 
  select(eye_color) %>% 
  group_by(eye_color) %>% 
  summarise(freq_eye = length(eye_color)) %>% 
  arrange(desc(freq_eye))
# Who is the youngest human
starwars %>% 
  filter(species == "Human") %>% 
  select(name, birth_year) %>% 
  arrange(birth_year)

# Which homeworld has the most characters
starwars %>% 
  select(homeworld) %>% 
  group_by(homeworld) %>% 
  summarise(most_char = length(homeworld)) %>% 
  arrange(desc(most_char))


# Create a new data with species mass, calculate the bmi, and join it with the starwars data

starwars %>% 
  mutate(bmi = mass/(0.01*height)^2) %>% 
  select(species,mass,bmi) %>% 
  left_join(starwars,by = "species")

# create a long format of the subset data of species, mass, height, and birth year with the species column, one column with the categories mass,height, birth year and one column with the values

starwars %>% 
  select(species,mass,height,birth_year) %>% 
  pivot_longer(cols = -species,names_to = "char_attribute", values_to = "values")

# Group the starwars dataset based on sex and nest it. 
# Add a column with linear model summary for each species 
# Add a column with a plot for the height as a function of mass for each sex
# plot all the plots side by side (use `cowplot` package)
lm_nest<- starwars %>% 
  group_by(sex) %>% 
  nest() %>% 
  mutate(lm_results = map(data,~ broom::tidy(summary(lm(height~mass,data =.))))) %>%
  mutate(lm_plot = map(data,~ggplot(.,aes(mass,height))+geom_point())) %>% 
  print()

lm_nest$lm_results[1]

cowplot::plot_grid(plotlist = lm_nest$lm_plot)