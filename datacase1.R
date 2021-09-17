##### PACKAGES REQUIRED #####

if (!require("tidyverse")) install.packages("tidiverse")
library(tidyverse)

##### DATA LOCATION #####

#-- Name of project: datacase --#

## Import dataset

data <- read.csv("C:/Users/usuario/Desktop/Treasure island/treasure_island.csv", sep = ",")

summary(transform_data$weeknum)

## Exploring data set for NA's

summary(data)

## need to transform variables regularactivity and receiveMailing intofactors

transform_data <- data %>%
  mutate(regularityactivity = as.factor(regularityactivity)) %>%
  mutate(receivedMailing = as.factor(if_else(receivedMailing == TRUE, 1, 0))) # 1 is for experimental and 0 is control

pre_data_control <- transform_data %>%
  filter(receivedMailing == 0) %>%
  filter(weeknum < 201903)

post_data_control <- transform_data %>%
  filter(receivedMailing == 0) %>%
  filter(weeknum >= 201903)

pre_data_exp <- transform_data %>%
  filter(receivedMailing == 1) %>%
  filter(weeknum < 201903)

post_data_exp <- transform_data %>%
  filter(receivedMailing == 1) %>%
  filter(weeknum >= 201903)

first_week_control <- transform_data %>%
  filter(receivedMailing == 0) %>%
  filter(weeknum == 201903)
  
first_week_exp <- transform_data %>%
  filter(receivedMailing == 1) %>%
  filter(weeknum == 201903)


#- Calculating the n for analysis--#

count_cases <- function (x){
  count(x) %>% as.numeric()
  }

total_ncases = count_cases(data)
pre_control_n = count_cases(pre_data_control)
pre_exp_n = count_cases(pre_data_exp)

first_control_n = count_cases(first_week_control)
first_exp_n = count_cases(first_week_exp)

total_ncases == pre_control_n + pre_exp_n + post_control_n + post_exp_n

#-- Calculate Inactive Users--##

count_activity <- function (x,y){
  x %>% filter(regularityactivity == y) %>% count() %>% as.numeric()
}

pre_inactive_control = count_activity(pre_data_control, "consistently inactive")
pre_inactive_exp = count_activity(pre_data_exp, "consistently inactive")

first_inactive_control = count_activity(first_week_control, "consistently inactive")
first_inactive_exp = count_activity(first_week_exp, "consistently inactive")

#-- Generating matrix for analysis--##


first_week_matrix = matrix(c(first_inactive_control, first_inactive_exp, (first_control_n-first_inactive_control),
                             (first_exp_n-first_inactive_exp)),
                           nrow = 2, ncol = 2)

control_matrix = matrix(c(pre_inactive_control, first_inactive_control, (pre_control_n-pre_inactive_control), 
                          (first_control_n - first_inactive_control)),
                           nrow = 2, ncol = 2)

exp_matrix = matrix(c(pre_inactive_exp, first_inactive_exp, (pre_exp_n-pre_inactive_exp), (first_exp_n-first_inactive_exp)),
                                  nrow = 2, ncol = 2) 


####--- FIRST QUESTION ---#####

##-- Proportion test Analysis --##

### We need to check if there has been any impact from pre-week the intervention and the first week after sending th mail
## for the control and experimental group

### Difference between pre-week control group vs. first week
### Difference between pre-week exp group vs. first week

proptest_control_matrix <- prop.test(control_matrix, conf.level = 0.95, correct = TRUE)
barplot1 <- barplot(proptest_control_matrix$estimate) 

proptest_exp_matrix <- prop.test(exp_matrix, conf.level = 0.95, correct = TRUE)
barplot2 <- barplot(proptest_exp_matrix$estimate)

proptest_first_matrix <- prop.test(first_week_matrix, conf.level = 0.95, correct = TRUE)
barplot3 <- barplot(proptest_first_matrix$estimate)


#####--- FIRST ANSWER ---####

### Conclusion: There has been a significant reduction of consistently inactive USERS between pre intervention week
## and the first week. This change is only significant for the experimental group, with no statistical difference
### for the control group after the intervention. Therefore we can assure there has been an short term effect
### from this activity

## It should be notice that in the analysis only first week of intervention is being considered due to the n sample size.
### In the following analysis we would see how long this effect lasted

####--- SECOND QUESTION ---#####

### First, we select the week post intervention and group the number of inactive users per week in order to plot
### the percentages and analyze the sustain effect

weekly_data <- post_data_exp %>% mutate(weeknum = as.factor(case_when(weeknum == 201903 ~1,
                                                      weeknum == 201904 ~2,
                                                      weeknum == 201905 ~3,
                                                      weeknum == 201906 ~4,
                                                      weeknum == 201907 ~5,
                                                      weeknum == 201908 ~6,
                                                      weeknum == 201909 ~7,
                                                      weeknum == 201910 ~8)))

summary(weekly_data$weeknum)

evolution_data_a <- weekly_data %>% filter(regularityactivity == "consistently inactive") %>%
  count(weeknum) %>% select(n) %>% rename(inactive = n)

evolution_data_b <- weekly_data %>% 
  count(weeknum) %>% select(n) %>% rename(total_perweek = n)

percent <- evolution_data_a/evolution_data_b
names(percent) <- "percent"

evolution_table <- data.frame(week = c(1:8),
                             inactive = evolution_data_a,
                             total_perweek = evolution_data_b,
                             percent = percent)
evolution_table

### Creating the names tag for every week

tag.weeks <- c()
for (i in 1:8 ) {
  tag.weeks <- append(tag.weeks, paste("Week", i))
}
  
labels_plot <- c(round(evolution_table$percent,2))

weekly_evolution.plot <- barplot(evolution_table$percent,
                                 names.arg = tag.weeks,
                                 main = "Inactive Users Weekly Evolution")
text(weekly_evolution.plot, 0,labels_plot, cex=1, pos=3)
weekly_evolution.plot <- abline(lm(percent ~ week, evolution_table)) 
weekly_evolution.plot

####--- SECOND ANSWER ---#####

### As we can see in the chart and evolution table, starting from Week six from the beginning of the intervention,
### the percentage of consistently inactive users starts increasing. The trend line is also showing a continue increase of
### inactive users for the last week

evolution_days <- weekly_data %>% 
  group_by(weeknum) %>%
  summarise(days.login = mean(logindays))

labels_plot_2 <- c(round(evolution_days$days.login,2))

weekly_days_evolution.plot <- barplot(evolution_days$days.login,
                                 names.arg = tag.weeks,
                                 main = "Logindays Weekly Average Evolution")
text(weekly_days_evolution.plot, 0,labels_plot_2, cex=1, pos=3)
weekly_days_evolution.plot <- abline(lm(days.login ~ week, weekly_days_evolution.plot)) 
weekly_days_evolution.plot

### We can also notice that around Week 5, the average login days started to decline, which contributes to the argument that 
### the intervention effect last for 5 to 6 weeks


