
library(tidyverse)

full_ma_data <- read_rds('data/output/full_ma_data.rds')

final_ma_data<- read_rds("data/output/final_ma_data.rds")
# Question 1 


question1 <- final_ma_data %>% group_by(fips, year) %>% summarize(log_n = log(n())) %>% filter(log_n != 0 ) 

q1.plot <- question1 %>%
  ggplot(aes(x = factor(year), y = log_n)) +
  geom_boxplot(fill = "#a9d1e8") +
  labs (
    x="Year", 
    y = " Ln of Number of Plans", 
    title = "Number of plans every year"
  ) +
  theme_bw()

q1.plot


# Question 2 
df_q2 <- final_ma_data%>% 
  filter(year %in% c(2009, 2012, 2015)) %>% 
  group_by(year, Star_Rating) %>% 
  summarise(count = n()) %>% 
  filter(!is.na(Star_Rating))

new_rows <- data.frame(
  year = c(2012, 2015, 2015),
  Star_Rating = c(1.5, 1.5,2),
  count = c(0,0,0)
)

df_q2 <- rbind(df_q2,new_rows)
# Add filter by fips 
graph_2 <- ggplot(df_q2, aes(x = Star_Rating, y = count, fill = as.factor(year))) + 
  geom_bar(position = "dodge", stat = "identity", color = "black") + 
  labs(x = "Star Ratings", y = "Count", title = "Distribution of Star Ratings (2009, 2012,2015)", fill = "Year") +
  # ylim(0, 25000) +
  xlim(1, 5) +
  theme_bw()+
  scale_fill_manual(values = c("#e8a9b7", "#bbe8a9", "#a9d1e8")) 

graph_2

# Question 3 

q3.plot <- final_ma_data %>% 
  filter(year >= 2009 & year <= 2015)%>%
  group_by(year) %>% 
  summarize(avg_rate = mean(ma_rate, na.rm =TRUE))%>% 
ggplot( aes(year, avg_rate))+
  geom_col(fill = "#bbe8a9", color = "black")+
  labs(x = "Year", y = "Average benchmark payment", title = "Average benchmark payment over time")+
  theme_bw()


# Question 4 


df <- final_ma_data %>% group_by(fips, year)%>% 
  filter(year >= 2009 & year <= 2015) %>% 
  summarize(enroll = first(avg_enrolled), medicare = first(avg_eligibles), bench = mean(ma_rate, na.rm = TRUE)) %>%
  mutate(mkt_share = enroll/medicare) %>% 
  group_by(year)%>% 
  summarize(avg_share = mean(mkt_share, na.rm = TRUE))

q4.plot <- ggplot(df, aes(year, avg_share))+
  geom_line(color = "#e8a9b7")+
  labs(x = "Year", y = "Average share of Medicare Advantage", title = "Share of Medicare Advantage over time")+
  theme_bw()

q4.plot
# Question 5 


data_09<- final_ma_data %>% filter(!(is.na(avg_enrollment)) & year == 2009)
data_09<- data_09 %>% mutate(raw_rating=rowMeans(
  cbind(breastcancer_screen,rectalcancer_screen,cv_cholscreen,diabetes_cholscreen,
        glaucoma_test,monitoring,flu_vaccine,pn_vaccine,physical_health,
        mental_health,osteo_test,physical_monitor,primaryaccess,
        hospital_followup,depression_followup,nodelays,carequickly,
        overallrating_care,overallrating_plan,calltime,
        doctor_communicate,customer_service,osteo_manage,
        diabetes_eye,diabetes_kidney,diabetes_bloodsugar,
        diabetes_chol,antidepressant,bloodpressure,ra_manage,
        copd_test,betablocker,bladder,falling,appeals_timely,
        appeals_review),
  na.rm=T))

plan_table <- data_09 %>% 
  mutate(rounded_30 = ifelse(raw_rating>=2.75 & raw_rating<3.00 & Star_Rating==3.0,1,0),
         rounded_35 = ifelse(raw_rating>=3.25 & raw_rating<3.50 & Star_Rating==3.5,1,0),
         rounded_40 = ifelse(raw_rating>=3.75 & raw_rating<4.00 & Star_Rating==4.0,1,0),
         rounded_45 = ifelse(raw_rating>=4.25 & raw_rating<4.50 & Star_Rating==4.5,1,0),
         rounded_50 = ifelse(raw_rating>=4.50 & raw_rating<5.00 & Star_Rating==5.0,1,0),
  ) %>%
  group_by(factor(Star_Rating)) %>%
  filter(Star_Rating %in% c(3, 3.5, 4, 4.5, 5)) %>%
  summarise(count_30=sum(rounded_30),
            count_35=sum(rounded_35),
            count_40=sum(rounded_40),
            count_45=sum(rounded_45),
            count_50=sum(rounded_50))

q5.table<- data.frame(
  "Star_Rating" = c(3.0, 3.5, 4.0, 4.5, 5.0), 
  "Count" = c(plan_table$count_30[1], plan_table$count_35[2], plan_table$count_40[3], plan_table$count_45[4], plan_table$count_45[4])
)

# Question 6 

q6.table <- data_09 %>%
  mutate(score1 = raw_rating - 2.75,
         score2 = raw_rating - 3.25, 
         score3 = raw_rating - 3.75, 
         score4 = raw_rating - 4.25,
         mkt_share = avg_enrollment/avg_eligibles,
         ln_share = log(mkt_share))



# Define the data
data <- q6.table
bw <- 0.125
order <- 1
kernel <- "uniform"
vce <- "hc0"
masspoints <- "off"
vars <- c("score1", "score2", "score3")
vars_2 <- c("round_2.5_3", "round_3_3.5", "round_3.5_4")
q6.table2 <- data.frame(variable = character(),
                    coefficient = double(),
                    std.error = double(), 
                    ci_lower = double(), 
                    ci_upper = double())
for (i in 1:length(vars)) {
  
  # Fit the regression using rdrobust
  reg <- rdrobust(y = data$mkt_share,
                  x = data[[vars[i]]],
                  c = 0,
                  h = bw,
                  p = order,
                  kernel = kernel,
                  vce = vce,
                  masspoints = masspoints)
  
  # Extract the coefficient and standard error
  coef <- reg$coef[1]
  se <- reg$se[1]
  ci_lo <- reg$ci[1,1]
  ci_up <- reg$ci[1,2]
  
  # Add a row to the table with the variable, coefficient, and standard error
  q6.table2 <- rbind(q6.table2, data.frame(variable = vars_2[i],
                                   coefficient = coef,
                                   std.error = se, 
                                   ci_lower = ci_lo, 
                                   ci_upper = ci_up))
}



q6.table2 <- round(q6.table2, 4)

q6.table2

# Question 7 


data <- q6.table
bw <- c(0.1,0.11, 0.12, 0.13, 0.14, 0.15)
order <- 1
kernel <- "uniform"
vce <- "hc0"
masspoints <- "off"
vars <- c("score1", "score2", "score3")
vars_2 <- c(3, 3.5, 4)
q7.table <- data.frame(variable = character(),
                       coefficient = double(),
                       std.error = double(), 
                       ci_lower = double(), 
                       ci_upper = double(), 
                       bandwidth  = double())

for (i in 1:length(vars)) {
  
  for (j in 1:length(bw)){
  
  # Fit the regression using rdrobust
  reg <- rdrobust(y = data$mkt_share,
                  x = data[[vars[i]]],
                  c = 0,
                  h = bw[j],
                  p = order,
                  kernel = kernel,
                  vce = vce,
                  masspoints = masspoints)
  
  # Extract the coefficient and standard error
  coef <- reg$coef[1]
  se <- reg$se[1]
  ci_lo <- reg$ci[1,1]
  ci_up <- reg$ci[1,2]
  # Add a row to the table with the variable, coefficient, and standard error
  q7.table <- rbind(q7.table, data.frame(variable = vars_2[i],
                                         coefficient = coef,
                                         std.error = se, 
                                         ci_lower = ci_lo, 
                                         ci_upper = ci_up, 
                                         bandwidth = bw[j]))
}

}



q7.table



q7.table %>% 
ggplot( aes(x = bandwidth, y = coefficient, shape = as.factor(variable))) + 
  geom_hline(aes(yintercept = 0), linetype = "dashed")+
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper))+
  geom_point() + 
  theme_classic()

# Question 8 
fil1 <- q6.table %>% filter(Star_Rating == 2.5 | Star_Rating == 3)
dens253 <- rddensity(fil1$score1, c=0)
rdplotdensity(dens253, fil1$score1)

fil2 <- q6.table %>% filter(Star_Rating == 3 | Star_Rating == 3.5)
dens335<- rddensity(fil2$score2, c=0)
rdplotdensity(dens335, fil2$score2)

fil3 <- q6.table %>% filter(Star_Rating == 3.5 | Star_Rating == 4)
dens354<- rddensity(fil3$score3, c=0)
rdplotdensity(dens354, fil3$score3)


# Question 9 


# HMO 
score1df <- q6.table %>% filter(Star_Rating == 2.5 | Star_Rating == 3 & score1 %in% c(-0.125, 0.125))
score1df$negative <- ifelse(score1df$score1<0,1,0)
mini_df1 <- score1df %>% group_by(negative) %>% summarize(avg_hmo = mean(plan_type == "HMO/HMOPOS" ))
score2df <- q6.table %>% filter(Star_Rating == 3 | Star_Rating == 3.5 & score2 %in% c(-0.125, 0.125))
score2df$negative <- ifelse(score2df$score2<0,1,0)
mini_df2 <- score2df %>% group_by(negative) %>% summarize(avg_hmo = mean(plan_type == "HMO/HMOPOS" ))
score3df <- q6.table %>% filter(Star_Rating == 3.5 | Star_Rating == 4 & score3 %in% c(-0.125, 0.125))
score3df$negative <- ifelse(score3df$score2<0,1,0)
mini_df3 <- score3df %>% group_by(negative) %>% summarize(avg_hmo = mean(plan_type == "HMO/HMOPOS" ))

q9.df <- rbind(mini_df1, mini_df2,mini_df3)


# Part d 

mini_df1_2 <- score1df %>% group_by(negative) %>% summarize(avg_partd = mean(partd == "Yes" ))
mini_df2_2 <- score2df %>% group_by(negative) %>% summarize(avg_partd = mean(partd == "Yes" ))
mini_df3_2 <- score3df %>% group_by(negative) %>% summarize(avg_partd = mean(partd == "Yes" ))

q9.df_2 <- rbind(mini_df1_2, mini_df2_2,mini_df3_2)









save.image("image.Rdata")



#Class notes

# Organize code better, clean data 
# Biggest bandwidth is 0.5 
# use matchit to make plots of covariates (HMO's in plan type )
