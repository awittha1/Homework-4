
full_ma_data <- read_rds('data/output/full_ma_data.rds')

final_ma_data<- read_rds("data/output/final_ma_data.rds")
# Question 1 


question1 <- final_ma_data %>% group_by(county, year) %>% summarize(n = n())

q1.plot <- question1 %>%
  ggplot(aes(x = factor(year), y = log(n))) +
  geom_boxplot() +
  labs (
    x="Year", 
    y = " Ln of Number of Plans"
  ) +
  theme_bw()

q1.plot


# Question 2 
df_q2 <- final_ma_data%>% 
  filter(year %in% c(2009, 2012, 2015)) %>% 
  group_by(year, Star_Rating) %>% 
  summarise(count = n()) %>% 
  filter(!is.na(Star_Rating))


# Add filter by fips 
graph_2 <- ggplot(df_q2, aes(x = Star_Rating, y = count, fill = as.factor(year))) + 
  geom_bar(position = "dodge", stat = "identity") + 
  labs(x = "Star Ratings", y = "Count", title = "Distribution of Star Ratings (2009, 2012,2015)") +
  ylim(0, 25000) +
  xlim(1, 5) +
  theme_classic()

graph_2

# Question 3 

q3.plot <- final_ma_data %>% 
  filter(year >= 2009 & year <= 2015)%>%
  group_by(year) %>% 
  summarize(avg_rate = mean(ma_rate, na.rm =TRUE))%>% 
ggplot( aes(year, avg_rate))+
  geom_col()


# Question 4 


df <- final_ma_data %>% group_by(fips, year)%>% 
  filter(year >= 2009 & year <= 2015) %>% 
  summarize(enroll = first(avg_enrolled), medicare = first(avg_eligibles), bench = mean(ma_rate, na.rm = TRUE)) %>%
  mutate(mkt_share = enroll/medicare) %>% 
  group_by(year)%>% 
  summarize(avg_share = mean(mkt_share, na.rm = TRUE))

ggplot(df, aes(year, avg_share))+
  geom_line()

q4.plot
# Question 5 

final.data.2009 <- final.plans %>% filter(year == 2009)


final.data.2009.new <- final.data.2009 %>%
  mutate(raw_rating=rowMeans(
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

final.data.2009.new <- final.data.2009.new[!is.na(final.data.2009.new$Star_Rating), ]


final.data.2009.new$indicator <- ifelse(final.data.2009.new$Star_Rating > final.data.2009.new$raw_rating, 1,0)

q5_table <- final.data.2009.new %>% group_by(Star_Rating) %>% summarize(avg_ind = mean(indicator))


# Question 6 

q6.table <- final.data.2009.new %>%
  mutate(score1 = raw_rating - 2.75,
         score2 = raw_rating - 3.25, 
         score3 = raw_rating - 3.75, 
         score4 = raw_rating - 4.25,
         mkt_share = avg_enrollment/avg_eligibles,
         ln_share = log(mkt_share))


reg_25 <- rdrobust(y=q6.table$mkt_share, x=q6.table$score1, c=0,
         h=0.125, p=1, kernel="uniform", vce="hc0",
         masspoints="off")

reg_3 <- rdrobust(y=q6.table$mkt_share, x=q6.table$score2, c=0,
                  h=0.125, p=1, kernel="uniform", vce="hc0",
                  masspoints="off")

reg_4 <- rdrobust(y=q6.table$mkt_share, x=q6.table$score3, c=0,
                  h=0.125, p=1, kernel="uniform", vce="hc0",
                  masspoints="off")

# Gives error bc as seen before, all are above threshold
reg_45 <-  rdrobust(y=q6.table$mkt_share, x=q6.table$score4, c=0,
                    h=0.125, p=1, kernel="uniform", vce="hc0",
                    masspoints="off")



# Question 7 

reg_b01 <- rdrobust(y=q6.table$mkt_share, x=q6.table$score1, c=0,
                   h=0.1, p=1, kernel="uniform", vce="hc0",
                   masspoints="off")

reg_b012 <- rdrobust(y=q6.table$mkt_share, x=q6.table$score1, c=0,
                    h=0.12, p=1, kernel="uniform", vce="hc0",
                    masspoints="off")

reg_b013 <- rdrobust(y=q6.table$mkt_share, x=q6.table$score1, c=0,
                     h=0.13, p=1, kernel="uniform", vce="hc0",
                     masspoints="off")

reg_b014 <- rdrobust(y=q6.table$mkt_share, x=q6.table$score1, c=0,
                     h=0.14, p=1, kernel="uniform", vce="hc0",
                     masspoints="off")

reg_b015 <- rdrobust(y=q6.table$mkt_share, x=q6.table$score1, c=0,
                     h=0.15, p=1, kernel="uniform", vce="hc0",
                     masspoints="off")



# Question 8 

filtered_data <- q6.table %>% filter(Star_Rating == 2.5 | Star_Rating == 3)
q8.plot3 <-  rdplot(y=filtered_data$mkt_share, x=filtered_data$score1, binselect="es",
       title="RD Plot: Market Share for 2.5 vs. 3 Star Rating", x.label="Summary Score",
       y.label="Market Share", masspoints="off")


filtered_data2 <- q6.table %>% filter(Star_Rating == 3 | Star_Rating == 3.5)
q8.plot4<- rdplot(y=filtered_data2$mkt_share, x=filtered_data2$score2, binselect="es",
                  title="RD Plot: Market Share for 3 vs. 3.5 Star Rating", x.label="Summary Score",
                  y.label="Market Share", masspoints="off")

filtered_data3 <- q6.table %>% filter(Star_Rating == 3.5 | Star_Rating == 4)
q8.plot5 <- rdplot(y=q6.table$mkt_share, x=q6.table$score3, binselect="es",
                   title="RD Plot: Market Share for 3.5 vs. 4 Star Rating", x.label="Summary Score",
                   y.label="Market Share", masspoints="off")


# Question 9 

sum1 <- q6.table %>% filter(score1>-0.25 & score1<0.25)


sum1$above <- ifelse(sum1$score1 > 0, 1,0 )

tab9 <- sum1 %>% group_by(above) %>% summarize(prop_partd = mean(partd == "Yes"))

save.image("image.Rdata")



#Class notes

# Organize code better, clean data 
# Biggest bandwidth is 0.5 
# use matchit to make plots of covariates (HMO's in plan type )
