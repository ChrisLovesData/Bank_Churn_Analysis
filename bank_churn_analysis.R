install.packages("readxl")
library(readxl)
churn_table <- read.csv("/Users/Owner/Downloads/Customer_Churn_Records.csv")
View(churn_table)
summary(churn_table)

install.packages("tidyverse")
library(tidyverse)

install.packages("plotly")
library(plotly)

# From this data set, how many customers exited?
data(churn_table)
library('dplyr')

churn_count <- churn_table %>%
  group_by(Exited) %>%
  summarize(total_customers = sum(Exited==1, Exited == 0))%>%
  mutate(percentage = total_customers/10000*100)
  
View(churn_count)

#Pie chart for churn count table
ggplot(churn_count, aes(x = "", y  total_customers, fill = as.factor(Exited))) +
  geom_bar(stat = "identity") +
  coord_polar(theta = "y") +
  theme_void() +
  labs(title = "Exit Analysis") +
  scale_fill_discrete(name = "Legend Title") +
  scale_fill_manual(name = "Exited", 
                    values = c("0" = "skyblue", "1" = "navyblue"),
                    labels = c("0" = "Not Exited", "1" = "Exited")) +
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_text(aes(label = total_customers), position = position_stack(vjust = 0.5))

#Explores the churn rates among different countries
country_breakdown <- churn_table %>%
  group_by(Geography) %>%
  summarize(total_exited = sum(Exited == 1), 
            total_customers = n()) %>%
  mutate(churn_percentage = round(total_exited/total_customers*100,2)) %>%
  arrange(desc(churn_percentage))
  
View(country_breakdown)

#Explores the amount of people who exited with an without complaining
complaint_analysis <- churn_table %>%
  group_by(Complain) %>%
  summarize(exited_count =sum(Exited==1))

View(complaint_analysis)

#Average age of customers and excluding any null values
avg_age <- churn_table %>%
  summarize(mean_age = round(mean(Age, na.rm = TRUE)))

View(avg_age)

#Lets check exit rates by age groups
age_ranges <- cut(churn_table$Age, breaks = c(17, 30, 45, 65,100),
                  labels = c("18-30", "31-45", "46-64", "65+"), right = FALSE)

churn_table$age_ranges <- age_ranges


age_analysis <- churn_table %>%
  group_by (age_ranges) %>%
  summarize(exited_customers = sum(Exited ==1),
            total_customers = n()) %>%
  mutate(percentage = round(exited_customers/total_customers*100,2))

View(age_analysis)

#Lets visualize exit rate by age group
ggplot(age_analysis, aes(x = age_ranges, y = percentage)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(title = "Percentage of Exited Customers by Age Range", x = "Age Range", y = "Percentage (%)") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))
  
#Churn rate by tenure
tenure_analysis <- churn_table %>%
  group_by(Tenure) %>%
  summarize(exited_count =sum(Exited==1),
            total_customers = n()) %>%
  mutate(churn_percentage = round(exited_count/total_customers*100))

View(tenure_analysis)

#Lets create a double y-axis chart that compares churn percentage and total customers against tenure
plot_ly(tenure_analysis, x = ~tenure_analysis$Tenure) %>%
  add_bars(y = ~tenure_analysis$total_customers, name = "Total Customers", marker = list(color = "rgba(255,165,0,0.7)")) %>%
  add_lines(y = ~tenure_analysis$churn_percentage, yaxis = "y2", name = "Churn Percentage", line = list(color = "rgba(0,0,255,0.7)")) %>%
  layout(title = "Tenure effect on Churn Percentage",
         xaxis = list(title = "Tenure"),
         yaxis = list(title = "Total Customers"),
         yaxis2 = list(title = "Churn Percentage", overlaying = "y", side = "right"))

#Lets do a logistic regression so that we can predict whether a customer may churn
model1 <- glm(Exited ~ Complain + Satisfaction.Score + Age + Tenure + IsActiveMember + NumOfProducts, data = churn_table, family = binomial(link = "logit"))

summary(model1)

install.packages("rmarkdown")
