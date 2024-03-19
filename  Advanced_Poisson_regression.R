
#load packaged
library(readr)
library(dplyr)
library(psych)
library(forcats)
library(ggplot2)
library(ordinal)
library(lmerTest)
library(sjPlot)
library(stargazer)
library(lme4)
library(MASS) 
library(pscl)
library(glmmTMB)

#Loading the data
student_data <-  read_csv("studentLab3-1.csv")

#Explore the data#loa
View(student_data)
str(student_data)
summary(student_data)
sum(is.na(student_data)) #There is 7 missing values in the data.
#Explore wh etherthe missing data is random or has valuble meaning 
colSums(is.na(student_data))
student_data[!complete.cases(student_data), ]
#The missing data do not seem to have a particular pattern.Therefore, they will be excluded.  
student_data <- na.omit(student_data)



#Prepare the data#

#garde variable
unique(student_data$grade)
student_data$grade <- factor(student_data$grade, levels = c("u", "g", "vg"), ordered = TRUE)


#school variable
unique(student_data$school)
student_data$school <- as.factor(student_data$school)
summary(student_data$school)

#Sex variable

student_data$sex <- as.factor(student_data$sex) 
# Mutate sex variable to male and other
student_data <- student_data %>%
  mutate(sex = fct_collapse(sex,
                            Others = c("O", "F"),
                            Male = "M"))
summary(student_data$sex)
  
#Age variable
str(student_data$age)
summary(student_data$age)# Error: There is student with age of 0. 
#Display the error rows
Error_age<- which(student_data$age == 0)
print(Error_age)
#There is only two rows with age of 0.
#We can replace these two values with the median of the age which is 17
student_data$age <- ifelse(student_data$age ==0, 17, student_data$age)

#summary stat agian
describe(student_data$age)


#traveltime variable
str(student_data$traveltime)
summary(student_data$traveltime)

#It seems that the travel time is in hour. According to the assigment it should be in mniutes.
#Transforming travel time from hour to minutes
student_data$traveltime <- student_data$traveltime * 60 
summary(student_data$traveltime)

#paid variable
student_data$paid <- as.factor(student_data$paid)
summary(student_data$paid)

#checking that everything looks good 
summary(student_data)



#Visulaisation: Exploratory analysis#

#The relationship between Studytime and grade
ggplot(student_data, aes(x = grade, y = studytime)) +
  geom_boxplot(aes(fill = grade), width = 0.5) + 
  stat_summary(fun = median, geom = "crossbar", width= 0.5, color = "red") + # Add median lines
  labs(x = "Grade", y = "Study Time (hours)", 
       title = "Relationship between Study Time and Grade") +
  theme_minimal()

#It seems that the mdian is same for all grades.
#Calculate the median for each grade category to confirm that!
print(median_study_times <- student_data %>%
  group_by(grade) %>%
  summarize(median_study_time = median(studytime)))
#From the plot and calculation, we can see that the median is same for all grades (median =2).



#The relationship between paying for an additional teacher and grade. 

ggplot(student_data, aes(x = grade, fill = paid)) +
  geom_bar(position = "dodge", width = 0.5)  +
  labs(title = "Relationship between additional teacher and Grade", x = "Grade", y = "Count", fill = "Paying for Teacher") +
  theme_minimal()






#Random effect?#

#There is three schools in the data.
#Explore if the school is clustered by grade
ggplot(student_data, aes(x = school, fill = grade)) +
  geom_bar(position = "dodge") +
  labs(x = "School", y = "Count", fill = "grade") +
  theme_minimal()
#It seems that the grade is clustered by grade.



###REGRESSIONS

#Ordinal regression (studytime and grade)

#model 1: 
model_1 <- clm(grade ~ studytime + paid + traveltime + age + sex + school, data = student_data)
summary(model_1)  
plot_model(model_1)

#model 2: we inclde school as random effect
model_2 <- clmm(grade ~ studytime + paid + traveltime + age + sex + (1|school), data = student_data)
summary(model_2)
plot_model(model_2)


#Check if random effect is normally distributed
random_values <- unlist(ranef(model_2))  #Extract the random effect values
print(random_values)
plot_model(model_2, type = "re") #Plot the random effect
shapiro.test(random_values) # p-value is relativley high 0.9787
#The random effect seems to be normally distributed. 


#Anova test to compare the two models
anova(model_1, model_2)
#The anova test shows that the two models are significantly different
#Where model 1 is slightly better fit than model 2
#However, since the school is clustered by grade
#We will include it as random effect in the model 
#Therefore, we will use model 2

#Summary table of model_2
sjPlot::tab_model(model_2,
                  title = "Mixed ordinal regression model",
                  dv.labels = "Dependent Variable: Grade")


#PREDICTION
#The prediction will be calculated by the regression formula
#The mean of the numric variables will be used for the prediction
mean(student_data$studytime)
mean(student_data$traveltime)







#Poission regression (Homeworks and grade)

#Homeworks variable
hist(student_data$homeworks) 
describe(student_data$homeworks)
table(student_data$homeworks)


#Explore if the school is clustered by homeworks
ggplot(student_data, aes(x = school, y = homeworks, fill = school)) +
  geom_boxplot()
#It seems that there are diffrences between the schools
#The MS school has lower median than GP and AD schools


#Mixed Posissionregression with schoolincluded as random effect

pois_model <- glmer(homeworks ~ studytime + paid + traveltime + age + sex + (1|school),
                      data = student_data, family = poisson)
summary(pois_model)

#check the assumption of normally distributed random effects
values <- unlist(ranef(pois_model)) 
print (values)
plot_model(pois_model, type = "re") # does not look good
shapiro.test(values) # p-value is (0.48) relativley higher 0.05 
#since the homeworks vary between schools, we will continue with using schools as random effect 

#summary table of poisson model
sjPlot::tab_model(pois_model,
                  title = "Mixed Poisson model",
                  dv.labels = "Dependent Variable: Homeworks")




#ASSUMPTIONS IN POISSON REGRESSI
#Checkun the assumption of the mean = variance
mean(student_data$homeworks)
var(student_data$homeworks)

#The mean = 0.6 and variance = 1.1 (mean < variance)
#Therefore, we conclude that the data is overdispersed!!

#In this case, it is more suitable to use mixed negative binomial regression instead of poisson regression
nb_model <- glmer.nb(homeworks ~ studytime + paid + traveltime + age + sex + (1|school), data = student_data)
summary(nb_model)

#summary table of nb model
sjPlot::tab_model(nb_model,
                  title =  "Mixed-effects zero-inflated binomial regression",
                  dv.labels = "Dependent Variable: Homeworks")





#We have noticed from summary of the homeworks varaible that it has a large number of zeros
#449 zeros!

# Visually distribution of the homeworks variable
ggplot(student_data, aes(x = homeworks)) +
  geom_histogram(bins = 4,
                 fill = "skyblue") +
  labs(title = "Histogram of Homeworks Not Submitted") +
  theme_minimal()

#Therefore, the zero-inflated binomial regression is more suitabl
#mixed-effects zero-inflated binomial regression:
zinb_model <- glmmTMB(homeworks ~ studytime + paid + traveltime + age + sex + (1|school),
                      ziformula = ~ 1,
                      family = nbinom2,  
                      data = student_data)
summary(zinb_model)


#summary table of zinb model
sjPlot::tab_model(zinb_model,
                  title =  "Mixed-effects zero-inflated binomial regression",
                  dv.labels = "Dependent Variable: Homeworks")





#comparing all models in one table
tab_model(pois_model, nb_model, zinb_model,
          title = "Models Comparison",
          dv.labels = "Dependent Variable: Homeworks",
          show.se = TRUE,
          show.ci = FALSE)
#comapring the models using AIC
AIC(pois_model, nb_model, zinb_model)




#NOTES
#I used the simple model for zero-inflated binomial regression where:
#The zero-inflation component of the zero-inflated negative binomial is predicted only by  intercept: ziformula = ~ 1
#It assumes that the studnets being in zero category is not influnecd of the values of any predictors
#Ii would be intressant to include all the predictors in the zero inflation component:
# ~ studytime + paid + traveltime + age + sex + (1 | school)
#But i got warning messages; the model did not converge
#I assume the model became too complex, especially that model includes random effect also
#Or because the data needed to be more prepared (transformed) before running the model
