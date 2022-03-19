library(tidyverse)
library(dplyr)
library(moderndive)
library(gapminder)
library(sjPlot)
library(stats)
library(jtools)
library(ggplot2)
library(MASS)
library(knitr)
library(janitor)
######################
#GLM analysis of the data
#Author: Shiqi Cheng
#this is a basic analysis of our data set, please open the week9 lab material to read it
#I can not ensure that all the codes are perfect
#if you have any questions about the codes, please tell me
#thank you so much!
#######################

films = read.csv("C:/Users/10170/Desktop/group project2/dataset4.csv")    #read raw data

films$new.rating = cut(films$rating, breaks = c(0, 7, 10),            
                        labels = c("Normal", "Excellent"))          #divide rating into two groups(below 7,and more than 7)
films$new.rating    
films[1:10,]
####################

#drop NA values in variables

ranks.year = films %>%
  dplyr::select(year, new.rating) %>%
  drop_na()                
ranks.year

ranks.length = films %>%
  dplyr::select(length, new.rating) %>%
  drop_na()                   #drop NA values in variable "length" 
ranks.length

ranks.budget = films %>%
  dplyr::select(budget, new.rating) %>%
  drop_na()
ranks.budget

ranks.votes = films %>%
  dplyr::select(votes, new.rating) %>%
  drop_na()
ranks.votes

#######################

#box plots of numerical variables

ggplot(data = ranks.year, aes(x = new.rating, y = year, fill = new.rating)) +
  geom_boxplot() +
  labs(x = "Rating", y = "Year")+ 
  theme(legend.position = "none")

ggplot(data = ranks.length, aes(x = new.rating, y = length, fill = new.rating)) +
  geom_boxplot() +
  labs(x = "Rating", y = "Length")+ 
  theme(legend.position = "none")

ggplot(data = ranks.budget, aes(x = new.rating, y = budget, fill = new.rating)) +
  geom_boxplot() +
  labs(x = "Rating", y = "Budget")+ 
  theme(legend.position = "none")

ggplot(data = ranks.votes, aes(x = new.rating, y = log(votes), fill = new.rating)) +     #here we take log values since "votes" ranges from 0 to 40000
  geom_boxplot() +
  labs(x = "Rating", y = "Log value of votes")+ 
  theme(legend.position = "none")

##############
#model1
model1 = glm(new.rating ~ year, data = ranks.year, 
             family = binomial(link = "logit"))
model1 %>%
  summary()   #invalid p values

summ(model1)
levels(ranks.year$new.rating)  #the baseline category is "Normal", so p = Prob(Excellent), 1-p = Prob(Normal)
model1coefs = round(coef(model1), 3)  #so here, ln(odds ratio) = 6.226 - 0.004*year, p represents Prob(Excellent)
model1coefs

confint(model1) %>%
  kable()          #confidence interval of parameters, point estimation of log-odd is  (-0.0075108,  0.0005119), including 0

plot_model(model1, show.values = TRUE, transform = NULL,
           title = "Log-Odds (Excellent films)", show.p = FALSE, vline.color = "cyan")   #confidence interval plot for log-odds
predict(model1)[1:20]  #only shows 20 data

ranks.year = ranks.year %>%
  mutate(logodds.Excellent = predict(model1)) %>%
  mutate(odds.Excellent = exp(logodds.Excellent)) %>%
  mutate(probs.Excellent = fitted(model1))
ranks.year[1:20,]       #column 3 means fitted values of log-odds, column 4 means fitted values of odds,only shows 20 data

ggplot(data = ranks.year, aes(x = year, y = probs.Excellent)) +
  geom_smooth(method="glm", 
              method.args = list(family="binomial"), 
              se = FALSE) +
  labs(x = "Year", y = "Probability of films considered as Excellent films")   #reject this model


#model2
model2 <- glm(new.rating ~ length, data = ranks.length, 
              family = binomial(link = "logit"))
model2 %>%
  summary()  #valid parameters
summ(model2)

levels(ranks.length$new.rating)  #the baseline category is "Normal", so p = Prob(Excellent), 1-p = Prob(Normal)

model1coefs = round(coef(model2), 3)  #so here, ln(odds ratio) = 2.567 - 0.041*length, p represents Prob(Excellent)
model1coefs

confint(model2) %>%
  kable()          #confidence interval is not including 0

plot_model(model2, show.values = TRUE, transform = NULL,
           title = "Log-Odds (Excellent films)", show.p = T, vline.color = "cyan") #confidence interval plot for log-odds
predict(model2)[1:20]  

ranks.length = ranks.length %>%
  mutate(logodds.Excellent = predict(model2)) %>%
  mutate(odds.Excellent = exp(logodds.Excellent)) %>%
  mutate(probs.Excellent = fitted(model2))
ranks.length[1:20,]       

ggplot(data = ranks.length, aes(x = length, y = probs.Excellent)) +
  geom_smooth(method="glm", 
              method.args = list(family="binomial"), 
              se = FALSE) +
  labs(x = "Length", y = "Probability of films considered as Excellent films")   # one of a valid model


#model3
model3 <- glm(new.rating ~ budget, data = ranks.budget, 
              family = binomial(link = "logit"))
model3 %>%
  summary()
summ(model3)
levels(ranks.budget$new.rating)  #the baseline category is "Normal", so p = Prob(Excellent), 1-p = Prob(Normal)


model1coefs = round(coef(model3), 3)  #so here, ln(odds ratio) = -3.099 + 0.195*budget, p represents Prob(Excellent)
model1coefs

confint(model3) %>%
  kable()          #confidence interval is not including 0

plot_model(model3, show.values = TRUE, transform = NULL,
           title = "Log-Odds (Excellent films)", show.p = T, vline.color = "cyan") #confidence interval plot for log-odds
                                                                                    #why there is no cyan vline on zero?
predict(model3)[1:20]  


ranks.budget = ranks.budget %>%
  mutate(logodds.Excellent = predict(model3)) %>%
  mutate(odds.Excellent = exp(logodds.Excellent)) %>%
  mutate(probs.Excellent = fitted(model3))
ranks.budget[1:20,]       

ggplot(data = ranks.budget, aes(x = budget, y = probs.Excellent)) +
  geom_smooth(method="glm", 
              method.args = list(family="binomial"), 
              se = FALSE) +
  labs(x = "Budget", y = "Probability of films considered as Excellent films")    #one of the possible model


#model4
model4 <- glm(new.rating ~ log(votes), data = ranks.votes, 
              family = binomial(link = "logit"))
model4 %>%
  summary()

summ(model4)
levels(ranks.votes$new.rating)  #the baseline category is "Normal", so p = Prob(Excellent), 1-p = Prob(Normal)


model1coefs = round(coef(model4), 3)  #so here, ln(odds ratio) = 0.005 - 0.190*log(votes), p represents Prob(Excellent)
model1coefs

confint(model4) %>%
  kable()          #intercept includes zero, while log(votes) does not

plot_model(model4, show.values = TRUE, transform = NULL,
           title = "Log-Odds (Excellent films)", show.p = T, vline.color = "cyan") #confidence interval plot for log-odds
predict(model4)[1:20]  


ranks.logvotes = ranks.votes %>%
  mutate(logodds.Excellent = predict(model4)) %>%
  mutate(odds.Excellent = exp(logodds.Excellent)) %>%
  mutate(probs.Excellent = fitted(model4))
ranks.logvotes[1:20,]       


ggplot(data = ranks.logvotes, aes(x = log(votes), y = probs.Excellent)) +
  geom_smooth(method="glm", 
              method.args = list(family="binomial"), 
              se = FALSE) +
  labs(x = "Log values of votes", y = "Probability of films considered as Excellent films")    #one of the possible model

################
#numerical model selection

#since we find model1 has invalid t value(|t| is not larger 1.96, P(|t|>1.96) > 0.05), we concentrate more on model2,3,4
AIC(model1, model2, model3, model4)  #seems that model2 is the best
BIC(model1, model2, model3, model4)  #seems that model2 is the best

#However, when it comes to which factor will evidently affect the rating of films, model2,3,4 all have valid model
#Therefore, we have sufficient statistical evidence to believe that length, budget and votes will affect the rating of films


##############################
##############################
#categorical model

films[1:10,]

ranks.genre = films %>%
  dplyr::select(genre, new.rating) %>%
  drop_na()  
ranks.genre[1:10,]

ranks.genre %>% 
  tabyl(new.rating, genre) %>% 
  adorn_percentages() %>% 
  adorn_pct_formatting() %>% 
  adorn_ns()      #show the percentage of films with certain new.rating and certain genre

ggplot(ranks.genre, aes(x= genre,  y = ..prop.., group=new.rating, fill=new.rating)) + 
  geom_bar(position="dodge", stat="count") +
  labs(y = "Proportion")    #the histogram of above table. In each genre, bars are not in the same height, but have evident difference
######################
#now let's concentrate on each category:

genre.Action = ranks.genre %>%
  filter(genre == "Action") 
genre.Action[1:10,]

genre.Animation = ranks.genre %>%
  filter(genre == "Animation") 
genre.Animation[1:10,]

genre.Comedy = ranks.genre %>%
  filter(genre == "Comedy") 
genre.Comedy[1:10,]

genre.Documentary = ranks.genre %>%
  filter(genre == "Documentary") 
genre.Documentary[1:10,]

genre.Drama = ranks.genre %>%
  filter(genre == "Drama") 
genre.Drama[1:10,]

genre.Romance = ranks.genre %>%
  filter(genre == "Romance") 
genre.Romance[1:10,]

genre.Short = ranks.genre %>%
  filter(genre == "Short") 
genre.Short[1:10,]
###########

#model5, full model
model5 = glm(new.rating ~ year + length + budget + log(votes) + genre, data = films, family = binomial(link = "logit"))
summary(model5)  # logit(genreAnimation)/logit(genreAction) = exp(-0.489039)

logit.step.forward = step(model5,direction="forward")   
summary(logit.step.forward)

logit.step.backward = step(model5,direction="backward")
summary(logit.step.backward)

logit.stepwise = step(model5,direction="both")
summary(logit.stepwise)     #keep invalid values, as long as the whole model is statistically evident

stepAIC(model5) 3#another solution


#model6, remove invalid variables
genre.noRomanceandAnimation = films %>%
  filter(genre != "Romance" ) %>%
  filter(genre != "Animation") %>%
  drop_na
genre.noRomanceandAnimation[1:20,]

#unique(genre.noRomanceandAnimation$genre)

model6 = glm(new.rating ~ year + length + budget + genre, data = genre.noRomanceandAnimation, family = binomial(link = "logit"))
summary(model6)

logit.stepwise.6 = step(model6, direction = "both")
summary(logit.stepwise.6)


####################
#model7 and 8, link functions are probit and cloglog
films$new.rating2 = cut(films$rating, breaks = c(0, 7, 10),            
                       labels = c(-1,0))          #divide rating into two groups(below 7,and more than 7)
films$new.rating2[which(films$new.rating2 == "1")] = 0
films$new.rating2[which(films$new.rating2 == "2")] = 1
films$new.rating2 = as.numeric(films$new.rating2)
films$new.rating2

model7 = glm(films$new.rating2 ~ year + length + budget + log(votes) + genre, data = films, family = binomial(link = "cloglog"))
model8 = glm(films$new.rating2 ~ year + length + budget + log(votes) + genre, data = films, family = binomial(link = "probit"))

summary(model7)
summary(model8)

qchisq(0.95, df = 10)  #m7,m8. df = 10 = 1833-1823, reject null assumptions, models are valid









