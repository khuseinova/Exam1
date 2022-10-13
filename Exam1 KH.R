#1. I want to compare two categories: people who are not working remotely at all and people who work at least 1 day in past week. 
#H0 = 0 
#H1 > 1,

#Hypothesis: people with higher degree qualifications have higher rate of working remotely than people with lower education.


#Had 1+ telework days:adv deg :5920
#No telework days: adv deg: 5861

#Had 1+ telework days:some hs: 44
#No telework days: some hs: 551

#We can see that there are more people working remotely if they have advanced deegre than high school. The result for people who have advanced degree and work at least 1 day per week is 5920, which is bigger than "no remote work". I can assume that is because people usually pursue advanced degree in the fields more related to finance, business, marketing, etc, where you have an opportunity to work remotely in the future.

#2.My hypothesis is that people who did not have Covid got the vaccine more than people who had it. In addition, people who had LONGCOVID got vaxx less than people who had usuall Covid.

#H0 = 0
#H1!= 0

#People who did not have Covid and got vaxx: 23102
#People who had Covid and also got vaxx: 17268

#People who had LONGCOVID and got vaxx: 5096
#People who had usuall Covid or without any symptomps and got vaxx: 17268

#First hypothesis: I think people getting Covid were not that interested in getting vaxx, as after Covid there is a less chance to get another one. There are also a lot of people who are against getting vaxx which could affect those numbers. We should consider people working remotely and vaccination is usually not required for them. As we see people who did not have a Covid more affected by getting Covid, and therefore vaccination is seen more often here.
#Second hypothesis: As we can see, people who had long covid were not that interested in getting vaccination, which is really shocking, as most of the time people used to get vaxx after having a long Covid. My only suggestion is that people usually can be concerned about side effects of the vaccination and deciding not to experiment with it.

#I want to try and look at wages and compare different situation where being a female and having a family have an effect on it.
#I will try to do the lm regression by incorporate pretty much everything first.

huge_lm <- lm(INCWAGE ~ AGE + Commute_bus + Commute_car + Commute_rail + Commute_subway + in_Nassau + in_Westchester + in_Queens + in_Bronx + in_Brooklyn + in_Manhattan + female + NCHILD + FAMSIZE + RENT  + educ_hs + educ_somecoll + educ_college + educ_advdeg)
summary(huge_lm)

#Most notably here are neighborhoods, where all of them have very high standard errors with estimates sometimes smaller than standard errors. For all of them, we will always assume $H_0 = 0, H_1 \ne 0$. As given in the linear regression, for Nassau and Westchester, we have NA, and for Brooklyn our t-value is -1.340. Therefore, we failed to reject null hypothesis for all of these values. \
#It's notably interesting, that Rent has 0 effect on this situation, as significance code is "0", we can exclude this one as well.


#I also want to see if races has any effect on this regression.

huge_lm <- lm(INCWAGE ~ AGE + Commute_bus + Commute_car + Commute_rail + Commute_subway + white + AfAm + Amindian + Asian + race_oth + in_Queens + in_Bronx + in_Manhattan + female + NCHILD + FAMSIZE + educ_hs + educ_somecoll + educ_college + educ_advdeg)
summary(huge_lm)

#The results are interesting, as shown here, there's no significance for a 95% confidence intervals. In fact, our smallest one is race_oth and there's no significance for a 90% confidence interval. For AfAm, which is often under represented, p-value = 0.1, which means we have significance only at an 80% confidence interval, which is hard to justify statistically speaking. \
#With this being said, it's a good idea to exclude those from the linear regression.

huge_lm <- lm(INCWAGE ~ AGE + Commute_bus + Commute_car + Commute_rail + Commute_subway + in_Queens + in_Bronx + in_Manhattan + female + NCHILD + FAMSIZE + educ_hs + educ_somecoll + educ_college + educ_advdeg)
summary(huge_lm)

#As shown above, everything now is very significant (p-value is <2e-16). This concludes our final linear regression. There are few important things, which we can observe here:
#1. Being a female would result in less wages. I assume that is because females are unequally treated; 
#2. College degree is extremely important and results in 20K difference in wages from someone without high school degree and almost 15K from someone with a high school degree. Advanced degree, however, doesn't seem to be affecting the wages as much as college degree.
#3. While rent and ownership cost do matter, the effect is so small that they are actually not correlating with wages in any significant way. This is surprising to find out.
#4. Family size contributes negatively to the wages, but number of child contributes positively to the wages. It's understandable that when the family size is bigger, there are more things to take care of and therefore less time to make money. Positive contribution from number of children could be explained in one of two ways: first, only people who can afford to raise more than one child would give birth to new children; 



#We can now try use knn clasification. Featured categories in my knn model will be ownership, housing_cost, inctot, race

dat_NYC <- subset(acs2017_ny, (acs2017_ny$AGE > 20) & (acs2017_ny$AGE < 66))
attach(dat_NYC)
borough_f <- factor((in_Bronx + 2*in_Manhattan + 3*in_StatenI + 4*in_Brooklyn + 5*in_Queens), levels=c(1,2,3,4,5),labels = c("Bronx","Manhattan","Staten Island","Brooklyn","Queens"))

is.na(FAMSIZE) <- which(FAMSIZE == 9999999) # that's how data codes NA values
housing_cost <- FAMSIZE + RENT
norm_inc_tot <- norm_varb(INCTOT)
norm_housing_cost <- norm_varb(housing_cost)     

set.seed(12345)
NN_obs <- sum(good_obs_data_use == 1)
select1 <- (runif(NN_obs) < 0.8)
train_data <- subset(dat_use,select1)
test_data <- subset(dat_use,(!select1))
cl_data <- y_use[select1]
true_data <- y_use[!select1]

summary(cl_data)
prop.table(summary(cl_data))
summary(train_data)
require(class)
for (indx in seq(1, 9, by= 2)) {
  FAMSIZE <- knn(train_data, test_data, cl_data, k = indx, l = 0, prob = FALSE, use.all = TRUE)
  num_correct_labels <- sum(FAMSIZE == true_data)
  correct_rate <- num_correct_labels/length(true_data)
  print(c(indx,correct_rate))
}