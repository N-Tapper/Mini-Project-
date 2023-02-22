##Installing and loading packages
install.packages("ggplot2")
install.packages("ggpubr")
install.packages("emmeans")
install.packages("ggeffects")
require(ggplot2)
require(ggpubr)
require(emmeans)
require(ggeffects)

##Reading in the data
setwd()
woodlice<- read.csv("Woodlice.csv", stringsAsFactors = T)
str(woodlice)


###SHELTERING###

#Investigating the data graphically with a boxplot and checking for outliers
boxplot(Shelter ~ Treatment, data = woodlice)
##Outliers were not removed because firstly, they were observations and thus biologically relevant.  
##Secondly, as the sample size is relatively small, removing the outliers would mean removing a considerable amount of data.
##For example, for sheltering, removing the outliers for LR would mean excluding nearly 20% of the data points for this treatment.  
 
##Investigating the data graphically with histograms to check the distribution
temp <- NULL
treatments_vector <- unique(woodlice$Treatment)
par(mfrow=c(1,1))

for(i in 1:length(treatments_vector)){
  temp <- subset(woodlice, Treatment==treatments_vector[i])
  histogram <- hist(temp$Shelter, main=paste("Histogram", treatments_vector[i]))
}

##Calculating central tendency and spread for sheltering
##Because the data is skewed, the median and range give a better indication of centrality and spread than mean and SD. 
attach(woodlice)

x <- Shelter[Treatment=="C"]
median(x)
range(x)

x <- Shelter[Treatment=="HC"]
median(x)
range(x)

x <- Shelter[Treatment=="HR"]
median(x)
range(x)

x <- Shelter[Treatment=="HW"]
median(x)
range(x)

x <- Shelter[Treatment=="LC"]
median(x)
range(x)

x <- Shelter[Treatment=="LR"]
median(x)
range(x)

x <- Shelter[Treatment=="LW"]
median(x)
range(x)

##Fitting linear model and checking assumptions
model <- lm (Shelter ~Treatment,data=woodlice)
par(mfrow=c(2,2))
plot(model)
summary(model)
##Assumptions of linear modelling are violated even when the response variable is transformed.  Thus, I fit a binomial GLM as this is likely to be more appropriate. 

##Fitting the GLM 
##A GLM with binomial error distribution was used as the response variable is proportion data.  
##The logit-link function, the natural logarithm of the odds ratio, transforms the proportion data into a suitable form for linear regression analysis thus allowing the interpretation of how treatment affects the odds of sheltering.
##The proportion of time spent sheltering is the continuous response variable and treatment is the categorical explanatory variable.
M1<- glm(Shelter ~ Treatment, data = woodlice, family = "binomial")
summary(M1)

##Null deviance summarises how well the response variable is predicted by a null model and the residual deviance summarises how well the response variable is predicted by the current model.  
##These are used to estimate the goodness-of-fit of the current model.  
##The PseudoR^2 (1 - (152.62/180.96)) = 0.16 and thus this model explains 16% of variation in sheltering.
##The dispersion Parameter (152.62/231) = 0.66.  
##Methods for underdispersion are undeveloped and the sample size is reasonably small so underdispersion is not problematic.  Therefore, I continued to interpret this model. 
 
##Checking the assumptions of linear modelling with diagnostic plots
par(mfrow=c(2,2))
plot(M1)
sum(cooks.distance(M1)>1)
##Assumptions of linear modelling are considered satisfied and no outliers were identified.  

##Checking the goodness of fit of my model.  
anova(M1, test = "Chisq")
##The significant Chi value demonstrates that treatment is having an effect on sheltering.

##Comparing my model to a null model
glm(Shelter ~ 1, data= woodlice, family = "binomial")
##The AIC is lower in my model than in the null model, indicating that my model has a greater goodness of fit.  
##However, as I am not comparing multiple models, but am instead only using one model with an explanatory variable of known biological importance, the AIC value is not of great relevance as it does not provide an absolute measure of model quality.   

##Conducting pairwise comparisons with an adjustment for multiple testing to compare and test for significant differences between the means of all the treatments.  
em <- emmeans(M1, "Treatment")
contrast(em, "pairwise", adjust = "Tukey")
##According to the pairwise comparisons, the only significant difference is between C and HC.  
##This could be due to the relatively small sample size in each treatment resulting in low statistical power, increasing the likelihood of Type II errors (false negatives).

##Calculating the difference in proportion of time spent sheltering between the treatments that differ significantly and pairs of interest.
emmeans(M1, specs = "Treatment", type = "response")
##C-HC: 0.4721 - 0.0471 = 0.43
##c-HR: 0.3669 -  0.0471 = 0.32
##HR-LR: 0.3669 - 0.1368 = 0.23.  ##Despite not statistically significant, this effect size could be biologically significant. 

##Plotting the model using ggpredict to visualise the differences in effect size between the treatments. 
test<-ggpredict(M1, term = "Treatment")
ggplot(test, aes(x=x, y=predicted))+
  geom_point()+
  geom_errorbar(aes(ymin=conf.low, ymax = conf.high)) +
  geom_point(woodlice,mapping = aes(x=Treatment, y=Shelter), color = "grey")+
  labs(x= "Treatment", y = "Proportion of time spent sheltering")+
  theme_classic()

dev.off()


###AGGREGATING###

##Investigating the data graphically with boxplot and checking for outliers
boxplot(Aggregation ~ Treatment, data = woodlice)
##Outliers were not removed because firstly, they were observations and thus biologically relevant.  
##Secondly, as the sample size is relatively small, removing the outliers would mean removing a considerable amount of data.

##Investigating the data graphically with histograms to check the distribution
temp <- NULL
treatments_vector <- unique(woodlice$Treatment)
par(mfrow=c(1,1))

for(i in 1:length(treatments_vector)){
  temp <- subset(woodlice, Treatment==treatments_vector[i])
  histogram <- hist(temp$Aggregation, main=paste("Histogram", treatments_vector[i]))
}

##Fitting linear model and checking assumptions
model2 <- lm (Aggregation~Treatment,data=woodlice)
par(mfrow=c(2,2))
plot(model2)
summary(model2)
##Assumptions of linear modelling are violated even when the response variable is transformed.  Thus, I fit binomial GLM as this is likely to be more appropriate.  

##Fitting the GLM
##A GLM with binomial error distribution was used as the response variable is proportion data.  
##The logit-link function, the natural logarithm of the odds ratio, transforms the proportion data into a suitable form for linear regression analysis thus allowing the interpretation of how treatment affects the odds of sheltering.
##The proportion of time spent aggregation is the continuous response variable and treatment was the categorical explanatory variable.
M2<- glm(Aggregation ~Treatment, data = woodlice, family = "binomial")
summary(M2)
##The PseudoR^2 (1 - (60.118/71.336) = 0.16 and thus this model explains 16% of variation in sheltering.
##The dispersion Parameter (60.118/231) = 0.26.  
##Methods for underdispersion are undeveloped and the sample size is reasonably small so underdispersion is not problematic.  Therefore, I continued to interpret this model. 

##Checking the assumptions of linear modelling with diagnostic plots
par(mfrow=c(2,2))
plot(M2)
sum(cooks.distance(M2)>1)
##Assumptions of linear modelling are considered satisfied (not perfect but sufficient for ecological data) and no outliers were identified.  

##Checking the goodness of fit of my model.
anova(M2, test = "Chisq")
##The non-significant Chi value indicates that treatment does not have an effect on aggregation.   

##Comparing my model to a null model
glm(Aggregation ~ 1, data= woodlice, family = "binomial")
##The AIC is lower in the null model compared to my model, indicating a lower goodness of fit in my model.  
##This is to be expected if treatment is not having an effect on aggregation.

##Conducting pairwise comparisons 
em <- emmeans(M2, "Treatment")
contrast(em, "pairwise", adjust = "Tukey")
##It is to be expected that no significant differences were to be found.  
##Although the relatively small sample size in each treatment reduces statistical power, increasing the likelihood of Type II errors (false negatives), the previous model validation suggests that treatment does not impact aggregation.
##Thus, larger sample sizes may have less of an effect on the pairwise comparisons for aggregating than for sheltering.

##Calculating the difference in proportion of time spent aggregating between the pair of interest (although not significantly different).
emmeans(M2, specs = "Treatment", type = "response")
##HW-C: 0.1934-0.0684 = 0.13

##Plotting the model using ggpredict to visualise the differences in time spent aggregating between the treatments. 
test2<-ggpredict(M2, term = "Treatment")
ggplot(test2, aes(x=x, y=predicted))+
  geom_point()+
  geom_errorbar(aes(ymin=conf.low, ymax = conf.high)) +
  geom_point(woodlice, mapping = aes(x=Treatment, y=Aggregation), color = "grey")+
  labs(x= "Treatment", y = "Proportion of time spent aggregating")+
  theme_classic()

##Calculating central tendency and spread for aggregating
##Because the data is skewed, the median and range give a better indication of centrality and spread than mean and SD. 

x <- Aggregation [Treatment=="C"]
median(x)
range(x)

x <- Aggregation [Treatment=="HC"]
median(x)
range(x)

x <- Aggregation [Treatment=="HR"]
median(x)
range(x)

x <- Aggregation [Treatment=="HW"]
median(x)
range(x)

x <- Aggregation [Treatment=="LC"]
median(x)
range(x)

x <- Aggregation [Treatment=="LR"]
median(x)
range(x)

x <- Aggregation [Treatment=="LW"]
median(x)
range(x)

