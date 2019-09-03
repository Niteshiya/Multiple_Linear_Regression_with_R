#Importing data-set
data <- read.csv("vehicle.csv")
head(data)
#lh=labour hours,lc=labour cost,mc=manufacturing cost
#we are interested in Mileage,lh,lc
pairs(data[,c(3,4,5)])
#We see lc and lh are very much related to a liner relationship
#------------------------------------------------------
#Multiple linear regression
results <- lm(lc~Mileage+lh,data=data)
results
#lc= 1.375e+00 -8.475e-05*(Mileage) + 7.355e+01*(lh)
summary(results)
#we also get the statistical analysis
#** in the summary shows how significant varriable
#if any varriable(like mileage) not playinga significant role we must delete it
#at bottom we get futher statistics 1-pvalue=confidance
#multiple r squared is 0.951 that means varriables on total give us 95%
results_improved <- lm(lc~lh,data=data)
results_improved
summary(results_improved)
#lc=-0.2359 + 73.50*(lh)
#Anova = analysis of varriance
anova(results_improved,results)
#prediction
pred <- predict(results_improved,data.frame(lh=10),interval = "confidence")
pred
