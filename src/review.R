#specify the packages of interest
packages = c("ALSM", "mosaic", "car", "leaps", "tidyverse", "glmnet", "caret", "MPV", "here")

#use this function to check if each package is on the local machine
#if a package is installed, it will be loaded
#if any are not, the missing package(s) will be installed and loaded
package.check <- lapply(packages, FUN = function(x) {
	if (!require(x, character.only = TRUE)) {
		install.packages(x, dependencies = TRUE)
		library(x, character.only = TRUE)
	}
})

spending_data = read_csv(here("data", "spending_subset.csv"))


model1 = lm(clothing_expenditure~., data=spending_data)
msummary(model1)
anova(model1)

cor(model.matrix(model1))

vif(model1)
(cor(model.matrix(model1)))[40:41,]
anova(model1)

model2 = lm(clothing_expenditure~ province+type_of_dwelling+income+marital_status+age_group+sex+food_expenditure+transportation_expenditure+personal_care_expenditure+recreation_expenditure+tobacco_alcohol_expenditure+total_consumption_expenditure+weeks_worked, data=spending_data)
vif(model2)
anova(model2)

model3 = lm(clothing_expenditure~ province+type_of_dwelling+income+marital_status+age_group+sex+food_expenditure+transportation_expenditure+personal_care_expenditure+recreation_expenditure+tobacco_alcohol_expenditure+weeks_worked, data=spending_data)
vif(model3)
anova(model3)


qqnorm(rstandard(model3))
plot(model3, which=1)


bc = MASS::boxcox(model3, lambda=seq(-2, 2, length.out =301))
bc$x[bc$y > max(bc$y) - 1/2 * qchisq(.95,1)]
abline(v=.3)

spending_data$clothing_transformed =  spending_data$clothing_expenditure^(.3)

model4 = lm(clothing_transformed~ province+type_of_dwelling+income+marital_status+age_group+sex+food_expenditure+transportation_expenditure+personal_care_expenditure+recreation_expenditure+tobacco_alcohol_expenditure+weeks_worked, data=spending_data)
plot(model4, which=1)
qqnorm(rstandard(model4))


MASS::boxcox(model4)

model_selection = summary(regsubsets(formula(model4), data=model.frame(model4), nvmax=100, nbest= 1))

model_selection$outmat

plot(model_selection$rsq, type='l', xlab="p", ylab="R^2")
plot(model_selection$cp, type='l', xlab="p", ylab="Cp")
plot(model_selection$adjr2, type='l', xlab="p", ylab="Adjusted R^2")
plot(model_selection$bic, type='l', xlab="p", ylab="BIC")

model_selection$outmat[11,]

model5 = lm(clothing_transformed~ province+type_of_dwelling+income+marital_status+sex+food_expenditure+personal_care_expenditure+recreation_expenditure, data=spending_data)
msummary(model5)
anova(model5)

anova(model5, model4)

anova(model5)["Residuals", "Sum Sq"]
MPV::PRESS(model5)


anova(model4)["Residuals", "Sum Sq"]
MPV::PRESS(model4)

summary(rstandard(model4))
which(is.na(rstandard(model4)))
hatvalues(model4)[550:559]
hatvalues(model5)[550:559]

c(spending_data[556:558,])
table(spending_data$age_group)


plot(rstudent(model5), type="h", ylim=c(-4.5, 4.5))
abline(h=c(-1, 1)*qt(1-.05/(2*dim(spending_data)[1]), model5$df.residual-1), lty=2)

plot(model5)

msummary(model5)


spending_2008 = read_csv(here("data", "spending_2008.csv")); 
spending_2008 = spending_2008[-which(spending_2008$province=="00"),]
spending_2008$clothing_transformed =  spending_2008$clothing_expenditure^(.3)
spending_2008$sex = ifelse(spending_2008$sex==1, "male", "female")
spending_2008

spending_data

predictions.train <- model5 %>% predict(spending_data)
data.frame( 
	SSE   = anova(model5)["Residuals", "Sum Sq"],
	PRESS = PRESS(model5),
	MSE   = anova(model5)["Residuals", "Mean Sq"],
	MSPR = RMSE(predictions.train, spending_data$clothing_transformed)^2,
	R2 = R2(predictions.train, spending_data$clothing_transformed))


predictions.2008 <- model5 %>% predict(spending_2008)
data.frame( 
	MSPR = RMSE(predictions.2008, spending_2008$clothing_transformed)^2,
	R2 = R2(predictions.2008, spending_2008$clothing_transformed))

model5.2008 = lm(clothing_transformed~ province+type_of_dwelling+income+marital_status+sex+food_expenditure+personal_care_expenditure+recreation_expenditure, data=spending_2008)
msummary(model5.2008)
msummary(model5)






model.int = lm(clothing_transformed~ (income+I(income^2))*(marital_status+sex)+province+type_of_dwelling+age_group+food_expenditure+transportation_expenditure+personal_care_expenditure+recreation_expenditure+tobacco_alcohol_expenditure+weeks_worked, data=spending_data)
msummary(model.int)

model_selection.int = summary(regsubsets(formula(model.int), data=model.frame(model.int), nvmax=15, nbest= 1))

model_selection.int$outmat

plot(model_selection.int$rsq, type='l', xlab="p", ylab="R^2")
plot(model_selection.int$cp, type='l', xlab="p", ylab="Cp")
plot(model_selection.int$adjr2, type='l', xlab="p", ylab="Adjusted R^2")
plot(model_selection.int$bic, type='l', xlab="p", ylab="BIC")

model_selection.int$outmat[11,]

model.int2 = lm(clothing_transformed~ (income)*(sex)+marital_status+province+type_of_dwelling+food_expenditure+personal_care_expenditure+recreation_expenditure, data=spending_data)
msummary(model.int2)
anova(model.int2)

anova(model.int2, model.int)

anova(model5, model.int2)

predictions.train <- model.int2 %>% predict(spending_data)
data.frame( 
	SSE   = anova(model.int2)["Residuals", "Sum Sq"],
	PRESS = PRESS(model.int2),
	MSE   = anova(model.int2)["Residuals", "Mean Sq"],
	MSPR = RMSE(predictions.train, spending_data$clothing_transformed)^2,
	R2 = R2(predictions.train, spending_data$clothing_transformed))


predictions.2008 <- model.int2 %>% predict(spending_2008)
data.frame( 
	MSPR = RMSE(predictions.2008, spending_2008$clothing_transformed)^2,
	R2 = R2(predictions.2008, spending_2008$clothing_transformed))
