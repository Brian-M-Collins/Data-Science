library(ggplot2)
library(pastecs)
library(dplyr)
library(Hmisc)
library(waffle)
library(car)
library(QuantPsyc)
library(SmartEDA)
library(lmtest)
library(simpleboot)
library(rcompanion)
library(scatterplot3d)

options(scipen = 999) #forcing R to display decimals instead of scientific notation
insurance_data <- read.csv(file="./Assignment 1 - data set.csv")

ExpNumViz(insurance_data, scatter=TRUE) #initial EDA

### You need to provide the committee with comprehensive descriptive analysis using appropriate graphs and charts (at least 7), analysis (central tendencies and dispersions measures) and extract useful information from the data set.

#creating table of descriptive statistics for insurance dataset

ins_sum <- stat.desc(insurance_data)
ins_sum <- round(ins_sum, digits=4)
ins_sum <- t(ins_sum)

#creating plots to explore dataset

ClaimFreq <- ggplot(insurance_data, aes(Claims))
ClaimFreq + geom_freqpoly(binwidth=75) + labs(x="Claims", y="Frequency") + ggtitle("Frequency Plot of Claims") + theme(plot.title=element_text(hjust=0.5))

InsuredFreq <- ggplot(insurance_data, aes(Insured))
InsuredFreq + geom_freqpoly(binwidth=1000) + labs(x="Insured", y="Frequency") + ggtitle("Frequency Plot of Insured") + theme(plot.title=element_text(hjust=0.5))

PaymentFreq <- ggplot(insurance_data, aes(Payment))
PaymentFreq + geom_freqpoly(binwidth=100000) + labs(x="Payment", y="Frequency") + ggtitle("Frequency Plot of Payment Values") + theme(plot.title=element_text(hjust=0.5))

parts <- summarise_at(group_by(insurance_data, Zone), vars(Claims), funs(sum))
waffleparts <- c("1" = 23174, "2"=21302, "3"=19938, "4"=31913, "5"=5962, "6"=10262, "7"=620)
waffle(waffleparts/250, rows=8, size=0.5, legend_pos="bottom", title="Breakdown of Claims by Geographic Region", xlab="1 Square = 250 claims", colors=c("#AB1D00", "#C73400", "#E34D00", "#F76709", "#FD5A42","#FF8091","#FFBFD9"))

KmClaim <- ggplot(insurance_data, aes(Kilometres, Claims))
KmClaim + stat_summary(fun="mean", geom="bar", fill="brown1") + stat_summary(fun.data="mean_cl_normal", geom="errorbar", width=0.3, alpha=0.5, colour="#495057") + theme_minimal() +ggtitle("Average Number of Insurance Claims by Travel Distance Group") + labs(x="Distance Group", y="Number of Claims") +theme(plot.title=element_text(hjust=0.5))

ZoneClaim <- ggplot(insurance_data, aes(Zone, Claims))
ZoneClaim + stat_summary(fun="mean", geom="bar", fill="brown1") + stat_summary(fun.data="mean_cl_normal", geom="errorbar", width=0.3, alpha=0.5, colour="#495057") + theme_minimal() +ggtitle("Average Number of Insurance Claims by Geographic Zone") + labs(x="Geographic Zone", y="Number of Claims") +theme(plot.title=element_text(hjust=0.5))

ZonePayment <- ggplot(insurance_data, aes(Zone, Payment))
ZonePayment + stat_summary(fun="mean", geom="bar", fill="brown1") + stat_summary(fun.data="mean_cl_normal", geom="errorbar", width=0.3, alpha=0.5, colour="#495057") + theme_minimal() +ggtitle("Average Value of Insurance Claims by Geographic Zone") + labs(x="Geographic Zone", y="Value of Claim (Skr)") +theme(plot.title=element_text(hjust=0.5))

ClaimsBonus <- ggplot(insurance_data, aes(Bonus, Claims)) 
ClaimsBonus + stat_summary(fun=mean, geom="point", colour="black") + stat_summary(fun=mean, geom="line", aes(group=1), linetype="dashed", colour="darkorange3") + stat_summary(fun.data = mean_cl_boot, geom="errorbar", width=0.2, alpha=0.5, colour="black") + theme_minimal() + ggtitle("Average Number of Claims by NCB Years") + theme(plot.title=element_text(hjust = 0.5)) + labs(x="Years No Claims Bonus", y="Number of Insurance Claims")

ClaimsBonusBox <- ggplot(insurance_data, aes(as.factor(Bonus), Claims)) 
ClaimsBonusBox + geom_boxplot() + theme_minimal() + ggtitle("Average Number of Claims by NCB Years") + theme(plot.title=element_text(hjust = 0.5)) + labs(x="Years No Claims Bonus", y="Number of Insurance Claims")

InsuredPayment <- ggplot(insurance_data, aes(Insured, Payment))
InsuredPayment + geom_point(shape="+") + theme_minimal() + ggtitle("Scatter of Claims vs Payment") + theme(plot.title=element_text(hjust=0.5)) + labs(x="Insured Policy Years", y="Payment (Skr)") + geom_smooth(method = "lm", linetype="dashed", colour="Orange")

ClaimsYears <- ggplot(insurance_data, aes(Claims, Insured))
ClaimsYears + geom_point(shape="+") + theme_minimal() + ggtitle("Average Number of Claims by Policy Age") + theme(plot.title=element_text(hjust = 0.5), legend.position = "bottom") + labs(x="Total Insured policy Years", y="Number of Insurance Claims") + geom_smooth(method="lm", linetype="dashed", colour="orange", alpha=0.3)

PaymentYears <- ggplot(insurance_data, aes(Payment, Insured))
PaymentYears + geom_point(shape="+") + theme_minimal() + ggtitle("Value of Claims by Policy Age") + theme(plot.title=element_text(hjust = 0.5), legend.position = "bottom") + geom_smooth(method="gam", linetype="dashed", colour="orange", alpha=0.3)

ClaimsPayments <- ggplot(insurance_data, aes(Claims, Payment))
ClaimsPayments + geom_point(shape="+") + theme_minimal() + ggtitle("Scatter of Claim Numbers and Payments") + theme(plot.title=element_text(hjust = 0.5), legend.position = "bottom") + geom_smooth(method="lm", linetype="dashed", colour="orange", alpha=0.3) + labs(x="Number of Policy Claims", y="Total Cost of Claims (Skr)")

MakeClaim <- ggplot(insurance_data, aes(as.factor(Make), Claims))
MakeClaim + geom_boxplot() + theme_minimal() +ggtitle("Boxplots of Insurance Claims by Vehicle Make") + theme(plot.title=element_text(hjust=0.5)) + labs(x="Vehicle Make", y="Number of Insurance Claims")

MakeClaimExploded <- ggplot(insurance_data, aes(as.factor(Make), Claims))
MakeClaimExploded + geom_boxplot(outlier.shape = NA) + theme_minimal() +ggtitle("Boxplots of Insurance Claims by Vehicle Make") + theme(plot.title=element_text(hjust=0.5)) + labs(x="Vehicle Make", y="Number of Insurance Claims") + coord_cartesian(ylim=c(0, 500))

MakeClaim <- ggplot(insurance_data, aes(as.factor(Make), Payment))
MakeClaim + geom_boxplot() + theme_minimal() +ggtitle("Boxplots of Claim Value by Vehicle Make") + theme(plot.title=element_text(hjust=0.5)) + labs(x="Vehicle Make", y="Value of Insurance Claim")


### Perform correlation analysis between variables and identify whether total payment is related to number of claims, the number of insured policy years or other factors. You need to perform appropriate test and validation to justify your analysis.

#running a Shapiro-Wilk text for normality of quantitative variables
shapiro.test(insurance_data$Claims)
shapiro.test(insurance_data$Payment)
shapiro.test(insurance_data$Insured)

#producing Spearman's Rho correlation matrix for variables, used for assessing correlation between continuous variables.
ins_cor <- Hmisc::rcorr(as.matrix(insurance_data), type="spearman")
ins_cor_spr_r <- as.data.frame(ins_cor$r)
ins_cor_spr_P <- as.data.frame(ins_cor$P)



### The committee wants to figure out the reasons for insurance payment increase and decrease. So they have decided to find whether distance, location, bonus, make, and insured amount or claims are affecting the payment or all or some of these are affecting it. You need to develop a regression model, test your model and use appropriate graphs to address their concerns.

payment_insurance_data <- insurance_data

#creating general model for initial analysis
paymentlrall <- lm(Payment ~ ., data=payment_insurance_data)
summary(paymentlrall)
fm <- lm(Payment ~ 1, data=payment_insurance_data)
step(fm, direction="both", scope=formula(paymentlrall))
step(fm, direction="forward", scope=formula(paymentlrall))
step(paymentlrall, direction="backward")
testing_model <- lm(formula = Payment ~ Kilometres + Zone + Bonus + Insured + Claims, data = insurance_data)
summary(testing_model)
vif(testing_model)
mean(vif(testing_model))

#significant mean VIF, so creating two seperate models testing each of the correlated variables seperatley
claims_model <-  lm(formula = Payment ~ Kilometres + Zone + Bonus + Claims, data = insurance_data)
summary(claims_model)
vif(claims_model)
mean(vif(claims_model))
payment_insurance_data$C_Standard_Residual <- rstandard(claims_model)
payment_insurance_data$C_Large_Residual <- abs(payment_insurance_data$C_Standard_Residual > 2.58)
sum(payment_insurance_data$C_Large_Residual) / 2182
payment_insurance_data$C_Cooks_Distance <- cooks.distance(claims_model)
payment_insurance_data[payment_insurance_data$C_Cooks_Distance > 1,]

insured_model <-  lm(formula = Payment ~ Kilometres + Zone + Bonus + Insured, data = insurance_data)
summary(insured_model)
vif(insured_model)
mean(vif(insured_model))
payment_insurance_data$I_Standard_Residual <- rstandard(insured_model)
payment_insurance_data$I_Large_Residual <- abs(payment_insurance_data$I_Standard_Residual > 2.58)
sum(payment_insurance_data$I_Large_Residual) / 2182
payment_insurance_data$I_Cooks_Distance <- cooks.distance(insured_model)
payment_insurance_data[payment_insurance_data$I_Cooks_Distance > 1,]

## given the identification of observations with a cooks distance >1, we'll create a new dataset removing those values in order to see the affect that this has on the models.

outlier_test_data <- insurance_data
outlier_test_data <- outlier_test_data[-c(252, 691, 9, 1132),]#represents two iterations of outlier removal, better to proceed with outliers and explain in accompanying report.
outlier_claims_model <-  lm(formula = Payment ~ Kilometres + Zone + Bonus + Claims, data = outlier_test_data)
summary(outlier_claims_model)
vif(outlier_claims_model)
mean(vif(outlier_claims_model))
outlier_test_data$C_Standard_Residual <- rstandard(outlier_claims_model)
outlier_test_data$C_Large_Residual <- abs(outlier_test_data$C_Standard_Residual > 2.58)
sum(outlier_test_data$C_Large_Residual) / 2182
outlier_test_data$C_Cooks_Distance <- cooks.distance(outlier_claims_model)
outlier_test_data[outlier_test_data$C_Cooks_Distance > 1,]

anova(insured_model, claims_model)
lrtest(insured_model, claims_model)

#selecting model using claims as the final model and plotting residuals

Final_Payment_Model <-  lm(formula = Payment ~ Kilometres + Zone + Bonus + Claims, data = insurance_data)
Payment_Residuals <- ggplot(Final_Payment_Model, aes(x=fitted(Final_Payment_Model), y=residuals(Final_Payment_Model)))
Payment_Residuals + geom_point(shape=19, alpha=0.5) + geom_hline(yintercept=0, linetype="dashed") + theme_minimal() + ggtitle("Payment Model Residuals Against Fitted Values") + labs(x="Model Predicted Values (Payment ~ Claims + Kilometres + Zone + Bonus)", y="Residuals") + theme(plot.title = element_text(hjust=0.5))
bptest(Final_Payment_Model)

#visualising model

payment_splot <- scatterplot3d(insurance_data$Payment ~ insurance_data$Kilometres + insurance_data$Zone + insurance_data$Bonus + insurance_data$Claims, type="h", pch=19)
payment_splot$plane3d(Final_Payment_Model, lty="dashed") #unable to resolve error in adding plane

#attempting weighted regression to see if this will resolve heteroscedasticity in residual plotting
wt_p <- 1 / lm(abs(Final_Payment_Model$residuals) ~ Final_Payment_Model$fitted.values)$fitted.values^2
wls_Final_Payment_Model <- lm(Payment ~ Kilometres + Zone + Bonus + Claims, data = insurance_data, weights=wt_p)
summary(wls_Final_Payment_Model)
weighted_payment_residuals <- ggplot(wls_Final_Payment_Model, aes(x=fitted(wls_Final_Payment_Model), y=residuals(wls_Final_Payment_Model)))
weighted_payment_residuals + geom_point(shape=19, alpha=0.5) + geom_hline(yintercept=0, linetype="dashed") + theme_minimal() + ggtitle("Weighted Payment Model Residuals Against Fitted Values") + labs(x="Model Predicted Values (Payment ~ Claims + Kilometres + Zone + Bonus)", y="Residuals") + theme(plot.title = element_text(hjust=0.5))
#lower r-squared and residual plot still showing heteroscedasticity - model still good but cannot generalise results beyond sample

#using lm.boot to  heteroscedasticity
lmodel_p <- lm(Payment ~ Kilometres + Zone + Bonus + Claims, data = insurance_data)
lboot_p <- lm.boot(lmodel_p, R=2000)
summary(lboot_p)
Payment_Residuals <- ggplot(lboot_p, aes(x=fitted(lboot_p), y=residuals(lboot_p)))
Payment_Residuals + geom_point(shape=19, alpha=0.5) + geom_hline(yintercept=0, linetype="dashed") + theme_minimal() + ggtitle("Payment Model Residuals Against Fitted Values") + labs(x="Model Predicted Values (Payment ~ Claims + Kilometres + Zone + Bonus)", y="Residuals") + theme(plot.title = element_text(hjust=0.5))
w <- runif(nrow(model.frame(lmodel_p)))
wt <- 1 / lm(abs(lmodel_p$residuals) ~ lmodel_p$fitted.values)$fitted.values^2

lboot_p_w <- lm.boot(lmodel_p, R=2000, weights=wt)
Payment_Residuals <- ggplot(lboot_p_w, aes(x=fitted(lboot_p_w), y=residuals(lboot_p_w)))
Payment_Residuals + geom_point(shape=19, alpha=0.5) + geom_hline(yintercept=0, linetype="dashed") + theme_minimal() + ggtitle("Payment Model Residuals Against Fitted Values") + labs(x="Model Predicted Values (Payment ~ Claims + Kilometres + Zone + Bonus)", y="Residuals") + theme(plot.title = element_text(hjust=0.5))


#transforming dependent variable with Log10 for regression modelling, attempted Box-Cox, cube root, sqrt, and Tukey transformations for dependent variable.
insurance_data_p <- insurance_data
insurance_data_p$Payment <- log10(insurance_data_p$Payment +1)
log_model_p <- lm(Payment ~ Kilometres + Zone + Bonus + Claims + Make, data = insurance_data_p)
Log_Payment_Residuals <- ggplot(log_model_p, aes(x=fitted(log_model_p), y=residuals(log_model_p)))
Log_Payment_Residuals + geom_point(shape=19, alpha=0.5) + geom_hline(yintercept=0, linetype="dashed") + theme_minimal() + ggtitle("Payment Model Residuals Against Fitted Values") + labs(x="Model Predicted Values (Payment ~ Claims + Kilometres + Zone + Bonus)", y="Residuals") + theme(plot.title = element_text(hjust=0.5))


### The committee wants to understand what affects their claim rates so as to decide the right premiums for a certain set of situations. Hence, they need to find whether the insured amount, zone, kilometer, bonus, or make affects the claim rates and to what extent. Develop an appropriate regression model, test it and use meaningful graphs to help the committee.

#duplicating above work for claims regression modelling
claims_insurance_data <- insurance_data
claimslrall <- lm(Claims ~ ., data=claims_insurance_data)
summary(claimslrall)
fm2 <- lm(Claims ~ 1, data=claims_insurance_data)
step(fm2, direction="both", scope=formula(claimslrall))
step(fm2, directon="forward", scope=formula(claimslrall))
step(claimslrall, direction="forward")
claims_testing_model <- lm(Claims ~ Payment + Insured + Kilometres + Zone + Make + Bonus, data = claims_insurance_data)
vif(claims_testing_model)
mean(vif(claims_testing_model))

payment_testing_model_2 <- lm(Claims ~ Payment + Kilometres + Zone + Make + Bonus, data = claims_insurance_data)
summary(payment_testing_model_2)
mean(vif(payment_testing_model_2))
claims_insurance_data <- insurance_data
claims_insurance_data$P_Standard_Residual <- rstandard(payment_testing_model_2)
claims_insurance_data$P_Large_Residual <- abs(claims_insurance_data$P_Standard_Residual) > 2.58
sum(claims_insurance_data$P_Large_Residual) / 2182
claims_insurance_data$P_Cooks_Distance <- cooks.distance(claims_testing_model)
claims_insurance_data[claims_insurance_data$P_Cooks_Distance >1,]

insured_testing_model <- lm(Claims ~ Insured + Kilometres + Zone + Make + Bonus, data = claims_insurance_data) 
summary(insured_testing_model)
mean(vif(insured_testing_model))
claims_insurance_data$I_Standard_Residual <- rstandard(insured_testing_model)
claims_insurance_data$I_Large_Residual <- abs(claims_insurance_data$I_Standard_Residual) > 2.58
sum(claims_insurance_data$I_Large_Residual) / 2182
claims_insurance_data$I_Cooks_Distance <- cooks.distance(insured_testing_model)
claims_insurance_data[claims_insurance_data$I_Cooks_Distance >1,]

anova(insured_testing_model, payment_testing_model_2)
lrtest(insured_testing_model, payment_testing_model_2)

#as with payment, creating a data subset to analyse affect of removing outliers. 

outlier_test_data_2 <- insurance_data
outlier_test_data_2 <- outlier_test_data_2[-c(252, 691, 9, 1132),]#again, repersenting  two iterations of outlier removal, better to proceed with outliers and explain in accompanying report.
outlier_payment_testing_model_2 <-  lm(formula = Payment ~ Kilometres + Zone + Bonus + Claims, data = outlier_test_data_2)
summary(outlier_payment_testing_model_2)
vif(outlier_payment_testing_model_2)
mean(vif(outlier_payment_testing_model_2))
outlier_test_data_2$C_Standard_Residual <- rstandard(outlier_payment_testing_model_2)
outlier_test_data_2$C_Large_Residual <- abs(outlier_test_data_2$C_Standard_Residual > 2.58)
sum(outlier_test_data_2$C_Large_Residual) / 2182
outlier_test_data_2$C_Cooks_Distance <- cooks.distance(outlier_payment_testing_model_2)
outlier_test_data_2[outlier_test_data_2$C_Cooks_Distance > 1,]

##selecting model using payment as final model

Final_Claim_Model <- lm(Claims ~ Payment + Kilometres + Zone + Make + Bonus, data = claims_insurance_data)
summary(Final_Claim_Model)

Claim_splot <- scatterplot3d(claims_insurance_data$Claims ~ claims_insurance_data$Payment + claims_insurance_data$Kilometres + claims_insurance_data$Zone + claims_insurance_data$Make + claims_insurance_data$Bonus, hype="h", pch=19)
Claim_splot$plane3d(Final_Claim_Model, lty="dashed") # again unable to add plane to complete output

Claims_Residuals <- ggplot(Final_Claim_Model, aes(x=fitted(Final_Claim_Model), y=residuals(Final_Claim_Model)))
Claims_Residuals + geom_point(shape=19, alpha=0.5) + geom_hline(yintercept=0, linetype="dashed") + theme_minimal() + ggtitle("Claim Model Residuals Against Fitted Values") + labs(x="Model Predicted Values (Claims ~ Payment + Kilometres + Zone + Make + Bonus)", y="Residuals") + theme(plot.title = element_text(hjust=0.5))

Insured_Residuals <- ggplot(insured_testing_model, aes(x=fitted(insured_testing_model), y=residuals(insured_testing_model)))
Insured_Residuals + geom_point(shape=19, alpha=0.5) + geom_hline(yintercept=0, linetype="dashed") + theme_minimal() + ggtitle("Claim(Insured) Model Residuals Against Fitted Values") + labs(x="Model Predicted Values (Claims ~ Insured + Kilometres + Zone + Make + Bonus)", y="Residuals") + theme(plot.title = element_text(hjust=0.5))

#attempting weighted regression to overcome heteroscedasticity
wt_c <- 1 / lm(abs(Final_Claim_Model$residuals) ~ Final_Claim_Model$fitted.values)$fitted.values^2
wls_Final_Claim_Model <- lm(Claims ~ Payment + Kilometres + Zone + Make + Bonus, data = insurance_data, weights=wt_c)
summary(wls_Final_Claim_Model)
Weighted_claim_plot <- ggplot(wls_Final_Claim_Model, aes(x=fitted(wls_Final_Claim_Model), y=residuals(wls_Final_Claim_Model)))
Weighted_claim_plot + geom_point(shape=19, alpha=0.5) + geom_hline(yintercept=0, linetype="dashed") + theme_minimal() + ggtitle("Claim Model Residuals Against Fitted Values") + labs(x="Model Predicted Values (Claims ~ Payment + Kilometres + Zone + Make + Bonus)", y="Residuals") + theme(plot.title = element_text(hjust=0.5))
#as with payment, lower r-squared and residual plot still showing heteroscedasticity - model still good but cannot generalise results beyond sample


# The committee is planning to extend their coverage over a few more cities/areas in near future and would like to predict their payments and number of claims. The below scenarios have been given to get an idea of the future.

expansion_model_claim <-  lm(Claims ~ Zone + Kilometres + Bonus + Make + Insured, data=insurance_data)
summary(expansion_model_claim)
mean(vif(expansion_model_claim))
expansion_model_payment <- lm(Payment ~ Zone + Kilometres + Bonus + Make + Insured, data=insurance_data)
summary(expansion_model_payment)
mean(vif(expansion_model_payment))

prediction_model_assessment <- insurance_data
prediction_model_assessment$C_Model_Std_Residual <- rstandard(expansion_model_claim)
prediction_model_assessment$C_Model_Lrg_Residual <- abs(prediction_model_assessment$C_Model_Std_Residual) > 2.58
sum(prediction_model_assessment$C_Model_Lrg_Residual) / 2182
prediction_model_assessment$C_Cooks <- cooks.distance(expansion_model_claim)
prediction_model_assessment[prediction_model_assessment$C_Cooks >1,]

prediction_model_assessment$P_Model_Std_Residual <- rstandard(expansion_model_payment)
prediction_model_assessment$P_Model_Lrg_Residual <- abs(prediction_model_assessment$P_Model_Std_Residual) > 2.58
sum(prediction_model_assessment$P_Model_Lrg_Residual) / 2182
prediction_model_assessment$P_Cooks <- cooks.distance(expansion_model_payment)
prediction_model_assessment[prediction_model_assessment$P_Cooks >1,]

## Case 1: Vittangi (A small city in the north), 8500 km travel per year, Bonus for 2 years, type 3 cars with 4621 insured amount.
vittangi_prediction <- data.frame(Zone=c(5), Kilometres=c(2), Bonus=c(2), Make=c(3), Insured=c(4621))
Vittangi_claim_prediction <- predict(expansion_model_claim, vittangi_prediction)
vittangi_payment_prediction <- predict(expansion_model_payment, vittangi_prediction)

## Case 2: Halmstad and outskirt, type 9 cars with average 12500 km travel per year, no claim bonus, average 9500 insured amount
halmstad_prediction <- data.frame(Zone=c(2), Kilometres=c(2), Bonus=c(0), Make=c(9), Insured=c(9500))
halmstad_claims_prediction <- predict(expansion_model_claim, halmstad_prediction)
halmstad_payment_prediction <- predict(expansion_model_payment, halmstad_prediction)

## Case 3: Uppsala (A large city), average 22300 km travel per year, estimation between 17500 to 25416 insured amount, type 3 car, 4 years bonus.
uppsala_prediction_lower <- data.frame(Zone=c(2), Kilometres=c(4), Bonus=c(4), Make=c(3), Insured=c(17500))
uppsala_prediction_higher <- data.frame(Zone=c(2), Kilometres=c(4), Bonus=c(4), Make=c(3), Insured=c(25416))
uppsala_claims_prediction_lower <- predict(expansion_model_claim, uppsala_prediction_lower)
uppsala_claims_prediction_higher <- predict(expansion_model_claim, uppsala_prediction_higher)
uppsala_payment_prediction_lower <- predict(expansion_model_payment, uppsala_prediction_lower)
uppsala_payment_prediction_higher <- predict(expansion_model_payment, uppsala_prediction_higher) 

Location <- c("Vittangi", "Halmstad", "Uppsala (lower)", "Uppsala (higher)")
Claims <- c(Vittangi_claim_prediction, halmstad_claims_prediction, uppsala_claims_prediction_lower, uppsala_claims_prediction_higher)
Payment <- c(vittangi_payment_prediction, halmstad_payment_prediction, uppsala_payment_prediction_lower, uppsala_payment_prediction_higher)

Coverage_Prediction <- insurance_data[c(1830,511,658,658),]
Coverage_Prediction$Location <- Location
Coverage_Prediction <- Coverage_Prediction[,6:8]
Coverage_Prediction <- Coverage_Prediction[,c(3,1,2)]
Coverage_Prediction <- rename(Coverage_Prediction, "Current Claim" = "Claims", "Current Payment"="Payment")
Coverage_Prediction$"Predicted Payment Increase" <- Payment 
Coverage_Prediction$"Predicted Claim Increase" <- Claims
Coverage_Prediction$"New Payment Value" <- Coverage_Prediction$"Current Payment"+Coverage_Prediction$"Predicted Payment Increase"
Coverage_Prediction$"New Claims Total" <- Coverage_Prediction$"Current Claim" + Coverage_Prediction$"Predicted Claim Increase"
Coverage_Prediction$"Claim Increase %age" <- (Coverage_Prediction$"New Claims Total"-Coverage_Prediction$"Current Claim")/Coverage_Prediction$"Current Claim"
Coverage_Prediction$"Payment Increase %age" <- (Coverage_Prediction$"New Payment Value"-Coverage_Prediction$"Current Payment")/Coverage_Prediction$"Current Payment"
Coverage_Prediction <- Coverage_Prediction[,c(1,2,4,5,6,3,7,8,9)]

write.csv(Coverage_Prediction, "./Exports for Report/Coverage Prediction.csv")


# The insurance company is planning to establish a new branch office, so they are interested to find at what location, kilometer, and bonus level their insured amount, claims, and payment get increased. Use developed models to do this task.

Bonus_Summary <- summarise_at(group_by(insurance_data,Bonus), vars(Insured, Claims, Payment), funs(mean(.,na.rm=TRUE)))
write.csv(Bonus_Summary, "./Exports for Report/bonussummary.csv")

lm.beta(Final_Claim_Model)
lm.beta(Final_Payment_Model)


#creating a model to establish standardised b coef. for insured as the target variable
insuredlrall <- lm(Insured ~ ., data=insurance_data)
summary(insuredlrall)
fm3 <- lm(Insured ~ 1, data=insurance_data)
step(fm3, direction="both", scope=formula(insuredlrall))
step(fm3, directon="forward", scope=formula(insuredlrall))
step(insuredlrall, direction="forward")
insur_bcoef_zone <- lm(Insured ~ Payment + Claims + Bonus + Make + Kilometres + Zone, data = insurance_data)
insur_Bcoef <- lm(Insured ~ Payment + Claims + Bonus + Make + Kilometres, data = insurance_data)

anova(insur_bcoef_zone, insur_Bcoef)
lrtest(insur_bcoef_zone, insur_Bcoef)


summary(insur_Bcoef)
lm.beta(insur_Bcoef)
