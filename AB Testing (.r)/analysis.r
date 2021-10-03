library(ggplot2)
library(Hmisc)
library(dplyr)
library(tidyverse)
library(reshape2)

options(scipen = 999) #forcing R to display decimals instead of scientific notation

cust_data <- read.csv(file="C:/Users/Brian/Desktop/Joined.csv")
test_results <- read.csv(file ="C:/Users/Brian/Desktop/summary table.csv")

#creating top-level analysis of actions
test_perc <- test_results %>% mutate_at(vars(-name), funs(./sum(.)))
final_table <- cbind(test_results, test_perc)
final_table <- final_table[!duplicated(as.list(final_table))]
final_table <- final_table[,c(1,2,4,3,5)]
names(final_table)[3] <- "C.%AGE_OF_TOTAL"
names(final_table)[5] <- "T.%AGE_OF_TOTAL"
final_table <- final_table %>% mutate_at(vars(3, 5), funs(round(.,4)))

cust_data$converted <- ifelse (cust_data$name == 'pay', 1,0)
Control_Subset_Pay <- cust_data %>% filter(variant == 'control', converted==1)
conversions_control <- nrow(Control_Subset_Pay)
control_total <- nrow(cust_data%>%filter(variant=='control'))
conversion_rate_control <- (conversions_control/control_total)
print(conversion_rate_control)

Treatment_Subset_Pay <- cust_data %>% filter(variant == 'treatment', converted==1)
conversions_treatment <- nrow(Treatment_Subset_Pay)
treatment_total <- nrow(cust_data%>%filter(variant=='treatment'))
conversion_rate_treatment <- (conversions_treatment/treatment_total)
print(conversion_rate_treatment)

uplift <- (conversion_rate_treatment - conversion_rate_control)/conversion_rate_control*100#7.56%

#Pooled sample proportion for treatment and control variants
pooled_probability <- (conversion_rate_control+conversion_rate_treatment)/(control_total+treatment_total)#0.00000001
#Calculating Std Err
SE_pooled_probability <- sqrt(pooled_probability*(1-pooled_probability) * ((1/control_total) + (1/treatment_total)))#0.0000001
#Calculating Margin of Error
MOE <- SE_pooled_probability*qnorm(0.975)#0.000001
#Calculating Point Estimate
d_hat <- conversion_rate_treatment-conversion_rate_control#0.0001945339
#Calculating z-score
z_score <- d_hat/SE_pooled_probability#358.4021
#Calculating p-value
p_value <-pnorm(q=-z_score, mean=0, sd=1)*2#0
#calculating confidence interval
conf_int_lower <- d_hat - MOE
conf_int_upper <- d_hat+MOE

#interval for control variant separately
X_hat_control <- conversions_control/control_total
se_hat_control <- sqrt(X_hat_control*(1-X_hat_control)/control_total) 
ci_control <- c(X_hat_control - qnorm(0.975)*se_hat_control, X_hat_control + qnorm(0.975)*se_hat_control)#0.002302461 0.002846684

#interval for treatment variant separately
X_hat_treatment <- conversions_treatment/treatment_total
se_hat_treatment <- sqrt(X_hat_treatment*(1-X_hat_treatment)/treatment_total) 
ci_treatment <- c(X_hat_treatment - qnorm(0.975)*se_hat_treatment, X_hat_treatment + qnorm(0.975)*se_hat_treatment)#0.002489971 0.003048242

results_df <- data.frame(
  metric=c(
    'Estimated Difference',
    'Relative Uplift(%)',
    'pooled sample proportion',
    'Standard Error of Difference',
    'z_score',
    'p-value',
    'Margin of Error',
    'CI-lower',
    'CI-upper'),
  value=c(
    conversion_rate_treatment-conversion_rate_control,
    uplift,
    pooled_probability,
    SE_pooled_probability,
    z_score,
    p_value,
    MOE,
    conf_int_lower,
    conf_int_upper
  ))

#visualising results code imported directly from https://github.com/etomaa/A-B-Testing/blob/master/ABTest_Visualization.R (due to time constraint)
visdata = data.frame(
  variant = factor(rep(c("Control", "Treatment"), each=200)),
  conf_interval = c(rnorm(200, 0.002302461, 0.002846684), rnorm(200, 0.002489971, 0.003048242))
)

pe = data.frame(variant = c("Control","Treatment"), conversion_rate = round(c(conversion_rate_control, conversion_rate_treatment),4), lower_confidence = round(c(conf_int_lower,conf_int_upper),4))
vis <- ggplot(visdata, aes(x = conf_interval))
vis + geom_density(aes(fill = variant), alpha = 0.3) +
  geom_vline(aes(xintercept = lower_confidence[1], color = variant),
             data = pe, linetype = "dashed") +
  geom_vline(aes(xintercept = lower_confidence[2], color = variant),
             data = pe, linetype = "dashed") +
  geom_vline(aes(xintercept = conversion_rate, color = variant),
             data = pe, linetype = "dashed") +
  scale_color_manual(values = c("#b8b7b8", "#b8b7b8"))+
  scale_fill_manual(values = c("#e9e8e9", "#4b779d")) +
  labs(title = "Expected Distributions of Control and Treatment Variants",
       subtitle = "Treatment Variant's observed conversion rate (0.28%) was 7.56% higher than Control Variant's (0.26%). \nWe can be 95% confident that this result relates to updated user prompt and not simply random chance.",
       x = "Confidence Interval",
       y = "") +
  theme_classic() +
  theme(legend.position = "none") +
  theme(axis.ticks.y = element_blank(), #remove y axis ticks
        axis.title.y = element_blank()) + #remove y axis title
  theme(plot.title = ggplot2::element_text(family="Century Gothic",
                                           size=20,
                                           #face="bold",
                                           color="#002849")) +
  theme(plot.subtitle = ggplot2::element_text(family="Century Gothic",
                                              size=16,
                                              face="bold",
                                              color = "#4b779d"))+
  theme(panel.grid.major.y = ggplot2::element_line(color="#FAFAFA"))

##additional tasks

#barchart of events by role and variant
table(cust_data['role'])
role_obs <- cust_data[,c("created_at", "name", "variant", "role")]
role_obs <- role_obs[!(role_obs$role=="NULL" | role_obs$variant=="NULL"),]

bar <- ggplot(role_obs, aes(x=role, fill=name)) 
bar + geom_bar(position='dodge') + scale_fill_brewer() + facet_wrap(~variant) + scale_y_continuous(trans='log10') + labs(x='Account Role', y='Count (log10)') + ggtitle("Count of Events by Role and Variant") + theme(plot.title=element_text(hjust=0.5))

freqpoly <- ggplot(role_obs, aes(created_at, colour=variant))
freqpoly + geom_freqpoly(binwidth=2) + labs(x="Event Occurences (days)", y='Frequency') + ggtitle("Frequency plot of All Event Occurences by Variant Group") + theme(plot.title=element_text(hjust=0.5))