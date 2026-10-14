library(lme4)
library(emmeans)
library(car)
library(tidyr)
library(dplyr)
library(ggplot2)
library(boot)
library(splines)

getwd()
setwd("/Users/meredithlee/Desktop")

myd <- read.csv("total as of july 5.txt", stringsAsFactors=T)
summary(myd)

# Exclude 5ee5e3b02eebe932ee9559c5 (did not finish task), 67ff6b52bd05ef852331bbdf (only has one trial for 0.75 and 0.5 Power)
exclude_ids <- c("5ee5e3b02eebe932ee9559c5", "67ff6b52bd05ef852331bbdf", "5c715022b2da8a0001ee0a84")
myd <- myd[!myd$ID %in% exclude_ids, ]
summary(myd)

# filter data
myd = subset(myd, frameRate>5)
myd = subset(myd, IRT<20)
summary(myd)

# add computed variables
ages = myd %>% group_by(ID) %>% summarize(m.age = mean(Age)) 
mean.age = mean(ages$m.age)
myd$c.power = myd$Power - 1
myd$c.age = myd$Age - mean.age 
myd$Waiting = ifelse(myd$IRT>5,1,0)
contrasts(myd$Sex) = contr.sum(3)

summary(myd)


# Calculate the standard deviation of the mean ages
sd_of_ages <- sd(ages$m.age)
print(sd_of_ages)

# Define custom age breaks
custom_breaks_fixed <- c(18, 30, 42, 54, 66) 
custom_labels_fixed <- c("18-29", "30-41", "42-53", "54-65")

ages_with_custom_bins <- ages %>%
  mutate(age_group = cut(m.age,
                         breaks = custom_breaks_fixed, 
                         labels = custom_labels_fixed,
                         include.lowest = TRUE,
                         right = FALSE))

table_1_custom_data <- ages_with_custom_bins %>%
  group_by(age_group) %>%
  summarize(
    Participant_Count = n()
  ) %>%
  ungroup()

library(knitr)
table_markdown <- kable(
  table_1_custom_data,
  format = "pipe",
  col.names = c("Age Range", "Number of Participants"),
  caption = "Table 1: Distribution of Participants by Age Group")

print(table_markdown)



# analyze data

### investigate how the probability of waiting is influenced by c.power, while accounting for individual differences and how individuals respond to changes in c.power.
myr <- glmer(Waiting ~ c.power + (c.power | ID), family=binomial, data=myd)
summary(myr)

emmeans(myr, ~c.power, at=list(c.power=c(.5, .75, 1, 1.25, 1.5) - 1)) # in log odds space
emmeans(myr, ~c.power, at=list(c.power=c(.5, .75, 1, 1.25, 1.5) - 1), type="response") # in percentage space


myr2 <- glmer(Waiting ~ c.power*c.age + (c.power | ID), family=binomial, data=myd)
summary(myr2)

emmeans(myr2, ~c.age, at=list(c.age=c(18, 28, 38, 48, 58, 65)- mean.age), type="response")

anova(myr, myr2)

myr3 <- glmer(Waiting ~ c.power*c.age + Sex + (c.power| ID), family=binomial, data=subset(myd, Sex != "Other"))
summary(myr3)

myr4 <- glmer(Waiting ~ c.power*c.age +I(c.age^2)*c.power+ (c.power | ID), family=binomial, data=myd)
summary(myr4)
anova(myr, myr4)

toplot = as.data.frame(emmeans(myr2, ~c.power, at=list(c.power=seq(-.5, .5, .01))))
ggplot(toplot, aes(x=c.power + 1, y = inv.logit(emmean))) + 
  geom_line() +
  geom_ribbon(aes(ymin=inv.logit(emmean-SE), ymax=inv.logit(emmean+SE)), alpha=.3) +
  ylim(0,1) + ylab("P(Waiting)") + xlab("Power") + 
  theme_bw()





# Probability of waiting for each power value by age group
representative_ages_for_plot <- c(
  (18 + 29) / 2, # for 18-29
  (30 + 41) / 2, # for 30-41
  (42 + 53) / 2, # for 42-53
  (54 + 65) / 2  # for 64-65
)

toplot <- as.data.frame(emmeans(myr2, ~c.power * c.age,
                                at = list(c.power = seq(-.5, .5, .01),
                                          c.age = representative_ages_for_plot - mean.age)))

age_map <- data.frame(
  raw_age = representative_ages_for_plot,
  label = custom_labels_fixed
)

toplot <- toplot %>%
  mutate(raw_age_from_c_age = c.age + mean.age) %>% 
  left_join(age_map, by = c("raw_age_from_c_age" = "raw_age")) %>%
  mutate(age_group_label = factor(label, levels = custom_labels_fixed))

ggplot(toplot, aes(x = c.power + 1, y = inv.logit(emmean))) +
  geom_line() +
  geom_ribbon(aes(ymin = inv.logit(emmean - SE), ymax = inv.logit(emmean + SE)), alpha = .3) +
  facet_grid(~age_group_label) +
  ylim(0, 1) + ylab("P(Waiting)") + xlab("Power") +
  theme_bw()

### Origninal***
# toplot = as.data.frame(emmeans(myr2, ~c.power*c.age, 
#                                at=list(c.power=seq(-.5, .5, .01),
#                                        c.age=c(18, 33, 48, 63)-mean.age)))
# ggplot(toplot, aes(x=c.power + 1, y = inv.logit(emmean))) + 
#   geom_line() +
#   geom_ribbon(aes(ymin=inv.logit(emmean-SE), ymax=inv.logit(emmean+SE)), alpha=.3) +
#   facet_grid(~round(c.age+mean.age)) +
#   ylim(0,1) + ylab("P(Waiting)") + xlab("Power") + 
#   theme_bw()

toplot = as.data.frame(emmeans(myr2, ~c.age, 
                               at=list(c.age=seq(18,65)- mean.age)))
ggplot(toplot, aes(x=c.age + mean.age, y = inv.logit(emmean))) + 
  geom_line() +
  geom_ribbon(aes(ymin=inv.logit(emmean-SE), ymax=inv.logit(emmean+SE)), alpha=.3) +
  ylim(0,1) + ylab("P(Waiting)") + xlab("Age") + 
  theme_bw()

toplot = as.data.frame(emtrends(myr2, ~c.age, var="c.power", at=list(c.age=seq(18,65)- mean.age)))
ggplot(toplot, aes(x=c.age + mean.age, y = c.power.trend)) + 
  geom_line() +
  geom_ribbon(aes(ymin=c.power.trend-SE, ymax=c.power.trend+SE), alpha=.3) +
  ylab("Power Sensitivity") + xlab("Age") + ylim(0, -7.5) +
  theme_bw()

myd$fitted = fitted(myr2)

# Individual differences
#Original
# ggplot(myd, aes(x=Power, y = fitted, group=ID, color=ID)) + 
#   geom_line() +
#   ylab("P(Waited)") + ylim(0,1) + 
#   facet_grid(~cut(Age,5)) + 
#   theme_bw() + 
#   theme(legend.position="none") 


plot_data_with_age_groups <- myd %>%
  mutate(
    age_group_label = cut(Age,
                          breaks = custom_breaks_fixed,
                          labels = custom_labels_fixed,
                          include.lowest = TRUE,
                          right = FALSE),
    age_group_label = factor(age_group_label, levels = custom_labels_fixed)
  )

ggplot(plot_data_with_age_groups, aes(x=Power, y = fitted, group=ID, color=ID)) +
  geom_line() +
  ylab("P(Waited)") +
  ylim(0,1) +
  facet_grid(~age_group_label) +
  theme_bw() +
  theme(legend.position="none") +
  ggtitle("Individual Differences in Waiting Behavior by Age Group")


totalTime = myd %>% group_by(ID,Age) %>% summarize(sumTime=sum(IRT),nFishCaught=max(RewardObject),
                                                   FishCaughtPerMinute=nFishCaught/sumTime*60) 
sensitivity = coef(myr2)$ID$c.power

hist(totalTime$FishCaughtPerMinute)
hist(sensitivity)


# Points per object isn't performing well because each object *should* 
# require the same number of points to destroy.  That's why the range of values
# is so small.  Variation is likely due to how long they waited after the bar was full.

# now to combine the results with age...  Does age predict 'optimality'?
jill <- lm(FishCaughtPerMinute~Age, data=totalTime)
summary(jill) # no relationship
julie <- lm(sensitivity~Age, data=totalTime) # This is already captured in the original analysis, myr2
summary(julie) # no relationship; much weaker approach than original since based on aggregate

ggplot(totalTime, aes(x=Age, y=FishCaughtPerMinute))+
  geom_point()+
  geom_smooth(method = "lm")+
  ylab("Fish Caught Per Minute")+
  theme_bw()

##########
# OKAY, s.d. 

stDevs = myd %>% group_by(ID, Age) %>% summarize(sd=sd(IRT))
jack = lm(sd~Age, stDevs) # Small *positive* relationship - more variability for older
summary(jack)                                               
plot(stDevs$sd ~ stDevs$Age) 

ggplot(stDevs, aes(x=Age, y=sd))+
  geom_point()+
  geom_smooth(method = "lm")+
  ylab("St. Dev. of IRT")+
  theme_bw()










