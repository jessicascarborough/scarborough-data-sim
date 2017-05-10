#### Data Simulation ####

set.seed(0)

# Overall Values

total.n.6_11 <- 134
total.n.12_18 <- 215
total.n.19_25 <- 179
total.n.26_36 <- 187


# Age

age.6_11 <- runif(n=total.n.6_11, min = 6, max = 11.9)
sd(age.6_11)
# sim sd: ~ 1.7
# original sd: 1.82
age.12_18 <- runif(n=total.n.12_18, min = 12, max = 18.9)
sd(age.12_18)
# sim sd: ~ 2.0
# original sd: 2.19

age.19_25 <- runif(n=total.n.19_25, min = 18, max = 25.9)
sd(age.19_25)
# sim sd: ~ 2.4
# original sd: 2.07

age.26_36 <- rnorm(n=total.n.26_36, mean=30.64, sd=3.07)
age.26_36 <- runif(n=total.n.26_36, min = 26, max = 36.9)
sd(age.26_36)
# sim sd: ~ 3.1
# original sd: 3.07


age <- c(age.6_11, age.12_18, age.19_25, age.26_36)
sd(age)
# sim sd: ~ 8.6
# original sd: 8.26

hist(age)
hist(age.6_11)
hist(age.12_18)
hist(age.19_25)
hist(age.26_36)

install.packages("truncnorm")
require(truncnorm)

# Touchscreen Use

# Touchscreen values for each age group

screen.mean.6_11 <- 8.53
screen.mean.12_18 <- 18.80
screen.mean.19_25 <- 25.18
screen.mean.26_36 <- 44.11
screen.mean.all <- 24.45

screen.sd.6_11 <- 15.54
screen.sd.12_18 <- 36.83
screen.sd.19_25 <- 37.46
screen.sd.26_36 <- 47.75
screen.sd.all <- 38.98

screen.n.6_11 <- 123
screen.n.12_18 <- 194
screen.n.19_25 <- 145
screen.n.26_36 <- 151
screen.n.all <- 612

# set.seed(0)

screen.6_11 <- (0.7)* rnorm(n=total.n.6_11, mean=(screen.mean.6_11-2.5), sd=screen.sd.6_11) + 
                (0.3)* rnorm(n=total.n.6_11, mean=3, sd=5)
screen.6_11 <- replace(screen.6_11, screen.6_11<0, 0)
screen.6_11[sample(1:length(screen.6_11), 
                   size = (total.n.6_11-screen.n.6_11), replace = FALSE)] <- NA
hist(screen.6_11)
sd(screen.6_11, na.rm = TRUE)
mean(screen.6_11, na.rm = TRUE)
# Sim mean typically hovers between 6.5-8.5
# Original mean: 7.53

# set.seed(0)
screen.12_18 <- (0.7)* rnorm(n=total.n.12_18, mean=(screen.mean.12_18-4), sd=screen.sd.12_18) + 
  (0.3)* rnorm(n=total.n.12_18, mean=10, sd=10)
screen.12_18 <- replace(screen.12_18, screen.12_18<0, 0)
screen.12_18[sample(1:length(screen.12_18), 
                    size = (total.n.12_18-screen.n.12_18), replace = FALSE)] <- NA
hist(screen.12_18)
sd(screen.12_18, na.rm = TRUE)
mean(screen.12_18, na.rm = TRUE)
# Sim mean typically hovers between 17-19.5
# Original mean: 18.8

# set.seed(0)
screen.19_25 <- (0.7)* rnorm(n=total.n.19_25, mean=(screen.mean.19_25-7), sd=screen.sd.19_25) + 
  (0.3)* rnorm(n=total.n.19_25, mean=15, sd=10)
screen.19_25 <- rnorm(n=total.n.19_25, mean=(25.18-7), sd=37.46)
screen.19_25 <- replace(screen.19_25, screen.19_25<0, 0)
screen.19_25[sample(1:length(screen.19_25), 
                    size = (total.n.19_25-screen.n.19_25), replace = FALSE)] <- NA
hist(screen.19_25)
sd(screen.19_25, na.rm = TRUE)
mean(screen.19_25, na.rm = TRUE)
# Final mean typically hovers between 24-28
# Original mean: 25.18

# set.seed(0)
screen.26_36 <- (0.7)* rnorm(n=total.n.26_36, mean=(screen.mean.26_36), sd=screen.sd.26_36) + 
  (0.3)* rnorm(n=total.n.26_36, mean=40, sd=10)
screen.26_36 <- replace(screen.26_36, screen.26_36<0, 0)
screen.26_36[sample(1:length(screen.26_36), 
                    size = (total.n.26_36-screen.n.26_36), replace = FALSE)] <- NA
hist(screen.26_36)
sd(screen.26_36, na.rm = TRUE)
mean(screen.26_36, na.rm = TRUE)
# Final mean typically hovers between 41-48
# Original mean: 44.11

hist(screen.6_11)
hist(screen.12_18)
hist(screen.19_25)
hist(screen.26_36)

screentime <- c(screen.6_11, screen.12_18, screen.19_25, screen.26_36)
hist(screentime)

impute_screen.6_11 <- replace(screen.6_11, is.na(screen.6_11), 
                              mean(screen.6_11, na.rm=TRUE))
impute_screen.12_18 <- replace(screen.12_18, is.na(screen.12_18), 
                               mean(screen.12_18, na.rm=TRUE))
impute_screen.19_25 <- replace(screen.19_25, is.na(screen.19_25), 
                               mean(screen.19_25, na.rm=TRUE))
impute_screen.26_36 <- replace(screen.26_36, is.na(screen.26_36), 
                               mean(screen.26_36, na.rm=TRUE))

# Sex

sex.6_11 <- rep(x = "Female", times = total.n.6_11)
sex.6_11[sample(1:length(sex.6_11), size=ceiling(total.n.6_11*0.5149), 
                replace = FALSE)] <- "Male"

sex.12_18 <- rep(x = "Female", times = total.n.12_18)
sex.12_18[sample(1:length(sex.12_18), size=ceiling(total.n.12_18*0.5302), 
                 replace = FALSE)] <- "Male"


sex.19_25 <- rep(x = "Female", times = total.n.19_25)
sex.19_25[sample(1:length(sex.19_25), size=ceiling(total.n.19_25*0.5587), 
                 replace = FALSE)] <- "Male"


sex.26_36 <- rep(x = "Female", times = total.n.26_36)
sex.26_36[sample(1:length(sex.26_36), size=ceiling(total.n.26_36*0.5134), 
               replace = FALSE)] <- "Male"

sex <- c(sex.6_11, sex.12_18, sex.19_25, sex.26_36)
length(sex)

# Background TV

# N values for each age group
tv.n.6_11 <- 123
tv.n.12_18 <- 194
tv.n.19_25 <- 145
tv.n.26_36 <- 151

# TV values for each age group

tv_sd.6_11 <- 186.08
tv_sd.12_18 <- 172.70
tv_sd.19_25 <- 162.99
tv_sd.26_36 <-183.24

tv_mean.6_11 <- 209.72
tv_mean.12_18 <- 189.62
tv_mean.19_25 <- 187.00
tv_mean.26_36 <- 219.01


# Ratio values for each age group
tv_screen_ratio.6_11 <- tv_mean.6_11/screen_mean.6_11
tv_screen_ratio.12_18 <- tv_mean.12_18/screen_mean.12_18
tv_screen_ratio.19_25 <- tv_mean.19_25/screen_mean.19_25
tv_screen_ratio.26_36 <- tv_mean.26_36/screen_mean.26_36


tv_mean.all <-200.27



tv.6_11 <- (0.6)*(impute_screen.6_11 * tv_screen_ratio.6_11) + 
  (0.4)*(tv_mean.6_11 + rnorm(tv.n.6_11, sd=tv_sd.6_11))
tv.6_11[sample(1:length(tv.6_11), size=(total.n.6_11-tv.n.6_11), 
               replace = FALSE)] <- NA
tv.6_11 <- replace(tv.6_11, tv.6_11<0, 0)
mean(tv.6_11, na.rm=TRUE)


tv.12_18 <- (0.6)*(impute_screen.12_18 * tv_screen_ratio.12_18) + 
  (0.4)*(tv_mean.12_18 + rnorm(tv.n.12_18, sd=tv_sd.12_18))
tv.12_18[sample(1:length(tv.12_18), size=(total.n.12_18-tv.n.12_18), 
                replace = FALSE)] <- NA
tv.12_18 <- replace(tv.12_18, tv.12_18<0, 0)
mean(tv.12_18, na.rm=TRUE)



tv.19_25 <- (0.6)*(impute_screen.19_25 * tv_screen_ratio.19_25) + 
  (0.4)*(tv_mean.19_25 + rnorm(tv.n.19_25, sd=tv_sd.19_25))
tv.19_25[sample(1:length(tv.19_25), size=(total.n.19_25-tv.n.19_25), 
                replace = FALSE)] <- NA
tv.19_25 <- replace(tv.19_25, tv.19_25<0, 0)
mean(tv.19_25, na.rm=TRUE)



tv.26_36 <- (0.6)*(impute_screen.26_36 * tv_screen_ratio.26_36) + 
  (0.4)*(tv_mean.26_36 + rnorm(tv.n.26_36, sd=tv_sd.26_36))
tv.26_36[sample(1:length(tv.26_36), size=(total.n.26_36-tv.n.26_36), 
                replace = FALSE)] <- NA
tv.26_36 <- replace(tv.26_36, tv.26_36<0, 0)
mean(tv.26_36, na.rm=TRUE)

tv <- c(tv.6_11, tv.12_18, tv.19_25, tv.26_36)

## Night-time sleep duration

# Overall values for each age group
ntsleep.n.6_11 <- 116
ntsleep.n.12_18 <- 176
ntsleep.n.19_25 <- 132
ntsleep.n.26_36 <- 130

ntsleep.sd.6_11 <- 64.81
ntsleep.sd.12_18 <- 50.13
ntsleep.sd.19_25 <- 60.12
ntsleep.sd.26_36 <-61.81

ntsleep.mean.6_11 <- 637.24
ntsleep.mean.12_18 <- 647.24
ntsleep.mean.19_25 <- 651.25
ntsleep.mean.26_36 <- 643.21


# Ratio values for each age group


ntsleep_screen_ratio.6_11 <- ntsleep.mean.6_11/screen.mean.6_11
ntsleep_screen_ratio.12_18 <- ntsleep.mean.12_18/screen.mean.12_18
ntsleep_screen_ratio.19_25 <- ntsleep.mean.19_25/screen.mean.19_25
ntsleep_screen_ratio.26_36 <- ntsleep.mean.26_36/screen.mean.26_36


ntsleep.6_11 <- rtruncnorm(n=total.n.6_11, a=(ntsleep.mean.6_11-ntsleep.sd.6_11*2), 
                            b=(ntsleep.mean.6_11+ntsleep.sd.6_11*3),
                            mean=ntsleep.mean.6_11+30,
                            sd=ntsleep.sd.6_11) - 
                0.05*(impute_screen.6_11 * ntsleep_screen_ratio.6_11)
ntsleep.6_11[sample(1:length(ntsleep.6_11), size=(total.n.6_11-ntsleep.n.6_11), 
               replace = FALSE)] <- NA
hist(ntsleep.6_11)
mean(ntsleep.6_11, na.rm=TRUE)
# Sim mean hovers around: 633-644
# Original mean: 637.24

ntsleep.12_18 <- rtruncnorm(n=total.n.12_18, a=(ntsleep.mean.12_18-ntsleep.sd.12_18*2), 
                           b=(ntsleep.mean.12_18+ntsleep.sd.12_18*3),
                           mean=ntsleep.mean.12_18+35,
                           sd=ntsleep.sd.12_18) - 
                0.05*(impute_screen.12_18 * ntsleep_screen_ratio.12_18)
ntsleep.12_18[sample(1:length(ntsleep.12_18), size=(total.n.12_18-ntsleep.n.12_18), 
                    replace = FALSE)] <- NA
hist(ntsleep.12_18)
mean(ntsleep.12_18, na.rm=TRUE)
# Sim mean hovers around: 644-652
# Original mean: 647.24


ntsleep.19_25 <- rtruncnorm(n=total.n.19_25, a=(ntsleep.mean.19_25-ntsleep.sd.19_25*2), 
                            b=(ntsleep.mean.19_25+ntsleep.sd.19_25*3),
                            mean=ntsleep.mean.19_25+35,
                            sd=ntsleep.sd.19_25) - 
                0.05*(impute_screen.19_25 * ntsleep_screen_ratio.19_25)
ntsleep.19_25[sample(1:length(ntsleep.19_25), size=(total.n.19_25-ntsleep.n.19_25), 
                     replace = FALSE)] <- NA
hist(ntsleep.19_25)
mean(ntsleep.19_25, na.rm=TRUE)
# Sim mean hovers between: 648-654
# Original mean: 651.25

ntsleep.26_36 <- rtruncnorm(n=total.n.26_36, a=(ntsleep.mean.26_36-ntsleep.sd.26_36*2), 
                            b=(ntsleep.mean.26_36+ntsleep.sd.26_36*3),
                            mean=ntsleep.mean.26_36+36,
                            sd=ntsleep.sd.26_36) - 
  0.05*(impute_screen.26_36 * ntsleep_screen_ratio.26_36)
ntsleep.26_36[sample(1:length(ntsleep.26_36), size=(total.n.26_36-ntsleep.n.26_36), 
                     replace = FALSE)] <- NA
hist(ntsleep.26_36)
mean(ntsleep.26_36, na.rm=TRUE)
# Sim mean hovers around: 641-649
# Original mean: 643.21


ntsleep <- c(ntsleep.6_11, ntsleep.12_18, ntsleep.19_25, ntsleep.26_36)
hist(ntsleep)



## Day-time sleep duration

# Overall values for each age group
daysleep.n.6_11 <- 116
daysleep.n.12_18 <- 176
daysleep.n.19_25 <- 132
daysleep.n.26_36 <- 126

daysleep_sd.6_11 <- 47.27
daysleep_sd.12_18 <- 38.40
daysleep_sd.19_25 <- 40.28
daysleep_sd.26_36 <- 53.20

daysleep_mean.6_11 <- 139.05
daysleep_mean.12_18 <- 122.47
daysleep_mean.19_25 <- 100.57
daysleep_mean.26_36 <- 68.25


# Ratio values for each age group
daysleep_screen_ratio.6_11 <- daysleep_mean.6_11/screen_mean.6_11
daysleep_screen_ratio.12_18 <- daysleep_mean.12_18/screen_mean.12_18
daysleep_screen_ratio.19_25 <- daysleep_mean.19_25/screen_mean.19_25
daysleep_screen_ratio.26_36 <- daysleep_mean.26_36/screen_mean.26_36


daysleep.6_11 <- (0.6)*(impute_screen.6_11 * daysleep_screen_ratio.6_11) + 
  (0.4)*(daysleep_mean.6_11 + rnorm(daysleep.n.6_11, sd=daysleep_sd.6_11))
daysleep.6_11[sample(1:length(daysleep.6_11), size=(total.n.6_11-daysleep.n.6_11),
                    replace = FALSE)] <- NA
daysleep.6_11 <- replace(daysleep.6_11, daysleep.6_11<0, 0)
mean(daysleep.6_11, na.rm = TRUE)


daysleep.12_18 <- (0.6)*(impute_screen.12_18 * daysleep_screen_ratio.12_18) + 
  (0.4)*(daysleep_mean.12_18 + rnorm(daysleep.n.12_18, sd=daysleep_sd.12_18))
daysleep.12_18[sample(1:length(daysleep.12_18), size=(total.n.12_18-daysleep.n.12_18),
                     replace = FALSE)] <- NA
daysleep.12_18 <- replace(daysleep.12_18, daysleep.12_18<0, 0)
mean(daysleep.12_18, na.rm = TRUE)


daysleep.19_25 <- (0.6)*(impute_screen.19_25 * daysleep_screen_ratio.19_25) + 
  (0.4)*(daysleep_mean.19_25 + rnorm(daysleep.n.19_25, sd=daysleep_sd.19_25))
daysleep.19_25[sample(1:length(daysleep.19_25), size=(total.n.19_25-daysleep.n.19_25),
                     replace = FALSE)] <- NA
daysleep.19_25 <- replace(daysleep.19_25, daysleep.19_25<0, 0)
mean(daysleep.19_25, na.rm = TRUE)


daysleep.26_36 <- (0.6)*(impute_screen.26_36 * daysleep_screen_ratio.26_36) + 
  (0.4)*(daysleep_mean.26_36 + rnorm(daysleep.n.26_36, sd=daysleep_sd.26_36))
daysleep.26_36[sample(1:length(daysleep.26_36), size=(total.n.26_36-daysleep.n.26_36),
                     replace = FALSE)] <- NA
daysleep.26_36 <- replace(daysleep.26_36, daysleep.26_36<0, 0)
mean(daysleep.26_36, na.rm = TRUE)

daysleep <- c(daysleep.6_11, daysleep.12_18, daysleep.19_25, daysleep.26_36)

# Total sleep

totalsleep <- ntsleep+daysleep


# Average number of night awakenings

# Overall values for each age group

wake.n.6_11 <- 116
wake.n.12_18 <- 177
wake.n.19_25 <- 133
wake.n.26_36 <- 130

wake_sd.6_11 <- 1.73
wake_sd.12_18 <- 1.33
wake_sd.19_25 <- 1.01
wake_sd.26_36 <- 0.81

wake_mean.6_11 <- 2
wake_mean.12_18 <- 1.29
wake_mean.19_25 <- 0.91
wake_mean.26_36 <- 0.58


# Ratio values for each age group
wake_screen_ratio.6_11 <- wake_mean.6_11/screen_mean.6_11
wake_screen_ratio.12_18 <- wake_mean.12_18/screen_mean.12_18
wake_screen_ratio.19_25 <- wake_mean.19_25/screen_mean.19_25
wake_screen_ratio.26_36 <- wake_mean.26_36/screen_mean.26_36


wake.6_11 <- ceiling((0.6)*(impute_screen.6_11 * wake_screen_ratio.6_11) + 
  (0.4)*(wake_mean.6_11 + rnorm(wake.n.6_11, sd=wake_sd.6_11)) - 0.55)
wake.6_11[sample(1:length(wake.6_11), size=(total.n.6_11-wake.n.6_11),
                     replace = FALSE)] <- NA
wake.6_11 <- replace(wake.6_11, wake.6_11<0, 0)
mean(wake.6_11, na.rm = TRUE)


wake.12_18 <- ceiling((0.6)*(impute_screen.12_18 * wake_screen_ratio.12_18) + 
  (0.4)*(wake_mean.12_18 + rnorm(wake.n.12_18, sd=wake_sd.12_18))-0.45)
wake.12_18[sample(1:length(wake.12_18), size=(total.n.12_18-wake.n.12_18),
                      replace = FALSE)] <- NA
wake.12_18 <- replace(wake.12_18, wake.12_18<0, 0)
mean(wake.12_18, na.rm = TRUE)


wake.19_25 <- floor((0.6)*(impute_screen.19_25 * wake_screen_ratio.19_25) + 
  (0.4)*(wake_mean.19_25 + rnorm(wake.n.19_25, sd=wake_sd.19_25)))
wake.19_25[sample(1:length(wake.19_25), size=(total.n.19_25-wake.n.19_25),
                      replace = FALSE)] <- NA
wake.19_25 <- replace(wake.19_25, wake.19_25<0, 0)
mean(wake.19_25, na.rm = TRUE)


wake.26_36 <- ceiling((0.6)*(impute_screen.26_36 * wake_screen_ratio.26_36) + 
  (0.4)*(wake_mean.26_36 + rnorm(wake.n.26_36, sd=wake_sd.26_36))+.3)
wake.26_36[sample(1:length(wake.26_36), size=(total.n.26_36-wake.n.26_36),
                      replace = FALSE)] <- NA
wake.26_36 <- replace(wake.26_36, wake.26_36<0, 0)
mean(wake.26_36, na.rm = TRUE)

wake <- c(wake.6_11, wake.12_18, wake.19_25, wake.26_36)



# Sleep onset

# Overall values for each age group
onset.n.6_11 <- 116
onset.n.12_18 <- 177
onset.n.19_25 <- 131
onset.n.26_36 <- 130

onset_sd.6_11 <- 23.69
onset_sd.12_18 <- 16.53
onset_sd.19_25 <- 16.97
onset_sd.26_36 <- 27.86

onset_mean.6_11 <- 22.80
onset_mean.12_18 <- 21.54
onset_mean.19_25 <- 22.22
onset_mean.26_36 <- 29.34


# Ratio values for each age group
onset_screen_ratio.6_11 <- onset_mean.6_11/screen_mean.6_11
onset_screen_ratio.12_18 <- onset_mean.12_18/screen_mean.12_18
onset_screen_ratio.19_25 <- onset_mean.19_25/screen_mean.19_25
onset_screen_ratio.26_36 <- onset_mean.26_36/screen_mean.26_36


onset.6_11 <- (0.6)*(impute_screen.6_11 * onset_screen_ratio.6_11) + 
  (0.4)*(onset_mean.6_11 + rnorm(onset.n.6_11, sd=onset_sd.6_11))
onset.6_11[sample(1:length(onset.6_11), size=(total.n.6_11-onset.n.6_11),
                     replace = FALSE)] <- NA
onset.6_11 <- replace(onset.6_11, onset.6_11<0, 0)
mean(onset.6_11, na.rm = TRUE)


onset.12_18 <- (0.6)*(impute_screen.12_18 * onset_screen_ratio.12_18) + 
  (0.4)*(onset_mean.12_18 + rnorm(onset.n.12_18, sd=onset_sd.12_18))
onset.12_18[sample(1:length(onset.12_18), size=(total.n.12_18-onset.n.12_18),
                      replace = FALSE)] <- NA
onset.12_18 <- replace(onset.12_18, onset.12_18<0, 0)
mean(onset.12_18, na.rm = TRUE)


onset.19_25 <- (0.6)*(impute_screen.19_25 * onset_screen_ratio.19_25) + 
  (0.4)*(onset_mean.19_25 + rnorm(onset.n.19_25, sd=onset_sd.19_25))
onset.19_25[sample(1:length(onset.19_25), size=(total.n.19_25-onset.n.19_25),
                      replace = FALSE)] <- NA
onset.19_25 <- replace(onset.19_25, onset.19_25<0, 0)
mean(onset.19_25, na.rm = TRUE)


onset.26_36 <- (0.6)*(impute_screen.26_36 * onset_screen_ratio.26_36) + 
  (0.4)*(onset_mean.26_36 + rnorm(onset.n.26_36, sd=onset_sd.26_36))
onset.26_36[sample(1:length(onset.26_36), size=(total.n.26_36-onset.n.26_36),
                      replace = FALSE)] <- NA
onset.26_36 <- replace(onset.26_36, onset.26_36<0, 0)
mean(onset.26_36, na.rm = TRUE)

onset <- c(onset.6_11, onset.12_18, onset.19_25, onset.26_36)


df <- data.frame(age, sex, tv, screentime, ntsleep, daysleep, totalsleep, wake, onset)

df$age_category <- factor(rep(x = "26-36 months", times=length(df$age)), 
                             levels = c("6-11 months", "12-18 months", 
                                        "19-25 months", "26-36 months"))
df$age_category[df$age < 26] <- "19-25 months"
df$age_category[df$age < 19] <- "12-18 months"
df$age_category[df$age < 12] <- "6-11 months"
#### Data Exploration ####

library(ggplot2)

ggplot(data=df, aes(x=tv, y=screentime, color=age_category)) + 
  geom_point() + 
  facet_wrap(~age_category) +
  labs(title = "Background TV vs. Touchscreen Use", 
       y = "Touchscreen Use/Day (minutes)", 
       x = "Night-time Sleep")

ggplot(data=df, aes(x=screentime, color=age_category, fill=age_category)) + 
  geom_density(alpha=0.5) + 
  labs(title = "Touchscreen Use by Age Category", 
       x = "Touchscreen Use (minutes)")

# Alternative method for simulating TV, not as good as the final chosen method

# tv.6_11 <- rnorm(total.n.6_11, mean=(209.72-10), sd=186.08)
# tv.6_11 <- replace(tv.6_11, tv.6_11<0, 0)
# na.tv.sample <- sample(tv.6_11, 
#                        size=(total.n.6_11-tv.n.6_11), 
#                        replace=FALSE)
# tv.6_11 <- replace(tv.6_11, na.tv.sample, NA)
# mean(tv.6_11, na.rm=TRUE)
# # Final mean falls between 190-220
# 
# tv.12_18 <- rnorm(total.n.12_18, mean=(189.62-17), sd=172.70)
# tv.12_18 <- replace(tv.12_18, tv.12_18<0, 0)
# na.tv.sample <- sample(tv.12_18, 
#                        size=(total.n.12_18-tv.n.12_18), 
#                        replace=FALSE)
# tv.12_18 <- replace(tv.12_18, na.tv.sample, NA)
# mean(tv.12_18, na.rm=TRUE)
# # Final mean falls between 170-200
# 
# tv.19_25 <- rnorm(total.n.19_25, mean=(187.00-12), sd=162.99)
# tv.19_25 <- replace(tv.19_25, tv.19_25<0, 0)
# na.tv.sample <- sample(tv.19_25, 
#                        size=(total.n.19_25-tv.n.19_25), 
#                        replace=FALSE)
# tv.19_25 <- replace(tv.19_25, na.tv.sample, NA)
# mean(tv.19_25, na.rm=TRUE)
# # Final mean falls between 170-210
# # Larger variation than previous age group because there's a smaller sample size
# 
# 
# tv.26_36 <- rnorm(total.n.26_36, mean=(219.01-7), sd=175.99)
# tv.26_36 <- replace(tv.26_36, tv.26_36<0, 0)
# na.tv.sample <- sample(tv.26_36, 
#                        size=(total.n.26_36-tv.n.26_36), 
#                        replace=FALSE)
# tv.26_36 <- replace(tv.26_36, na.tv.sample, NA)
# mean(tv.26_36, na.rm=TRUE)
# # Final mean falls between 205-235


