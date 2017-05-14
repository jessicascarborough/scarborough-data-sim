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
mean(age)
# Simulated mean: 20.167
# Original mean: 19.52
shapiro.test(age)
# data is normal
# 2.496e-13


hist(age)
hist(age.6_11)
hist(age.12_18)
hist(age.19_25)
hist(age.26_36)



impute.age.6_11 <- replace(age.6_11, is.na(age.6_11), 
                              mean(age.6_11, na.rm=TRUE))
impute.age.12_18 <- replace(age.12_18, is.na(age.12_18), 
                               mean(age.12_18, na.rm=TRUE))
impute.age.19_25 <- replace(age.19_25, is.na(age.19_25), 
                               mean(age.19_25, na.rm=TRUE))
impute.age.26_36 <- replace(age.26_36, is.na(age.26_36), 
                               mean(age.26_36, na.rm=TRUE))





# install.packages("truncnorm")
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
mean(screentime, na.rm=TRUE)
# Simulated mean: 24.5721
# Original mean: 24.45
shapiro.test(screentime)
# data is normal
# p-value < 2.2 e -16

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

tv.sd.6_11 <- 186.08
tv.sd.12_18 <- 172.70
tv.sd.19_25 <- 162.99
tv.sd.26_36 <-183.24

tv.mean.6_11 <- 209.72
tv.mean.12_18 <- 189.62
tv.mean.19_25 <- 187.00
tv.mean.26_36 <- 219.01


# Ratio values for each age group
tv.screen.ratio.6_11 <- tv.mean.6_11/screen.mean.6_11
tv.screen.ratio.12_18 <- tv.mean.12_18/screen.mean.12_18
tv.screen.ratio.19_25 <- tv.mean.19_25/screen.mean.19_25
tv.screen.ratio.26_36 <- tv.mean.26_36/screen.mean.26_36


tv.mean.all <-200.27



tv.6_11 <- (0.6)*(impute_screen.6_11 * tv.screen.ratio.6_11) + 
  (0.4)*(tv.mean.6_11 + rnorm(tv.n.6_11, sd=tv.sd.6_11))
tv.6_11[sample(1:length(tv.6_11), size=(total.n.6_11-tv.n.6_11), 
               replace = FALSE)] <- NA
tv.6_11 <- replace(tv.6_11, tv.6_11<0, 0)
hist(tv.6_11)
mean(tv.6_11, na.rm=TRUE)
# Sim mean hovers between: 199-211
# Original mean: 209.72


tv.12_18 <- (0.6)*(impute_screen.12_18 * tv.screen.ratio.12_18) + 
  (0.4)*(tv.mean.12_18 + rnorm(tv.n.12_18, sd=tv.sd.12_18))
tv.12_18[sample(1:length(tv.12_18), size=(total.n.12_18-tv.n.12_18), 
                replace = FALSE)] <- NA
tv.12_18 <- replace(tv.12_18, tv.12_18<0, 0)
hist(tv.12_18)
mean(tv.12_18, na.rm=TRUE)
# Sim mean hovers between: 185-196
# Original mean: 189.62


tv.19_25 <- (0.6)*(impute_screen.19_25 * tv.screen.ratio.19_25) + 
  (0.4)*(tv.mean.19_25 + rnorm(tv.n.19_25, sd=tv.sd.19_25))
tv.19_25[sample(1:length(tv.19_25), size=(total.n.19_25-tv.n.19_25), 
                replace = FALSE)] <- NA
tv.19_25 <- replace(tv.19_25, tv.19_25<0, 0)
hist(tv.19_25)
mean(tv.19_25, na.rm=TRUE)
# Sim mean hovers between: 175-195
# Original mean: 187.00

tv.26_36 <- (0.6)*(impute_screen.26_36 * tv.screen.ratio.26_36) + 
  (0.4)*(tv.mean.26_36 + rnorm(tv.n.26_36, mean=-10, sd=tv.sd.26_36))
tv.26_36[sample(1:length(tv.26_36), size=(total.n.26_36-tv.n.26_36), 
                replace = FALSE)] <- NA
tv.26_36 <- replace(tv.26_36, tv.26_36<0, 0)
mean(tv.26_36, na.rm=TRUE)
# Sim mean hovers between: 215-225
# Original mean: 219.01


tv <- c(tv.6_11, tv.12_18, tv.19_25, tv.26_36)
hist(tv)
mean(tv, na.rm=TRUE)
# Simulated mean: 203.1756
# Original mean: 200.27
shapiro.test(tv)
# data is normal
# p = 9.042e-13

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
                            mean=ntsleep.mean.6_11+50,
                            sd=ntsleep.sd.6_11) - 
                0.08*(impute_screen.6_11 * ntsleep_screen_ratio.6_11)
ntsleep.6_11[sample(1:length(ntsleep.6_11), size=(total.n.6_11-ntsleep.n.6_11), 
               replace = FALSE)] <- NA
hist(ntsleep.6_11)
mean(ntsleep.6_11, na.rm=TRUE)
# Sim mean hovers around: 632-643
# Original mean: 637.24

ntsleep.12_18 <- rtruncnorm(n=total.n.12_18, a=(ntsleep.mean.12_18-ntsleep.sd.12_18*2), 
                           b=(ntsleep.mean.12_18+ntsleep.sd.12_18*3),
                           mean=ntsleep.mean.12_18+60,
                           sd=ntsleep.sd.12_18) - 
                0.08*(impute_screen.12_18 * ntsleep_screen_ratio.12_18)
ntsleep.12_18[sample(1:length(ntsleep.12_18), size=(total.n.12_18-ntsleep.n.12_18), 
                    replace = FALSE)] <- NA
hist(ntsleep.12_18)
mean(ntsleep.12_18, na.rm=TRUE)
# Sim mean hovers around: 639-656
# Original mean: 647.24


ntsleep.19_25 <- rtruncnorm(n=total.n.19_25, a=(ntsleep.mean.19_25-ntsleep.sd.19_25*2), 
                            b=(ntsleep.mean.19_25+ntsleep.sd.19_25*3),
                            mean=ntsleep.mean.19_25+50,
                            sd=ntsleep.sd.19_25) - 
                0.08*(impute_screen.19_25 * ntsleep_screen_ratio.19_25)
ntsleep.19_25[sample(1:length(ntsleep.19_25), size=(total.n.19_25-ntsleep.n.19_25), 
                     replace = FALSE)] <- NA
hist(ntsleep.19_25)
mean(ntsleep.19_25, na.rm=TRUE)
# Sim mean hovers between: 641-658
# Original mean: 651.25

ntsleep.26_36 <- rtruncnorm(n=total.n.26_36, a=(ntsleep.mean.26_36-ntsleep.sd.26_36*2), 
                            b=(ntsleep.mean.26_36+ntsleep.sd.26_36*3),
                            mean=ntsleep.mean.26_36+56,
                            sd=ntsleep.sd.26_36) - 
  0.08*(impute_screen.26_36 * ntsleep_screen_ratio.26_36)
ntsleep.26_36[sample(1:length(ntsleep.26_36), size=(total.n.26_36-ntsleep.n.26_36), 
                     replace = FALSE)] <- NA
hist(ntsleep.26_36)
mean(ntsleep.26_36, na.rm=TRUE)
# Sim mean hovers around: 638-651
# Original mean: 643.21


ntsleep <- c(ntsleep.6_11, ntsleep.12_18, ntsleep.19_25, ntsleep.26_36)
hist(ntsleep)
mean(ntsleep, na.rm=TRUE)
# Simulated mean: 646.496
# Original mean: 645.16
shapiro.test(ntsleep)
# data is normal
# p-value = 1.271e-07

## Day-time sleep duration

# Overall values for each age group
daysleep.n.6_11 <- 116
daysleep.n.12_18 <- 176
daysleep.n.19_25 <- 132
daysleep.n.26_36 <- 126

daysleep.sd.6_11 <- 47.27
daysleep.sd.12_18 <- 38.40
daysleep.sd.19_25 <- 40.28
daysleep.sd.26_36 <- 53.20

daysleep.mean.6_11 <- 139.05
daysleep.mean.12_18 <- 122.47
daysleep.mean.19_25 <- 100.57
daysleep.mean.26_36 <- 68.25


# Ratio values for each age group
daysleep.screen_ratio.6_11 <- daysleep.mean.6_11/screen.mean.6_11
daysleep.screen_ratio.12_18 <- daysleep.mean.12_18/screen.mean.12_18
daysleep.screen_ratio.19_25 <- daysleep.mean.19_25/screen.mean.19_25
daysleep.screen_ratio.26_36 <- daysleep.mean.26_36/screen.mean.26_36


daysleep.6_11 <- rtruncnorm(n=total.n.6_11, a=(daysleep.mean.6_11-daysleep.sd.6_11*3), 
                            b=(daysleep.mean.6_11+daysleep.sd.6_11*2),
                            mean=daysleep.mean.6_11-5,
                            sd=daysleep.sd.6_11) + 
            0.05*(impute_screen.6_11 * daysleep.screen_ratio.6_11)
daysleep.6_11[sample(1:length(daysleep.6_11), size=(total.n.6_11-daysleep.n.6_11),
                    replace = FALSE)] <- NA
daysleep.6_11 <- replace(daysleep.6_11, daysleep.6_11<0, 0)
mean(daysleep.6_11, na.rm = TRUE)
# Simulated mean hovers around: 128-144
# Original mean: 139.05


daysleep.12_18 <- rtruncnorm(n=total.n.12_18, a=(daysleep.mean.12_18-daysleep.sd.12_18*3), 
                            b=(daysleep.mean.12_18+daysleep.sd.12_18*2),
                            mean=daysleep.mean.12_18-5,
                            sd=daysleep.sd.12_18) + 
  0.05*(impute_screen.12_18 * daysleep.screen_ratio.12_18)
daysleep.12_18[sample(1:length(daysleep.12_18), size=(total.n.12_18-daysleep.n.12_18),
                     replace = FALSE)] <- NA
daysleep.12_18 <- replace(daysleep.12_18, daysleep.12_18<0, 0)
mean(daysleep.12_18, na.rm = TRUE)
# Simulated mean hovers around: 120-125
# Original mean: 122.47


daysleep.19_25 <- rtruncnorm(n=total.n.19_25, a=(daysleep.mean.19_25-daysleep.sd.19_25*3), 
                             b=(daysleep.mean.19_25+daysleep.sd.19_25*2),
                             mean=daysleep.mean.19_25-5,
                             sd=daysleep.sd.19_25) + 
  0.05*(impute_screen.19_25 * daysleep.screen_ratio.19_25)
daysleep.19_25[sample(1:length(daysleep.19_25), size=(total.n.19_25-daysleep.n.19_25),
                     replace = FALSE)] <- NA
daysleep.19_25 <- replace(daysleep.19_25, daysleep.19_25<0, 0)
mean(daysleep.19_25, na.rm = TRUE)
# Simulated mean hovers around: 97-102
# Original mean: 100.57


daysleep.26_36 <- rtruncnorm(n=total.n.26_36, a=(daysleep.mean.26_36-daysleep.sd.26_36*3), 
                             b=(daysleep.mean.26_36+daysleep.sd.26_36*2),
                             mean=daysleep.mean.26_36-4,
                             sd=daysleep.sd.26_36) + 
  0.05*(impute_screen.26_36 * daysleep.screen_ratio.26_36)
daysleep.26_36[sample(1:length(daysleep.26_36), size=(total.n.26_36-daysleep.n.26_36),
                     replace = FALSE)] <- NA
daysleep.26_36 <- replace(daysleep.26_36, daysleep.26_36<0, 0)
mean(daysleep.26_36, na.rm = TRUE)
# Simulated mean hovers around: 59-78
# Original mean: 68.25


daysleep <- c(daysleep.6_11, daysleep.12_18, daysleep.19_25, daysleep.26_36)
hist(daysleep)
mean(daysleep, na.rm=TRUE)
# Simulated mean: 106.32
# Original mean: 108.29
shapiro.test(daysleep)
# Data is normal 
# p-value = 0.003424

# Total sleep

totalsleep <- ntsleep+daysleep


# Average number of night awakenings

# Age means 
age.mean.6_11 <- 8.99
age.mean.12_18 <- 14.40
age.mean.19_25 <- 21.94
age.mean.26_36 <- 30.64


# Overall values for each age group

wake.n.6_11 <- 116
wake.n.12_18 <- 177
wake.n.19_25 <- 133
wake.n.26_36 <- 130

wake.sd.6_11 <- 1.73
wake.sd.12_18 <- 1.33
wake.sd.19_25 <- 1.01
wake.sd.26_36 <- 0.81

wake.mean.6_11 <- 2
wake.mean.12_18 <- 1.29
wake.mean.19_25 <- 0.91
wake.mean.26_36 <- 0.58


# Ratio values for each age group
wake.age_ratio.6_11 <- wake.mean.6_11/age.mean.6_11
wake.age_ratio.12_18 <- wake.mean.12_18/age.mean.12_18
wake.age_ratio.19_25 <- wake.mean.19_25/age.mean.19_25
wake.age_ratio.26_36 <- wake.mean.26_36/age.mean.26_36


wake.6_11 <- floor(rtruncnorm(n=total.n.6_11, a=(wake.mean.6_11-wake.sd.6_11*2), 
                            b=(wake.mean.6_11+wake.sd.6_11*3),
                            mean=wake.mean.6_11+0.75,
                            sd=wake.sd.6_11) - 
                  0.15*(impute.age.6_11 * wake.age_ratio.6_11))
wake.6_11[sample(1:length(wake.6_11), size=(total.n.6_11-wake.n.6_11),
                     replace = FALSE)] <- NA
wake.6_11 <- replace(wake.6_11, wake.6_11<0, 0)
mean(wake.6_11, na.rm = TRUE)
# Simulated mean hovers around: 1.75-2.19
# Original mean: 2


wake.12_18 <- floor(rtruncnorm(n=total.n.12_18, a=(wake.mean.12_18-wake.sd.12_18*2), 
                              b=(wake.mean.12_18+wake.sd.12_18*3),
                              mean=wake.mean.12_18+0.6,
                              sd=wake.sd.12_18) - 
                     0.15*(impute.age.12_18 * wake.age_ratio.12_18))
wake.12_18[sample(1:length(wake.12_18), size=(total.n.12_18-wake.n.12_18),
                      replace = FALSE)] <- NA
wake.12_18 <- replace(wake.12_18, wake.12_18<0, 0)
mean(wake.12_18, na.rm = TRUE)
# Simulated mean hovers around: 1.23-1.37
# Original mean: 1.29


wake.19_25 <- floor(rtruncnorm(n=total.n.19_25, a=(wake.mean.19_25-wake.sd.19_25*2), 
                               b=(wake.mean.19_25+wake.sd.19_25*3),
                               mean=wake.mean.19_25+0.55,
                               sd=wake.sd.19_25) - 
                      0.15*(impute.age.19_25 * wake.age_ratio.19_25))
wake.19_25[sample(1:length(wake.19_25), size=(total.n.19_25-wake.n.19_25),
                      replace = FALSE)] <- NA
wake.19_25 <- replace(wake.19_25, wake.19_25<0, 0)
mean(wake.19_25, na.rm = TRUE)
# Simulated mean hovers around: 0.85-1.06
# Original mean: 0.91


wake.26_36 <- floor(rtruncnorm(n=total.n.26_36, a=(wake.mean.26_36-wake.sd.26_36*2), 
                               b=(wake.mean.26_36+wake.sd.26_36*3),
                               mean=wake.mean.26_36+0.55,
                               sd=wake.sd.26_36) - 
                      0.15*(impute.age.26_36 * wake.age_ratio.26_36))
wake.26_36[sample(1:length(wake.26_36), size=(total.n.26_36-wake.n.26_36),
                      replace = FALSE)] <- NA
wake.26_36 <- replace(wake.26_36, wake.26_36<0, 0)
mean(wake.26_36, na.rm = TRUE)
# Simulated mean hovers around: 0.53-0.67
# Original mean: 0.58


wake <- c(wake.6_11, wake.12_18, wake.19_25, wake.26_36)
mean(wake, na.rm=TRUE)
# Sim mean: 1.23
# Original mean: 1.17
shapiro.test(wake)
# Data is normal
# p-value < 2.2e-16

# Sleep onset

# Overall values for each age group
onset.n.6_11 <- 116
onset.n.12_18 <- 177
onset.n.19_25 <- 131
onset.n.26_36 <- 130

onset.sd.6_11 <- 23.69
onset.sd.12_18 <- 16.53
onset.sd.19_25 <- 16.97
onset.sd.26_36 <- 27.86

onset.mean.6_11 <- 22.80
onset.mean.12_18 <- 21.54
onset.mean.19_25 <- 22.22
onset.mean.26_36 <- 29.34


# Ratio values for each age group
onset.screen_ratio.6_11 <- onset.mean.6_11/screen.mean.6_11
onset.screen_ratio.12_18 <- onset.mean.12_18/screen.mean.12_18
onset.screen_ratio.19_25 <- onset.mean.19_25/screen.mean.19_25
onset.screen_ratio.26_36 <- onset.mean.26_36/screen.mean.26_36



onset.6_11 <- rtruncnorm(n=total.n.6_11, a=(onset.mean.6_11-onset.sd.6_11), 
                            b=(onset.mean.6_11+onset.sd.6_11),
                            mean=onset.mean.6_11-5,
                            sd=onset.sd.6_11) + 
  0.07*(impute_screen.6_11 * onset.screen_ratio.6_11)
onset.6_11[sample(1:length(onset.6_11), size=(total.n.6_11-onset.n.6_11),
                     replace = FALSE)] <- NA
onset.6_11 <- replace(onset.6_11, onset.6_11<0, 0)
mean(onset.6_11, na.rm = TRUE)
# Simulated mean hovers around: 19.5-23.5
# Original mean: 22.8


onset.12_18 <- rtruncnorm(n=total.n.12_18, a=(onset.mean.12_18-onset.sd.12_18), 
                         b=(onset.mean.12_18+onset.sd.12_18),
                         mean=onset.mean.12_18-4.5,
                         sd=onset.sd.12_18) + 
  0.07*(impute_screen.12_18 * onset.screen_ratio.12_18)
onset.12_18[sample(1:length(onset.12_18), size=(total.n.12_18-onset.n.12_18),
                      replace = FALSE)] <- NA
onset.12_18 <- replace(onset.12_18, onset.12_18<0, 0)
mean(onset.12_18, na.rm = TRUE)
# Simulated mean hovers around: 19.5-22.5
# Original mean: 21.54


onset.19_25 <- rtruncnorm(n=total.n.19_25, a=(onset.mean.19_25-onset.sd.19_25), 
                         b=(onset.mean.19_25+onset.sd.19_25),
                         mean=onset.mean.19_25-2.5,
                         sd=onset.sd.19_25) + 
  0.07*(impute_screen.19_25 * onset.screen_ratio.19_25)
onset.19_25[sample(1:length(onset.19_25), size=(total.n.19_25-onset.n.19_25),
                      replace = FALSE)] <- NA
onset.19_25 <- replace(onset.19_25, onset.19_25<0, 0)
mean(onset.19_25, na.rm = TRUE)
# Simulated mean hovers around: 20-24
# Original mean: 22.22


onset.26_36 <- rtruncnorm(n=total.n.26_36, a=(onset.mean.26_36-onset.sd.26_36), 
                         b=(onset.mean.26_36+onset.sd.26_36),
                         mean=onset.mean.26_36-4.5,
                         sd=onset.sd.26_36) + 
  0.07*(impute_screen.26_36 * onset.screen_ratio.26_36)
onset.26_36[sample(1:length(onset.26_36), size=(total.n.26_36-onset.n.26_36),
                      replace = FALSE)] <- NA
onset.26_36 <- replace(onset.26_36, onset.26_36<0, 0)
mean(onset.26_36, na.rm = TRUE)
# Simulated mean hovers around: 27-32
# Original mean: 29.34


onset <- c(onset.6_11, onset.12_18, onset.19_25, onset.26_36)
hist(onset)
mean(onset, na.rm=TRUE)
# Simulated mean: 23.16
# Original mean: 23.79
shapiro.test(onset)
# Data is normal
# p-value = 6.551e-09



df <- data.frame(age, sex, tv, screentime, ntsleep, daysleep, totalsleep, wake, onset)

df$age_category <- factor(rep(x = "26-36 months", times=length(df$age)), 
                             levels = c("6-11 months", "12-18 months", 
                                        "19-25 months", "26-36 months"))
df$age_category[df$age < 26] <- "19-25 months"
df$age_category[df$age < 19] <- "12-18 months"
df$age_category[df$age < 12] <- "6-11 months"
#### Data Exploration ####

library(ggplot2)
library(corrplot)
library(psych)

# NOTE: 
# Taken from: https://cran.r-project.org/web/packages/corrplot/vignettes/corrplot-intro.html**

cor.mtest <- function(mat, conf.level = 0.95){
  mat <- as.matrix(mat)
  n <- ncol(mat)
  p.mat <- lowCI.mat <- uppCI.mat <- matrix(NA, n, n)
  diag(p.mat) <- 0
  diag(lowCI.mat) <- diag(uppCI.mat) <- 1
  for(i in 1:(n-1)){
    for(j in (i+1):n){
      tmp <- cor.test(mat[,i], mat[,j], conf.level = conf.level)
      p.mat[i,j] <- p.mat[j,i] <- tmp$p.value
      lowCI.mat[i,j] <- lowCI.mat[j,i] <- tmp$conf.int[1]
      uppCI.mat[i,j] <- uppCI.mat[j,i] <- tmp$conf.int[2]
    }
  }
  return(list(p.mat, lowCI.mat, uppCI.mat))
}

df_nocat <- data.frame(age, tv, screentime, ntsleep, daysleep, totalsleep, wake, onset)
cor1 <- cor.mtest(df_nocat, 0.95)
corrplot(cor(df_nocat, use = "complete.obs"), p.mat = cor1[[1]], type='lower', sig.level = 0.5)
corrplot(cor(df_nocat, use = "complete.obs"), method = 'number', p.mat = cor1[[1]],
         type='lower', sig.level = 0.5, bg='gray72')


ggplot(data=df, aes(x=age_category)) + 
  geom_bar() + 
  labs(title = "Number of patients in each age category",
       x = "Age Category",
       y = "Count")
# Gives us a general idea of the age distribution


ggplot(data=df, aes(x=screentime)) + 
  geom_histogram() + 
  labs(title = "Distribution of Touchscreen Time",
       x = "Touchscreen Time (Minutes)", 
       y = "Count")

ggplot(data=df, aes(x=ntsleep)) + 
  geom_histogram() + 
  labs(title = "Distribution of Nighttime Sleep",
       x = "Nighttime Sleep (Minutes)", 
       y = "Count")

ggplot(data=df, aes(x=daysleep)) + 
  geom_histogram() + 
  labs(title = "Distribution of Daytime Sleep",
       x = "Daytime Sleep (Minutes)", 
       y = "Count")

ggplot(data=df, aes(x=onset)) + 
  geom_histogram() + 
  labs(title = "Distribution of Onset of Sleep",
       x = "Time to Fall Asleep (Minutes)", 
       y = "Count")

ggplot(data=df, aes(x=tv)) + 
  geom_histogram() + 
  labs(title = "Distribution of Background TV Time",
       x = "Background TV Time (Minutes)", 
       y = "Count")

ggplot(data=df, aes(x=screentime, y=daysleep, color=age_category)) + 
  geom_point() +
  labs(title = "Daytime Sleep vs. Touchscreen Use", 
       y = "Day time sleep (minutes)", 
       x = "Touchscreen Time")
# This shows that touchscreen time has a slightly positive correlation with 
# day time sleep in all age categories. The corrplot (which doesn't separate by age category)
# shows a nearly non-existent negative correlation between these two variables (0.03)

ggplot(data=df, aes(x=tv, y=wake, color=age_category)) + 
  geom_point() +
  labs(title = "Background TV vs. Touchscreen Use", 
       y = "Avg. Number of Awakenings", 
       x = "Background TV (minutes)")
# These are more difficult to read, but it does show that more nighttime awakenings
# (>3) are almost exclusively associated with the lowest two age categories. 

# Additionally, background TV doesn't appear to be associated with average awakenings, 
# but due to the crowding of this plot, it is not a good method for making this conclusion. 


ggplot(data=df, aes(x=screentime, color=age_category, fill=age_category)) + 
  geom_density(alpha=0.5) + 
  labs(title = "Touchscreen Use by Age Category", 
       x = "Touchscreen Use (minutes)")
# Touchscreen use certainly increases with age, with the smallest differences between the 
# 12-18 and 19-25 month categories

ggplot(data=df, aes(x=tv, color=age_category, fill=age_category)) + 
  geom_density(alpha=0.5) + 
  labs(title = "Background TV by Age Category", 
       x = "Background TV (minutes)")
# Similar background TV use by age group, but slightly higher in the oldest 

ggplot(data=df, aes(x=daysleep, color=age_category, fill=age_category)) + 
  geom_density(alpha=0.5) + 
  labs(title = "Daysleep by Age Category", 
       x = "Time spend asleep during the day (minutes)")
# Although the distribution is rather spread out, it is clear that sleeping during the day
# decreases with age

ggplot(data=df, aes(x=wake, color=age_category, fill=age_category)) + 
  geom_density(alpha=0.5) + 
  labs(title = "Daysleep by Age Category", 
       x = "Time spend asleep during the day (minutes)")
# Although the distribution is rather spread out, average number of nighttime
# awakenings does decreases with age

ggplot(data=df, aes(x=wake)) + 
  geom_histogram() + 
  facet_wrap(~age_category)
# Confirms that the average number of awakenings as a group decreases with age

ggplot(data=df, aes(x=tv, color=sex, fill=sex)) + 
  geom_density(alpha=0.5) + 
  labs(title = "Background TV by Sex", 
       x = "Background TV (minutes)")
# Female infants have slightly less background TV than males

ggplot(data=df, aes(x=screentime, color=sex, fill=sex)) + 
  geom_density(alpha=0.5) + 
  labs(title = "Touchscreen Time by Sex", 
       x = "Touchscreen Time (minutes)")
# Female infants have slightly less background TV than males


##### Modeling #####

## Basic Regression



## Multivariate Multiple Regression




## Path Analysis





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


