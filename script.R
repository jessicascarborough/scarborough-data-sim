#### Data Simulation ####

set.seed(0)

# Overall Values

total.n.6_11 <- 134
total.n.12_18 <- 215
total.n.19_25 <- 179
total.n.26_36 <- 187


# Age

age.6_11 <- rnorm(n=total.n.6_11, mean=8.99, sd=1.82)
age.12_18 <- rnorm(n=total.n.12_18, mean=14.40, sd=2.19)
age.19_25 <- rnorm(n=total.n.19_25, mean=21.94, sd=2.07)
age.26_36 <- rnorm(n=total.n.26_36, mean=30.64, sd=3.07)

age <- c(age.6_11, age.12_18, age.19_25, age.26_36)

# Touchscreen Use

screen.n.6_11 <- 123
screen.n.12_18 <- 194
screen.n.19_25 <- 145
screen.n.26_36 <- 151

# set.seed(0)
screen.6_11 <- rnorm(n=total.n.6_11, mean=(8.53-4), sd=15.54)
screen.6_11 <- replace(screen.6_11, screen.6_11<0, 0)
na_sample.6_11 <- sample(x=screen.6_11, 
                         size=(total.n.6_11-screen.n.6_11), 
                         replace=FALSE)
screen.6_11 <- replace(screen.6_11, na_sample.6_11, NA)
sd(screen.6_11, na.rm = TRUE)
mean(screen.6_11, na.rm = TRUE)
# Final mean typically hovers between 8-9

# set.seed(0)
screen.12_18 <- rnorm(n=total.n.12_18, mean=(18.80-12), sd=36.83)
screen.12_18 <- replace(screen.12_18, screen.12_18<0, 0)
na_sample.12_18 <- sample(x=screen.12_18, 
                          size=(total.n.12_18-screen.n.12_18), 
                          replace=FALSE)
screen.12_18 <- replace(screen.12_18, na_sample.12_18, NA)
sd(screen.12_18, na.rm = TRUE)
mean(screen.12_18, na.rm = TRUE)

# Final mean typically hoers between 17-20

# set.seed(0)
screen.19_25 <- rnorm(n=total.n.19_25, mean=(25.18-7), sd=37.46)
screen.19_25 <- replace(screen.19_25, screen.19_25<0, 0)
na_sample.19_25 <- sample(x=screen.19_25, 
                          size=(total.n.19_25-screen.n.19_25), 
                          replace=FALSE)
screen.19_25 <- replace(screen.19_25, na_sample.19_25, NA)
sd(screen.19_25, na.rm = TRUE)
mean(screen.19_25, na.rm = TRUE)
# Final mean typically hovers between 22-27

# set.seed(0)
screen.26_36 <- rnorm(n=total.n.26_36, mean=(44.11-4), sd=47.75)
screen.26_36 <- replace(screen.26_36, screen.26_36<0, 0)
na_sample.26_36 <- sample(x=screen.26_36, 
                          size=(total.n.26_36-screen.n.26_36), 
                          replace=FALSE)
screen.26_36 <- replace(screen.26_36, na_sample.26_36, NA)
sd(screen.26_36, na.rm = TRUE)
mean(screen.26_36, na.rm = TRUE)
# Final mean typically hovers between 40-47

screentime <- c(screen.6_11, screen.12_18, screen.19_25, screen.26_36)


# Sex

sex.6_11 <- rep(x = "Female", times = total.n.6_11)
male_sample.6_11 <- sample(sex.6_11, 
                           size = ceiling(total.n.6_11*0.5149), 
                           replace=FALSE)
sex.6_11 <- replace(sex.6_11, male_sample.6_11, "Male")


sex.12_18 <- rep(x = "Female", times = total.n.12_18)
male_sample.12_18 <- sample(sex.12_18, 
                            size = ceiling(total.n.12_18*0.5302), 
                            replace=FALSE)
sex.12_18 <- replace(sex.12_18, male_sample.12_18, "Male")


sex.19_25 <- rep(x = "Female", times = total.n.19_25)
male_sample.19_25 <- sample(sex.19_25, 
                            size = ceiling(total.n.19_25*0.5587), 
                            replace=FALSE)
sex.19_25 <- replace(sex.19_25, male_sample.19_25, "Male")


sex.26_36 <- rep(x = "Female", times = total.n.26_36)
male_sample.26_36 <- sample(sex.26_36, 
                            size = ceiling(total.n.26_36*0.5134), 
                            replace=FALSE)
sex.26_36 <- replace(sex.26_36, male_sample.26_36, "Male")

sex <- c(sex.6_11, sex.12_18, sex.19_25, sex.26_36)


# Background TV

# N values for each age group
tv.n.6_11 <- 123
tv.n.12_18 <- 194
tv.n.19_25 <- 145
tv.n.26_36 <- 151

# Ratio values for each age group
tv_screen_ratio.all <- 200.27/24.45
tv_screen_ratio.6_11 <- 209.72/8.53
tv_screen_ratio.12_18 <- 189.62/18.80
tv_screen_ratio.19_25 <- 187.00/25.18
tv_screen_ratio.26_36 <- 219.01/44.11

# TV values for each age group

tv_sd.6_11 <- 186.08
tv_sd.12_18 <- 172.70
tv_sd.19_25 <- 162.99
tv_sd.26_36 <-183.24

tv_mean.6_11 <- 209.72
tv_mean.12_18 <- 189.62
tv_mean.19_25 <- 187.00
tv_mean.26_36 <- 219.01

# Touchscreen mean for each age group
screen_mean.6_11 <- 8.53
screen_mean.12_18 <- 18.80
screen_mean.19_25 <- 25.18
screen_mean.26_36 <- 44.11

screen_sd.6_11 <- 15.54
screen_sd.12_18 <- 36.83
screen_sd.19_25 <- 37.46
screen_sd.26_36 <- 47.75

tv_mean.all <-200.27


impute_screen.6_11 <- replace(screen.6_11, is.na(screen.6_11), 
                              mean(screen.6_11, na.rm=TRUE))
tv.6_11 <- (0.8)*(impute_screen.6_11 * tv_screen_ratio.6_11) + 
  (0.2)*(tv_mean.6_11 + rnorm(tv.n.6_11, sd=tv_sd.6_11))
tv.6_11 <- matrix(tv.6_11)
tv.6_11[sample(1:length(tv.6_11), size=(total.n.6_11-tv.n.6_11), 
               replace = FALSE)] <- NA
tv.6_11 <- replace(tv.6_11, tv.6_11<0, 0)
mean(tv.6_11, na.rm=TRUE)


impute_screen.12_18 <- replace(screen.12_18, is.na(screen.12_18), 
                               mean(screen.12_18, na.rm=TRUE))
tv.12_18 <- (0.8)*(impute_screen.12_18 * tv_screen_ratio.12_18) + 
  (0.2)*(tv_mean.12_18 + rnorm(tv.n.12_18, sd=tv_sd.12_18))
tv.12_18 <- matrix(tv.12_18)
tv.12_18[sample(1:length(tv.12_18), size=(total.n.12_18-tv.n.12_18), 
                replace = FALSE)] <- NA
tv.12_18 <- replace(tv.12_18, tv.12_18<0, 0)
mean(tv.12_18, na.rm=TRUE)

impute_screen.19_25 <- replace(screen.19_25, is.na(screen.19_25), 
                               mean(screen.19_25, na.rm=TRUE))
tv.19_25 <- (0.8)*(impute_screen.19_25 * tv_screen_ratio.19_25) + 
  (0.2)*(tv_mean.19_25 + rnorm(tv.n.19_25, sd=tv_sd.19_25))
tv.19_25 <- matrix(tv.19_25)
tv.19_25[sample(1:length(tv.19_25), size=(total.n.19_25-tv.n.19_25), 
                replace = FALSE)] <- NA
tv.19_25 <- replace(tv.19_25, tv.19_25<0, 0)
mean(tv.19_25, na.rm=TRUE)



impute_screen.26_36 <- replace(screen.26_36, is.na(screen.26_36), 
                               mean(screen.26_36, na.rm=TRUE))
tv.26_36 <- (0.8)*(impute_screen.26_36 * tv_screen_ratio.26_36) + 
  (0.2)*(tv_mean.26_36 + rnorm(tv.n.26_36, sd=tv_sd.26_36))
tv.26_36 <- matrix(tv.26_36)
tv.26_36[sample(1:length(tv.26_36), size=(total.n.26_36-tv.n.26_36), 
                replace = FALSE)] <- NA
tv.26_36 <- replace(tv.26_36, tv.26_36<0, 0)
mean(tv.26_36, na.rm=TRUE)





# Alternative method, not as good as the final chosen method

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


