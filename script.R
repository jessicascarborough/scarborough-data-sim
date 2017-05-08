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


