rm (list(ls(all.names = TRUE)))
#### Idea: Simulate data accoridng to random-ic logistic model
####  - Include variability in simulation parameters but assume independence
###   - Vary the start of the exposure (or randomization) per school
###   - Seasonal probability of infected dependent on month
###   - Model using glmer logistic (as in ?glmer example).
###
###
###   - 20230519JC: time between the start of the trial & installation date -> considered as a non-exposed period?

source('airvent_package_functions.R')

#########################################
###         Create a dataset          ###
#########################################
### Study period
fu_start = as.Date('2023/08/01')
fu_end = as.Date('2024/07/31')					  
period <-  c('aug', 'sep', 'oct', 'nov', 'dec', 'jan', 'feb', 'mar', 'apr', 'may', 'jun', 'jul')

### installation period
installation_start = as.Date('2023/08/01')
installation_end = as.Date('2023/12/31')  


### Defining start & end date of each month
### This is to later easily calculate the number of weekdays of each month
date_end_month <- lubridate::floor_date(
  seq(fu_start %m+% months(1), fu_end %m+% months(1), by = "months"), "month") -1

date_start_month <- lubridate::floor_date(
  seq(fu_start, fu_end, by = "months"), "month") 


### sample size
nschool <- 100
nclass_perschool <- runif(nschool, min = 5, max = 10) %>% round(0) #average?
nclass <- sum(nclass_perschool)
nstudent_perclass <- rpois(nclass, lambda = 24) #average 23-24 per class
#hist(nclass_perschool * nstudent_percalss)


### Assign exposure to the half of the school
### 'exp_start' indicates when the new ventilation system is installed.
### 'exp_start' varies within the installation period, but it is always at the 1st of each month.
startexp_dat <- data.table(school.id = 1:as.integer(nschool/2)) %>% 
  mutate(
    arm = 1,
    exp_start = sample(seq(installation_start, installation_end, by = "months"), n(), replace = TRUE)
  ) %>% 
  bind_rows(
    data.table(school.id = (as.integer(nschool/2) + 1) : nschool) %>% 
      mutate(arm = 0,
             exp_start = NA)
    ) 

### Create a complete dataset
### TODO: This step is slow. 
dat <- data.table(
  school.id = rep(1:nschool, times = nclass_perschool),
  class.id = expand_class_id (nclass_perschool),
  nstudent_perclass
) %>% 
  left_join(startexp_dat, by = "school.id") %>% 
  slice(rep(1:n(), each = length(period))) %>%
  mutate(
    period = rep(period, times = nclass),
    date_end_month = rep(date_end_month, times = nclass),
    date_start_month = rep(date_start_month, times = nclass),
    arm = case_when(date_start_month >= exp_start | is.na(exp_start) ~ 1,
                         TRUE ~ 0),
    num_weekdays_total = num_weekdays(date_start_month, date_end_month) * nstudent_perclass,
  )  %>% 
  select(-date_end_month, 
         - date_start_month)
  
     
#View(dat)
  

#########################################
###     Create a infection model      ###
#########################################
### pr(sick absence) is generate with a random intercept model
### log(pij/1-pij) = b0ij + b1ij*treatment + b2ij*period
### b0 = r0 + u0j


r0 <- -2.5
u0j <- 0.05
b1 <- log(2) #treatment effect (OR = 2)

b0_dat <- data.frame(
  school.id = 1:nschool,
  random_ic = rnorm(nschool, r0, u0j)
)

### baseline pr(infected for each school) = ~ 0.078 
### TODO: how to define?
#hist(1 / (1 + exp(-b0_dat$random_ic)))

### periodic probability of getting infected
### TODO: how to define?
b1_dat <- data.frame(
  period = period, 
  period_pi = c(
    0.025, #aug
    0.05,  #sep
    0.10,  #oct
    0.15,  #nov
    0.20,  #dec
    0.15,  #jan
    0.10,  #feb
    0.10,  #mar
    0.05,  #apr
    0.05,  #may
    0.025,  #jun
    0.025  #jul
  )
) %>% 
  mutate(period_logodds = log(period_pi/(1-period_pi))) %>% 
  select(-period_pi)

dat_fin <- dat %>% 
  left_join(b0_dat, by= "school.id") %>% 
  left_join(b1_dat, by = "period")


### A linear predictor for pr(infected)
linpred <- dat_fin$random_ic + 2*dat_fin$arm + dat_fin$period_logodds
prob <- 1 / (1 + exp(-linpred))
#hist(prob)

### Randomly select the number of sick days based on the linear predictor
dat_fin$num_sickdays <- sapply(
  1:nrow(dat_fin), FUN = function(i) rbinom(1, dat$num_weekdays_total[i], prob[i])) %>% 
  round(0)
#hist(dat_fin$num_sickdays)

#########################################
###     Data generation finished      ###
#########################################


#########################################
###            Model fit              ###
#########################################
m <- glmer(cbind(dat_fin$num_sickdays, dat_fin$num_weekdays_total - dat_fin$num_sickdays) ~ 
             arm + period + (1|school.id) + (1|class.id), data = dat_fin, family = binomial)
summary(m)
fixef(m)
exp(fixef(m))
