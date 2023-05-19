#### Idea: Simulate data accoridng to random-ic logistic model
####  - Include variability in simulation parameters but assume independence
###   - Vary the start of the exposure (or randomization) per school
###   - Seasonal probability of infected dependent on month
###   - Model using glmer logistic (as in ?glmer example).
###
###
###   - 20230519JC: time between the start of the trial & installation date -> considered as a non-exposed period?


#########################################
###         Create a dataset          ###
#########################################
### Study period
fu_start = as.Date('2023/08/01')
fu_end = as.Date('2024/07/31')					  
period <-  c('aug', 'sep', 'oct', 'nov', 'dec', 'jan', 'feb', 'mar', 'apr', 'may', 'jun', 'jul')

### installation period
installation_start = as.Date('2023/08/01')# air vent installation period starts
installation_end = as.Date('2023/12/31')  # air vent installation period ends


### Defining start & end date of each month
### This is to later easily calculate the number of weekdays of each month
date_end_month <- lubridate::floor_date(
  seq(fu_start %m+% months(1), fu_end %m+% months(1), by = "months"), "month") -1

date_start_month <- lubridate::floor_date(
  seq(fu_start, fu_end, by = "months"), "month") 


### sample size
nschool <- 20
nclass_perschool <- runif(nschool, min = 5, max = 10) %>% round(0) #average?
nclass <- sum(nclass_perschool)
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
dat <- data.table(
  school.id = rep(1:nschool, times = nclass_perschool),
  class.id = expand_class_id (nclass_perschool),
  nstudent_perclass
) %>% 
  left_join(startexp_dat, by = "school.id") %>% 
  slice(rep(1:n(), each = length(months))) %>%
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
### Yij = b0ij + b1ij*treatment + b2ij*period
### b0 = r0 + u0j

var_school <- rep(rnorm(nschool, 0, 0.025), nclass_perschool) #school level: u0j
b0 <- 0.5 + var_school
b1 <- 2 #treatment effect (OR)
  
#periodic probability of getting infected
b2_dat <- data.frame(
  period = months, 
  pi = c(
    0.025, #aug
    0.05,  #sep
    0.10,  #oct
    0.15,  #nov
    0.20,  #dec
    0.15,  #jan
    0.10,  #feb
    0.05   #mar
  )
) %>% 
  mutate(logodds = log(pi/(1-pi)))



linpred <- b0  + b1*arm + var_class + b2_dat$logodds[b2_dat$period == dat_in_month$period]
prob <- 1 / (1 + exp(-linpred))



var_school_dat <- data.frame(
  school.id,
  var_school = 
)


sim <- function(
  nschool,
  nclass,
  lambda, #nstudent per class
  riskratio
  
)
  

#add random variation 
d2$logodd <- log(d2$pi/(1 - d2$pi)) * d2$multfact1 * d2$multfact2 
d2$pi_fin <- exp(d2$logodd)/(1+exp(d2$logodd))
#hist(exp(d2$logodd)/(1+exp(d2$logodd)))


#generate the number of sick days per class
d2$num.sickdays <- sapply(1:nrow(d2), FUN = function(i) rbinom(1, d2$num.days[i], d2$pi_fin[i]))
#hist(d2$num.sickdays)


modtmp <- glmer(num.sickdays ~ arm + (1|class.id) + (1|school.id) +  factor(period) + offset(log(num.days)),
      data = d2,
      family = poisson)
summary(modtmp)


coefficients(summary(modtmp))


