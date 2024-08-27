library(rstpm2)
library(survival)
library(lubridate)
library(haven)
library(tidyverse)
library(splines)
 # colon_data <- read_dta('./data/colon.dta')
colon_data <- read_dta('./data/stset.dta')
melanoma_data <- read_dta('./data/melanoma.dta')
# colon_data <- colon_data %>%
#   mutate(exit = ymd(exit)) %>%
#   mutate(censor_status = ifelse(year8594 == 0 & year(exit) > 1984,0,status)) %>%
#   mutate(censor_date = pmin(ymd("1984-12-31"),exit)) %>%
#   mutate(exposure = as.integer(exit - dx)/365) 
  # mutate(exit = ifelse(year8594 == 0 & year(exit) > 1984,
  #                      pmax(exit, as.Date("1984-12-31", format = "%Y-%m-%d")),
  #                      exit))
 # mutate(exposure = as.integer(exit - dx)/365) 

OneA<- stpm2(
  formula = Surv(`_t` ,`_d`) ~ 
                as.factor(sex) + 
                as.factor(stage) + 
                # model age with splines
                ns(age, df = 3),
                #as.factor(yydx),
                df = 5,
                data = colon_data %>% filter(`_st` != 0)
)

eform(OneA)

pred_hazard <- predict(OneA,
                       newdata = colon_data %>% 
                                      filter(year8594 == 1) %>% 
                                      filter(stage != 0) %>%
                                      mutate(yydx = 1984),
                       type = "meansurv",
                       grid=TRUE, 
                       full=TRUE, 
                        )
pred_hazard


ggplot(pred_hazard,
       aes(x=exposure,y=Estimate)) +
  xlab("Time since diagnosis (Days)") +
  ylab("S(t)") +
  #geom_ribbon(alpha=0.6) +
  geom_line()
