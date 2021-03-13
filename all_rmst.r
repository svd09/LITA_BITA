# obtaining RMST for unmatched and then matched datasets.

library(easypackages)
libraries(c("survival","survRM2","tidyverse",
	"tidylog"))


df <- read_csv("D:/lita_bita3/complete_dataset.csv")

# some small changes before tables...

df$diabetes[df$diabetes == 8]<- 0

df$left_main_disease[df$left_main_disease == 2]<- 0

df <- df %>% filter(distals_n != 1)

# km 


df$surv_years <- (df$survival_days + 1)/365.24

s <- survfit(Surv(surv_years, died) ~ bima, data = df)

summary(s, times = c(0,5,10,15))

survdiff(Surv(surv_years, died) ~ bima, data = df)

# rmst 

survRM2::rmst2(time = df$surv_years,
status = df$died,
arm = df$bima,
tau = 5)

survRM2::rmst2(time = df$surv_years,
status = df$died,
arm = df$bima,
tau = 10)


survRM2::rmst2(time = df$surv_years,
status = df$died,
arm = df$bima,
tau = 15)


# now for the matched dataset.

dfm = read_csv("D:/lita_bita3/matched_data.csv")


dfm$surv_years <- (dfm$survival_days + 1)/365.24


# rmst 

survRM2::rmst2(time = dfm$surv_years,
status = dfm$died,
arm = dfm$bima,
tau = 5)

survRM2::rmst2(time = dfm$surv_years,
status = dfm$died,
arm = dfm$bima,
tau = 10)


survRM2::rmst2(time = dfm$surv_years,
status = dfm$died,
arm = dfm$bima,
tau = 15)

# now to do the same for ROMA unmatched and then matched data.

rm = read_csv("D:/lita_bita3/df_roma_dataset.csv")

glimpse(rm)

rm$surv_years = ( 1 + rm$survival_days)/365.24

survRM2::rmst2(time = rm$surv_years,
status = rm$died,
arm = rm$bima,
tau = 5)

survRM2::rmst2(time = rm$surv_years,
status = rm$died,
arm = rm$bima,
tau = 10)


survRM2::rmst2(time = rm$surv_years,
status = rm$died,
arm = rm$bima,
tau = 15)


ROMA matched :-

rm_m <- read_csv("D:/lita_bita3/matched_data_roma.csv")

glimpse(rm_m)

survRM2::rmst2(time = rm_m$surv_years,
status = rm_m$died,
arm = rm_m$bima,
tau = 5)

survRM2::rmst2(time = rm_m$surv_years,
status = rm_m$died,
arm = rm_m$bima,
tau = 10)


survRM2::rmst2(time = rm_m$surv_years,
status = rm_m$died,
arm = rm_m$bima,
tau = 15)
