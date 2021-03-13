library(survRM2);library(tidyverse);library(survival)


#WHOLE GROUP MATCHED:
  

df <- read_csv("D:/lita_bita3/matched_data.csv")

t5 <- survRM2::rmst2(time = df$surv_years,
                     status = df$died,
                     arm = df$bima,
                     tau = 5)

t5


t10 <- survRM2::rmst2(time = df$surv_years,
                     status = df$died,
                     arm = df$bima,
                     tau = 10)

t10


t15 <- survRM2::rmst2(time = df$surv_years,
                      status = df$died,
                      arm = df$bima,
                      tau = 15)

t15

#ROMA group matched:

df_r <- read_csv("D:/lita_bita3/matched_data_roma.csv")

t5_r <- survRM2::rmst2(time = df_r$surv_years,
                     status = df_r$died,
                     arm = df_r$bima,
                     tau = 5)

t5_r


t10_r <- survRM2::rmst2(time = df_r$surv_years,
                      status = df_r$died,
                      arm = df_r$bima,
                      tau = 10)

t10_r


t15_r <- survRM2::rmst2(time = df_r$surv_years,
                      status = df_r$died,
                      arm = df_r$bima,
                      tau = 15)

t15_r

#kmsurv:

s <- survfit(Surv(surv_years, died) ~ bima, data = df)

summary(s, times = c(0,5,10,15))

s_r <- survfit(Surv(surv_years, died) ~ bima, data = df_r)

summary(s_r, times = c(0,5,10,15))
