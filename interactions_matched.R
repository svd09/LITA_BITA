# interaction using the matched whole group.


library(easypackages)
libraries(c("survival","rms","Hmisc","survminer","tidyverse",
            "tidylog", "Publish"))

df = read_csv('D:\\lita_bita3\\matched_data.csv')

df2 = df


df2$surv_years = (1 + df2$survival_days)/365.24

df2$age10 = df2$age_at_surgery/10

df2$oldage <- with(df2, ifelse(age_at_surgery > 70, 1, 0))



df2 = data.frame(df2)

df2$bima = factor(df2$bima, levels = c(0,1), labels = c("sita","bita"))

df2$diabetes = factor(df2$diabetes, levels = c(0,1), labels = c("no_dm","dm"))

df2$gender = factor(df2$gender, levels = c(1,2), labels = c("male","female"))

df2$oldage = factor(df2$oldage, levels = c(0,1),
                    labels = c("young","old"))


m2 = coxph(Surv(surv_years, died) ~ bima + oldage + art_hypertension + 
             smoker + copd + diabetes + pad + pre_dialysis + gender +  
             left_main_disease + prior_pci + pre_stroke, data = df2)


summary(m2)




sub_cox = subgroupAnalysis(m2, df2, treatment = "bima",
                           subgroups = c("diabetes","gender","oldage"))


res <- summary(sub_cox)


res <- res %>% tibble()

res

# save this results of interaction 

write_csv(res,
         "D:/lita_bita3/matched_interactions.csv")