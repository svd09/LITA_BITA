# look at ITA to RCA vs SVG to RCA with veins to LCX 

library(easypackages)
libraries(c("survival","rms","Hmisc","survminer","tidyverse",
	"tidylog", "Publish"))

# get the whole dataset here.


df <- read_csv("D:/lita_bita3/complete_dataset.csv")

# some small changes before tables...

df$diabetes[df$diabetes == 8]<- 0

df$left_main_disease[df$left_main_disease == 2]<- 0

df2 <- df %>% filter(distals_n != 1)

dim(df)

dim(df2)

# save this dataset as the actual final complete dataset.

write_csv(df2, "D:/lita_bita3/complete_dataset_final.csv")

glimpse(df2)



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

str(res)

rest <- tibble(res)

str(rest)

write_csv(rest, "D:/lita_bita3/rest.csv")

tb <- rest %>% filter(subgroups == 'oldage')

tb2 <- (tb[, c(1,2,9:13)])

tb2

write_csv(tb2, "D:/lita_bita3/age_interact.csv")

# now to compare groups:-

glimpse(df2)

## lima + svg to lcx = svg to rca vs lima + rima to rca + svg to lcx

lcx_vein = df2 %>% filter(lcx_graft == 1)

lcx_vein %>% group_by(bima) %>% count(rca_graft)

# some patients may not have an RCA graft at all.

lcx_vein2 = lcx_vein %>% filter(rca_graft != 0)

lcx_vein2 %>% group_by(bima) %>% count(rca_graft)

# now lcx_vein2 has vein to lcx and either svg to rima to rca.

bima_rca = coxph(Surv(surv_years, died) ~ bima + age10 + art_hypertension + 
	smoker + copd + diabetes + pad + pre_dialysis + gender +  
	left_main_disease + prior_pci + pre_stroke, data = lcx_vein2)


summary(bima_rca)

# lima + svg to rca + bima to lcx VS lima + svg to rca + svg to lcx

rca_vein = df2 %>% filter(rca_graft == 1)

rca_vein2 = rca_vein %>% filter(lcx_graft != 0)

rca_vein2 %>% group_by(bima) %>% count(lcx_graft)

rca_vein2$keep = with(rca_vein2, ifelse(bima == "sita" & lcx_graft == 1, 1,
	ifelse(bima == "bita" & lcx_graft %in% c(3,4), 1, 0)))

rca_vein2 %>% count(keep)

rca_vein3 =  rca_vein2 %>% filter(keep == 1)

rca_vein3 %>% count(bima)


bima_lcx = coxph(Surv(surv_years, died) ~ bima + age10 + art_hypertension + 
	smoker + copd + diabetes + pad + pre_dialysis + gender +  
	left_main_disease + prior_pci + pre_stroke, data = rca_vein3)


summary(bima_lcx)

