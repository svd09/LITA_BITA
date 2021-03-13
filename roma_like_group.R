#################################################################
##                     ROMA-like study pop                     ##
#################################################################
#
# This is going to be a subanalysis of our whole cohort. we will
# get the roma-like population and then do a multivariable model
# to look at all-cause mortality in this group of patients.
# plan for using rmst to obtain difference in survival between lita and
# bita groups for patients < 70.

# for the roma like population.
# remove > 70 years
# remove lvef < 35%
# remove esrd
# keep only primary CABG - already done.
# keep only patients with reasonable life expectancy - already done.
# keep those patients that had RITA --- LCx only as the second graft.
# if they had RITA --- Lcx, then the conduit used for the RCA does not matter.

# get the whole dataset here.

library(pacman)
p_load(
  tidyverse, survival, rms, Hmisc, survminer,
  survRM2, relsurv
)


library(easypackages)
libraries(c(
  "tidyverse", "haven", "readxl",
  "survival", "lubridate", "WeightIt",
  "cobalt", "rms", "Hmisc", "survminer",
  "cmprsk", "splines"
))

library(cobalt)
library(Matching)
library(MatchIt)
library(WeightIt)
library(twang)
library(MatchThem)
library(naniar)
library("MatchIt")
library("lmtest")
library("sandwich")
library("boot")
library("survival")
library(gmodels)

df <-
  read_csv("D:/lita_bita3/comp_dataset_multi.csv")


describe(df$age_at_surgery)

df$age70b <- with(df, ifelse(age_at_surgery <= 71, 1, 0))

# now to limit the data to <= 70 years.

df %>% count(age70b)

# age70b     n
# <dbl> <int>
#   1      0  1991
# 2      1  1702

CrossTable(df$bima, df$age70b)

# Total Observations in Table:  3693

#
#                 | df$age70b
# df$bima |         0 |         1 | Row Total |
#   -------------|-----------|-----------|-----------|
#   0 |      1851 |      1012 |      2863 |
#   |    61.251 |    71.651 |           |
#   |     0.647 |     0.353 |     0.775 |
#   |     0.930 |     0.595 |           |
#   |     0.501 |     0.274 |           |
#   -------------|-----------|-----------|-----------|
#   1 |       140 |       690 |       830 |
#   |   211.278 |   247.152 |           |
#   |     0.169 |     0.831 |     0.225 |
#   |     0.070 |     0.405 |           |
#   |     0.038 |     0.187 |           |
#   -------------|-----------|-----------|-----------|
#   Column Total |      1991 |      1702 |      3693 |
#   |     0.539 |     0.461 |           |
#   -------------|-----------|-----------|-----------|


df2 <- df %>% filter(age70b == 1)

# now to limit to only EF >= 35%.

describe(df2$pre_lvef)

df2$ef_good <- with(df2, ifelse(pre_lvef >= 35, 1, 0))


CrossTable(df2$bima, df2$ef_good)

# Total Observations in Table:  1702


# |                 df2$ef_good
# df2$bima |         0 |         1 | Row Total |
#   -------------|-----------|-----------|-----------|
#   0 |        16 |       996 |      1012 |
#   |     0.087 |     0.001 |           |
#   |     0.016 |     0.984 |     0.595 |
#   |     0.640 |     0.594 |           |
#   |     0.009 |     0.585 |           |
#   -------------|-----------|-----------|-----------|
#   1 |         9 |       681 |       690 |
#   |     0.127 |     0.002 |           |
#   |     0.013 |     0.987 |     0.405 |
#   |     0.360 |     0.406 |           |
#   |     0.005 |     0.400 |           |
#   -------------|-----------|-----------|-----------|
#   Column Total |        25 |      1677 |      1702 |
#   |     0.015 |     0.985 |           |
#   -------------|-----------|-----------|-----------|




# remove these 25 patients now.

df3 <- df2 %>% filter(ef_good == 1)

# now to remove those patients that did not have RITA --- Lcx.


df3 %>%
  filter(bima == 1) %>%
  count(lcx_graft)


df3 %>% count(pre_dialysis)

# dialysis     n
# <dbl> <int>
# 1        0  1662
# 2        1    17


# remove 17 patients that are on dialysis.

df4 <- df3 %>% filter(pre_dialysis == 0)

# now according to the ROMA criteria, these are
# the patients that can be a part of the analysis ...

# now 1662 patients form part of the ROMA like cohort.
# save this data as roma_data.

# now the data contains information regarding
# conduit used for the Lcx. Acc. to ROMA criteria,
# we need to only include patients that had either
# LITA/RITA to the Lcx.
# conduit type 3/4 = arterial LITA/RITA.

df4 %>% count(lcx_graft)

# first we can filter and separate the df into two df.
# then we can only filter those patients in the bima group
# that had 3/4 == lcx_graft.

df_sita <- df4 %>% filter(bima == 0)

df_bita <- df4 %>% filter(bima == 1)

# now to see Lcx_grafts.

df_bita %>% count(lcx_graft)

# # A tibble: 4 x 2
#   lcx_graft     n
#       <dbl> <int>
# 1         0   114
# 2         1   207
# 3         3    58
# 4         4   295

df4 %>% count(bima)

df_bita2 <- df_bita %>% filter(lcx_graft %in% c(3, 4))

dim(df_bita2)

# # now to combine the lita and bita together to
# # to create the whole dataset.

df_roma_w <- rbind(df_sita, df_bita2)

df_roma_w %>% count(bima)

# after applying all the exclusion criteria,
# we have 986 & 353 LITA/BITA patients as part
# of the ROMA cohort.

write_csv(
  df_roma_w,
  "D:/lita_bita3/df_roma_dataset.csv"
)

# now to get this dataset again and then
# do the same analysis as before.

df <- read_csv("D:/lita_bita3/df_roma_dataset.csv")

# table 1.

vars <- c(
  "age_at_surgery", "gender", "height_cm", "weight_kg",
  "bmi", "diabetes", "art_hypertension",
  "smoker", "hyperlipemia", "copd", "pad", "creat", "pre_dialysis",
  "pre_lvef", "pre_lvef_function", "elective", "cad_disease",
  "left_main_disease", "prior_pci", "pre_stroke", "bima"
)

factors <- c(
  "gender", "diabetes", "art_hypertension",
  "smoker", "hyperlipemia", "copd", "pad", "pre_dialysis",
  "pre_lvef_function", "elective", "cad_disease",
  "left_main_disease", "prior_pci", "pre_stroke", "bima"
)

t1 <- tableone::CreateTableOne(
  vars = vars,
  factorVars = factors,
  data = df,
  strata = c("bima")
)

tab1_roma <- print(t1,
  nonnormal = c("age", "creat", "pre_lvef", "bmi")
)

write.csv(
  tab1_roma,
  "D:/lita_bita3/tables/table1_roma.csv"
)

# table 2.


vars <- c(
  "opcab", "conversion_to_onpump", "onpump_beating_heart",
  "no_touch_aorta", "length_of_surgery", "bypass_time", "cross_clamp_time", "distals_n", "distals_svg_n", "svg_grafts", "seq_n", "proximals_n", "y_graft_n", "bima"
)

factors <- c(
  "opcab", "conversion_to_onpump", "onpump_beating_heart",
  "no_touch_aorta",
  "bima"
)


t2 <- tableone::CreateTableOne(
  vars = vars,
  factorVars = factors,
  data = df,
  strata = c("bima")
)


tab2_roma <-
  print(t2, nonnormal = c("length_of_surgery", "bypass_time", "cross_clamp_time"))

write.csv(
  tab2_roma,
  "D:/lita_bita3/tables/table2_roma.csv"
)

# early postoperative results.


vars <- c(
  "post_low_cardiac_output",
  "post_iabp", "post_ecmo", "post_mi", "post_reanimation", "post_cardiac_arrhythmia",
  "post_redo_heart", "post_rethorax_bleeding", "post_stroke", "post_dswi",
  "post_sepsis", "died_inhouse", "postop_los", "bima"
)

factors <- c(
  "post_low_cardiac_output",
  "post_iabp", "post_ecmo", "post_mi", "post_reanimation", "post_cardiac_arrhythmia",
  "post_redo_heart", "post_rethorax_bleeding", "post_stroke", "post_dswi",
  "post_sepsis", "died_inhouse", "post_dialysis"
)

t3 <- tableone::CreateTableOne(
  vars = vars,
  factorVars = factors,
  data = df,
  strata = c("bima")
)


t3 <- print(t3, nonnormal = c("postop_los"))

write.csv(
  t3,
  "d:/lita_bita3/tables/tab3_roma.csv"
)

# unadjusted survival curve.

df$surv_years <- (1 + df$survival_days) / 365.24

s <- survfit(Surv(surv_years, died) ~ bima,
  data = df
)

summary(s, times = c(0, 5, 10, 15))

#                bima=0
#  time n.risk n.event survival std.err lower 95% CI upper 95% CI
#     0    986       0    1.000  0.0000        1.000        1.000
#     5    840     114    0.883  0.0103        0.863        0.903
#    10    410     110    0.746  0.0150        0.717        0.776
#    15    120      65    0.584  0.0220        0.542        0.628

#                 bima=1
#  time n.risk n.event survival std.err lower 95% CI upper 95% CI
#     0    353       0    1.000  0.0000        1.000        1.000
#     5    288      18    0.945  0.0127        0.920        0.970
#    10    142      22    0.854  0.0220        0.812        0.898
#    15     12       6    0.787  0.0398        0.713        0.869





a <- ggsurvplot(s,
  surv.scale = "percent",
  risk.table = T,
  xlim = c(0, 15),
  break.x.by = 5,
  conf.int = T,
  legend.labs = c("SITA", "BITA"),
  palette = "lancet",
  censor.size = 0
)


ggsave(
  plot = print(a),
  filename = "D:/lita_bita3/figures/surv_roma.tiff",
  height = 6,
  width = 7,
  units = "in",
  dpi = 1200,
  device = "tiff"
)

# now to do the matching.


formula <- bima ~ age_at_surgery + female + diabetes + obese + art_hypertension +
  copd + pad + pre_stroke + pre_lvef + creat + left_main_disease + cad_disease + prior_pci + hyperlipemia + pre_dialysis

match1 <- matchit(
  data = df, formula = formula,
  method = "nearest", caliper = 0.25,
  replace = F, ratio = 1
)

match1

summary(match1)

love.plot(match1, abs = T, thresholds = 0.1)

# get the SD for table.

bal <- cobalt::bal.tab(match1, un = T)

# making a good love plot

v_names <- var.names(bal,type = "df",file = "D:/lita_bita3/var_roma.csv")

v_names2 <- read.csv("D:/lita_bita3/var_roma2.csv")


mb_roma <- love.plot(bal,
                     var.names = v_names2,
                     color = c("gray","tomato3"),
                     abs = T,thresholds = 0.1,shapes = c(19,17),
                     grid = T)
                     
                     


mb_roma

ggsave(mb_roma, filename = "D:/lita_bita3/mb_romaplot.tiff",
       height = 7, width = 5, unit = "in",
       dpi = 900)

match_roma <- match.data(match1)

match_roma %>% count(bima)

# now to save this dataset.

write_csv(
  match_roma,
  "D:/lita_bita3/matched_data_roma.csv"
)


# no touch aorta for ROMA group, unmatched and matched.

no_t <- read_excel("D:/lita_bita3/no_touch.xlsx",
                   sheet = 1)

no_t$no_touch <- 1

names(no_t) <- tolower(names(no_t)) 

no_t

dim(df)

df2 = left_join(df, no_t, by = "pat_id")

df2 %>% count(no_touch)

df2$no_touch[is.na(df2$no_touch)]<- 0

tableone::CreateCatTable(vars = "no_touch",
  data = df2,
  strata = "bima")


# now for the matched ROMA group.

roma_m = read_csv(  "D:/lita_bita3/matched_data_roma.csv")


roma_m2 = left_join(roma_m, no_t, by = "pat_id")

 

roma_m2$no_touch[is.na(roma_m2$no_touch)]<- 0


tableone::CreateCatTable(vars = "no_touch",
  data = roma_m2,
  strata = "bima")


# model for unmatched ROMA group.

df$surv_years = ( 1 + df$survival_days)/365.24

model_whole_roma = coxph(Surv(surv_years, died) ~ bima +  art_hypertension + 
             smoker + copd + diabetes + pad +  gender +  
             left_main_disease + prior_pci + pre_stroke, data = df)

summary(model_whole_roma)
