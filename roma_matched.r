# script for analysis of the matched ROMA group.
# get all the libraries we need.csv


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

df <- read_csv("D:/lita_bita3/matched_data_roma.csv")

# need to get the no_touch_aorta correct.

nt <- read_excel("D:/lita_bita3/no_touch.xlsx", sheet = 1)

glimpse(nt)


df$nt <- with(df, ifelse(pat_id %in% nt$Pat_ID, 1, 0))

df %>% count(nt)


# table 1.

vars <- c(
    "age_at_surgery", "gender", "height_cm", "weight_kg",
    "bmi", "diabetes", "art_hypertension",
    "smoker", "hyperlipemia", "copd", "pad", "creat",
    "pre_lvef", "pre_lvef_function", "elective", "cad_disease",
    "left_main_disease", "prior_pci", "pre_stroke", "bima"
)

factors <- c(
    "gender", "diabetes", "art_hypertension",
    "smoker", "hyperlipemia", "copd", "pad",
    "pre_lvef_function", "elective", "cad_disease",
    "left_main_disease", "prior_pci", "pre_stroke", "bima"
)

t1 <- tableone::CreateTableOne(
    vars = vars,
    factorVars = factors,
    data = df,
    strata = c("bima")
)

tab1_roma_m <- print(t1,
    nonnormal = c("age", "creat", "pre_lvef", "bmi")
)

tab1_roma_m

write.csv(
    tab1_roma_m,
    "D:/lita_bita3/tables/table1_roma_matched.csv"
)

# table 2.


vars <- c(
    "opcab", "conversion_to_onpump", "onpump_beating_heart",
    "nt", "length_of_surgery", "bypass_time", "cross_clamp_time", "distals_n", "distals_svg_n", "svg_grafts", "seq_n", "proximals_n", "y_graft_n", "bima"
)

factors <- c(
    "opcab", "conversion_to_onpump", "onpump_beating_heart",
    "nt",
    "bima"
)


t2 <- tableone::CreateTableOne(
    vars = vars,
    factorVars = factors,
    data = df,
    strata = c("bima")
)


tab2_roma_m <-
    print(t2, nonnormal = c("length_of_surgery", "bypass_time", "cross_clamp_time"))

write.csv(
    tab2_roma_m,
    "D:/lita_bita3/tables/table2_roma_matched.csv"
)

# early postoperative results.


vars <- c(
    "post_low_cardiac_output",
    "post_iabp", "post_ecmo", "post_mi", "post_reanimation", "post_cardiac_arrhythmia",
    "post_redo_heart", "post_rethorax_bleeding", "post_stroke", "post_dswi",
    "post_sepsis", "died_inhouse", "postop_los", "post_dialysis"
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


tab3_roma_match <- print(t3, nonnormal = c("postop_los"))

write.csv(
    tab3_roma_match,
    "d:/lita_bita3/tables/tab3_roma_matched.csv"
)

# KM curve for this data.


s_rm <- survfit(Surv(surv_years, died) ~ bima, data = df)

summary(s_rm, times = c(0, 5, 10, 15))

survdiff(Surv(surv_years, died) ~ bima, data = df)

roma_m_s <- ggsurvplot(s_rm,
                surv.scale = "percent",
                risk.table = T,
                xlim = c(0, 15),
                break.x.by = 5,
                conf.int = T,
                legend.labs = c("SITA", "BITA"),
                palette = "lancet",
                censor.size = 0)


ggsave(
    plot = print(roma_m_s),
    filename = "D:/lita_bita3/figures/surv_roma_matched.tiff",
    height = 6,
    width = 7,
    units = "in",
    dpi = 1200,
    device = "tiff"
)

# looking at the overall CPH model.

cph <- coxph(Surv(surv_years, died) ~ bima, 
             data = df)

summary(cph)

# Call:
#     coxph(formula = Surv(surv_years, died) ~ bima + age_at_surgery, 
#           data = df)
# 
# n= 518, number of events= 107 
# 
# coef exp(coef)  se(coef)      z Pr(>|z|)  
# bima           -0.493417  0.610537  0.203092 -2.430   0.0151 *
#     age_at_surgery  0.008961  1.009001  0.015597  0.575   0.5656  
# ---
#     Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# exp(coef) exp(-coef) lower .95 upper .95
# bima              0.6105     1.6379    0.4101     0.909
# age_at_surgery    1.0090     0.9911    0.9786     1.040
# 
# Concordance= 0.574  (se = 0.029 )
# Likelihood ratio test= 6.61  on 2 df,   p=0.04
# Wald test            = 6.37  on 2 df,   p=0.04
# Score (logrank) test = 6.5  on 2 df,   p=0.04


# use RMST to obtain the difference in survival between groups.

rmst_res <- surv2sampleComp::surv2sample(time = df$surv_years,
                             status = df$died,
                             arm = df$bima,
                             tau = 15,
                             timepoints = c(5,10,15))

rmst_res$group0

# results for SITA:

# Est.  Lower 95%  Upper 95%         SE
# RMST                      12.3185084 11.7655185 12.8718357 0.28319670
# Loss time                  2.6814916  2.1281643  3.2344815 0.28319670
# Prob at 5                  0.8812558  0.8416535  0.9168186 0.01953932
# Prob at 10                 0.7838603  0.7281208  0.8365872 0.02828289
# Prob at 15                 0.6000727  0.5111078  0.6844163 0.04338231
# Quantile at 10 %           4.3998467  3.5759364  6.2233052 0.78233013
# Quantile at 15 %           7.5922681  4.7010185  8.9858723 1.28508847
# Quantile at 20 %           8.9858723  7.6798817 11.2172818 0.94984419
# Ave of t-year event rates  0.7550629  0.7072142  0.8022654 0.02484550
# Ave percentiles            6.9926624  5.3699850  8.7837906 0.89266457

rmst_res$group1

# RMST for the BITA group:

#                                 Est.  Lower 95%  Upper 95%         SE
# RMST                      13.2740437 12.7213533 13.7441484 0.26265117
# Loss time                  1.7259563  1.2558516  2.2786467 0.26265117
# Prob at 5                  0.9377413  0.9027367  0.9632504 0.01572081
# Prob at 10                 0.8351464  0.7776583  0.8848827 0.02706405
# Prob at 15                 0.7559597  0.6341785  0.8402100 0.05205989
# Quantile at 10 %           6.6832767  5.3197897  9.0187274 1.22094749
# Quantile at 15 %           9.6128573  7.1980068 14.2919724        NaN
# Quantile at 20 %          11.1871646  9.0187274        Inf        NaN
# Ave of t-year event rates  0.8429491  0.7873286  0.8895253 0.02579136
# Ave percentiles            9.1610996  7.3750593        Inf        NaN

rmst_res$contrast.diff10
# 
# Est.    Lower 95%  Upper 95%      p-val
# RMST Group1-Group0                       0.95553531  0.186783275  1.7242873 0.01484349
# Loss time Group1-Group0                 -0.95553531 -1.724287349 -0.1867833 0.01484349
# Prob at 5 Group1-Group0                  0.05648548  0.006598483  0.1063725 0.02647273
# Prob at 10 Group1-Group0                 0.05128613 -0.025660108  0.1282324 0.19143246
# Prob at 15 Group1-Group0                 0.15588703  0.023993261  0.2877808 0.02053055
# Quantile at 10 % Group1-Group0           2.28343007 -0.654983569  5.2218437 0.12773855
# Quantile at 15 % Group1-Group0           2.02058920          NaN        NaN        NaN
# Quantile at 20 % Group1-Group0           2.20129230          NaN        NaN        NaN
# Ave of t-year event rates Group1-Group0  0.08788621  0.017772630  0.1579998 0.01401876
# Ave percentiles Group1-Group0            2.16843719          NaN        NaN     


# rmst at 5, 10 and 15 using survRM2.


t5 <- survRM2::rmst2(time = df$surv_years,
               status = df$died,
               arm = df$bima,
               tau = 5)


t5

# The truncation time: tau = 5  was specified. 
# 
# Restricted Mean Survival Time (RMST) by arm 
# Est.    se lower .95 upper .95
# RMST (arm=1) 4.838 0.047     4.745     4.930
# RMST (arm=0) 4.715 0.060     4.597     4.833
# 
# 
# Restricted Mean Time Lost (RMTL) by arm 
# Est.    se lower .95 upper .95
# RMTL (arm=1) 0.162 0.047     0.070     0.255
# RMTL (arm=0) 0.285 0.060     0.167     0.403
# 
# 
# Between-group contrast 
# Est. lower .95 upper .95     p
# RMST (arm=1)-(arm=0) 0.123    -0.027     0.273 0.108
# RMST (arm=1)/(arm=0) 1.026     0.994     1.059 0.109
# RMTL (arm=1)/(arm=0) 0.569     0.281     1.151 0.117

t10 <- survRM2::rmst2(time = df$surv_years,
                      status = df$died,
                      arm = df$bima,
                      tau = 10)

t10

# The truncation time: tau = 10  was specified. 
# 
# Restricted Mean Survival Time (RMST) by arm 
# Est.    se lower .95 upper .95
# RMST (arm=1) 9.290 0.131     9.034     9.547
# RMST (arm=0) 8.918 0.158     8.608     9.227
# 
# 
# Restricted Mean Time Lost (RMTL) by arm 
# Est.    se lower .95 upper .95
# RMTL (arm=1) 0.710 0.131     0.453     0.966
# RMTL (arm=0) 1.082 0.158     0.773     1.392
# 
# 
# Between-group contrast 
# Est. lower .95 upper .95     p
# RMST (arm=1)-(arm=0) 0.373    -0.029     0.775 0.069
# RMST (arm=1)/(arm=0) 1.042     0.997     1.089 0.070
# RMTL (arm=1)/(arm=0) 0.656     0.414     1.039 0.072

# t15

t15 <- survRM2::rmst2(time = df$surv_years,
                      status = df$died,
                      arm = df$bima,
                      tau = 15)

t15

# The truncation time: tau = 15  was specified. 
# 
# Restricted Mean Survival Time (RMST) by arm 
# Est.    se lower .95 upper .95
# RMST (arm=1) 13.274 0.258    12.769    13.779
# RMST (arm=0) 12.319 0.288    11.754    12.883
# 
# 
# Restricted Mean Time Lost (RMTL) by arm 
# Est.    se lower .95 upper .95
# RMTL (arm=1) 1.726 0.258     1.221     2.231
# RMTL (arm=0) 2.681 0.288     2.117     3.246
# 
# 
# Between-group contrast 
# Est. lower .95 upper .95     p
# RMST (arm=1)-(arm=0) 0.956     0.198     1.713 0.013
# RMST (arm=1)/(arm=0) 1.078     1.015     1.144 0.014
# RMTL (arm=1)/(arm=0) 0.644     0.449     0.923 0.017