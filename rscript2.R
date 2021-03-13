# script for the paper.
# some additions to the tables 1,2 for the paper.

library(easypackages)
libraries(c(
  "tidyverse", "haven", "readxl",
  "survival", "lubridate", "WeightIt",
  "cobalt", "rms", "Hmisc", "survminer",
  "cmprsk"
))

library(cobalt)
library(Matching)
library(MatchIt)
library(WeightIt)
library(twang)
library(MatchThem)
library(naniar)

library(surv2sampleComp)



df <- read_csv("D:/lita_bita3/complete_dataset.csv")

# some small changes before tables...

df$diabetes[df$diabetes == 8]<- 0

df$left_main_disease[df$left_main_disease == 2]<- 0

df <- df %>% filter(distals_n != 1)

# now to save this df again.

write_csv(df,
          "D:/lita_bita3/comp_dataset_multi.csv")

# do some more for the tables.

describe(df$pre_lvef)

hist(df$pre_lvef, bins = 20)

# distal locations: 

# vein distals number:

tableone::CreateContTable(vars = c("distals_svg_n","distals_n"),
                          data = df,
                          strata = c("bima"))

# convert seq and y graft to only 0/1:

df$seq_yes = with(df, ifelse(seq_n > 0, 1, 0))

df$y_yes = with(df, ifelse(y_graft_n > 0, 1, 0))

tableone::CreateCatTable(vars = c("seq_yes", "y_yes"),
                         data = df,
                         strata = c("bima"))


# now to do the same analyses for matched data.

df_wm <- 
  read_csv("D:/lita_bita3/matched_data.csv")


tableone::CreateContTable(vars = c("distals_svg_n","distals_n"),
                          data = df_wm,
                          strata = c("bima"))

# convert seq and y graft to only 0/1:

df$seq_yes = with(df, ifelse(seq_n > 0, 1, 0))

df$y_yes = with(df, ifelse(y_graft_n > 0, 1, 0))

# need for dialysis postop.

tableone::CreateCatTable(vars = 'post_dialysis',
                         data = df,
                         strata = "bima")



tableone::CreateCatTable(vars = 'post_dialysis',
                         data = df_wm,
                         strata = "bima")
# to obtain no touch aorta to the data.

no_t <- read_excel("D:/lita_bita3/no_touch.xlsx",
                   sheet = 1)

no_t$no_touch <- 1

names(no_t) <- tolower(names(no_t))

df2 <- left_join(df, no_t, by = "pat_id")

df_wm2 <- left_join(df_wm, no_t, by = "pat_id")

df2$no_touch[is.na(df2$no_touch)]<- 0

df_wm2$no_touch[is.na(df_wm2$no_touch)]<- 0

# no touch only for opcab, so first only limit to opcab.

tableone::CreateCatTable(vars = "no_touch",
                         data = df2[df2$opcab == 1, ],
                         strata = "bima")


tableone::CreateCatTable(vars = "no_touch",
                         data = df_wm2[df_wm2$opcab == 1, ],
                         strata = "bima")

