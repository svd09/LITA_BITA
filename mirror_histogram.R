# obtain ps and then prepare mirror histogram for both models.

library(easypackages)
libraries(c("tidyverse","survival","rms","Hmisc",
            "cobalt"))



df <- read_csv("D:/lita_bita3/complete_dataset.csv")

# some small changes before tables...

df$diabetes[df$diabetes == 8]<- 0

df$left_main_disease[df$left_main_disease == 2]<- 0

df <- df %>% filter(distals_n != 1)


# now to run the glm model and get fitted values.


formula <- bima ~ age_at_surgery + female + diabetes + obese + art_hypertension + 
  copd + pad + pre_stroke + pre_lvef + creat + left_main_disease + cad_disease + prior_pci + hyperlipemia + pre_dialysis


m <- glm(formula, data = df, family = "binomial")


# now to obtain the ps scores.

df$ps = fitted.values(m)

describe(df$ps)


# ps for lita 

df_l = df %>% filter(bima == 0)

l_ps = df_l$ps

# ps for bita 

df_b = df %>% filter(bima == 1)

b_ps = df_b$ps




par(mfrow = c(2,1))

par(mar = c(0,5,3,3))
hist(b_ps, xlab = " ", ylab = "BITA", col = "tomato3",
     ylim = c(0,20), xaxt = "n", breaks =   100, las = 1,
     main = " ")

par(mar = c(5,5,0,3))
hist(l_ps, xlab = "Distribution of PS scores", 
     ylab = "SITA", col = "blue",
     ylim = c(200,0), breaks =   100, las = 1, main  = " ")



# save the plot.


tiff("D:/lita_bita3/mir_whole.tiff",
     width = 7, height = 5, units = "in", 
     res = 900)

par(mfrow = c(2,1))

par(mar = c(0,5,3,3))
hist(b_ps, xlab = " ", ylab = "BITA", col = "tomato3",
     ylim = c(0,20), xaxt = "n", breaks =   100, las = 1,
     main = " ")

par(mar = c(5,5,0,3))
hist(l_ps, xlab = "Distribution of PS scores", 
     ylab = "SITA", col = "blue",
     ylim = c(200,0), breaks =   100, las = 1, main  = " ")

dev.off()


# now plot for ROMA group.
# get the ROMA like patients here

df <- read_csv("D:/lita_bita3/df_roma_dataset.csv")


formula <- bima ~ age_at_surgery + female + diabetes + obese + art_hypertension +
  copd + pad + pre_stroke + pre_lvef + creat + left_main_disease + cad_disease + prior_pci + hyperlipemia + pre_dialysis

m2 <- glm(formula, data = df, family = "binomial")


df$ps = fitted.values(m2)



# ps for lita 

df_l = df %>% filter(bima == 0)

l_ps = df_l$ps

# ps for bita 

df_b = df %>% filter(bima == 1)

b_ps = df_b$ps




tiff("D:/lita_bita3/mir_roma.tiff",
     width = 7, height = 5, units = "in", 
     res = 900)

par(mfrow = c(2,1))

par(mar = c(0,5,3,3))
hist(b_ps, xlab = " ", ylab = "BITA", col = "tomato3",
     ylim = c(0,10), xaxt = "n", breaks =   100, las = 1,
     main = " ")

par(mar = c(5,5,0,3))
hist(l_ps, xlab = "Distribution of PS scores", 
     ylab = "SITA", col = "blue",
     ylim = c(100,0), breaks =   100, las = 1, main  = " ")

dev.off()

