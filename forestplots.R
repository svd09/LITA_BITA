library(tidyverse)
library(rlang)
library(ggforestplot)

labels = c('5',"10","15")
estimate = c(0.1,0.45,1.08)
lower <- c(0,0.2,0.63)
upper <- c(0.2,0.7,1.53)
pval <- c('0.03','<0.01','<0.01') 
group <- c("W","W","W")
pval <- c("0.02", "0.01", "0.01")
  
dfw <- tibble(labels, estimate, upper, lower, pval)

a <- forestplot2(df = dfw,
            pvalue = pval,
            upper = upper,
            name = labels,
            estimate = estimate)
  

a2 <- a + xlab("RMST Difference")

a2


labels <- c("5","10","15")
estimate <- c(0.12,0.37,0.95)
lower <- c(-0.02,-0.02,0.19)
upper <- c(0.27, 0.77, 1.71)
group <- c("R","R","R")
pval <- c("0.01", "0.01", "0.01")

dfr <- tibble(labels, estimate, lower, upper, group, pval)

b <- forestplot2(df = dfr,
              pvalue = pval,
              upper = upper,
              name = labels,
              estimate = estimate)

b2 <- b + xlab("RMST Difference")

b2

ggsave(plot = a2, 
       filename = "D:/lita_bita3/rmstw.tiff",
       height = 7,
       width = 5,
       units = "in",
       dpi = 900)



ggsave(plot = b2, 
       filename = "D:/lita_bita3/rmstroma.tiff",
       height = 7,
       width = 5,
       units = "in",
       dpi = 900)










  
  
  0.10 (0 , 0.2)	0.03
0.45 (0.2 , 0.7)	< 0.01
1.08 (0.63 , 1.53)	< 0.01
