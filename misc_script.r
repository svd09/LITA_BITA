# some miscellaneous code for the paper.

# get the matched data.

library(easypackages)
libraries(c("tidyverse","rms","tableone",
	'Hmisc'))

 mdf = read_csv("D:/lita_bita3/matched_data.csv")

glimpse(mdf)

mdf$y_done = with(mdf, ifelse(y_graft_n > 0, 1, 0))


t  = tableone::CreateCatTable(vars = "y_done",
	data = mdf,
	strata = "bima")

t

df <- read_csv("D:/lita_bita3/complete_dataset.csv")

df$y_done = with(df, ifelse(y_graft_n > 0, 1, 0))

tableone::CreateCatTable(vars = "y_done",
                         data = df,
                         strata = "bima")


mdf$seq_done = with(mdf, ifelse(seq_n > 0, 1, 0))

df$seq_done = with(df, ifelse(seq_n > 0, 1, 0))


tableone::CreateCatTable(vars = "seq_done",
                         data = df,
                         strata = "bima")


tableone::CreateCatTable(vars = "seq_done",
                         data = mdf,
                         strata = "bima")

# ROMA like group 

roma = read_csv("D:/lita_bita3/df_roma_dataset.csv")

glimpse(roma)


roma$y_done = with(roma, 
                   ifelse(y_graft_n > 0, 1, 0))



roma$seq_done = with(roma, 
                   ifelse(seq_n > 0, 1, 0))


tableone::CreateCatTable(vars = c("y_done","seq_done","pre_dialysis"),
                         data = roma,
                         strata = "bima")


tableone::CreateCatTable(vars = "post_dialysis",
                         data = roma,
                         strata = "bima")

# get ROMA matched group here.


r_match = read_csv("D:/lita_bita3/matched_data_roma.csv")


r_match$y_done = with(r_match, 
                   ifelse(y_graft_n > 0, 1, 0))



r_match$seq_done = with(r_match, 
                     ifelse(seq_n > 0, 1, 0))


tableone::CreateCatTable(vars = c('seq_done','y_done'),
                         data = r_match,
                         strata = 'bima')



