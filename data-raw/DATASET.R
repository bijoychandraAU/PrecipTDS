library(readr)
observed=read_csv("C:/Users/tzs0075/Box/PhD-Bijoychandra/Data3/obs.csv",
              col_names = TRUE,col_types = cols())

model=read_csv("C:/Users/tzs0075/Box/PhD-Bijoychandra/Data3/model_hour.csv",
              col_names = TRUE,col_types = cols())

usethis::use_data(observed, overwrite = TRUE)
usethis::use_data(model, overwrite = TRUE)
