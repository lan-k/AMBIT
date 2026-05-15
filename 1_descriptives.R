##descriptives
library(arsenal)
library(labelled)

load(file = "../ambit.rds")


cont_vars <- c("age",)


cat_vars <- c("sex","cald" ,"indigenous",
              "prevbirths","prevpreg","prev_caesar",
              "aboriginalmum", 
              "country","medcomp2",
              "medcomp3", "medcomp4","medcomp5","medcomp6",
              "medcomp7","medcomp8","obscomp7", "Sex")

vars <- c(cont_vars, cat_vars)
