library("readxl")
library("tidyverse")

df1 <- read_excel("data-raw/austria.xlsx", sheet = 1, skip = 2)
df2 <- read_excel("data-raw/austria.xlsx", sheet = 2, skip = 2)
df3 <- read_excel("data-raw/austria.xlsx", sheet = 3, skip = 2)
df4 <- read_excel("data-raw/austria.xlsx", sheet = 4, skip = 2)
df5 <- read_excel("data-raw/austria.xlsx", sheet = 5, skip = 2)

#lots of warning messages
#take care of age labels in hfd
df4 <- df4 %>%
  mutate(Age = ifelse(test = Age=="-12", yes = 12, no = Age),
         Age = ifelse(test = Age=="55+", yes = 55, no = Age),
         Age = as.numeric(Age))

df0 <- df3 %>%
  select(-Total) %>%
  rename(Nx_f = Female, Nx_m = Male) %>%
  #join on the female lifetable
  left_join(df1) %>%
  select(Year, Age, Nx_f, Nx_m, Lx) %>%
  rename(Lx_f = Lx) %>%
  #join on the male lifetable
  left_join(df2) %>%
  select(Year, Age, Nx_f, Nx_m, Lx_f, Lx) %>%
  rename(Lx_m = Lx) %>%
  #take care of age labels in the hmd
  mutate(Age = ifelse(test = is.na(Age), yes = 110, no = Age)) %>%
  #join on the hfd
  left_join(df4) %>%
  rename(Fx = ASFR) %>%
  mutate(Fx = ifelse(test = is.na(Fx), yes = 0, no = Fx)) %>%
  #find sx and lx
  left_join(df5) %>%
  select(-Total) %>%
  rename(Dx_m = Male,
         Dx_f = Female) %>%
  mutate(Dx_m = ifelse(is.na(Dx_m), 0, Dx_m),
         Dx_f = ifelse(is.na(Dx_f), 0, Dx_f))


austria <- df0
rm(df0, df1, df2, df3, df4)
save(austria, file = "data/austria.rda")



