un1956 <- read_excel("data-raw/iussp.xlsx", sheet = 4)
save(un1956, file = "data/un1956.rda")

load("data/un1956.rda")
