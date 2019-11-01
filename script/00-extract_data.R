unzip(here::here("data", "scjs1718.zip"),
      exdir = here::here("data", "stata.zip"))


unzip(here::here("data", "stata.zip", "8498stata_7221C7C2CA171C4A711683D5BBDC0740_V1.zip"),
      exdir = here::here("data"))

sjcs <- 
haven::read_dta(
  here::here("data", "UKDA-8498-stata", "stata", "stata11", "scjs1718__nvf-main_y2_eul_20190508.dta")
)

saveRDS(sjcs,
        here::here("data", "scjs1718.rds"))


# 16-17 sweep

unzip(here::here("data", "8365stata_176854D746A94B03E516A3B077DC3AF6_V1.zip"),
      exdir = here::here("data"))

scjs16 <- 
haven::read_dta(
  here::here("data", "UKDA-8365-stata", "stata", "stata11", "scjs1617_nvf-main_y1_eul.dta"))

saveRDS(scjs16,
        here::here("data", "scjs1617.rds"))
