library(tidyverse)
library(foreign)
library(brms)
library(lme4)
library(ICCbin)

scjs <- haven::read_dta(here::here("dat", "scjs1718small_missing_tmp.dta"))

scjs <- janitor::clean_names(scjs)

scjs <- 
scjs %>% 
  mutate_all(as.character) %>% 
  mutate_all(~ if_else(.x == "-1", NA_character_, .x)) %>% 
  mutate_all(~ if_else(.x == "-2", NA_character_, .x)) %>% 
  mutate_all(as.factor) %>% 
  mutate(wgtgindiv_scale = as.numeric(wgtgindiv_scale),
         crimep = as.numeric(crimep) - 1)

naniar::vis_miss(scjs)

# select only ethnicity, gender, age and area deprivation

strata <- 
scjs %>% 
  select(qdeth3, qdgen, tabage, simd_quint) %>% 
  group_by_all() %>% 
  count() %>% 
  ungroup() %>%
  arrange(desc(n)) %>% 
  mutate(stratum = row_number())

scjs <- left_join(scjs, strata)


scjs %>% 
  mutate(survey_year = 2017)

# modelling ---------------------------------------------------------------

# set priors
prior2 <- c(set_prior("normal(0,4)", class = "Intercept"),
            set_prior("exponential(2)", class = "sd")
)

brm_mod2 <- 
  brm(
    crimep ~ 1 + (1 | stratum),
    data = scjs,
    family = "bernoulli",
    prior = prior2,
    cores = 4,
    control = list(adapt_delta = 0.95,
                   max_treedepth = 12)
  )

brm_mod2


# interactions ------------------------------------------------------------

signif_interactions <- 
  broom::tidy(brm_mod2) %>% 
  as_tibble() %>% 
  select(-std.error) %>% 
  mutate_if(is.numeric, exp) %>% # gives odds ratios
  filter(stringr::str_detect(term, "strat")) %>% 
  mutate(
    signif = case_when(
      lower < 1 & upper < 1 ~ 1,
      lower > 1 & upper > 1 ~ 1,
      TRUE ~ 0
    ))

stratum_counts <- 
  brm_mod2$data %>% 
  as_tibble() %>% 
  count(stratum)


signif_interactions <- 
  signif_interactions %>% 
  mutate(stratum = stringr::str_extract(term, "(\\d)+"),
         ordered_class = stringr::str_match(term, "__mu(.*?)\\[")[,2]) %>% 
  filter(!is.na(stratum))

signif_interactions <- 
  signif_interactions %>% 
  left_join(stratum_counts %>% 
              mutate(stratum = as.character(stratum)) %>% 
              rename(stratum_n = n))

signif_interactions <- 
signif_interactions %>% 
  arrange(desc(stratum_n)) %>% 
  mutate(stratum = row_number())


caterpillar_plot <- 
  signif_interactions %>% 
  ggplot(aes(x = as.numeric(stratum), y = estimate, colour = fct_rev(as.factor(signif)))) +
  geom_point(aes(size = stratum_n)) +
  geom_errorbar(aes(ymin = lower, ymax = upper), alpha = 0.7) +
  geom_point(data = 
               filter(signif_interactions, signif == 1),
             aes(size = stratum_n)) +
  geom_errorbar(
    data = 
      filter(signif_interactions, signif == 1),
    aes(ymin = lower, ymax = upper), alpha = 0.7) +
  ggrepel::geom_text_repel(    data = 
                                 filter(signif_interactions, signif == 1),
                               aes(label = stratum),
                               min.segment.length = 0.1) +
  facet_wrap(~ ordered_class) +
  scale_colour_grey() +
  geom_hline(yintercept = 1, linetype = "dashed") +
  theme_bw() +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank()) +
  labs(x = "Stratum",
       colour = "Significance",
       y = "Estimate",
       caption = "x-axis is in square-root scale, compressing smaller strata (on right hand of the plot) together)") +
  scale_x_sqrt(breaks = NULL)

caterpillar_plot


scjs %>% 
  filter(stratum == 75) %>% 
  select(qdeth3, qdgen, tabage, simd_quint) %>% 
  distinct()

# minority ethnic young men in deprived areas

scjs_dta <- haven::read_dta(here::here("data", "scjs1718small_missing_tmp.dta"))

scjs_dta %>% 
  janitor::clean_names() %>% 
  select(2:(ncol(.) - 3)) %>% 
  select(qdgen) %>% 
  distinct()

scjs %>% 
    filter(stratum == 18) %>% 
    select(qdeth3, qdgen, tabage, simd_quint) %>% 
    distinct()
  

scjs_dta %>% 
  janitor::clean_names() %>% 
  select(qdeth3, qdgen, tabage, simd_quint) %>% 
  distinct()

# also white women in deprived areas

scjs %>% 
  filter(stratum == 41) %>% 
  select(qdeth3, qdgen, tabage, simd_quint) %>% 
  distinct()



# scjs model main effects -------------------------------------------------

# set priors
prior_main <- c(set_prior("normal(0,4)", class = "Intercept"),
                set_prior("normal(0,4)", class = "b"),
            set_prior("exponential(2)", class = "sd")
)


brm_mod_main <- 
  brm(
    crimep ~ 1 + 
      qdeth3 + qdgen + tabage + simd_quint +
      (1 | stratum),
    data = scjs,
    family = "bernoulli",
    prior = prior_main,
    cores = 4,
    control = list(adapt_delta = 0.95,
                   max_treedepth = 12)
  )

get_signif_interactions <- function(brms_model){
  
  signif_interactions <- 
    broom::tidy(brms_model) %>% 
    as_tibble() %>% 
    select(-std.error) %>% 
    mutate_if(is.numeric, exp) %>% # gives odds ratios
    filter(stringr::str_detect(term, "strat")) %>% 
    mutate(
      signif = case_when(
        lower < 1 & upper < 1 ~ 1,
        lower > 1 & upper > 1 ~ 1,
        TRUE ~ 0
      ))
  
  stratum_counts <- 
    brms_model$data %>% 
    as_tibble() %>% 
    count(stratum)
  
  
  signif_interactions <- 
    signif_interactions %>% 
    mutate(stratum = stringr::str_extract(term, "(\\d)+"),
           ordered_class = stringr::str_match(term, "__mu(.*?)\\[")[,2]) %>% 
    filter(!is.na(stratum))
  
  signif_interactions <- 
    signif_interactions %>% 
    left_join(stratum_counts %>% 
                mutate(stratum = as.character(stratum)) %>% 
                rename(stratum_n = n))
  
  signif_interactions <- 
    signif_interactions %>% 
    arrange(desc(stratum_n)) %>% 
    mutate(stratum = row_number())

  
}

signif <- get_signif_interactions(brms_model = brm_mod_main)



signif

caterpillar_plot <- function(signif_interactions){
signif_interactions %>% 
  ggplot(aes(x = as.numeric(stratum), y = estimate, colour = fct_rev(as.factor(signif)))) +
  geom_point(aes(size = stratum_n)) +
  geom_errorbar(aes(ymin = lower, ymax = upper), alpha = 0.7) +
  geom_point(data = 
               filter(signif_interactions, signif == 1),
             aes(size = stratum_n)) +
  geom_errorbar(
    data = 
      filter(signif_interactions, signif == 1),
    aes(ymin = lower, ymax = upper), alpha = 0.7) +
  ggrepel::geom_text_repel(    data = 
                                 filter(signif_interactions, signif == 1),
                               aes(label = stratum),
                               min.segment.length = 0.1) +
  scale_colour_grey() +
  geom_hline(yintercept = 1, linetype = "dashed") +
  theme_bw() +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank()) +
  labs(x = "Stratum",
       colour = "Significance",
       y = "Estimate",
       caption = "x-axis is in square-root scale, compressing smaller strata (on right hand of the plot) together)") +
  scale_x_sqrt(breaks = NULL)
}

caterpillar_plot(signif)

# nothing doing



# adding 2016 sweep -------------------------------------------------------



scjs16 <- readRDS(here::here("dat", "scjs1617.rds"))

scjs16 <- scjs16 %>% 
  janitor::clean_names()

scjs16 %>% 
  count(prevviolent, prevthreat)

scjs16 <- 
  scjs16 %>% 
  mutate_all(as.character) %>% 
  mutate_all(~ if_else(.x == "-1", NA_character_, .x)) %>% 
  mutate_all(~ if_else(.x == "-2", NA_character_, .x)) %>% 
  mutate_all(as.factor) %>% 
  mutate(crimep = if_else(prevviolent == 1  | prevthreat == 1, 1, 0))

scjs16_slim <- 
  scjs16 %>% 
  select(qdeth3, qdgen, tabage, simd_quint, crimep, wgtgindiv_scale) 

scjs16 <- 
  scjs16 %>% 
  mutate(survey_year = 2016)

