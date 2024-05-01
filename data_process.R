library(kableExtra)
library(randomForest)
library(caret)
library(DiagrammeR)
library(xgboost)
library(MASS)
library(knitr)
library(tidyverse)
library(skimr)
library(foreign)
library(cowplot)
library(gridExtra)
library(nhanesA)
library(rpart)
library(plotly)

slq_p <- nhanes('P_SLQ', includelabels = TRUE, cleanse_numeric = TRUE)
dpq_p <- nhanes('P_DPQ', includelabels = TRUE, cleanse_numeric = TRUE, translated = FALSE)
alq_p <- nhanes('P_ALQ', includelabels = TRUE, cleanse_numeric = TRUE)
cdq_p <- nhanes('P_CDQ', includelabels = TRUE, cleanse_numeric = TRUE, translated = FALSE)
demo_p <- nhanes('P_DEMO', includelabels = TRUE, cleanse_numeric = TRUE)

dpq_p_m <- dpq_p %>% 
  mutate_all(~ ifelse(. %in% c(7, 9, '.', ''), NA, .))
dpq_p_m <- dpq_p_m %>% 
  mutate(score = rowSums(dpq_p_m, na.rm = TRUE) - SEQN) %>% 
  mutate(extent = ifelse(score < 10, "rare", 
        ifelse(score >= 15, "severe", "normal"))) %>% dplyr::select(SEQN, score)

cdq_p_m <- cdq_p %>%
  mutate(
    Grade1_Angina = ifelse(CDQ001 == 1 & CDQ002 == 1 & CDQ003 != 1 & CDQ004 == 1 & CDQ005 == 1 & CDQ006 == 1 & ((CDQ009D == 4 | CDQ009E == 5) | (CDQ009F == 6 & CDQ009G == 7)), TRUE, FALSE),
    Grade2_Angina = ifelse(CDQ001 == 1 & CDQ002 == 1 & CDQ003 == 1 & CDQ004 == 1 & CDQ005 == 1 & CDQ006 == 1 & ((CDQ009D == 4 | CDQ009E == 5) | (CDQ009F == 6 & CDQ009G == 7)), TRUE, FALSE)
  ) %>% 
  mutate(
    Grade1_Angina = replace_na(Grade1_Angina, FALSE),
    Grade2_Angina = replace_na(Grade2_Angina, FALSE)
  ) %>% 
  mutate(Angina = (Grade1_Angina|Grade2_Angina)) %>% 
  dplyr::select(SEQN, Angina, CDQ008, CDQ010) %>% 
  mutate_all(~ ifelse(. %in% c(7, 9, '.', ''), NA, .))

slq_p_m <- slq_p %>% 
  mutate(duration_avg = SLD012 * 5/7 + SLD013 * 2/7) %>% 
  mutate(if_enough_sleep = as.factor(ifelse(duration_avg >= 7, TRUE, FALSE)))

slq_p_m_2 <- slq_p_m %>% 
  dplyr::select(SEQN, SLQ030, SLQ050, SLQ120, SLQ030, duration_avg, if_enough_sleep)

demo_p_m <- demo_p %>% 
  filter(RIDAGEYR >= 18 & RIDAGEYR <= 60) %>% 
  dplyr::select(SEQN, RIAGENDR, DMDEDUC2, RIDAGEYR)

alq_p_m <- alq_p %>% 
  dplyr::select(-ALQ290, -ALQ280, -ALQ270, -ALQ111, -ALQ130, -ALQ151)

all_p_m_plot <- demo_p_m %>% 
  left_join(slq_p_m_2, by = join_by(SEQN)) %>% 
  left_join(dpq_p_m, by = join_by(SEQN)) %>% 
  left_join(alq_p_m, by = join_by(SEQN)) %>% 
  mutate_if(is.factor, as.character) %>% 
  drop_na() %>% 
  dplyr::select(-SEQN)

all_p_m_plot[all_p_m_plot == "Don't know"] <- NA
all_p_m_plot[all_p_m_plot == "Refused"] <- NA

all_p_m_plot <- all_p_m_plot %>% 
  mutate_if(is.character, as.factor) %>% 
  drop_na()

all_p_mr_plot <- all_p_m_plot %>% 
  rename(
    gender = RIAGENDR, 
    education_level = DMDEDUC2,
    age = RIDAGEYR,
    snore_freq = SLQ030,
    if_trouble_sleeping = SLQ050,
    feel_sleepy_freq = SLQ120,
    if_enough_sleep = if_enough_sleep,
    depression_score = score,
    drink_freq_last_year = ALQ121,
    more_drink_freq = ALQ142,
    more_drink_30days = ALQ170,
    duration_avg = duration_avg
  )

all_p_mr <- all_p_mr_plot %>% 
  dplyr::select(-duration_avg)
  




