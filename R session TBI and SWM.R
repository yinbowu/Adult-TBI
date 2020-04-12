# TBI, spatial working memory and Attention in adults

# Packages ----------------------------------------------------------------

install.packages("psych")
install.packages("PerformanceAnalytics")
install.packages("Hmisc")
library("Hmisc")
library(tidyverse)
library(naniar)
library(psych)
library(PerformanceAnalytics)


# data file ---------------------------------------------------------------

adultswm <- read_csv("adultswm.csv")
ANT <- read.csv("adult_SWM_averages_ANT.csv")
swm <- read.csv("swm.csv")
ANT_summary_alldata <- read.csv("ANT_summary_alldata.csv")
concussion_severity <- read_csv("concussion_severity.csv")
attention <- read_csv("attention-hlm.csv")


# Spatial working memory --------------------------------------------------

swm$Subject <- as.factor(swm$Subject)

dplyr::count(spaceship1, distractorn20)
dplyr::count(spaceship1, distractorn125)
dplyr::count(spaceship1, distractorn5)
dplyr::count(spaceship1, distractorp5)
dplyr::count(spaceship1, distractorp125)
dplyr::count(spaceship1, distractorp5)

spaceship1 <- spaceship1 %>% 
  mutate(distractorn20 = case_when(n_distractor <= -20 ~ 1,
                                   TRUE ~ 0)) %>% 
  mutate(distractorn125 = case_when(n_distractor <= -12.5 & n_distractor > -20 ~ 1,
                                    TRUE ~ 0)) %>% 
  mutate(distractorn5 = case_when(n_distractor <= -5 & n_distractor > -12.5 ~ 1,
                                  TRUE ~ 0)) %>% 
  
  mutate(distractorp5 = case_when(n_distractor <= 5 & n_distractor > 0 ~ 1,
                                  TRUE ~ 0)) %>%
  mutate(distractorp125 = case_when(n_distractor <= 12.5 & n_distractor > 5 ~ 1,
                                    TRUE ~ 0)) %>%
  mutate(distractorp20 = case_when(n_distractor <= 20 & n_distractor > 12.5 ~ 1,
                                   TRUE ~ 0))


write.csv(spaceship1, file = "spaceship1.csv", row.names = FALSE)


# -- Spaceship1 -----------------------------------------------------------

spaceship1_mean <- spaceship1 %>% 
  group_by(Subject, TargDeg, n_distractor) %>% 
  summarise(mean_errdir = mean(errdir, na.rm = TRUE))


spaceship1_mean <- spaceship1_mean %>% 
  mutate(dn20 = case_when(n_distractor <= -20 ~ 1,
                          TRUE ~ 0)) %>% 
  mutate(dn125 = case_when(n_distractor <= -12.5 & n_distractor > -20 ~ 1,
                           TRUE ~ 0)) %>% 
  mutate(dn5 = case_when(n_distractor <= -5 & n_distractor > -12.5 ~ 1,
                         TRUE ~ 0)) %>% 
  
  mutate(dp5 = case_when(n_distractor <= 5 & n_distractor > 0 ~ 1,
                         TRUE ~ 0)) %>%
  mutate(dp125 = case_when(n_distractor <= 12.5 & n_distractor > 5 ~ 1,
                           TRUE ~ 0)) %>%
  mutate(dp20 = case_when(n_distractor <= 20 & n_distractor > 12.5 ~ 1,
                          TRUE ~ 0))
# ---Spaceship2 -----------------------------------------------------------

spaceship2 <- swm %>% 
  filter(distractor != "NA")

spaceship2 <- spaceship2 %>% 
  mutate(n_distractor = case_when(distractor <= -25       ~ (distractor +20)*(-1),
                                  distractor <= 0 & distractor >= -15 ~ (distractor +20)*(-1),
                                  distractor >= 45        ~ (distractor -40),
                                  distractor <= 35 & distractor >= 20 ~ (distractor -40)))

dplyr::count(spaceship2, n_distractor)

spaceship2 <- spaceship2 %>% 
  select(-Session, -Delay, -distractor)

write.csv(spaceship2, file = "spaceship2.csv", row.names = FALSE)


spaceship2_mean <- spaceship2 %>% 
  group_by(Subject, TargDeg, n_distractor) %>% 
  summarise(mean_errdir = mean(errdir, na.rm = TRUE))

write.csv(spaceship2_mean, file = "spaceship2_mean.csv", row.names = FALSE)


# Attention Network Task --------------------------------------------------

options(digits=10)

attention <-  ANT %>%
  select(Subject, Sex, age, ANT_alertingeffect:ANT_conflicteffect)

write.csv(attention, file = "attention-hlm.csv", row.names = FALSE)

attse <- left_join(attention, concussion_severity, by = "Subject")

attsems <- attse %>% 
  filter(severity != "NA") %>% 
  group_by(severity) %>% 
  summarise(alert_mean = mean(ANT_alertingeffect, na.rm = TRUE), 
            alert_sd = sd(ANT_alertingeffect, na.rm = TRUE),
            orient_mean = mean(ANT_orientingeffect, na.rm = TRUE),
            orient_sd = sd(ANT_orientingeffect, na.rm = TRUE),
            conflict_mean = mean(ANT_conflicteffect, na.rm = TRUE),
            conflict_sd = sd(ANT_conflicteffect, na.rm = TRUE))

# --- ANT raw data --------------------------------------------------------

ant_raw <- ANT_summary_alldata %>% 
  select(Subject = script.subjectid, Congruent = expressions.meanrt_correctCongruent,
         Incongruent = expressions.meanrt_correctInCongruent,
         alert = expressions.alertingeffect, orient = expressions.orientingeffect,
         conflict = expressions.conflicteffect)

write.csv(ant_raw, file = "ant_raw.csv", row.names = FALSE)


# TBI ---------------------------------------------------------------------

concussion <- ANT %>% 
  select(Subject, Sex, age, TBI_LOC:worst)

concussion$yesinjd <- ifelse(concussion$age_injury == "0", 0, 1)

concussioninjd <- concussion %>% 
  filter(age_injury != 0)

write.csv(concussion, file = "concussion-hlm.csv", row.names = FALSE)
write.csv(concussioninjd, file = "concussioninjd-hlm.csv", row.names = FALSE)


# Dummy code: severity ----------------------------------------------------


concussion_severity <- concussion %>% 
  mutate(severity = case_when(worst >= 3  ~ 3, 
                              worst <3 ~ worst))

concussion_severity <- concussion_severity %>% 
  select(Subject, worst, yesinjd, severity)

concussion_severity <- concussion_severity %>% 
  mutate(s1 = case_when(severity <= 1 & severity >0 ~ 1, 
                        TRUE ~ 0),
         s2 = case_when(severity <=2 & severity > 1 ~ 1,
                        TRUE ~0),
         s3 = case_when(severity <=3 & severity > 2 ~ 1,
                        TRUE ~0))

write.csv(concussion_severity, file = "concussion_severity.csv", row.names = FALSE)


# Figures for the paper ---------------------------------------------------

adultswm <- adultswm %>% 
  arrange(Subject)

concussion <- read_csv("concussion.csv")
concussioninjd <- read_csv("concussioninjd.csv")

con <- left_join(adultswm, concussion, by = "Subject")
coninjd <- left_join(adultswm, concussioninjd, by = "Subject")
conse <- left_join(adultswm, concussion_severity, by = "Subject")

# -- 5 distractor: age of injury & memory biases -----------------------------

con_d5 <- con %>% 
  filter(distractor == 5
  )

con_d5 <- con_d5 %>% 
  group_by(Subject, age_injury) %>% 
  summarise(errdir = mean(errdir, na.rm = TRUE)) %>% 
  filter(age_injury != "NA") %>% 
  mutate(acate = case_when( age_injury <= 0 ~  0,
                            age_injury %in% (1:7)~ 1, 
                            age_injury %in% (8:14) ~ 2,
                            age_injury >= 15 ~ 3)
  )

con_d5$acate <- as.factor(con_d5$acate)
con_d5$Subject <- as.factor(con_d5$Subject)
ANOVA_results <- aov(errdir ~ acate, data=con_d5)
summary(ANOVA_results)

TukeyHSD(ANOVA_results, "acate", ordered = TRUE)

con_d5 %>% 
  group_by(acate) %>% 
  summarise(n = n())

# -- injuried V.S. not injuried ----------------------------------------------

con$Subject <- as.factor(con$Subject)
coninjd$Subject <- as.factor(coninjd$Subject)
concussion$Subject <- as.factor(concussion$Subject)
concussioninjd$Subject <- as.factor(concussioninjd$Subject)

df <- spaceship1 %>% 
  group_by(Subject) %>% 
  summarise(ferrdir_mean = mean(ferrdir, na.rm = TRUE))

df <- left_join(df, concussion, by = "Subject")

df <- df %>% 
  filter(Subject != 11084)

df$yesinjd <- as.factor(df$yesinjd)

ggplot(data = df, mapping = aes(x = yesinjd, y = ferrdir_mean)) + 
  geom_boxplot()+
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))


# -- age * errdir * distractor --------------------------------------------------

df <- adultswm %>% 
  filter (distractor == 5 | distractor == 0) %>% 
  select(Subject, distractor, errdir)

df <- left_join(df, concussioninjd, by = "Subject")

df <- df %>% 
  filter(!is.na(age_injury))

df <- df %>% 
  group_by(Subject, age_injury, distractor) %>% 
  summarise(errdir_mean = mean(errdir, na.rm = TRUE))

df$distractor <- as.factor(df$distractor)

ggplot(data = df, mapping = aes(x = age_injury, y = errdir_mean, linetype = distractor)) + 
  geom_smooth(se = FALSE)+
  theme_bw() + theme(panel.border = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))


# -- errdir by target & by distractor -------------------------------------

spaceship1$n_distractor <- as.factor(spaceship1$n_distractor)

df <- spaceship1 %>% 
  group_by(TargDeg, n_distractor) %>% 
  summarise(ferrdir_mean = mean(ferrdir, na.rm = TRUE))

df$TargDeg <- as.factor(df$TargDeg)

ggplot(data = df, mapping = aes(x = n_distractor, y =ferrdir_mean)) +
  geom_boxplot()+
  theme_bw() + theme(panel.border = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))

ggplot(data = df, mapping = aes(x = TargDeg, y =ferrdir_mean)) +
  geom_boxplot()+
  theme_bw() + theme(panel.border = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))


# -- errdir * 5 degree distractor -----------------------------------------

df <- con %>% 
  filter(n_distractor == 5) %>% 
  filter(Subject != 11084)

df <- df %>% 
  group_by(Subject, yesinjd) %>% 
  summarise(ferrdir_mean = mean(ferrdir, na.rm = TRUE))

df$yesinjd <- as.factor(df$yesinjd)

ggplot(data = df, mapping = aes(x = yesinjd, y =ferrdir_mean)) +
  geom_boxplot()+
  theme_bw() + theme(panel.border = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))


# -- errdir by distractor by injury ------------------------------------------

concussion <- read_csv("concussion.csv")
concussioninjd <- read_csv("concussioninjd.csv")

con <- left_join(adultswm, concussion, by = "Subject")
coninjd <- left_join(adultswm, concussioninjd, by = "Subject")



# --- injuried VS. not injuried -------------------------------------------

con$Subject <- as.factor(con$Subject)
coninjd$Subject <- as.factor(coninjd$Subject)
concussion$Subject <- as.factor(concussion$Subject)
concussioninjd$Subject <- as.factor(concussioninjd$Subject)

conse_ca <- conse %>% 
  group_by(Subject, distractor, severity) %>% 
  summarise(errdir_mean = mean(errdir, na.rm = TRUE))

conse_ca$severity <- as.factor(conse_ca$severity)

conse_ca <- conse_ca %>% 
  filter(Subject != 11084)

ggplot(data = conse_ca, mapping = aes(x = severity, y =errdir_mean, fill = severity)) +
  geom_boxplot()+
  facet_grid(~distractor)


# -- Attention  ------------------------------------------------------

attention <- read.csv("attention-hlm.csv")

df <- left_join(attention, concussion, by = "Subject")

df$yesinjd <- as.factor(df$yesinjd)

df <- df %>% 
  filter(yesinjd != 11084)

df0 <- df %>% 
  group_by(yesinjd) %>% 
  summarise(alert = mean(ANT_alertingeffect, na.rm = TRUE),
            alert_sd = sd(ANT_alertingeffect, na.rm = TRUE)) 

d1 <- df %>% 
  group_by(yesinjd) %>% 
  summarise(orient = mean(ANT_orientingeffect, na.rm = TRUE),
            orient_sd = sd(ANT_orientingeffect, na.rm = TRUE)) 

d2 <- df %>% 
  group_by(yesinjd) %>% 
  summarise(conflict = mean(ANT_conflicteffect, na.rm = TRUE),
            conflict_sd = sd(ANT_conflicteffect, na.rm = TRUE)) 

df <- left_join(concussioninjd, attention, by = "Subject")

ggplot(data = df, mapping = aes(x = worst, y = ANT_conflicteffect))+
  geom_smooth(se = FALSE)+
  theme_bw() + theme(panel.border = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))


# -- ABS ---------------------------------------------------------------------

df <- con %>% 
  select(Subject, ABS_mm, yesinjd) %>% 
  filter(Subject != 11084)

df <- df %>% 
  group_by(Subject, yesinjd) %>% 
  summarise(ABS_mean = mean(ABS_mm, na.rm = TRUE))

df$yesinjd <- as.factor(df$yesinjd)

ggplot(data = df, mapping = aes(x = yesinjd, y = ABS_mean)) + 
  geom_boxplot()+
  theme_bw() + theme(panel.border = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))


# 2020-02-14- latest concussion data - data management --------------------------------

adultswm <- adultswm %>% dplyr::na_if("?")

adultswm <- adultswm %>% 
  select(Subject, Delay, distractor, TargDeg, ABS_mm, errdir) %>% 
  filter(Delay == 10000) %>% 
  select(-Delay
  )

adultswm <- adultswm %>% 
  mutate(ferrdir = case_when(TargDeg <0  ~ errdir*(-1), 
                             TargDeg > 0 ~ errdir)
  )

write.csv(adultswm, file = "adultswm.csv", row.names = FALSE
)

adultswm <- read_csv("adultswm.csv"
)

adultswm <- adultswm %>% 
  mutate(n_distractor = case_when(distractor <= -25       ~ (distractor +20)*(-1),
                                  distractor <= 0 & distractor >= -15 ~ (distractor +20)*(-1),
                                  distractor >= 45        ~ (distractor -40),
                                  distractor <= 35 & distractor >= 20 ~ (distractor -40),
                                  is.na(distractor) ~ 0)
  )

adultswm <- adultswm %>% 
  select(Subject, TargDeg, distractor = n_distractor, ABS_mm, errdir = ferrdir)


adultswm <- adultswm %>% 
  mutate(dn20 = case_when(distractor <= -20 ~ 1,
                          TRUE ~ 0)) %>% 
  mutate(dn125 = case_when(distractor <= -12.5 & distractor > -20 ~ 1,
                           TRUE ~ 0)) %>% 
  mutate(dn5 = case_when(distractor <= -5 & distractor > -12.5 ~ 1,
                         TRUE ~ 0)) %>% 
  
  mutate(dp5 = case_when(distractor <= 5 & distractor > 0 ~ 1,
                         TRUE ~ 0)) %>%
  mutate(dp125 = case_when(distractor <= 12.5 & distractor > 5 ~ 1,
                           TRUE ~ 0)) %>%
  mutate(dp20 = case_when(distractor <= 20 & distractor > 12.5 ~ 1,
                          TRUE ~ 0)
  )

write.csv(adultswm, file = "adultswm.csv", row.names = FALSE
)

