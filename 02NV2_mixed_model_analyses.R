---
## Maud Westendorp
## June 2021
## Mixed model analysis
---

## Load in libraries:
library(tidyverse)
library(lme4)
library(afex)
library(sommer)
library(patchwork)

## Load in data:
ev2 <- read_csv2("../data/ev2.csv")
mainwh <- read_csv2("../data/mainwh.csv")

## Optional embedded V2
#### make v2/v3
ev2$WordOrderSimple <- "V3"
ev2$WordOrderSimple[ev2$WordOrder == "VA"] <- "V2"
ev2$WordOrderSimple[ev2$WordOrder == "OTHER"] <- "OTHER"

#### prep
ev2_stats <- ev2 %>% 
  mutate(WO_fac = factor(WordOrderSimple)) %>% 
  filter(WO_fac != "OTHER") %>%
  mutate(Northern_fac = factor(Northern)) 

ev2_stats_ofte <- ev2 %>% 
  filter(Adverb == "ofte") %>% 
  mutate(WO_fac = factor(WordOrderSimple)) %>% 
  filter(WO_fac != "OTHER") %>%
  mutate(Northern_fac = factor(Northern)) 

ev2_stats_clean <- ev2 %>%  # clean dataset has ambiguous sentences removed
  filter(!SentenceID %in% c("ass.oft5", "fac.oft2", "int.oft1", "int.oft4")) %>% 
  mutate(WO_fac = factor(WordOrderSimple)) %>% 
  filter(WO_fac != "OTHER") %>%
  mutate(Northern_fac = factor(Northern)) 

ev2_stats$WO_fac <- droplevels(ev2_stats$WO_fac)
ev2_stats_clean$WO_fac <- droplevels(ev2_stats_clean$WO_fac)

ev2_stats$WO_fac <- relevel(ev2_stats$WO_fac, ref = "V3")
ev2_stats_clean$WO_fac <- relevel(ev2_stats_clean$WO_fac, ref = "V3")

### Adverb model (categorical effect) and comparison with null model using afex package
ev2_adv_mdl <- glmer(WO_fac ~ Adverb + 
                       (1|Informant) + (1|UniqueNumb), 
                     data = ev2_stats, 
                     family = binomial,
                     control = glmerControl(optimizer = 'bobyqa'))

ev2_adv_clean_mdl <- glmer(WO_fac ~ Adverb + 
                             (1|Informant) + (1|UniqueNumb), 
                           data = ev2_stats_clean, 
                           family = binomial, 
                           control=glmerControl(optimizer="bobyqa"))

ev2_adv_afex <- mixed(WO_fac ~ 1 + Adverb + 
                        (1|Informant) + (1|UniqueNumb), 
                      data = ev2_stats, 
                      family = binomial,
                      method = "LRT")

ev2_adv_clean_afex <- mixed(WO_fac ~ 1 + Adverb + 
                              (1|Informant) + (1|UniqueNumb), 
                            data = ev2_stats_clean, 
                            family = binomial,
                            method = "LRT")

#### Calculate predictions
fixef(ev2_adv_clean_mdl)
plogis(fixef(ev2_adv_clean_mdl)[1]) %>% round(3) # probability V2 with ikke
plogis(fixef(ev2_adv_clean_mdl)[1] + fixef(ev2_adv_clean_mdl)[2]) %>% round(3) # probability V2 with aldri
plogis(fixef(ev2_adv_clean_mdl)[1] + fixef(ev2_adv_clean_mdl)[3]) %>% round(3) # probability V2 with alltid
plogis(fixef(ev2_adv_clean_mdl)[1] + fixef(ev2_adv_clean_mdl)[4]) %>% round(3) # probability V2 with ofte


#### Clause type model (categorical effect) and comparison with null model using afex package
ev2_clause_mdl <- glmer(WO_fac ~ Subcondition +
                          (1|Informant) + (1|UniqueNumb) + (1|Adverb), 
                        data = ev2_stats_clean,
                        family = binomial, 
                        control=glmerControl(optimizer="bobyqa"))

ev2_clause_afex <- mixed(WO_fac ~ 1 + Subcondition + 
                           (1|Informant) + (1|UniqueNumb) + (1|Adverb), 
                         data = ev2_stats_clean, 
                         family = binomial,
                         method = "LRT")

### Northern vs. non-Northern Norwegian model (categorical effect) and comparison with null model using afex package
ev2_region_afex <- mixed(WO_fac ~ 1 + Northern_fac + 
                           (1|Informant) + (1|UniqueNumb) + (1|Subcondition), 
                         data = ev2_stats_clean, 
                         family = binomial,
                         method = "LRT")

## Optional V2 in main clause wh-questions
### effects dialect group (Northern Norwegian or not) on V2-V3
mainwh %>% group_by(Northern) %>%
  summarise(count = n_distinct(Informant))

mainwh %>% group_by(Northern) %>% 
  count(WordOrderSimple) %>% 
  mutate(prop = n/sum(n),
         prop = round(prop, digits = 3),
         perc = prop * 100,
         perc = str_c(perc, "%")) 

mainwh <- mainwh %>% mutate(WO_fac = factor(WordOrderSimple)) %>% 
  filter(WO_fac != "OTHER") %>%
  mutate(Northern_fac = factor(Northern)) 

mainwh_mdl <- glmer(WO_fac ~ 1 + Northern_fac +
                      (1|Informant) + (1|UniqueNumb) + (1|Subcondition), 
                    data = mainwh, 
                    family = binomial,
                    control=glmerControl(optimizer="bobyqa"))

mainwh_mdl_null <- glmer(WO_fac ~ 1 +
                           (1|Informant) + (1|UniqueNumb) + (1|Subcondition), 
                         data = mainwh, 
                         family = binomial,
                         control=glmerControl(optimizer="bobyqa"))

anova(mainwh_mdl_null, mainwh_mdl, test = 'Chisq')

plogis(fixef(mainwh_mdl)[1] + fixef(mainwh_mdl)[2] * 0) %>% round(3) # probability V3 word order for non-Northern
plogis(fixef(mainwh_mdl)[1] + fixef(mainwh_mdl)[2] * 1) %>% round(3)  # probability V3 word order for Northern
