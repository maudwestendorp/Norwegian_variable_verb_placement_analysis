---
## Maud Westendorp
## July 2021
## Mixed model analyses
---
######### Embedded Verb Second #########
# Make the adjusted WordOrderSimple a binary outcome by removing "other" responses and keeping only V2 and V3 orders:
ev2 <- ev2 %>% 
  mutate(WO_fac = factor(WordOrderSimple))

ev2_stats <- ev2 %>% 
            filter(WO_fac != "OTHER", Adverb !="ikke")

ev2_stats$WO_fac <- droplevels(ev2_stats$WO_fac) %>% 
  relevel(ev2_stats$WO_fac, ref = "V3")

# Are the levels correct?
levels(ev2_stats$WO_fac)

# Additional tibble for modeling without potentially ambiguous items (with adverb ofte), "ikke" removed because it is not used across all clause types:
ev2_stats_clean <- ev2_stats %>%  
            filter(!SentenceID %in% 
                     c("ass.oft5", "fac.oft2", "int.oft1", "int.oft4"),
                   Adverb != "ikke")

# Models for testing effect of Adverb on word order:
# First with complete dataset:
ev2.adv_mdl <- glmer(WO_fac ~ Adverb + 
                       (Adverb|Informant) + (1|UniqueNumb), 
                     data = ev2_stats, 
                     family = binomial, 
                     control = glmerControl(optimizer="bobyqa"))

# Check the results of modeling (Word order ~ Adverb):
summary(ev2.adv_mdl)
fixef(ev2.adv_mdl) %>% round(digits = 2)

# Then make models with cleaned dataset (removed potentially ambiguous items):
ev2.adv.clean_mdl <- glmer(WO_fac ~ Adverb + 
                       (Adverb|Informant) + (1|UniqueNumb), 
                     data = ev2_stats_clean, 
                     family = binomial, 
                     control = glmerControl(optimizer="bobyqa"))

# Have a look at the fixed and random effects:
summary(ev2.adv.clean_mdl)

# Extract coefficients for clean models:
fixef(ev2.adv.clean_mdl)
plogis(fixef(ev2.adv.clean_mdl)[1]) %>% round(3) # probability V2 with aldri
plogis(fixef(ev2.adv.clean_mdl)[1] + fixef(ev2.adv.clean_mdl)[2] * 1 ) %>% round(3) # probability V2 with alltid
plogis(fixef(ev2.adv.clean_mdl)[1] + fixef(ev2.adv.clean_mdl)[3] * 1 ) %>% round(3) # probability V2 with ofte

# Compare models, create null-comparison models first, then compare with anova:
ev2.adv_null.mdl <- glmer(WO_fac ~ 1 + 
                       (Adverb|Informant) + (1|UniqueNumb), 
                     data = ev2_stats, 
                     family = binomial, 
                     control = glmerControl(optimizer="bobyqa"))

ev2.adv.clean_null.mdl <- glmer(WO_fac ~ 1 + 
                       (Adverb|Informant) + (1|UniqueNumb), 
                     data = ev2_stats_clean, 
                     family = binomial, 
                     control = glmerControl(optimizer="bobyqa"))

# Comparing models:
anova(ev2.adv_null.mdl, ev2.adv_mdl, test = "Chisq")
anova(ev2.adv.clean_null.mdl, ev2.adv.clean_mdl, test = "Chisq")

# Models for testing effect of Clause type on word order:
ev2_stats_clause <- ev2 %>% 
            filter(WO_fac != "OTHER", !SentenceID %in% 
                     c("ass.oft5", "fac.oft2", "int.oft1", "int.oft4"))

ev2.clause_mdl <- glmer(WO_fac ~ Subcondition + 
                           (Subcondition|Informant) + (1|UniqueNumb),
                           data = ev2_stats_clause, 
                           family = binomial,
                           control = glmerControl(optimizer="bobyqa"))

ev2.clause_null.mdl <- glmer(WO_fac ~ 1 + 
                           (Subcondition|Informant) + (1|UniqueNumb),
                           data = ev2_stats_clause, 
                           family = binomial,
                           control = glmerControl(optimizer="bobyqa"))

anova(ev2.clause_null.mdl, ev2.clause_mdl, test = "Chisq")

# Check the results of the modeling (Word order ~ Clause type):
summary(ev2.clause_mdl)
fixef(ev2.clause_mdl) %>% round(digits = 2)

# Models for testing effect of experiment version and dialect background (Northern-Norwegian or not) on word order:
ev2.version_afex <- mixed(WO_fac ~ Version + 
                    (1|Informant) + (1|UniqueNumb),
                    data = ev2_stats, 
                    family = binomial,
                    method = "LRT")

ev2.region_afex <- mixed(WO_fac ~ Northern + 
                    (1|Informant) + (1|UniqueNumb),
                    data = ev2_stats, 
                    family = binomial,
                    method = "LRT")

# Check the results for modeling (Word order ~ Northern Norwegian dialect):
ev2.version_afex
ev2.region_afex

######### Main clause wh-questions #########
# Convert Word Order to factor for modeling and drop "other" (non-target) responses to ensure binary outcome variable:
mainwh_stats <- mainwh %>% 
            mutate(WO_fac = factor(WordOrderSimple)) %>% 
            mutate(Northern_fac = factor(Northern)) %>% 
            filter(WO_fac != "OTHER")

mainwh_stats$WO_fac <- droplevels(mainwh_stats$WO_fac)
levels(mainwh_stats$WO_fac)

# Models for testing effect of Dialect region on word order:
mainwh_mdl <- glmer(WO_fac ~ Northern +
                        (1|Informant) + (1|UniqueNumb) + (1|Subcondition), 
                        data = mainwh_stats, 
                        family = binomial)

region_afex <- mixed(WO_fac ~ 1 + Northern + 
                    (1|Informant) + (1|UniqueNumb) + (1|Subcondition), 
                    data = mainwh_stats, 
                    family = binomial,
                    method = "LRT") 

# Examine models and extract coefficients:
summary(mainwh_mdl)
region_afex
fixef(mainwh_mdl)

plogis(fixef(mainwh_mdl)[1] + fixef(mainwh_mdl)[2] * 0) %>% round(3) # probability V3 word order for non-Northern
plogis(fixef(mainwh_mdl)[1] + fixef(mainwh_mdl)[2] * 1) %>% round(3)  # probability V3 word order for Northern
