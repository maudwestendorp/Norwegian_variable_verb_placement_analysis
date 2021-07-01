---
## Maud Westendorp
## June 2021
## Preprocessing script and descriptive stats/overview
---
## Load in libraries:
library(tidyverse)
library(patchwork)

## Load in data:
no2 <- read_csv2("../data/NOexp2.csv")

## Check data:
##### remove non-native speakers
no2 <- filter(no2, 
              Mother.tongue %in% c("NORWEGIAN", "NORWEGIAN/SAMI", 
                                   "NORWEGIAN/ENGLISH", "NORTHERN SAMI/NORWEGIAN", 
                                   "NORWEGIAN/DARI"))

no2 <- dplyr::select(no2, Informant, NordN, Maudnr, read1, exp, gruppe, UniqueNumb, type, Realization, Type_Element1, Type_Element2, Exact_category, Produced_Word_Order, Comment)

no2 <- rename(no2,
              Version = exp,
              InformantGroup = gruppe,
              Northern = NordN,
              read = read1,
              SentenceID = Maudnr,
              Adverb = Type_Element2,
              Reflexive = Type_Element1,
              Subcondition = Exact_category,
              WordOrder = Produced_Word_Order
)

#### fix error in Subcondition 
no2$Subcondition[no2$UniqueNumb == "2403" & no2$SentenceID == "embobq"] <- NA
no2$Subcondition[no2$UniqueNumb == "2505" & no2$SentenceID == "embobq"] <- NA


## Optional EV2: 
#### dataframe for Embedded V2-condition
ev2 <- dplyr::filter(no2, 
                     Subcondition %in% c("BridgeV", "EmbQ", "Non-BridgeV"))
ev2 <-  filter(ev2, type != "Read") %>% 
  filter(!is.na(WordOrder))

#### reorder for convenience
ev2$Subcondition <- factor(ev2$Subcondition, levels = c("BridgeV", "Non-BridgeV", "EmbQ"))
ev2$WordOrder <- factor(ev2$WordOrder, levels = c("AV", "VA", "OTHER"))
ev2$Adverb <- factor(ev2$Adverb, levels = c("ikke", "aldri", "alltid", "ofte"))

#### per subcondition - get the numbers:
count(ev2, Subcondition)
ev2 %>% 
  group_by(Subcondition) %>% 
  count(WordOrder) %>% 
  mutate(prop = n/sum(n),
         prop = round(prop, digits = 3),
         perc = prop * 100,
         perc = str_c(perc, "%")) 

#### per clause type - calculate the percentages (and plot):
ev2_clause <- ev2 %>% 
  group_by(Subcondition) %>% 
  count(WordOrder) %>% 
  mutate(prop = n/sum(n),
         prop = round(prop, digits = 3),
         perc = prop * 100,
         perc = str_c(perc, "%")) 

#### AFTER filtering out ambiguous items > per clause type - calculate the percentages:
ev2_clause_clean <- ev2 %>%
  filter(!SentenceID %in% c("ass.oft5", "fac.oft2", "int.oft1", "int.oft4")) %>%
  group_by(Subcondition) %>%
  count(WordOrder) %>%
  mutate(prop = n/sum(n),
         prop = round(prop, digits = 3),
         perc = prop * 100,
         perc = str_c(perc, "%"))

#### per adverb - get the numbers:
ev2 %>% 
  group_by(Adverb) %>% 
  count(WordOrder) %>% 
  mutate(prop = n/sum(n),
         prop = round(prop, digits = 3),
         perc = prop * 100,
         perc = str_c(perc, "%")) 

#### per adverb - per clause type:
adv_clause <- ev2 %>% 
  group_by(Adverb, Subcondition) %>% 
  count(WordOrder, .drop = FALSE) %>% 
  mutate(prop = n/sum(n),
         prop = round(prop, digits = 3),
         perc = prop * 100,
         perc = str_c(perc, "%")) %>% 
  filter(WordOrder == "VA")

adv_clause_clean <- ev2 %>% 
  filter(!SentenceID %in% c("ass.oft5", "fac.oft2", "int.oft1", "int.oft4")) %>% # remove ambiguous
  group_by(Adverb, Subcondition) %>% 
  count(WordOrder, .drop = FALSE) %>% 
  mutate(prop = n/sum(n),
         prop = round(prop, digits = 3),
         perc = prop * 100,
         perc = str_c(perc, "%")) %>% 
  filter(WordOrder == "VA") %>% 
  filter(Adverb == "ofte") %>% 
  mutate(Adverb = str_c(Adverb, "_adjusted"))

advclause <- bind_rows(adv_clause, adv_clause_clean)

adv_clause_p <-
  ggplot(advclause, aes(x = factor(Subcondition), y = prop*100)) + 
  geom_point(aes(color=Adverb), size = 3, alpha = 0.85) +
  geom_line(aes(group = Adverb, linetype = Adverb, color=Adverb)) +
  labs(x = "clause type", y = "percentage Verb > Adverb ") +
  scale_linetype_manual(name = c("aldri", "alltid", "ikke", "ofte", "ofte_adjusted"),
                        values=c("solid", "solid", "solid", "dotted", "solid"), 
                        labels = c("aldri 'never'", "alltid 'always'", "ikke 'not'", "ofte (including ambiguous cases)", "ofte 'often' (unambiguous)")) +
  guides(linetype = guide_legend(reverse = TRUE)) +
  scale_color_manual(name = c("aldri", "alltid", "ikke", "ofte", "ofte_adjusted"),
                     values=c("#FC4E07", "#E7B800", "#C3D7A4", "#00AFBB", "#00AFBB"), 
                     labels = c("aldri 'never'", "alltid 'always'", "ikke 'not'", "ofte (including ambiguous cases)", "ofte 'often' (unambiguous)")) +
  guides(color = guide_legend(reverse = TRUE)) +
  scale_y_continuous(limits = c(-2,30), 
                     breaks = c(0, 5, 10, 15, 20, 25)) +
  scale_x_discrete(labels = c("assertive", "factive", "interrogative")) +
  theme_minimal() +
  theme(legend.title = element_blank(),
        legend.text = element_text(size = 10))

adv_clause_p

#### Northern vs. non-Northern Norwegian
ev2 %>% group_by(Northern) %>%
  summarise(count = n_distinct(Informant))

ev2_NN <- ev2 %>% 
  filter(!SentenceID %in% c("ass.oft5", "fac.oft2", "int.oft1", "int.oft4")) %>% # remove ambiguous
  group_by(Northern, Subcondition) %>% 
  count(WordOrder) %>% 
  mutate(prop = n/sum(n),
         prop = round(prop, digits = 3),
         perc = prop * 100,
         perc = str_c(perc, "%")) %>% 
  filter(WordOrder == "VA")

ev2_NN 

## Variable V2 in main clause wh-questions
#### filter for condition
mainwh <- filter(no2,
                 Subcondition %in% c("ObjQshort", "SubQshort", "ObjQlong", "SubQlong"))
mainwh <- filter(mainwh, type != "Read")

#### reorder for convenience
mainwh$Subcondition <- factor(mainwh$Subcondition, levels = c("ObjQshort", "ObjQlong", "SubQshort", "SubQlong"))
mainwh$WordOrder <- factor(mainwh$WordOrder, levels = c("VS", "SV", "NON", "SOM", "CLEFT", "OTHER"))

#### get numbers for table
count(mainwh, Subcondition)

mainwh %>% 
  group_by(Subcondition) %>% 
  count(WordOrder) %>% 
  mutate(prop = n/sum(n),
         prop = round(prop, digits = 3),
         perc = prop * 100,
         perc = str_c(perc, "%")) 

#### make v2/v3
mainwh$WordOrderSimple <- "V2"
mainwh$WordOrderSimple[mainwh$WordOrder == "SOM"] <- "V3"
mainwh$WordOrderSimple[mainwh$WordOrder == "SV"] <- "V3"
mainwh$WordOrderSimple[mainwh$Comment == "V3 cleft"] <- "V3"
mainwh$WordOrderSimple[mainwh$WordOrder == "OTHER"] <- "OTHER"

mainwh %>% 
  group_by(Subcondition) %>% 
  count(WordOrderSimple) %>% 
  mutate(prop = n/sum(n),
         prop = round(prop, digits = 3),
         perc = prop * 100,
         perc = str_c(perc, "%")) 

write.table(mainwh, 'mainwh.csv', sep = ',', row.names = F)
write.table(mainwh, 'ev2.csv', sep = ',', row.names = F)
