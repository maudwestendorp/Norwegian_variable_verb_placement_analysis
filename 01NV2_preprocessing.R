---
## Maud Westendorp
## June 2021, tidied and minor adjustments July 15
## Preprocessing script and descriptive stats/overview
---
######### Setup/preprocessing ######### 
# Load in libraries:
library(tidyverse)
library(lme4)
library(afex)
library(patchwork)

### Load in data:
no2 <- read_csv2("../NOexp2.csv")

# Remove non-native speakers:
no2 <- filter(no2, 
               Mother.tongue %in% c("NORWEGIAN", "NORWEGIAN/SAMI", 
                                    "NORWEGIAN/ENGLISH", "NORTHERN SAMI/NORWEGIAN", 
                                    "NORWEGIAN/DARI"))

# Selecting columns relevant for analysis:
no2 <- dplyr::select(no2, Informant, NordN, 
              Maudnr, read1, exp, gruppe, 
              UniqueNumb, type, Realization,
              Type_Element2, Exact_category, 
              Produced_Word_Order, Comment)

# Rename these columns for readability:
no2 <- rename(no2,
                Version = exp,
                InformantGroup = gruppe,
                Northern = NordN,
                SentenceID = Maudnr,
                Adverb = Type_Element2,
                Subcondition = Exact_category,
                WordOrder = Produced_Word_Order,
                Type = type,
                Background.sentence = read1,
                Response = Realization)

# Reorder these columns for even more readability:
no2 <- relocate(no2,
               Informant, InformantGroup, Northern, 
               Version, UniqueNumb, SentenceID, Type, 
               Subcondition, Adverb, Background.sentence, 
               Response, WordOrder, Comment)

######### Setup different dataframes per experimental condition: #########
# Create dataframe for Embedded V2-condition:
ev2 <- no2 %>% filter(Subcondition %in% c("BridgeV", "EmbQ", "Non-BridgeV"),
                      Type != "Read",
                      !is.na(WordOrder))

# Reorder for convenience:
ev2$Subcondition <- factor(ev2$Subcondition, 
                           levels = c("BridgeV", "Non-BridgeV", "EmbQ"))
ev2$WordOrder <- factor(ev2$WordOrder, 
                        levels = c("AV", "VA", "OTHER"))
ev2$Adverb <- factor(ev2$Adverb, 
                     levels = c("ikke", "aldri", "alltid", "ofte"))

# Make WordOrderSimple column:
ev2$WordOrderSimple <- "V3"
ev2$WordOrderSimple[ev2$WordOrder == "VA"] <- "V2"
ev2$WordOrderSimple[ev2$WordOrder == "OTHER"] <- "OTHER"

# Filter for Embedded wh-questions condition:
embwh <- filter(no2, 
                Subcondition %in% c("EmbObQ", "EmbSubQ")) 
embwh <- filter(embwh, Type != "Read")

# Reorder for convenience:
embwh$Subcondition <- factor(embwh$Subcondition, levels = c("EmbSubQ", "EmbObQ"))
embwh$WordOrder <- factor(embwh$WordOrder, levels = c("SV", "VS", "SOM", "NON", "CLEFT", "OTHER"))

# Filter for main clause adverbs condition:
adv <- filter(no2,
              Subcondition %in% c("V2adv", "V3adv"))
adv <- filter(adv, Type != "Read")

# Reorder for convenience:
adv$WordOrder <- factor(adv$WordOrder, levels = c("VA", "AV", "OTHER", "ADV.FIRST"))

# Filter for main clause wh-questions condition:
mainwh <- filter(no2,
              Subcondition %in% c("ObjQshort", "SubQshort", "ObjQlong", "SubQlong"))
mainwh <- filter(mainwh, Type != "Read")

# Reorder for convenience:
mainwh$Subcondition <- factor(mainwh$Subcondition, levels = c("ObjQshort", "ObjQlong", "SubQshort", "SubQlong"))
mainwh$WordOrder <- factor(mainwh$WordOrder, levels = c("VS", "SV", "NON", "SOM", "CLEFT", "OTHER"))
```

# Setup for comparing of elicitation modes:
# Which informants did both the written (2) and the spoken experiment (3):
inf2 <- no2 %>%  filter(Version == "2") %>%  distinct(Informant)
inf3 <- no2 %>%  filter(Version == "3") %>%  distinct(Informant)
inf23 <- inner_join(inf2, inf3) # participants who did both version 2 and 3

# Filter main df to only participants who did both experiments:
spokwrit <- filter(no2,
                       Informant %in% inf23$Informant)
spokwrit <- filter(spokwrit, Type != "Read") %>% 
    filter(!is.na(WordOrder))

# Subset dataframe to only wh-questions:
spwr_wh <- filter(spokwrit,
              Subcondition %in% c("ObjQshort", "SubQshort"))

# Mutate Word Orders to simpler terms:
spwr_wh$WordOrderSimple <- "V2"
spwr_wh$WordOrderSimple[spwr_wh$WordOrder == "SOM"] <- "V3"
spwr_wh$WordOrderSimple[spwr_wh$WordOrder == "SV"] <- "V3"
spwr_wh$WordOrderSimple[spwr_wh$Comment == "V3 cleft"] <- "V3"
spwr_wh$WordOrderSimple[spwr_wh$WordOrder == "OTHER"] <- "OTHER"

# Subset data frame for only EV2 conditions:
ev2_2 <- spokwrit %>%  
          filter(Subcondition %in% c("BridgeV", "EmbQ", "Non-BridgeV"), 
                 Type == "Produce") %>%  
          distinct(SentenceID) %>% 
          arrange(SentenceID)
ev2_3 <- spokwrit %>%  
          filter(Subcondition %in% c("BridgeV", "EmbQ", "Non-BridgeV"),
                 Type == "Spoken") %>%
          distinct(SentenceID) %>% 
          arrange(SentenceID)
ev2_23 <- inner_join(ev2_2, ev2_3) # EV2 items in both version 2 and 3

rm(inf2, inf3, ev2_2, ev2_3)
