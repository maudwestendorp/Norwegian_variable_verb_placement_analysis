---
## Maud Westendorp
## July 2021
## Descriptive statistics
---

######### Embedded V2 condition ######### 
# Get an overview of different items in this condition:
EV2items <- ev2 %>% 
              group_by(SentenceID) %>% 
              distinct(Background.sentence)
write_csv(EV2items, "../data/ev2items.csv") 

# How many informants and versions do we have data from in this condition?
ev2 %>% summarise(count = n_distinct(Informant))
ev2 %>% summarise(count = n_distinct(Version))

# Per subcondition - get the numbers:
count(ev2, Subcondition)

ev2_clause <- ev2 %>% 
     group_by(Subcondition) %>% 
      count(WordOrder) %>% 
      mutate(prop = n/sum(n),
           prop = round(prop, digits = 3),
           perc = prop * 100,
           perc = str_c(perc, "%")) 

# Per adverb - get the numbers:
count(ev2, Adverb)

ev2_adv <- ev2 %>% 
    group_by(Adverb) %>% 
    count(WordOrder) %>% 
      mutate(prop = n/sum(n),
           prop = round(prop, digits = 3),
           perc = prop * 100,
           perc = str_c(perc, "%")) 

# Per item - get the numbers (and save for plotting):
ev2_item <- ev2 %>% 
      group_by(SentenceID, Subcondition, Adverb) %>% 
      count(WordOrder, .drop = FALSE) %>% 
      mutate(prop = n/sum(n),
          prop = round(prop, digits = 3),
          perc = prop * 100,
          perc = str_c(perc, "%")) %>% 
      filter(WordOrder == "VA") %>% 
      drop_na()

ev2_item %>% arrange(desc(prop))

# Which item with ofte has the most VA-orders?
ev2 %>% filter(Adverb == "ofte") %>% 
        group_by(SentenceID) %>% count(WordOrder) %>% 
        mutate(prop = n/sum(n),
               prop = round(prop, digits = 3), 
               perc = prop * 100,                                                                    
               perc = str_c(perc, "%")) %>% 
       filter(WordOrder == "VA") %>% 
      arrange(desc(prop))

# AFTER filtering out ambiguous items > per clause type - calculate the percentages:
ev2_clause_clean <-
    ev2 %>%
    filter(!SentenceID %in% c("ass.oft5", "fac.oft2", "int.oft1", "int.oft4")) %>%
    group_by(Subcondition) %>%
      count(WordOrder) %>%
      mutate(prop = n/sum(n),
           prop = round(prop, digits = 3),
           perc = prop * 100,
           perc = str_c(perc, "%"))

# What are the percentages WO per adverb AND per clause type:
adv_clause <- ev2 %>% 
                group_by(Adverb, Subcondition) %>% 
                count(WordOrder, .drop = FALSE) %>% 
                mutate(prop = n/sum(n),
                       prop = round(prop, digits = 3),
                       perc = prop * 100,
                       perc = str_c(perc, "%")) %>% 
                filter(WordOrder == "VA")

# Let's remove the ambiguous sentences again:
adv_clause_clean <- ev2 %>% 
                filter(!SentenceID %in% c("ass.oft5", "fac.oft2", "int.oft1", "int.oft4")) %>% # remove ambiguous
                group_by(Adverb, Subcondition) %>% 
                count(WordOrder, .drop = FALSE) %>% 
                mutate(prop = n/sum(n),
                       prop = round(prop, digits = 3),
                       perc = prop * 100,
                       perc = str_c(perc, "%")) %>% 
                filter(WordOrder == "VA", Adverb == "ofte") %>% 
                mutate(Adverb = str_c(Adverb, "_adjusted"))

# And save the combination:
advclause <- bind_rows(adv_clause, adv_clause_clean)

# Northern vs. non-Northern - get the numbers:
ev2 %>% group_by(Northern) %>%
     summarise(count = n_distinct(Informant))

ev2_NN <- ev2 %>% 
     group_by(Northern, Subcondition) %>% 
      count(WordOrder) %>% 
      mutate(prop = n/sum(n),
      prop = round(prop, digits = 3),
      perc = prop * 100,
      perc = str_c(perc, "%")) %>% 
      filter(WordOrder == "VA")

# Split NN vs. non-NN by clause type and adverb:
ev2 %>% group_by(Northern, Subcondition, Adverb) %>% 
    count(WordOrderSimple) %>% 
    mutate(prop = n/sum(n),
           prop = round(prop, digits = 3),
           perc = prop * 100,
           perc = str_c(perc, "%")) %>% filter(WordOrderSimple == "V2") %>% arrange(Subcondition)

######### Embedded wh-questions ##########
# Calculate percentages Word Order in responses per Subcondition:
embwh %>%
    group_by(Subcondition) %>%
      count(WordOrder) %>%
      mutate(prop = n/sum(n),
           prop = round(prop, digits = 3),
           perc = prop * 100,
           perc = str_c(perc, "%"))

# Simplify word order to collapse to V2/V3 in both question types:
embwh$WordOrderSimple<- "V3"
embwh$WordOrderSimple[embwh$WordOrder == "VS"] <- "V2"
embwh$WordOrderSimple[embwh$WordOrder == "NON"] <- "V2"
embwh$WordOrderSimple[embwh$WordOrder == "OTHER"] <- "OTHER"
embwh$WordOrderSimple[embwh$WordOrder == "CLEFT"] <- "CLEFT"

# Get new numbers with Simple WO for table:
embwh %>% count(Subcondition)

embwh %>%
    group_by(Subcondition) %>%
      count(WordOrderSimple) %>%
      mutate(prop = n/sum(n),
           prop = round(prop, digits = 3),
           perc = prop * 100,
           perc = str_c(perc, "%"))

######### Main clause adverbs ######### 
# Calculate counts and percentages of word order used in responses for table:
adv %>% count(Subcondition) 

adv %>% group_by(Subcondition) %>%
        count(WordOrder) %>%
        mutate(prop = n/sum(n),
           prop = round(prop, digits = 3),
           perc = prop * 100,
           perc = str_c(perc, "%"))

# Group responses by item to check if there are unusual items:
adv_item <- adv %>% 
     group_by(UniqueNumb, Subcondition, WordOrder) %>% 
     summarise(count = n()) %>% 
     mutate(perc = count/sum(count))

# Split responses per adverb:
adv_V2 <- adv %>% 
  filter(Subcondition == "V2adv") %>% 
     group_by(Adverb, WordOrder) %>% 
     summarise(count = n()) %>% 
     mutate(perc = count/sum(count))

adv_V3 <- adv %>% 
  filter(Subcondition == "V3adv") %>% 
     group_by(Adverb, WordOrder) %>% 
     summarise(count = n()) %>% 
     mutate(perc = count/sum(count))

# Inspect per participant V3 adverbs with variation only:
adv_bt <- droplevels(subset(adv, adv$Adverb == "bokstavelig talt"))
adv_bt %>%
     group_by(Informant) %>%
                filter(WordOrder %in% c("AV", "VA")) %>% 
                count(WordOrder, .drop = FALSE) %>% 
                mutate(prop = n/sum(n),
                prop = round(prop, digits = 3),
                perc = prop * 100,
                perc = str_c(perc, "%")) %>% 
    filter(perc !="100%" & perc != "0%") %>% 
    distinct(Informant)

adv_rs <- droplevels(subset(adv, adv$Adverb == "rett og slett"))
adv_rs %>%
     group_by(Informant) %>%
                filter(WordOrder %in% c("AV", "VA")) %>% 
                count(WordOrder, .drop = FALSE) %>% 
                mutate(prop = n/sum(n),
                prop = round(prop, digits = 3),
                perc = prop * 100,
                perc = str_c(perc, "%")) %>% 
    filter(perc !="100%" & perc != "0%") %>% 
    distinct(Informant)

adv_sgs <- droplevels(subset(adv, adv$Adverb == "så godt som"))
adv_sgs %>%
     group_by(Informant) %>%
                filter(WordOrder %in% c("AV", "VA")) %>% 
                count(WordOrder, .drop = FALSE) %>% 
                mutate(prop = n/sum(n),
                prop = round(prop, digits = 3),
                perc = prop * 100,
                perc = str_c(perc, "%")) %>% 
    filter(perc !="100%" & perc != "0%") %>% 
    distinct(Informant)

adv_spt <- droplevels(subset(adv, adv$Adverb == "simpelthen"))
adv_spt %>%
     group_by(Informant) %>%
                filter(WordOrder %in% c("AV", "VA")) %>% 
                count(WordOrder, .drop = FALSE) %>% 
                mutate(prop = n/sum(n),
                prop = round(prop, digits = 3),
                perc = prop * 100,
                perc = str_c(perc, "%")) %>% 
    filter(perc !="100%" & perc != "0%") %>% 
    distinct(Informant)

adv_nst <- droplevels(subset(adv, adv$Adverb == "nesten"))
adv_nst %>%
     group_by(Informant) %>%
                filter(Subcondition == "V3adv") %>% 
                filter(WordOrder %in% c("AV", "VA")) %>% 
                count(WordOrder, .drop = FALSE) %>% 
                mutate(prop = n/sum(n),
                prop = round(prop, digits = 3),
                perc = prop * 100,
                perc = str_c(perc, "%")) %>% 
    filter(perc !="100%" & perc != "0%") %>% 
    distinct(Informant)
    
######### Main clause wh ############
# Get number of observations per Subcondition for table:
count(mainwh, Subcondition)

# Calculate percentages of Word Order used in responses:
mainwh %>% 
    group_by(Subcondition) %>%
    count(WordOrder) %>%
    mutate(prop = n/sum(n),
           prop = round(prop, digits = 3),
           perc = prop * 100,
           perc = str_c(perc, "%"))

# How many clefts are actually V3?
mainwh %>% 
    filter(WordOrder == "CLEFT") %>% 
    replace_na(list(Comment = "NA")) %>% 
    group_by(Subcondition) %>% 
    count(Comment == "V3 cleft")

# Make Word Order only V2/V3 for easier interpretation across Subconditions:
mainwh$WordOrderSimple <- "V2"
mainwh$WordOrderSimple[mainwh$WordOrder == "SOM"] <- "V3"
mainwh$WordOrderSimple[mainwh$WordOrder == "SV"] <- "V3"
mainwh$WordOrderSimple[mainwh$Comment == "V3 cleft"] <- "V3"
mainwh$WordOrderSimple[mainwh$WordOrder == "OTHER"] <- "OTHER"

# Is there an effect of dialect group (Northern Norwegian or not) on V2-V3 choice:
mainwh %>% group_by(Northern) %>%
     summarise(count = n_distinct(Informant))

mainwh %>% group_by(Northern) %>% 
    count(WordOrderSimple) %>% 
    mutate(prop = n/sum(n),
           prop = round(prop, digits = 3),
           perc = prop * 100,
           perc = str_c(perc, "%")) 

# Calculate percentages V2/non-V2 with long wh's:
mainwh %>% 
    filter(Subcondition %in% c("SubQlong", "ObjQlong")) %>% 
    count(WordOrderSimple) %>%     
    mutate(prop = n/sum(n),
              prop = round(prop, digits = 3),
              perc = prop * 100,
              perc = str_c(perc, "%"))

# Calculate percentages per word order with short wh's:
mainwh %>% 
    filter(Subcondition %in% c("SubQshort", "ObjQshort")) %>% 
    count(WordOrderSimple) %>%
    mutate(prop = n/sum(n),
                   prop = round(prop, digits = 3),
                   perc = prop * 100,
                   perc = str_c(perc, "%")

######### Comparing elicitation modes: Spoken vs. written ##########
# Let's have a look at EV2 first:
# How many responses with each mode do we have?
spokwrit %>% filter(SentenceID %in% ev2_23$SentenceID) %>%
          count(Type, Subcondition)

# Calculate percentages per Subcondition per Mode:
spokwrit %>% filter(SentenceID %in% ev2_23$SentenceID) %>% 
    mutate(WordOrder = factor(WordOrder)) %>% 
    group_by(Subcondition, Type) %>% 
    count(WordOrder, .drop = FALSE) %>% 
    mutate(prop = n/sum(n),
           prop = round(prop, digits = 3),
           perc = prop * 100,
           perc = str_c(perc, "%")) %>% 
    filter(WordOrder == "VA")

# Now let's have a look at the wh-questions:
# Count number of observations SubQshort per group and elicitation mode:
spwr_wh %>% filter(Subcondition == "SubQshort") %>% 
    group_by(InformantGroup, Type) %>% 
    count()

# Calculate percentages word order used in SubQshort per group and elicitation mode:
spwr_wh %>% filter(Subcondition == "SubQshort") %>% 
    group_by(InformantGroup, Type) %>% 
    count(WordOrder) %>% 
    mutate(prop = n/sum(n),
           prop = round(prop, digits = 3),
           perc = prop * 100,
           perc = str_c(perc, "%")) 

# Count number of observations ObjQshort per group and elicitation mode:
spwr_wh %>% filter(Subcondition == "ObjQshort") %>% 
    group_by(InformantGroup, Type) %>% 
    count()

# Calculate percentages word order used in ObjQshort per group and elicitation mode:
spwr_objq <- spwr_wh %>% 
                filter(Subcondition == "ObjQshort") %>% 
                mutate(WordOrder = factor(WordOrder)) %>% #made factor so I can do .drop = FALSE
                group_by(Informant, InformantGroup, Type) %>% 
                count(WordOrder, .drop = FALSE) %>% 
                mutate(prop = n/sum(n),
                prop = round(prop, digits = 3),
                perc = prop * 100,
                perc = str_c(perc, "%")) 

# Not per group but average:
spwr_objq %>% group_by(Type) %>%
                  filter(WordOrder == "SV") %>% 
                  summarize(M_V3 = mean(prop)*100,
                           SD_V3 = sd(prop)*100) 

# Calculate percentages word order used with V2- and V3-adverbs per group and elicitation mode:
spwr_adv <- filter(spokwrit,
              Subcondition %in% c("V2adv", "V3adv"))

spwr_adv3 <- spwr_adv %>% 
                filter(Subcondition == "V3adv") %>% 
                mutate(WordOrder = factor(WordOrder)) %>% 
                group_by(InformantGroup, Type, Subcondition, Informant) %>% 
                count(WordOrder, .drop = FALSE) %>% 
                mutate(prop = n/sum(n),
                prop = round(prop, digits = 3),
                perc = prop * 100,
                perc = str_c(perc, "%")) 

# Check dropped adverb responses in V3adv subcondition:
spwr_adv %>% filter(Subcondition == "V3adv") %>% 
                filter(Comment %in% 
                c("drop adverb", "hesitate, restart, drop adverb", "drop adverb + hesitation (stutter)", "hesitation + drop adverb", "aux + drop", "no adverb")) %>% 
                mutate(WordOrder = factor(WordOrder)) %>% 
                group_by(InformantGroup, Type, Subcondition) %>% 
                count(WordOrder, .drop = FALSE)

spwr_adv %>%  filter(Subcondition == "V3adv") %>% 
                mutate(WordOrder = factor(WordOrder)) %>% 
                group_by(InformantGroup, Type) %>% 
                count()

# Check if there is a difference in topicalization of V2adverbs between modes:
adv %>% filter(Subcondition == "V2adv") %>% 
        group_by(Version) %>% 
        count(WordOrder) %>% 
        mutate(prop = n/sum(n),
           prop = round(prop, digits = 3),
           perc = prop * 100,
           perc = str_c(perc, "%")) 
