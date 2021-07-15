---
## Maud Westendorp
## July 2021
## Graphics
---

######### Embedded V2 condition #########
# Plot word orders per Clause type:
ggplot(ev2_clause, aes(x = factor(Subcondition), 
                       y = prop*100, 
                       fill = factor(WordOrder))) +
     geom_bar(stat="identity", width = 0.7) +
     labs(x = "Clause type", y = "percent", fill = "Word order")

# Plot per Clause type (potentially ambiguous items with "ofte" removed this time):
ggplot(ev2_clause_clean, aes(x = factor(Subcondition), 
                       y = prop*100, 
                       fill = factor(WordOrder))) +
     geom_bar(stat="identity", width = 0.7) +
     labs(x = "Clause type", y = "percent", fill = "Word order")

# Create plot for V2 per clause type and split by adverb (Figure 2):
# Setup plot and add geoms:
adv_clause_p <- 
    ggplot(advclause, 
           aes(x = factor(Subcondition), y = prop*100)) + 
    geom_point(aes(color=Adverb), 
               size = 3, alpha = 0.85) +
    geom_line(aes(group = Adverb, linetype = Adverb, color=Adverb))

# Tweak axes:
adv_clause_p <- adv_clause_p +
    scale_linetype_manual(
      name = c("aldri", "alltid", "ikke", "ofte", "ofte_adjusted"),
      values=c("solid", "solid", "solid", "dotted", "solid"), 
      labels = c("aldri 'never'", "alltid 'always'", "ikke 'not'",
                 "ofte (including ambiguous cases)", "ofte 'often' (unambiguous)")) +
     guides(linetype = guide_legend(reverse = TRUE)) +
     scale_color_manual(
       name = c("aldri", "alltid", "ikke", "ofte", "ofte_adjusted"),
       values=c("#FC4E07", "#E7B800", "#C3D7A4", "#00AFBB", "#00AFBB"), 
       labels = c("aldri 'never'", "alltid 'always'", "ikke 'not'",
                  "ofte (including ambiguous cases)", "ofte 'often' (unambiguous)")) +
     guides(color = guide_legend(reverse = TRUE)) +
     scale_y_continuous(limits = c(-2,30), 
                    breaks = c(0, 5, 10, 15, 20, 25)) +
     scale_x_discrete(labels = c("assertive", "factive", "interrogative"))

# Tweak cosmetics:
adv_clause_p <- adv_clause_p +
    labs(x = "clause type", y = "percentage Verb > Adverb ") +
    theme_minimal() +
    theme(legend.title = element_blank(),
          legend.text = element_text(size = 10),
          axis.title.y = element_text(margin = margin(t = 0, r = 6,
                                                    b = 0, l = 0)),
          axis.title.x = element_text(margin = margin(t = 6, r = 0,
                                                    b = 0, l = 0)))

# Save plot:
# ggsave()

# Plot percentage VA-orders per items to check if any item might has more V2 than average:
# Setup plot and add geoms:
ev2_item_p <- 
    ggplot(ev2_item, aes(x = factor(SentenceID), y = prop * 100, fill = Adverb)) + 
    geom_bar(stat = "identity", position = "dodge") +
    facet_wrap(~Subcondition, scales = "free")

# Tweak axes:
ev2_item_p <- ev2_item_p + 
    scale_y_continuous(limits = c(0,50), 
                    breaks = c(0,10, 20, 30, 40)) +
    scale_fill_brewer(palette = "Spectral")
    
# Tweak cosmetics:
ev2_item_p <- ev2_item_p + 
    labs(x = "Item", y = "percent") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
    
########## Main clause adverbs #########
# plot per item to inspect:
ggplot(adv_item, 
       aes(x = factor(UniqueNumb), y = perc*100, fill = factor(WordOrder))) + 
    geom_bar(stat="identity") +
    labs(x = "Item", y = "percent", fill = "Word order") +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
    facet_wrap(~Subcondition)

# Create plot for responses per adverb (Figure 3):
# V2adverbs first 
adv$WordOrder <- factor(adv$WordOrder, levels = c("AV", "VA", "ADV.FIRST", "OTHER"))

# code colours:
cols <- c("AV" = "#00AFBB", "VA" = "#C3D7A4", "ADV.FIRST" = "#FC4E07", "OTHER" = "#E7B800")

# setup plot and add geoms:
adv2_p <- ggplot(adv_V2, 
                 aes(x = factor(Adverb), y = perc*100, fill = forcats::fct_rev(WordOrder))) +
    geom_bar(stat="identity")
    
# tweak axes:
adv2_p <- adv2_p +
    scale_fill_manual(values = cols) + 
    guides(fill = guide_legend(reverse=TRUE))

# tweak cosmetics:
adv2_p <- adv2_p +
    labs(x = "V2-adverbs", y = "percent", fill = "Word order") +
    theme(legend.position = 'none') +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

# then plot the V3-adverbs 
# just to make sure the order and labels will be the same:
adv$WordOrder <- factor(adv$WordOrder, levels = c("VA", "AV", "OTHER", "ADV.FIRST"))
labels <- c("Adverb initial", "Other", "Adverb > Verb", "Verb > Adverb")

# setup plot and add geoms:
adv3_p <- ggplot(adv_V3, 
                 aes(x = factor(Adverb), y = perc*100, fill = forcats::fct_rev(WordOrder))) +
    geom_bar(stat="identity")

# tweak axes:
adv3_p <- adv3_p +
    scale_fill_manual(values = cols, labels = labels) + 
    guides(fill = guide_legend(reverse=TRUE))

# tweak cosmetics:
adv3_p <- adv3_p +
    labs(x = "V3-adverbs", y = "", fill = "Word order") +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
          legend.text = element_text(size = 10))

# combine plots:
adv2_p + adv3_p

######### Comparing elicitation modes #########
# Create graph for ObjQshort (Figure 4a):
# Filter only V3-responses and setup plot:
spwr_objq_plot <- spwr_objq %>% group_by(InformantGroup, Type) %>%
                  filter(WordOrder == "SV") %>% 
                  summarize(M_V3 = mean(prop)*100,
                           SD_V3 = sd(prop)*100) %>% 
                  ggplot(aes(x = Type, y = M_V3, 
                             group = InformantGroup, color = InformantGroup)) 

# Add geoms:
spwr_objq_plot <- spwr_objq_plot +
  geom_line() +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin=M_V3-SD_V3, ymax=M_V3+SD_V3), width=.2,
                position=position_dodge(0.05))

# Tweak axes:
spwr_objq_plot <- spwr_objq_plot +
  scale_y_continuous(limits = c(-10,80), 
                    breaks = c(0,20, 40, 60, 80)) +
  scale_x_discrete(label = c("written", "spoken")) +
  scale_color_manual(label = c("group C (high school)", "group D (university)"), values = c("#00AFBB", "#FC4E07"))

# Tweak cosmetics:
spwr_objq_plot <- spwr_objq_plot +
  labs(x="Elicitation mode", y="Percentage V3 in non-subject wh-questions") +
  theme_minimal() +
  ggtitle('a') +
  theme(plot.title = element_text(face = "bold")) +
  theme(legend.position="top", legend.title = element_blank())

# Create graph for V2- and V3-adverbs (Figure 4b):
# Filter only V3-responses and setup plot
spwr_adv_plot <- spwr_adv3 %>% group_by(InformantGroup, Type) %>%
                  filter(WordOrder == "AV") %>% 
                  summarize(M_V3 = mean(prop)*100,
                           SD_V3 = sd(prop)*100) %>% 
                  ggplot(aes(x = Type, y = M_V3, group = InformantGroup, 
                             color = InformantGroup)) 

# Add geoms:
spwr_adv_plot <- spwr_adv_plot +
  geom_line() +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin=M_V3-SD_V3, ymax=M_V3+SD_V3), width=.2,
                position=position_dodge(0.05))

# Tweak axes:
spwr_adv_plot <- spwr_adv_plot +
  scale_y_continuous(limits = c(-10,80), 
                    breaks = c(0,20, 40, 60, 80)) +
  scale_x_discrete(label = c("written", "spoken")) +
  scale_color_manual(label = c("group C (high school)", "group D (university)"), values = c("#00AFBB", "#FC4E07"))

# Tweak cosmetics:
spwr_adv_plot <- spwr_adv_plot +
  labs(x="Elicitation mode", y="Percentage V3 with preverbal adverbs") +
  theme_minimal() +
  ggtitle('b') +
  theme(plot.title = element_text(face = "bold")) +
  theme(legend.position="top", legend.title = element_blank())

spwr_objq_plot + spwr_adv_plot
