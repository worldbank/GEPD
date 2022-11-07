library(waffle)

# 2nd visit

absence_plt <- teacher_roster_anon %>%
  mutate(absence_2nd_cat = case_when(
    m2sbq6_efft==6 ~ 'Absent from School',
    m2sbq6_efft==5 ~ 'Absent from Classroom',
    TRUE ~ 'Present'
  )
  ) %>% filter(!is.na(urban_rural))

absence_plt <- absence_plt %>%
  mutate(absence_2nd_fact=factor(absence_2nd_cat, levels=c('Present','Absent from School', 'Absent from Classroom'))) %>%
  arrange(absence_2nd_fact)

absence_perc <- absence_plt %>%
  ungroup() %>% 
  count(urban_rural, absence_2nd_fact) %>% 
  arrange(desc(n)) %>%
  group_by(absence_2nd_fact) %>% 
  mutate(cumulative = sum(n)) %>% 
  ungroup() %>% 
  mutate(Group = factor(absence_2nd_fact, levels = c("Present", "Absent from School", "Absent from Classroom")),
         midpoint = cumulative - n / 2,
         label = paste0(Group, " (", round(cumulative / sum(n) * 100, 1), "%)"))


ggplot(absence_perc, aes(fill=label, values=n)) +
  facet_wrap(~urban_rural, nrow = 1, strip.position = "bottom") +
  geom_waffle(
    n_rows = 32, color = "white", flip = TRUE
  ) +
  scale_x_discrete() +
  scale_y_continuous(labels = function(x) x * 32, # make this multiplyer the same as n_rows
                     expand = c(0,0)) +
  ggthemes::scale_fill_tableau(name=NULL) +
  coord_equal() +
  theme(strip.text.x = element_text(hjust = 0.5),
        legend.position = 'bottom') +
  theme(panel.spacing.x = unit(0, "npc")) +
  theme(strip.text.x = element_text(hjust = 0.5),
        legend.position = 'bottom') +
  ggtitle("Teacher Presence during unannounced visit") +
  labs(caption='One tile = One teacher',
       fill='Teacher Status') +
  theme_minimal(base_family = "Roboto Condensed") +
  theme(panel.grid = element_blank(), axis.ticks.y = element_line()) +
  guides(fill = guide_legend(reverse = F))

