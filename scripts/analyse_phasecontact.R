#boxplot

plotcontact <-
  dfappui %>% ggplot(aes(x = instant , y = phase_de_contact)) +
  theme_bw() + theme(
    plot.title = element_text(hjust = 0.5 , size = 12 , face = "bold") ,
    axis.text = element_text(size = 7) ,
    axis.title = element_text(size = 8 , face = "bold"),
    legend.position = "right" ,
    legend.title = element_text(size = 8 , face = "bold") ,
    legend.text = element_text(size = 6) ,
    legend.background = element_rect(color = "black" , size = 0.1)
  ) +
  geom_boxplot(aes(color = condition), size = 0.7) +
  geom_point(
    aes(color = condition , shape = condition),
    size = 0.5 ,
    position = position_dodge2(width = 0.75 ,
                               preserve = "single")
  ) +
  stat_summary(
    fun.y = mean,
    aes(group = condition) ,
    color = "black" ,
    shape = 17 ,
    position = position_dodge2(width = 0.75 ,
                               preserve = "single")
  ) +
  labs(title = "Phase_contact_PRE_POST_POST48")

plotcontact

dfappui %>% identify_outliers(phase_de_contact)

# plot indiv

plotindiv3 <- ggplot(dfappui, aes( x = instant , y = phase_de_contact )) +
  theme_bw() + theme(
    plot.title = element_text(hjust = 0.5 , size = 12 , face = "bold") ,
    axis.text = element_text(size = 7) ,
    axis.title = element_text(size = 8 , face = "bold") ,
    strip.background = element_rect(color = "black" , fill = "#373737")
    ,
    strip.text = element_text(
      color = "white" ,
      face = "bold" ,
      size = 8
    ) ,
    legend.position = "right" ,
    legend.title = element_text(size = 8 , face = "bold") ,
    legend.text = element_text(size = 6) ,
    legend.background = element_rect(color = "black" , size = 0.1)
  ) +
  geom_line(
    aes(
      x = instant ,
      group = sujet ,
      color = as.factor(sujet)
    ) ,
    size = 0.7 ,
    position = "identity" ,
    linetype = "dashed"
  ) +
  geom_point(
    aes(x = instant , group = sujet),
    shape = 21,
    colour = "black",
    size = 2,
    position = "identity"
  ) +
  geom_boxplot( outlier.shape = NA, coef = 0 ,
                aes(x = instant , y = phase_de_contact) ,
                width = .35,
                fill = "white" , alpha = 0.3
  )  +
  scale_color_manual(
    values = c(
      "purple" ,
      "#0416f5" ,
      "#b00000" ,
      "#19a3e8" ,
      "#fd4c4c" ,
      "#E7B800" ,
      "#5ef11a" ,
      "#c58ede" ,
      "#3e020b" ,
      "#febd02" ,
      "#16161e" ,
      "#24844b" ,
      "#f604fd" ,
      "#439bab" ,
      "#c5c896" ,
      "#6e711d" ,
      "#109c02" #,
      #"#b71385"
    )) +
  labs(color = "sujet") +
  stat_summary(
    geom = "errorbar" ,
    fun.data = mean_sd ,
    colour = "black" ,
    size = 1 ,
    width = 0.2
  ) +
  stat_summary(
    fun = mean,
    shape = 17 ,
    size = 1 ,
    position = "identity",
    color = "#ff0000"
  ) +
  labs(title = "pahse_de_contact_indiv_PRE_POST_POST48") +
  facet_wrap(vars(condition) , scales = "free_y")

plotindiv3

# normalité + plot

normalityplot2 <- dfappui %>%  ggqqplot("phase_de_contact" , ggtheme = theme_bw()) +
  facet_grid(instant ~ condition)

normalityplot2

dfappui %>% ggdensity("phase_de_contact" , fill = "lightgray") +
  facet_grid(instant ~ condition)

dfappui %>% group_by(condition , instant) %>% shapiro_test(phase_de_contact)

# comparaison des moyennes anova

dfappui %>% anova_test(dv = phase_de_contact , wid = sujet , within = c(condition , instant))


dfappui %>%  group_by(instant) %>% pairwise_t_test(phase_de_contact ~ condition ,
                                                   paired = TRUE ,
                                                   p.adjust.method = "bonferroni")
