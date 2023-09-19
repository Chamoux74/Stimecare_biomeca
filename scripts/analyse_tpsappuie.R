
#toujours charger la data avec analyse tpsvol

dfappui <- dfbiomec %>% filter(sujet != "Maurin")

#plot + stats descriptive

plottpsap <-
  ggplot(data = dfappui , aes(x = instant , y = temps_appui)) +
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
                               preserve = "single"
    )
  ) +
  labs(title = "temps_appui_PRE_POST_POST48")

plottpsap

dfappui %>%  identify_outliers(temps_appui)

# plot indiv

plotindiv2 <- ggplot(dfappui, aes( x = instant , y = temps_appui )) +
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
                aes(x = instant , y = temps_appui) ,
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
  labs(title = "temps_de_vol_indiv_PRE_POST_POST48") +
  facet_wrap(vars(condition) , scales = "free_y")

plotindiv2

# analyse normalité + plot

normalityplot1 <- dfappui %>%  ggqqplot("temps_appui" , ggtheme = theme_bw()) +
  facet_grid(instant ~ condition)

normalityplot1

dfappui %>% ggdensity("temps_appui" , fill = "lightgray") +
  facet_grid(instant ~ condition)

dfappui %>% group_by(condition , instant) %>% shapiro_test(temps_appui)

# comparaison des moyennes anova

dfappui %>% anova_test(dv = temps_appui , wid = sujet , within = c(condition , instant))

dfappui %>% group_by(condition) %>% pairwise_t_test(temps_appui ~ instant ,
                                                    paired = TRUE ,
                                                    p.adjust.method = "bonferroni")

dfappui %>%  group_by(instant) %>% pairwise_t_test(temps_appui ~ condition ,
                                                   paired = TRUE ,
                                                   p.adjust.method = "bonferroni")

#analyse diff pourcentage post post48

postplacebo <-
  dfappui %>%  filter(condition == "Placebo") %>% filter(instant == "Post") %>% select(temps_appui)

postpatch <-
  dfappui %>%  filter(condition == "Patch") %>% filter(instant == "Post") %>% select(temps_appui)

post48placebo <-
  dfappui %>%  filter(condition == "Placebo") %>% filter(instant == "Post48h") %>% select(temps_appui)

post48patch <-
  dfappui %>%  filter(condition == "Patch") %>% filter(instant == "Post48h") %>% select(temps_appui)

dfpostpost48 <- cbind(postplacebo, postpatch , post48placebo , post48patch)
colnames(dfpostpost48) <- c("Tempsap_POST_PB", "Tempsap_POST_P" , "Tempsap_POST48_PB", "Tempsap_POST48_P")

dfpostpost48pb <- mutate(dfpostpost48 , POST_POST48_PB = (Tempsap_POST_PB - Tempsap_POST48_PB)/Tempsap_POST_PB*100) %>% select(POST_POST48_PB)
dfpostpost48p <- mutate(dfpostpost48 , POST_POST48_P = (Tempsap_POST_P - Tempsap_POST48_P)/Tempsap_POST_P*100) %>% select(POST_POST48_P)

placebo <- "placebo"
patch <- "patch"

dfpostpost48pb <- cbind(dfpostpost48pb, placebo)
dfpostpost48p <- cbind(dfpostpost48p, patch)

colnames(dfpostpost48p) <- c("tempsap_POST_POST48" , "condition")
colnames(dfpostpost48pb) <- c("tempsap_POST_POST48" , "condition")

dfpostpost48 <- rbind(dfpostpost48p , dfpostpost48pb)

ttestpourc <- dfpostpost48 %>% t_test(tempsap_POST_POST48 ~ condition , paired = T)

