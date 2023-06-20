eizou <- read.xlsx("./../interview.xlsx",sheet="eizou4")
eizou <-subset(eizou,ID!="4") #only yes_image

Eizou_df <-
  ggplot() +
  geom_boxplot(aes(x = eizou$colum, y = eizou$value, fill = eizou$colum)) +
  labs(x = "group", y = "Annoyance_yes_image1") +
  coord_cartesian(ylim = c(1,7))



eizou_annoyace <- eizou[grep("annoyance", eizou$colum),]
Eizou_annoyance_df <-
  ggplot() +
  geom_boxplot(aes(x = eizou_annoyace$colum, y = eizou_annoyace$value, fill = eizou_annoyace$group)) +
  labs(x = "group", y = "Value") +
  coord_cartesian(ylim = c(1,7))
plot(Eizou_annoyance_df)

eizou_stress <- eizou[grep("stress", eizou$colum),]
Eizou_stress_df <-
  ggplot() +
  geom_boxplot(aes(x = eizou_stress$colum, y = eizou_stress$value, fill = eizou_stress$group)) +
  labs(x = "group", y = "Value") +
  coord_cartesian(ylim = c(1,7))
plot(Eizou_stress_df)

eizou_like <- eizou[grep("like", eizou$colum),]
Eizou_like_df <-
  ggplot() +
  geom_boxplot(aes(x = eizou_like$colum, y = eizou_like$value, fill = eizou_like$group)) +
  stat_boxplot(coef = Inf)+
  labs(x = "group", y = "Value") +
  coord_cartesian(ylim = c(1,7))
plot(Eizou_like_df)

eizou_forgive <- eizou[grep("forgive", eizou$colum),]
Eizou_forgive_df <-
  ggplot() +
  geom_boxplot(aes(x = eizou_forgive$colum, y = eizou_forgive$value, fill = eizou_forgive$group)) +
  labs(x = "group", y = "Value") +
  coord_cartesian(ylim = c(1,7))
plot(Eizou_forgive_df)

eizou_level <- eizou[grep("level", eizou$colum),]
Eizou_level_df <-
  ggplot() +
  geom_boxplot(aes(x = eizou_level$colum, y = eizou_level$value, fill = eizou_level$group)) +
  labs(x = "group", y = "Value") +
  coord_cartesian(ylim = c(1,10))
plot(Eizou_level_df)

eizou_annoyace <- eizou[grep("annoyance", eizou$colum),]
Eizou_annoyance_df <-
  ggplot() +
  geom_boxplot(aes(x = eizou_annoyace$colum, y = eizou_annoyace$value)) +
  labs(x = "group", y = "Value") +
  coord_cartesian(ylim = c(1,7))
plot(Eizou_annoyance_df)

eizou_stress <- eizou[grep("stress", eizou$colum),]
Eizou_stress_df <-
  ggplot() +
  geom_boxplot(aes(x = eizou_stress$colum, y = eizou_stress$value)) +
  labs(x = "group", y = "Value") +
  coord_cartesian(ylim = c(1,7))
plot(Eizou_stress_df)

eizou_like <- eizou[grep("like", eizou$colum),]
Eizou_like_df <-
  ggplot() +
  geom_boxplot(aes(x = eizou_like$colum, y = eizou_like$value)) +
  stat_boxplot(geom= "errorbar", coef = Inf, width = 0.2)+
  stat_boxplot(coef = Inf)+
  labs(x = "group", y = "Value") +
  coord_cartesian(ylim = c(1,7))
plot(Eizou_like_df)

eizou_forgive <- eizou[grep("forgive", eizou$colum),]
Eizou_forgive_df <-
  ggplot() +
  geom_boxplot(aes(x = eizou_forgive$colum, y = eizou_forgive$value)) +
  labs(x = "group", y = "Value") +
  coord_cartesian(ylim = c(1,7))
plot(Eizou_forgive_df)

eizou_level <- eizou[grep("level", eizou$colum),]
Eizou_level_df <-
  ggplot() +
  geom_boxplot(aes(x = eizou_level$colum, y = eizou_level$value)) +
  labs(x = "group", y = "Value") +
  coord_cartesian(ylim = c(1,10))
plot(Eizou_level_df)

