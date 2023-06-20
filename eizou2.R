eizou2 <- read.xlsx("./../interview.xlsm",sheet="eizou4")
eizou2<-subset(eizou2,ID!="4") #only yes_image

eizou2 <- eizou2 %>%
  gather(Annoyance, Like, Stress, Forgive, Level,Time,acceptance, key="colum", value="value")
eizou2$volume <- as.factor(eizou2$volume)

for(i in 1:7){
  c <-switch(i,
             "1" = {
               c <- "Annoyance"
             },
             "2" = {
               c <-  "Like"
             },
             "3" = {
               c <- "Stress"
             },
             "4" = {
               c <-  "Forgive"
             },
             "5" = {
               c <- " Level"
             },
             "6" = {
               c <- "Time"
             },
             "7" = {
               c <- "acceptance"
             },
  )
  eizou2_c<- filter(eizou2, colum == c)
  Eizou2_group_Dif_c_df <-
    ggplot() +
    geom_boxplot(aes(x = eizou2_c$volume, y = eizou2_c$value, fill = eizou2_c$group)) +
    labs(x = "volume", y = c )+
    coord_cartesian(ylim=c(0,7))
  plot(Eizou2_group_Dif_c_df)
  Eizou2_Dif_c_df <-
    ggplot() +
    geom_boxplot(aes(x = eizou2_c$volume, y = eizou2_c$value)) +
    labs(x = "volume", y = c)+
    coord_cartesian(ylim=c(0,7))
  plot(Eizou2_Dif_c_df)
}

for (i in 1:1) {
  df <- filter(eizou2, eizou2$ID == i)
  post_pre_Annoyance <- c(0,0,0)
  
  for (j in 1:3){
    pre_Annoyance <- df[j,5]
    post_Annoyance <-df[j+3,5]
    post_pre_Annoyance[j] <- post_Annoyance - pre_Annoyance
  }
  post_pre_Annoyance_df <- data.frame(ID = rep(1,3), group = df[1,2], volume = c("1","2","3"), colum = (rep("post_pre_Annoyance",3)), value = (post_pre_Annoyance[]))
  
  post_pre_Like <- c(0,0,0)
  for (j in 1:3){
    pre_Like <- df[j+6*1,5]
    post_Like <-df[j+3+6*1,5]
    post_pre_Like[j] <- post_Like - pre_Like
  }
  post_pre_Like_df <- data.frame(ID = rep(1,3), group = df[1,2], volume = c("1","2","3"), colum = (rep("post_pre_Like",3)), value = (post_pre_Like[]))
  
  post_pre_Stress <- c(0,0,0)
  for (j in 1:3){
    pre_Stress <- df[j+6*2,5]
    post_Stress <-df[j+3+6*2,5]
    post_pre_Stress[j] <- post_Stress - pre_Stress
  }
  post_pre_Stress_df <- data.frame(ID = rep(1,3), group = df[1,2], volume = c("1","2","3"), colum = (rep("post_pre_Stress",3)), value = (post_pre_Stress[]))  
  
  post_pre_Forgive <- c(0,0,0)
  for (j in 1:3){
    pre_Forgive <- df[j+6*3,5]
    post_Forgive <-df[j+3+6*3,5]
    post_pre_Forgive[j] <- post_Forgive - pre_Forgive
  }
  post_pre_Forgive_df <- data.frame(ID = rep(1,3), group = df[1,2], volume = c("1","2","3"), colum = (rep("post_pre_Forgive",3)), value = (post_pre_Forgive[]))
  
  post_pre_Level <- c(0,0,0)
  for (j in 1:3){
    pre_Level <- df[j+6*4,5]
    post_Level <-df[j+3+6*4,5]
    post_pre_Level[j] <- post_Level - pre_Level
  }
  post_pre_Level_df <- data.frame(ID = rep(1,3), group = df[1,2], volume = c("1","2","3"), colum = (rep("post_pre_Level",3)), value = (post_pre_Level[]))
  
  post_pre_Time<- c(0,0,0)
  for (j in 1:3){
    pre_Time <- df[j+6*5,5]
    post_Time <-df[j+3+6*5,5]
    post_pre_Time[j] <- post_Time - pre_Time
  }
  post_pre_Time_df <- data.frame(ID = rep(1,3), group = df[1,2], volume = c("1","2","3"), colum = (rep("post_pre_Time",3)), value = (post_pre_Time[]))
  
  post_pre_acceptance <- c(0,0,0)
  for (j in 1:3){
    pre_acceptance <- df[j+6*6,5]
    post_acceptance <-df[j+3+6*6,5]
    post_pre_acceptance[j] <- post_acceptance - pre_acceptance
  }
  post_pre_acceptance_df <- data.frame(ID = rep(1,3), group = df[1,2], volume = c("1","2","3"), colum = (rep("post_pre_acceptance",3)), value = (post_pre_acceptance[]))
  
  post_pre_i <- bind_rows(post_pre_Annoyance_df, post_pre_Like_df, post_pre_Stress_df, post_pre_Forgive_df, post_pre_Level_df, post_pre_Time_df, post_pre_acceptance_df)
  post_pre <- post_pre_i
  
}
for (i in 2:100) {
  df <- filter(eizou2, eizou2$ID == i)
  post_pre_Annoyance <- c(0,0,0)
  
  for (j in 1:3){
    pre_Annoyance <- df[j,5]
    post_Annoyance <-df[j+3,5]
    post_pre_Annoyance[j] <- post_Annoyance - pre_Annoyance
  }
  post_pre_Annoyance_df <- data.frame(ID = rep(i,3), group = df[1,2], volume = c("1","2","3"), colum = (rep("post_pre_Annoyance",3)), value = (post_pre_Annoyance[]))
  
  post_pre_Like <- c(0,0,0)
  for (j in 1:3){
    pre_Like <- df[j+6*1,5]
    post_Like <-df[j+3+6*1,5]
    post_pre_Like[j] <- post_Like - pre_Like
  }
  post_pre_Like_df <- data.frame(ID = rep(i,3), group = df[1,2], volume = c("1","2","3"), colum = (rep("post_pre_Like",3)), value = (post_pre_Like[]))
  
  post_pre_Stress <- c(0,0,0)
  for (j in 1:3){
    pre_Stress <- df[j+6*2,5]
    post_Stress <-df[j+3+6*2,5]
    post_pre_Stress[j] <- post_Stress - pre_Stress
  }
  post_pre_Stress_df <- data.frame(ID = rep(i,3), group = df[1,2], volume = c("1","2","3"), colum = (rep("post_pre_Stress",3)), value = (post_pre_Stress[]))  
  
  post_pre_Forgive <- c(0,0,0)
  for (j in 1:3){
    pre_Forgive <- df[j+6*3,5]
    post_Forgive <-df[j+3+6*3,5]
    post_pre_Forgive[j] <- post_Forgive - pre_Forgive
  }
  post_pre_Forgive_df <- data.frame(ID = rep(i,3), group = df[1,2], volume = c("1","2","3"), colum = (rep("post_pre_Forgive",3)), value = (post_pre_Forgive[]))
  
  post_pre_Level <- c(0,0,0)
  for (j in 1:3){
    pre_Level <- df[j+6*4,5]
    post_Level <-df[j+3+6*4,5]
    post_pre_Level[j] <- post_Level - pre_Level
  }
  post_pre_Level_df <- data.frame(ID = rep(i,3), group = df[1,2], volume = c("1","2","3"), colum = (rep("post_pre_Level",3)), value = (post_pre_Level[]))
  
  post_pre_Time<- c(0,0,0)
  for (j in 1:3){
    pre_Time <- df[j+6*5,5]
    post_Time <-df[j+3+6*5,5]
    post_pre_Time[j] <- post_Time - pre_Time
  }
  post_pre_Time_df <- data.frame(ID = rep(i,3), group = df[1,2], volume = c("1","2","3"), colum = (rep("post_pre_Time",3)), value = (post_pre_Time[]))
  
  post_pre_acceptance <- c(0,0,0)
  for (j in 1:3){
    pre_acceptance <- df[j+6*6,5]
    post_acceptance <-df[j+3+6*6,5]
    post_pre_acceptance[j] <- post_acceptance - pre_acceptance
  }
  post_pre_acceptance_df <- data.frame(ID = rep(i,3), group = df[1,2], volume = c("1","2","3"), colum = (rep("post_pre_acceptance",3)), value = (post_pre_acceptance[]))
  
  post_pre_i <- bind_rows(post_pre_Annoyance_df, post_pre_Like_df, post_pre_Stress_df, post_pre_Forgive_df, post_pre_Level_df, post_pre_Time_df, post_pre_acceptance_df)
  post_pre <- bind_rows(post_pre, post_pre_i)
  
}

for(i in 1:7){
  c <-switch(i,
             "1" = {
               c <- "post_pre_Annoyance"
             },
             "2" = {
               c <-  "post_pre_Like"
             },
             "3" = {
               c <- "post_pre_Stress"
             },
             "4" = {
               c <-  "post_pre_Forgive"
             },
             "5" = {
               c <- "post_pre_Level"
             },
             "6" = {
               c <- "post_pre_Time"
             },
             "7" = {
               c <- "post_pre_acceptance"
             },
  )
  Post_Pre_Dif_c<- filter(post_pre, colum == c)
  Post_Pre_group_Dif_c_df <-
    ggplot() +
    geom_boxplot(aes(x = Post_Pre_Dif_c$volume, y = Post_Pre_Dif_c$value, fill = Post_Pre_Dif_c$group)) +
    labs(x = "volume", y = c)+
    coord_cartesian(ylim=c(-7,7))
  plot(Post_Pre_group_Dif_c_df)
  Post_Pre_Dif_c_df <-
    ggplot() +
    geom_boxplot(aes(x = Post_Pre_Dif_c$volume, y = Post_Pre_Dif_c$value)) +
    labs(x = "volume", y = c)+
    coord_cartesian(ylim=c(-7,7))
  plot(Post_Pre_Dif_c_df)
}

post_pre_1 <-filter(post_pre, colum == "post_pre_Stress", volume=="1")
post_pre_1_A <- filter(post_pre_1, group=="A")
post_pre_1_B <- filter(post_pre_1, group=="B")
post_pre_1_C <- filter(post_pre_1, group=="C")
AC_stress_1 <- mean(post_pre_1_A$value) - mean(post_pre_1_C$value)
BC_stress_1 <- mean(post_pre_1_B$value) - mean(post_pre_1_C$value)
post_pre_2 <-filter(post_pre, colum == "post_pre_Stress", volume=="2")
post_pre_2_A <- filter(post_pre_2, group=="A")
post_pre_2_B <- filter(post_pre_2, group=="B")
post_pre_2_C <- filter(post_pre_2, group=="C")
AC_stress_2 <- mean(post_pre_2_A$value) - mean(post_pre_2_C$value)
BC_stress_2 <- mean(post_pre_2_B$value) - mean(post_pre_2_C$value)
post_pre_3 <-filter(post_pre, colum == "post_pre_Stress", volume=="3")
post_pre_3_A <- filter(post_pre_3, group=="A")
post_pre_3_B <- filter(post_pre_3, group=="B")
post_pre_3_C <- filter(post_pre_3, group=="C")
AC_stress_3 <- mean(post_pre_3_A$value) - mean(post_pre_3_C$value)
BC_stress_3 <- mean(post_pre_3_B$value) - mean(post_pre_3_C$value)

AC_stress <- c(AC_stress_1,AC_stress_2,AC_stress_3)
BC_stress <- c(BC_stress_1,BC_stress_2,BC_stress_3)
AC_BC <-data.frame(volume=c(1,2,3,1,2,3),group=c(rep("AC",3),rep("BC",3)), value=c(AC_stress,BC_stress))

AC_BC_df <-
  ggplot()+
  geom_point(aes(x = AC_BC$volume, y= AC_BC$value, color=AC_BC$group))+
  geom_line(aes(x = AC_BC$volume, y= AC_BC$value, color=AC_BC$group))+
  labs(x = "音量", y = "Cとの平均差別")
plot(AC_BC_df)


