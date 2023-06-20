analyzer <- read.xlsx("./../All_Analyzer.xlsx",sheet="all2")
analyzer<-subset(analyzer,ID!="4") #only yes_image

analyzer <- analyzer %>%
  gather(Dif_Like, Dif_Interest, Dif_Concentration, Dif_Calmness, Dif_Stress, key="colum", value="value")
analyzer$volume <- as.factor(analyzer$volume)

for(i in 1:5){
  c <-switch(i,
             "1" = {
               c <- "Dif_Like"
             },
             "2" = {
               c <- "Dif_Interest"
             },
             "3" = {
               c <- "Dif_Concentration"
             },
             "4" = {
               c <- "Dif_Calmness"
             },
             "5" = {
               c <- "Dif_Stress"
             },
             )
  analyzer_c<- filter(analyzer, colum == c)
  Analyzer_group_Dif_c_df <-
    ggplot() +
    geom_boxplot(aes(x = analyzer_c$volume, y = analyzer_c$value, fill = analyzer_c$group)) +
    labs(x = "volume", y = c )+
    coord_cartesian(ylim=c(-20,20))
  plot(Analyzer_group_Dif_c_df)
  Analyzer_Dif_c_df <-
    ggplot() +
    geom_boxplot(aes(x = analyzer_c$volume, y = analyzer_c$value)) +
    labs(x = "volume", y = c)+
    coord_cartesian(ylim=c(-20,20))
  plot(Analyzer_Dif_c_df)
}

for (i in 1:1) {
  df <- filter(analyzer, analyzer$ID == i)
  post_pre_Dif_Like <- c(0,0,0)
 
   for (j in 1:3){
    pre_Dif_Like <- df[j,5]
    post_Dif_Like <-df[j+3,5]
    post_pre_Dif_Like[j] <- post_Dif_Like - pre_Dif_Like
  }
  post_pre_Dif_Like_df <- data.frame(ID = rep(1,3), group = df[1,2], volume = c("1","2","3"), colum = (rep("post_pre_Dif_Like",3)), value = (post_pre_Dif_Like[]))
  
  post_pre_Dif_Interest <- c(0,0,0)
  for (j in 1:3){
    pre_Dif_Interest <- df[j+6*1,5]
    post_Dif_Interest <-df[j+3+6*1,5]
    post_pre_Dif_Interest[j] <- post_Dif_Interest - pre_Dif_Interest
  }
  post_pre_Dif_Interest_df <- data.frame(ID = rep(1,3), group = df[1,2], volume = c("1","2","3"), colum = (rep("post_pre_Dif_Interest",3)), value = (post_pre_Dif_Interest[]))

  post_pre_Dif_Concentration <- c(0,0,0)
  for (j in 1:3){
    pre_Dif_Concentration <- df[j+6*2,5]
    post_Dif_Concentration <-df[j+3+6*2,5]
    post_pre_Dif_Concentration[j] <- post_Dif_Concentration - pre_Dif_Concentration
  }
  post_pre_Dif_Concentration_df <- data.frame(ID = rep(1,3), group = df[1,2], volume = c("1","2","3"), colum = (rep("post_pre_Dif_Concentration",3)), value = (post_pre_Dif_Concentration[]))  
  
  post_pre_Dif_Calmness <- c(0,0,0)
  for (j in 1:3){
    pre_Dif_Calmness <- df[j+6*3,5]
    post_Dif_Calmness <-df[j+3+6*3,5]
    post_pre_Dif_Calmness[j] <- post_Dif_Calmness - pre_Dif_Calmness
  }
  post_pre_Dif_Calmness_df <- data.frame(ID = rep(1,3), group = df[1,2], volume = c("1","2","3"), colum = (rep("post_pre_Dif_Calmness",3)), value = (post_pre_Dif_Calmness[]))
  
  post_pre_Dif_Stress <- c(0,0,0)
  for (j in 1:3){
    pre_Dif_Stress <- df[j+6*4,5]
    post_Dif_Stress <-df[j+3+6*4,5]
    post_pre_Dif_Stress[j] <- post_Dif_Stress - pre_Dif_Stress
  }
  post_pre_Dif_Stress_df <- data.frame(ID = rep(1,3), group = df[1,2], volume = c("1","2","3"), colum = (rep("post_pre_Dif_Stress",3)), value = (post_pre_Dif_Stress[]))
  
  post_pre_i <- bind_rows(post_pre_Dif_Like_df, post_pre_Dif_Interest_df, post_pre_Dif_Concentration_df, post_pre_Dif_Calmness_df, post_pre_Dif_Stress_df)
  post_pre <- post_pre_i
  
  }
for (i in 2:100) {
  df <- filter(analyzer, analyzer$ID == i)
  post_pre_Dif_Like <- c(0,0,0)
  
  for (j in 1:3){
    pre_Dif_Like <- df[j,5]
    post_Dif_Like <-df[j+3,5]
    post_pre_Dif_Like[j] <- post_Dif_Like - pre_Dif_Like
  }
  post_pre_Dif_Like_df <- data.frame(ID = rep(i,3), group = df[1,2], volume = c("1","2","3"), colum = (rep("post_pre_Dif_Like",3)), value = (post_pre_Dif_Like[]))
  
  post_pre_Dif_Interest <- c(0,0,0)
  for (j in 1:3){
    pre_Dif_Interest <- df[j+6*1,5]
    post_Dif_Interest <-df[j+3+6*1,5]
    post_pre_Dif_Interest[j] <- post_Dif_Interest - pre_Dif_Interest
  }
  post_pre_Dif_Interest_df <- data.frame(ID = rep(i,3), group = df[1,2], volume = c("1","2","3"), colum = (rep("post_pre_Dif_Interest",3)), value = (post_pre_Dif_Interest[]))
  
  post_pre_Dif_Concentration <- c(0,0,0)
  for (j in 1:3){
    pre_Dif_Concentration <- df[j+6*2,5]
    post_Dif_Concentration <-df[j+3+6*2,5]
    post_pre_Dif_Concentration[j] <- post_Dif_Concentration - pre_Dif_Concentration
  }
  post_pre_Dif_Concentration_df <- data.frame(ID = rep(i,3), group = df[1,2], volume = c("1","2","3"), colum = (rep("post_pre_Dif_Concentration",3)), value = (post_pre_Dif_Concentration[]))  
  
  post_pre_Dif_Calmness <- c(0,0,0)
  for (j in 1:3){
    pre_Dif_Calmness <- df[j+6*1,5]
    post_Dif_Calmness <-df[j+3+6*1,5]
    post_pre_Dif_Calmness[j] <- post_Dif_Calmness - pre_Dif_Calmness
  }
  post_pre_Dif_Calmness_df <- data.frame(ID = rep(i,3), group = df[1,2], volume = c("1","2","3"), colum = (rep("post_pre_Dif_Calmness",3)), value = (post_pre_Dif_Calmness[]))
  
  post_pre_Dif_Stress <- c(0,0,0)
  for (j in 1:3){
    pre_Dif_Stress <- df[j+6*1,5]
    post_Dif_Stress <-df[j+3+6*1,5]
    post_pre_Dif_Stress[j] <- post_Dif_Stress - pre_Dif_Stress
  }
  post_pre_Dif_Stress_df <- data.frame(ID = rep(i,3), group = df[1,2], volume = c("1","2","3"), colum = (rep("post_pre_Dif_Stress",3)), value = (post_pre_Dif_Stress[]))
  
  post_pre_i <- bind_rows(post_pre_Dif_Like_df, post_pre_Dif_Interest_df, post_pre_Dif_Concentration_df, post_pre_Dif_Calmness_df, post_pre_Dif_Stress_df)
  post_pre <- bind_rows(post_pre, post_pre_i)
  
}
post_pre <- na.omit(post_pre)

for(i in 1:5){
  c <-switch(i,
             "1" = {
               c <- "post_pre_Dif_Like"
             },
             "2" = {
               c <- "post_pre_Dif_Interest"
             },
             "3" = {
               c <- "post_pre_Dif_Concentraiton"
             },
             "4" = {
               c <- "post_pre_Dif_Calmness"
             },
             "5" = {
               c <- "post_pre_Dif_Stress"
             },
  )
  Post_Pre_Dif_c<- filter(post_pre, colum == c)
  Post_Pre_group_Dif_c_df <-
    ggplot() +
    geom_boxplot(aes(x = Post_Pre_Dif_c$volume, y = Post_Pre_Dif_c$value, fill = Post_Pre_Dif_c$group)) +
    labs(x = "volume", y = c)+
    coord_cartesian(ylim=c(-20,20))
  plot(Post_Pre_group_Dif_c_df)
  Post_Pre_Dif_c_df <-
    ggplot() +
    geom_boxplot(aes(x = Post_Pre_Dif_c$volume, y = Post_Pre_Dif_c$value)) +
    labs(x = "volume", y = c)+
    coord_cartesian(ylim=c(-20,20))
  plot(Post_Pre_Dif_c_df)
}

post_pre_1 <-filter(post_pre, colum == "post_pre_Dif_Stress", volume=="1")
post_pre_1_A <- filter(post_pre_1, group=="A")
post_pre_1_B <- filter(post_pre_1, group=="B")
post_pre_1_C <- filter(post_pre_1, group=="C")
AC_stress_1 <- mean(post_pre_1_A$value) - mean(post_pre_1_C$value)
BC_stress_1 <- mean(post_pre_1_B$value) - mean(post_pre_1_C$value)
post_pre_2 <-filter(post_pre, colum == "post_pre_Dif_Stress", volume=="2")
post_pre_2_A <- filter(post_pre_2, group=="A")
post_pre_2_B <- filter(post_pre_2, group=="B")
post_pre_2_C <- filter(post_pre_2, group=="C")
AC_stress_2 <- mean(post_pre_2_A$value) - mean(post_pre_2_C$value)
BC_stress_2 <- mean(post_pre_2_B$value) - mean(post_pre_2_C$value)
post_pre_3 <-filter(post_pre, colum == "post_pre_Dif_Stress", volume=="3")
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


