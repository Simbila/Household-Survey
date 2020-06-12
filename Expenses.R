 # Household Survey data analysis - EMPLOYMENT STATUS
    # Harmonic Biosphere Company Limited 
    # June, 2020
    
    # Set working directory --------------------------------------------------------
    setwd("~")
    dir.create("RAP", showWarnings = FALSE)
    setwd("~/RAP")
    
    # Load/install required packageslibralies ---------------------------------------
    library(tidyverse)
    library(scales)
    library(ggrepel)
    
    
    options(scipen = 999)
    
    
    df<-read.csv("Household_Survey.csv",check.names = TRUE,stringsAsFactors = FALSE)
    
    
    # Renaming varibales
    df<-df %>% 
      rename(Respondent_Name = Respondent_.Name,
             HoH_N = HoH_.N, HM1_N = HM1_.N, HM7_G = HM6_G.1, HM8_G = HM_G)
    
    
    # EXPENSES  --------------------
    df_Expense <- df %>% 
      select(Village, Expenses1,Expenses_other1, Expenses_Amount..TZS.1,Expenses2, Expenses_other2,Expenses_Amount..TZS.2,
             Expenses3,Expenses_other3,Expenses_Amount..TZS.3,Expenses4,Expenses_other4,Expenses_Amount..TZS.4) %>% 
      mutate(Expenses1 = ifelse(Expenses1 == " Other" , Expenses_other1, Expenses1),
             Expenses2 = ifelse(Expenses2 == " Other", Expenses_other2, Expenses2),
             Expenses3 = ifelse(Expenses3 == " Other", Expenses_other3, Expenses3),
             Expenses4 = ifelse(Expenses4 == " Other", Expenses_other4, Expenses4)) %>% 
      select(-contains("other")) %>% 
      view()
    
    df_Kisewe <- df_Expense %>% 
      filter(Village =="Kisewe") %>% 
      view()
    
    df_KS1 <- df_Kisewe %>% 
      select(Expenses1, Expenses_Amount..TZS.1) %>% 
      group_by(Expenses1) %>%
      summarise(ExpensSum1 = sum(Expenses_Amount..TZS.1)) %>% 
      arrange(Expenses1) %>% 
      view()
    
    df_KS2 <- df_Kisewe %>% 
      select(Expenses2, Expenses_Amount..TZS.2) %>% 
      group_by(Expenses2) %>%
      summarise(ExpensSum2 = sum(Expenses_Amount..TZS.2)) %>% 
      arrange(Expenses2) %>% 
      view()
    
    df_KS3 <- df_Kisewe %>% 
      select(Expenses3, Expenses_Amount..TZS.3) %>% 
      group_by(Expenses3) %>%
      summarise(ExpensSum3 = sum(Expenses_Amount..TZS.3)) %>% 
      arrange(Expenses3) %>% 
      view()
    
    df_KS4 <- df_Kisewe %>% 
      select(Expenses4, Expenses_Amount..TZS.4) %>% 
      group_by(Expenses4) %>%
      summarise(ExpensSum4 = sum(Expenses_Amount..TZS.4)) %>% 
      arrange(Expenses4) %>% 
      view()
    
    # Kisewe Join dataframe Expenditure
    df_KSCombine <- df_KS1 %>% 
      mutate(Expenses2 = df_KS2$Expenses2[match(Expenses1, df_KS2$Expenses2)],
             Expenses3 = df_KS3$Expenses3[match(Expenses1, df_KS3$Expenses3)],
             Expenses4 = df_KS4$Expenses4[match(Expenses1, df_KS4$Expenses4)],
             ExpensSum2 = df_KS2$ExpensSum2[match(Expenses1, df_KS2$Expenses2)],
             ExpensSum3 = df_KS3$ExpensSum3[match(Expenses1, df_KS3$Expenses3)],
             ExpensSum4 = df_KS4$ExpensSum4[match(Expenses1, df_KS4$Expenses4)]) %>% 
      select(Expenses1, 3:5, ExpensSum1, 6:8) %>% 
      mutate(Expenses2 = ifelse(is.na(Expenses2), Expenses1, Expenses2), 
             Expenses4 = ifelse(is.na(Expenses4),Expenses1, Expenses4)) %>% 
      replace(is.na(.), 0)%>% 
      mutate(Total = rowSums(.[5:8])) %>% 
      mutate(Percentages = round(Total/sum(Total)*100, digits = 2)) %>% 
      view()
    
    # Kisewe Expense
    df_KSCombine %>% 
      mutate(Expenses1 = fct_reorder(Expenses1, -Percentages)) %>% 
      ggplot(aes(x = "", y = Percentages, fill = Expenses1))+
      geom_bar( stat = "identity")+
      coord_polar("y", start = 0)+
      geom_text(aes(label = ifelse(Percentages > 1, str_c(Percentages, "%"), "")), position = position_stack(vjust = 0.5))+
      geom_label_repel(aes(label = ifelse(Percentages < 1, str_c(Percentages, "%"), "")), size = 5, hjust = 0.5, nudge_x = 0.55, show.legend = F)+
      labs(title = "Households Expenditure variation - Kisewe Village", caption = "HBCL RAP 2020", fill = "Expenses")+
      theme_void()+
      theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 15, vjust = 0.5))+
      theme(plot.caption = element_text(hjust = 0.5, vjust = 25, size = 5))+
      theme(legend.title = element_text(face = "bold"))
    
    # Makanga -----------------------------------------------
    df_Makanga <- df_Expense %>% 
      filter(Village =="Makanga") %>% 
      view()
    
    df_MK1 <- df_Makanga %>% 
      select(Expenses1, Expenses_Amount..TZS.1) %>% 
      group_by(Expenses1) %>%
      summarise(ExpensSum1 = sum(Expenses_Amount..TZS.1)) %>% 
      arrange(Expenses1) %>% 
      view()
    
    df_MK2 <- df_Makanga %>% 
      select(Expenses2, Expenses_Amount..TZS.2) %>% 
      group_by(Expenses2) %>%
      summarise(ExpensSum2 = sum(Expenses_Amount..TZS.2)) %>% 
      arrange(Expenses2) %>% 
      view()
    
    df_MK3 <- df_Makanga %>% 
      select(Expenses3, Expenses_Amount..TZS.3) %>% 
      group_by(Expenses3) %>%
      summarise(ExpensSum3 = sum(Expenses_Amount..TZS.3)) %>% 
      arrange(Expenses3) %>% 
      view()
    
    df_MK4 <- df_Makanga %>% 
      select(Expenses4, Expenses_Amount..TZS.4) %>% 
      group_by(Expenses4) %>%
      summarise(ExpensSum4 = sum(Expenses_Amount..TZS.4)) %>% 
      arrange(Expenses4) %>% 
      view()
    
    # Makanga Join dataframe Expenditure
    df_MKCombine <- df_MK2 %>% 
      mutate(Expenses1 = df_MK1$Expenses1[match(Expenses2, df_MK1$Expenses1)],
             Expenses3 = df_MK3$Expenses3[match(Expenses2, df_MK3$Expenses3)],
             Expenses4 = df_MK4$Expenses4[match(Expenses2, df_MK4$Expenses4)],
             ExpensSum1 = df_MK1$ExpensSum1[match(Expenses2, df_MK1$Expenses1)],
             ExpensSum3 = df_MK3$ExpensSum3[match(Expenses2, df_MK3$Expenses3)],
             ExpensSum4 = df_MK4$ExpensSum4[match(Expenses2, df_MK4$Expenses4)]) %>% 
      select(Expenses1, Expenses2, 4:5, ExpensSum1, ExpensSum2, 7:8) %>% 
      mutate(Expenses1 = ifelse(is.na(Expenses1), Expenses2, Expenses1), 
             Expenses4 = ifelse(is.na(Expenses4),Expenses2, Expenses4),
             Expenses3 = ifelse(is.na(Expenses3), Expenses2, Expenses3)) %>% 
      replace(is.na(.), 0)%>% 
      mutate(Total = rowSums(.[5:8])) %>% 
      mutate(Percentages = round(Total/sum(Total)*100, digits = 2)) %>% 
      view()
    
    # Makanga Expense
    df_MKCombine %>% 
      mutate(Expenses1 = fct_reorder(Expenses1, -Percentages)) %>% 
      ggplot(aes(x = "", y = Percentages, fill = Expenses1))+
      geom_bar( stat = "identity")+
      coord_polar("y", start = 0)+
      geom_text(aes(label = ifelse(Percentages > 3, str_c(Percentages, "%"), "")), position = position_stack(vjust = 0.5))+
      geom_text(aes(label = ifelse(Percentages == 1.71, str_c(Percentages, "%"), "")), position = position_stack(vjust = 0.5), angle = 90, hjust = -2, vjust=1)+
      geom_label_repel(aes(label = ifelse(Percentages < 1, str_c(Percentages, "%"), "")), size = 5, hjust = 0.5, nudge_x = 0.55, show.legend = F)+
      labs(title = "Households Expenditure variation - Makanga Village", caption = "HBCL RAP 2020", fill = "Expenses")+
      theme_void()+
      theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 15, vjust = 0.5))+
      theme(plot.caption = element_text(hjust = 0.5, vjust = 25, size = 5))+
      theme(legend.title = element_text(face = "bold"))
    
    # Mdindo -----------------------------------------------
    df_Mdindo <- df_Expense %>% 
      filter(Village =="Mdindo") %>% 
      view()
    
    df_MD1 <- df_Mdindo %>% 
      select(Expenses1, Expenses_Amount..TZS.1) %>% 
      group_by(Expenses1) %>%
      summarise(ExpensSum1 = sum(Expenses_Amount..TZS.1)) %>% 
      arrange(Expenses1) %>% 
      view()
    
    df_MD2 <- df_Mdindo %>% 
      select(Expenses2, Expenses_Amount..TZS.2) %>% 
      group_by(Expenses2) %>%
      summarise(ExpensSum2 = sum(Expenses_Amount..TZS.2)) %>% 
      arrange(Expenses2) %>% 
      view()
    
    df_MD3 <- df_Mdindo %>% 
      select(Expenses3, Expenses_Amount..TZS.3) %>% 
      group_by(Expenses3) %>%
      summarise(ExpensSum3 = sum(Expenses_Amount..TZS.3)) %>% 
      arrange(Expenses3) %>% 
      view()
    
    df_MD4 <- df_Mdindo %>% 
      select(Expenses4, Expenses_Amount..TZS.4) %>% 
      group_by(Expenses4) %>%
      summarise(ExpensSum4 = sum(Expenses_Amount..TZS.4)) %>% 
      arrange(Expenses4) %>% 
      view()
    
    # Mdindo Join dataframe Expenditure
    df_MDCombine <- df_MD1 %>% 
      mutate(Expenses2 = df_MD2$Expenses2[match(Expenses1, df_MD2$Expenses2)],
             Expenses3 = df_MD3$Expenses3[match(Expenses1, df_MD3$Expenses3)],
             Expenses4 = df_MD4$Expenses4[match(Expenses1, df_MD4$Expenses4)],
             ExpensSum2 = df_MD2$ExpensSum2[match(Expenses1, df_MD2$Expenses2)],
             ExpensSum3 = df_MD3$ExpensSum3[match(Expenses1, df_MD3$Expenses3)],
             ExpensSum4 = df_MD4$ExpensSum4[match(Expenses1, df_MD4$Expenses4)]) %>% 
      select(Expenses1, 3:5, ExpensSum1, 6:8) %>% 
      mutate(Expenses2 = ifelse(is.na(Expenses2), Expenses1, Expenses2), 
             Expenses4 = ifelse(is.na(Expenses4), Expenses1, Expenses4),
             Expenses3 = ifelse(is.na(Expenses3), Expenses1, Expenses3)) %>% 
      replace(is.na(.), 0)%>% 
      mutate(Total = rowSums(.[5:8])) %>% 
      mutate(Percentages = round(Total/sum(Total)*100, digits = 2)) %>% 
      view()
    
    # Mdindo Expense
    df_MDCombine %>% 
      mutate(Expenses1 = fct_reorder(Expenses1, -Percentages)) %>% 
      ggplot(aes(x = "", y = Percentages, fill = Expenses1))+
      geom_bar( stat = "identity")+
      coord_polar("y", start = 0)+
      geom_text(aes(label = ifelse(Percentages > 1, str_c(Percentages, "%"), "")), position = position_stack(vjust = 0.5))+
      geom_label_repel(aes(label = ifelse(Percentages < 1, str_c(Percentages, "%"), "")), size = 5, hjust = 0.5, nudge_x = 0.55, show.legend = F)+
      labs(title = "Households Expenditure variation - Mdindo Village", caption = "HBCL RAP 2020", fill = "Expenses")+
      theme_void()+
      theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 15, vjust = 0.5))+
      theme(plot.caption = element_text(hjust = 0.5, vjust = 25, size = 5))+
      theme(legend.title = element_text(face = "bold"))
    
    
    # Nawenge 
    df_Nawenge <- df_Expense %>% 
      filter(Village =="Nawenge") %>% 
      view()
    
    df_NW1 <- df_Nawenge %>% 
      select(Expenses1, Expenses_Amount..TZS.1) %>% 
      group_by(Expenses1) %>%
      summarise(ExpensSum1 = sum(Expenses_Amount..TZS.1)) %>% 
      arrange(Expenses1) %>% 
      view()
    
    df_NW2 <- df_Nawenge %>% 
      select(Expenses2, Expenses_Amount..TZS.2) %>% 
      group_by(Expenses2) %>%
      summarise(ExpensSum2 = sum(Expenses_Amount..TZS.2)) %>% 
      arrange(Expenses2) %>% 
      view()
    
    df_NW3 <- df_Nawenge %>% 
      select(Expenses3, Expenses_Amount..TZS.3) %>% 
      group_by(Expenses3) %>%
      summarise(ExpensSum3 = sum(Expenses_Amount..TZS.3)) %>% 
      arrange(Expenses3) %>% 
      view()
    
    df_NW4 <- df_Nawenge %>% 
      select(Expenses4, Expenses_Amount..TZS.4) %>% 
      group_by(Expenses4) %>%
      summarise(ExpensSum4 = sum(Expenses_Amount..TZS.4)) %>% 
      arrange(Expenses4) %>% 
      view()
    
    # Nawenge Join dataframe Expenditure
    df_NWCombine <- df_NW1 %>% 
      mutate(Expenses2 = df_NW2$Expenses2[match(Expenses1, df_NW2$Expenses2)],
             Expenses3 = df_NW3$Expenses3[match(Expenses1, df_NW3$Expenses3)],
             Expenses4 = df_NW4$Expenses4[match(Expenses1, df_NW4$Expenses4)],
             ExpensSum2 = df_NW2$ExpensSum2[match(Expenses1, df_NW2$Expenses2)],
             ExpensSum3 = df_NW3$ExpensSum3[match(Expenses1, df_NW3$Expenses3)],
             ExpensSum4 = df_NW4$ExpensSum4[match(Expenses1, df_NW4$Expenses4)]) %>% 
      select(Expenses1, 3:5, ExpensSum1, 6:8) %>% 
      mutate(Expenses2 = ifelse(is.na(Expenses2), Expenses1, Expenses2), 
             Expenses4 = ifelse(is.na(Expenses4), Expenses1, Expenses4),
             Expenses3 = ifelse(is.na(Expenses3), Expenses1, Expenses3)) %>% 
      replace(is.na(.), 0)%>% 
      mutate(Total = rowSums(.[5:8])) %>% 
      mutate(Percentages = round(Total/sum(Total)*100, digits = 2)) %>% 
      view()
    
    # Nawenge Expense
    df_NWCombine %>% 
      mutate(Expenses1 = fct_reorder(Expenses1, -Percentages)) %>% 
      ggplot(aes(x = "", y = Percentages, fill = Expenses1))+
      geom_bar( stat = "identity")+
      coord_polar("y", start = 0)+
      geom_text(aes(label = ifelse(Percentages > 1, str_c(Percentages, "%"), "")), position = position_stack(vjust = 0.5))+
      geom_label_repel(aes(label = ifelse(Percentages < 1, str_c(Percentages, "%"), "")), size = 5, hjust = 0.5, nudge_x = 0.55, show.legend = F)+
      labs(title = "Households Expenditure variation - Nawenge Village", caption = "HBCL RAP 2020", fill = "Expenses")+
      theme_void()+
      theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 15, vjust = -3))+
      theme(plot.caption = element_text(hjust = 0.5, vjust = 25, size = 5))+
      theme(legend.title = element_text(face = "bold"))
