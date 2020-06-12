    # Household Survey data analysis - MINING
    # Harmonic Biosphere Company Limited 
    # Wilbert J. Simbila
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
    
    # b)	Percentage of the PAPS informal gold mining each of affected village by the project
    df_Mine <- df %>% 
      select(Village, Mining) %>%
      mutate(Mining = ifelse(Mining == "-", "No", Mining)) %>% 
      group_by(Village, Mining) %>% 
      summarise(Mine_Activ = n()) %>%
      ungroup()%>% 
      pivot_wider(names_from = "Mining", values_from = "Mine_Activ")%>%
      replace(is.na(.),0)%>%
      mutate(Total = rowSums(.[2:3]))%>%
      mutate(No_Perc = round(No/Total*100, digits = 2),
             Yes_Perc = round(Yes/Total*100, digits = 2))%>% 
      pivot_longer(cols = 5:6, names_to = "Mining", values_to = "Percentages") %>% 
      mutate(Mining = ifelse(Mining == "No_Perc", "No Mining", "Mining")) %>% 
      view()
    
    # Kisewe Mining Pie Chat
    df_Mine %>% 
      filter(Village == "Kisewe") %>% 
      ggplot(aes(x = "", y = Percentages, fill = Mining))+
      geom_bar(stat = "identity")+
      coord_polar("y", start = 0)+
      geom_text(aes(label =  str_c(Percentages, "%")), position = position_stack(vjust = 0.5), vjust = 2.5, hjust = 1)+
      labs(title = "Households Practicing Mining - Kisewe Village", caption = "HBCL RAP 2020")+
      theme_void()+
      theme(plot.title = element_text(size = 15, vjust = -5.5, hjust = 0.5, face = "bold"))+
      theme(plot.caption = element_text(size = 5, vjust = 25, hjust = 0.5))+
      theme(legend.title = element_text(face = "bold"))
    
    # Makanga Mining Pie Chat
    df_Mine %>% 
      filter(Village == "Makanga") %>% 
      ggplot(aes(x = "", y = Percentages, fill = Mining))+
      geom_bar(stat = "identity")+
      coord_polar("y", start = 0)+
      geom_text(aes(label =  str_c(Percentages, "%")), position = position_stack(vjust = 0.5), vjust = -2.5, hjust = 0.5)+
      labs(title = "Households Practicing Mining - Makanga Village", caption = "HBCL RAP 2020")+
      theme_void()+
      theme(plot.title = element_text(size = 15, vjust = -5.5, hjust = 0.5, face = "bold"))+
      theme(plot.caption = element_text(size = 5, vjust = 25, hjust = 0.5))+
      theme(legend.title = element_text(face = "bold"))
    
    # Mdindo Mining Pie Chat
    df_Mine %>% 
      filter(Village == "Mdindo") %>% 
      ggplot(aes(x = "", y = Percentages, fill = Mining))+
      geom_bar(stat = "identity")+
      coord_polar("y", start = 0)+
      geom_text(aes(label =  str_c(Percentages, "%")), position = position_stack(vjust = 0.5), vjust = -2.5, hjust = 0.5)+
      labs(title = "Households Practicing Mining - Mdindo Village", caption = "HBCL RAP 2020")+
      theme_void()+
      theme(plot.title = element_text(size = 15, vjust = -5.5, hjust = 0.5, face = "bold"))+
      theme(plot.caption = element_text(size = 5, vjust = 25, hjust = 0.5))+
      theme(legend.title = element_text(face = "bold"))
    
    # Nawenge Mining Pie Chat
    df_Mine %>% 
      filter(Village == "Nawenge") %>%
      filter(Percentages > 0) %>% 
      ggplot(aes(x = "", y = Percentages, fill = Mining))+
      geom_bar(stat = "identity")+
      coord_polar("y", start = 0)+
      geom_text(aes(label =  str_c(Percentages, "%")), position = position_stack(vjust = 0.5), vjust = -10, hjust = 0.5)+
      labs(title = "Households Practicing Mining - Nawenge Village", caption = "HBCL RAP 2020")+
      theme_void()+
      theme(plot.title = element_text(size = 15, vjust = -5.5, hjust = 0.5, face = "bold"))+
      theme(plot.caption = element_text(size = 5, vjust = 25, hjust = 0.5))+
      theme(legend.title = element_text(face = "bold"))
    
