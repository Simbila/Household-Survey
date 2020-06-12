    # Household Survey data analysis - AGRICULTURE
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
    
    
    # AGRICULTURE ----------------------------------
    # a)	percentage of the PAPS who practice agriculture in each of affected village by the project
    df_Agric <- df %>% 
      select(Village, Activities) %>%
      mutate(Activities, ifelse(Activities == "Actvities", "Agriculuture", Activities)) %>% 
      group_by(Village, Activities) %>% 
      summarise(Agriculture = n()) %>% 
      ungroup()%>% 
      pivot_wider(names_from = "Activities", values_from = "Agriculture")%>% 
      replace(is.na(.),0)%>%
      mutate(Total = rowSums(.[2:3]))%>%
      mutate(No_Perc = round(No/Total*100, digits = 2),
             Yes_Perc = round(Yes/Total*100, digits = 2))%>% 
      pivot_longer(cols = 5:6, names_to = "Agriculture", values_to = "Percentages") %>% 
      mutate(Agriculture = ifelse(Agriculture == "No_Perc", "No Agriculture", "Agriculture")) %>% 
    view()
    
    # Kisewe Agriculture Pie Chart ---------------------
    df_Agric %>% 
      filter(Village == "Kisewe") %>% 
      ggplot(aes(x = "", y = Percentages, fill = Agriculture))+
      geom_bar(stat = "identity")+
      coord_polar("y", start = 0)+
      geom_text(aes(label = ifelse(Percentages > 1, str_c(Percentages, "%"), "")), position = position_stack(vjust = 0.5))+
      geom_label_repel(aes(label = ifelse(Percentages < 1, str_c(Percentages, "%"), "")), size = 4,nudge_x = 0.5, hjust= -1,  show.legend = F)+
      labs(title = "Households Practicing Agriculture - Kisewe Village", caption = "HBCL RAP 2020")+
      theme_void()+
      theme(plot.title = element_text(size = 15, vjust = 1, hjust = 0.5, face = "bold"))+
      theme(plot.caption = element_text(size = 5, vjust = 25, hjust = 0.5))+
      theme(legend.title = element_text(face = "bold"))
    
    # Makanga Agriculture Pie Chart ---------------------
    df_Agric %>% 
      filter(Village == "Makanga") %>% 
      filter(Percentages > 0) %>% 
      ggplot(aes(x = "", y = Percentages, fill = Agriculture))+
      geom_bar(stat = "identity")+
      coord_polar("y", start = 0)+
      geom_text(aes(label = str_c(Percentages, "%")), position = position_stack(vjust = 0.5), vjust = -10)+
      labs(title = "Households Practicing Agriculture - Makanga Village", caption = "HBCL RAP 2020")+
      theme_void()+
      theme(plot.title = element_text(size = 15, vjust = -2.5, hjust = 0.5, face = "bold"))+
      theme(plot.caption = element_text(size = 5, vjust = 25, hjust = 0.5))+
      theme(legend.title = element_text(face = "bold"))
    
    # Mdindo Agriculture Pie Chart ---------------------
    df_Agric %>% 
      filter(Village == "Mdindo") %>% 
      ggplot(aes(x = "", y = Percentages, fill = Agriculture))+
      geom_bar(stat = "identity")+
      coord_polar("y", start = 0)+
      geom_text(aes(label =  str_c(Percentages, "%")), position = position_stack(vjust = 0.5), vjust = -6, hjust = 0.2)+
      labs(title = "Households Practicing Agriculture - Mdindo Village", caption = "HBCL RAP 2020")+
      theme_void()+
      theme(plot.title = element_text(size = 15, vjust = -5.5, hjust = 0.5, face = "bold"))+
      theme(plot.caption = element_text(size = 5, vjust = 25, hjust = 0.5))+
      theme(legend.title = element_text(face = "bold"))
    
    # Nawenge Agriculture Pie Chart ---------------------
    df_Agric %>% 
      filter(Village == "Nawenge") %>% 
      filter(Percentages > 0) %>% 
      ggplot(aes(x = "", y = Percentages, fill = Agriculture))+
      geom_bar(stat = "identity")+
      coord_polar("y", start = 0)+
      geom_text(aes(label = str_c(Percentages, "%")), position = position_stack(vjust = 0.5), vjust = -10)+
      labs(title = "Households Practicing Agriculture - Nawenge Village", caption = "HBCL RAP 2020")+
      theme_void()+
      theme(plot.title = element_text(size = 15, vjust = -2.5, hjust = 0.5, face = "bold"))+
      theme(plot.caption = element_text(size = 5, vjust = 25, hjust = 0.5))+
      theme(legend.title = element_text(face = "bold"))
