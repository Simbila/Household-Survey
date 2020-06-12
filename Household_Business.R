    # Household Survey data analysis - AGRICULTURE
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
    
    # BUSINESS ---------------
    # c) Percentage of other economic activities [small business activities etc. in each affected village by the project
    df_Business <- df %>% 
      select(Village, Business, Other_sources) %>%
      mutate(Business = ifelse(Business == "-", NA, Business),
             Other_sources = ifelse(Other_sources == "-", NA,Other_sources)) %>%
      filter(!is.na(Business)) %>% 
      view()
    
    # Business
    Busines <- df_Business %>% 
      group_by(Village, Business) %>% 
      summarise(Busines_Activ = n()) %>%
      ungroup()%>% 
      pivot_wider(names_from = "Business", values_from = "Busines_Activ")%>%
      replace(is.na(.),0)%>%
      mutate(Total = rowSums(.[2:3]))%>%
      mutate(No_Perc = round(No/Total*100, digits = 2),
             Yes_Perc = round(Yes/Total*100, digits = 2))%>% 
      pivot_longer(cols = 5:6, names_to = "Business", values_to = "Percentages") %>% 
      mutate(Business = ifelse(Business == "Yes_Perc", "Yes", "No")) %>% 
      view()
    # Kisewe pie chart plot
    Busines %>% 
      filter(Village == "Kisewe") %>% 
      ggplot(aes(x = "", y = Percentages, fill = Business))+
      geom_bar(stat = "identity")+
      geom_text(aes(label = ifelse(Percentages > 90, str_c(Percentages, "%"), "")), position = position_stack(vjust = 0.5), vjust = -6, size = 5)+
      geom_text(aes(label = ifelse(Percentages < 90, str_c(Percentages, "%"), "")), position = position_stack(vjust = 0.5), angle = 90, hjust = -0.5, size = 5)+
      coord_polar("y", start = 0)+
      labs(title = "Household Involved in Business - Kisewe Village", caption = "HBCL RAP 2020")+
      theme_void()+
      theme(plot.title = element_text(hjust = 0.5, size = 15, face = "bold", vjust = -8))+
      theme(plot.caption = element_text(hjust = 0.5, size = 5, vjust = 25))+
      theme(legend.title = element_text(face = "bold", size = 10))
    
    # Makanga pie chart plot
    Busines %>% 
      filter(Village == "Makanga") %>% 
      ggplot(aes(x = "", y = Percentages, fill = Business))+
      geom_bar(stat = "identity")+
      geom_text(aes(label =  str_c(Percentages, "%")), position = position_stack(vjust = 0.5), vjust = -2, size = 5)+
      #geom_text(aes(label = ifelse(Percentages < 90, str_c(Percentages, "%"), "")), position = position_stack(vjust = 0.5), angle = 90, hjust = -0.5, size = 5)+
      coord_polar("y", start = 0)+
      labs(title = "Household Involved in Business - Makanga Village", caption = "HBCL RAP 2020")+
      theme_void()+
      theme(plot.title = element_text(hjust = 0.5, size = 15, face = "bold", vjust = -8))+
      theme(plot.caption = element_text(hjust = 0.5, size = 5, vjust = 25))+
      theme(legend.title = element_text(face = "bold", size = 10))
    
    # Mdindo pie chart plot
    Busines %>% 
      filter(Village == "Mdindo") %>% 
      ggplot(aes(x = "", y = Percentages, fill = Business))+
      geom_bar(stat = "identity")+
      geom_text(aes(label =  str_c(Percentages, "%")), position = position_stack(vjust = 0.5), vjust = -2, size = 5)+
      #geom_text(aes(label = ifelse(Percentages < 90, str_c(Percentages, "%"), "")), position = position_stack(vjust = 0.5), angle = 90, hjust = -0.5, size = 5)+
      coord_polar("y", start = 0)+
      labs(title = "Household Involved in Business - Mdindo Village", caption = "HBCL RAP 2020")+
      theme_void()+
      theme(plot.title = element_text(hjust = 0.5, size = 15, face = "bold", vjust = -8))+
      theme(plot.caption = element_text(hjust = 0.5, size = 5, vjust = 25))+
      theme(legend.title = element_text(face = "bold", size = 10))
    
    # Nawenge pie chart plot
    Busines %>% 
      filter(Village == "Nawenge") %>% 
      filter(Percentages > 0) %>% 
      ggplot(aes(x = "", y = Percentages, fill = Business))+
      geom_bar(stat = "identity")+
      geom_text(aes(label =  str_c(Percentages, "%")), position = position_stack(vjust = 0.5), vjust = -8, size = 5)+
      #geom_text(aes(label = ifelse(Percentages < 90, str_c(Percentages, "%"), "")), position = position_stack(vjust = 0.5), angle = 90, hjust = -0.5, size = 5)+
      coord_polar("y", start = 0)+
      labs(title = "Household Involved in Business - Nawenge Village", caption = "HBCL RAP 2020")+
      theme_void()+
      theme(plot.title = element_text(hjust = 0.5, size = 15, face = "bold", vjust = -8))+
      theme(plot.caption = element_text(hjust = 0.5, size = 5, vjust = 25))+
      theme(legend.title = element_text(face = "bold", size = 10))
    
    # Other Busines
    other_Busines <- df_Business %>% 
      group_by(Village, Other_sources) %>% 
      summarise(Other_Activ = n()) %>%
      ungroup()%>% 
      pivot_wider(names_from = "Other_sources", values_from = "Other_Activ")%>%
      replace(is.na(.),0)%>%
      mutate(Total = rowSums(.[2:3]))%>%
      mutate(No_Perc = round(No/Total*100, digits = 2),
             Yes_Perc = round(Yes/Total*100, digits = 2))%>% 
      view()
