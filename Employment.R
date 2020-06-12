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
    
    
    # EMPLOYMENT -------------------------------
    df_employee <- df %>% 
      select(Village, Employed) %>% 
      mutate_all(na_if,"-") %>% 
      mutate_all(na_if,"") %>% 
      mutate(Employed = ifelse(Employed == "Public servant employee", "Employed", Employed)) %>% 
      group_by(Village, Employed) %>% 
      summarise(Employment = n()) %>%
      ungroup()%>% 
      pivot_wider(names_from = "Employed", values_from = "Employment")%>%
      replace(is.na(.),0)%>%
      rename(Unemployed = "NA") %>% 
      mutate(Total = rowSums(.[2:3]))%>%
      mutate(Unemployed_Perc = round(Unemployed/Total*100, digits = 2),
             Employed_Perc = round(Employed/Total*100, digits = 2))%>% 
      pivot_longer(cols = 5:6, names_to = "Employment", values_to = "Percentages") %>% 
      mutate(Employment = ifelse(Employment == "Unemployed_Perc", "Unemployment", "Employment")) 
    
    # Kisewe Employment Status
    df_employee %>% 
      filter(Village == "Kisewe") %>%
      filter(Percentages > 0) %>% 
      ggplot(aes(x = "", y = Percentages, fill = Employment))+
      geom_bar(stat = "Identity")+
      coord_polar("y", start = 0)+
      geom_text(aes(label = str_c(Percentages, "%")), position = position_stack(vjust = 0.5), vjust = -10)+
      labs(title = "Employment Status - Kisewe Village", caption = "HBCL RAP 2020")+
      theme_void()+
      theme(plot.title = element_text(face = "bold", size = 15, hjust = 0.5, vjust = -7.5))+
      theme(legend.title = element_text(face = "bold"))+
      theme(plot.caption = element_text(vjust = 20, hjust = 0.5, size = 5))
    
    # Makanga Employment Status
    df_employee %>% 
      filter(Village == "Makanga") %>%
      filter(Percentages > 0) %>% 
      ggplot(aes(x = "", y = Percentages, fill = Employment))+
      geom_bar(stat = "Identity")+
      coord_polar("y", start = 0)+
      geom_text(aes(label = str_c(Percentages, "%")), position = position_stack(vjust = 0.5), vjust = -10)+
      labs(title = "Employment Status - Makanga Village", caption = "HBCL RAP 2020")+
      theme_void()+
      theme(plot.title = element_text(face = "bold", size = 15, hjust = 0.5, vjust = -7.5))+
      theme(legend.title = element_text(face = "bold"))+
      theme(plot.caption = element_text(vjust = 20, hjust = 0.5, size = 5))
    
    # Mdindo Employment Status
    df_employee %>% 
      filter(Village == "Mdindo") %>%
      filter(Percentages > 0) %>% 
      ggplot(aes(x = "", y = Percentages, fill = Employment))+
      geom_bar(stat = "Identity")+
      coord_polar("y", start = 0)+
      geom_text(aes(label = ifelse(Percentages > 90, str_c(Percentages, "%"), "")), position = position_stack(vjust = 0.5), vjust = -8)+
      geom_text(aes(label = ifelse(Percentages < 90, str_c(Percentages, "%"), "")), position = position_stack(vjust = 0.5), vjust = 0.2, angle = 90, hjust = -1.5)+
      labs(title = "Employment Status - Mdindo Village", caption = "HBCL RAP 2020")+
      theme_void()+
      theme(plot.title = element_text(face = "bold", size = 15, hjust = 0.5, vjust = -7.5))+
      theme(legend.title = element_text(face = "bold"))+
      theme(plot.caption = element_text(vjust = 20, hjust = 0.5, size = 5))
    
    # Nawenge Employment Status
    df_employee %>% 
      filter(Village == "Nawenge") %>%
      filter(Percentages > 0) %>% 
      ggplot(aes(x = "", y = Percentages, fill = Employment))+
      geom_bar(stat = "Identity")+
      coord_polar("y", start = 0)+
      geom_text(aes(label = str_c(Percentages, "%")), position = position_stack(vjust = 0.5), vjust = -10)+
      labs(title = "Employment Status - Nawenge Village", caption = "HBCL RAP 2020")+
      theme_void()+
      theme(plot.title = element_text(face = "bold", size = 15, hjust = 0.5, vjust = -7.5))+
      theme(legend.title = element_text(face = "bold"))+
      theme(plot.caption = element_text(vjust = 20, hjust = 0.5, size = 5))
    
