    # Household Survey data analysis - LAND TENURE
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
    
    # LAND TENURE ----------------------------
    LandTenure <- df%>% 
      select(Village, Land.Tenure)%>% 
      mutate(Land.Tenure = ifelse(Land.Tenure == "Customery/Certification Right of Occupancy", "Customary_law", Land.Tenure),
             Land.Tenure = ifelse(Land.Tenure == "1. Leasehold/Certificate of ownership", "Customary_law", Land.Tenure),
             Land.Tenure = ifelse(Land.Tenure == "Customary law", "Customary_law", Land.Tenure),
             Land.Tenure = ifelse(Land.Tenure == "Share cropping", "Share_crop", Land.Tenure),
             Land.Tenure = ifelse(Land.Tenure == "Other form of tenure", "Other_tenure", Land.Tenure)) %>% 
      group_by(Village, Land.Tenure)%>% 
      summarise(Tenure = n())%>% 
      ungroup() %>% 
      pivot_wider(names_from = "Land.Tenure", values_from = "Tenure")%>% 
      replace(is.na(.),0)%>%
      mutate(Total = rowSums(.[2:5]))%>% 
      mutate(Borrow_Perc = round(Borrowed/Total*100, digits = 2),
             Custom_law_Perc= round(Customary_law/Total*100, digits = 2),
             Share_crop_Perc = round(Share_crop/Total*100, digits = 2),
             Other_tenure_Perc = round(Other_tenure/Total*100, digits = 2)) %>% 
      pivot_longer(cols = 7:10, names_to = "LandTenure", values_to = "Percentages") %>% 
      mutate(LandTenure = ifelse(LandTenure == "Borrow_Perc", "Borrowed", LandTenure),
             LandTenure = ifelse(LandTenure == "Custom_law_Perc", "Customer Law", LandTenure),
             LandTenure = ifelse(LandTenure == "Share_crop_Perc", "Shared Crops", LandTenure),
             LandTenure = ifelse(LandTenure == "Other_tenure_Perc", "Other Land Tenure", LandTenure))
    
    # Kisewe Land Tenure Pie Chart
    LandTenure %>% 
      filter(Village == "Kisewe") %>% 
      ggplot(aes(x = "", y = Percentages, fill = LandTenure))+
      geom_bar(stat = "Identity")+
      coord_polar("y", start = 0)+
      geom_text(aes(label = ifelse(Percentages > 3, str_c(Percentages, "%"), "")), position = position_stack(vjust = 0.5), hjust = 0.6, vjust = -6)+
      geom_label_repel(aes(label = ifelse(Percentages < 3, str_c(Percentages, "%"), "")), size = 4, nudge_x = 0.7, show.legend = F)+
      labs(title = "Land Tenure Type - Kisewe Village", caption = "HBCL RAP 2020")+
      theme_void()+
      theme(plot.title = element_text(size = 15, hjust = 0.5, vjust = 1.5, face = "bold"))+
      theme(plot.caption = element_text(size = 5, hjust = 0.5, vjust = 25))+
      theme(legend.title = element_text(face = "bold"))
    
    # Makanga Land Tenure Pie Chart
    LandTenure %>% 
      filter(Village == "Makanga") %>%
      filter(Percentages > 0) %>% 
      ggplot(aes(x = "", y = Percentages, fill = LandTenure))+
      geom_bar(stat = "Identity")+
      coord_polar("y", start = 0)+
      geom_text(aes(label =  str_c(Percentages, "%")), position = position_stack(vjust = 0.5), hjust = 0.5, vjust = -10)+
      labs(title = "Land Tenure Type - Makanga Village", caption = "HBCL RAP 2020")+
      theme_void()+
      theme(plot.title = element_text(size = 15, hjust = 0.5, vjust = 0.5, face = "bold"))+
      theme(plot.caption = element_text(size = 5, hjust = 0.5, vjust = 25))+
      theme(legend.title = element_text(face = "bold"))
    
    # Mdindo Land Tenure Pie Chart
    LandTenure %>% 
      filter(Village == "Mdindo") %>%
      filter(Percentages > 0) %>% 
      mutate(LandTenure = fct_reorder(LandTenure, -Percentages)) %>% 
      ggplot(aes(x = "", y = Percentages, fill = LandTenure))+
      geom_bar(stat = "Identity")+
      coord_polar("y", start = 0)+
      geom_text(aes(label = ifelse(Percentages > 90, str_c(Percentages, "%"), "")), position = position_stack(vjust = 0.5), hjust = 0.6, vjust = -6)+
      geom_label_repel(aes(label = ifelse(Percentages < 90, str_c(Percentages, "%"), "")), size = 4, nudge_x = 0.7, show.legend = F)+
      labs(title = "Land Tenure Type - Mdindo Village", caption = "HBCL RAP 2020")+
      theme_void()+
      theme(plot.title = element_text(size = 15, hjust = 0.5, vjust = 2.5, face = "bold"))+
      theme(plot.caption = element_text(size = 5, hjust = 0.5, vjust = 25))+
      theme(legend.title = element_text(face = "bold"))
    
    # Nawenge Land Tenure Pie Chart
    LandTenure %>% 
      filter(Village == "Nawenge") %>%
      filter(Percentages > 0) %>% 
      #mutate(LandTenure = fct_reorder(LandTenure, -Percentages)) %>% 
      ggplot(aes(x = "", y = Percentages, fill = LandTenure))+
      geom_bar(stat = "Identity")+
      coord_polar("y", start = 0)+
      geom_text(aes(label = str_c(Percentages, "%")), position = position_stack(vjust = 0.5), hjust = 0.5, vjust = -10)+
      #geom_label_repel(aes(label = ifelse(Percentages < 90, str_c(Percentages, "%"), "")), size = 4, nudge_x = 0.7, show.legend = F)+
      labs(title = "Land Tenure Type - Nawenge Village", caption = "HBCL RAP 2020")+
      theme_void()+
      theme(plot.title = element_text(size = 15, hjust = 0.5, vjust = -2.5, face = "bold"))+
      theme(plot.caption = element_text(size = 5, hjust = 0.5, vjust = 25))+
      theme(legend.title = element_text(face = "bold"))
