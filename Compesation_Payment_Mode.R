      # Household Survey data analysis - COMPENSATION  PAYMENT MODE 
      # Harmonic Biosphere Company Limited
      # W. Simbila
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
      
      # MODE OD COMPENSATION PAYMENT -------------------------------
      df_payment <- df %>% 
        select(Village, Mode.of.Compersation) %>%
        mutate(Mode.of.Compersation = ifelse(Mode.of.Compersation == "", NA, Mode.of.Compersation)) %>% 
        group_by(Village, Mode.of.Compersation) %>% 
        summarise(Payment_Mode = n()) %>% 
        filter(!is.na(Mode.of.Compersation)) %>% 
        mutate(Payment_Mode_Per = round(Payment_Mode/sum(Payment_Mode)*100, digits = 2)) %>% 
        view()
      
      
      # Kisewe Village Compesation payment mode Pie Chart Plot
      df_Kisewe <- df_payment %>% 
        filter(Village == "Kisewe")
      ggplot(data = df_Kisewe, aes(x = "", y = Payment_Mode_Per, fill = Mode.of.Compersation))+
        geom_bar(stat = "identity")+
        coord_polar("y", start = 0)+
        geom_text(aes(label = str_c(Payment_Mode_Per, "%")), position = position_stack(vjust = 0.5), size = 5, colour = "white")+
        labs(title = "Household Prefered Compensation Mode of Payment - Kisewe Village", caption = "HBCL RAP 2020", fill = "Mode of Payment")+
        theme_void()+
        theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold", vjust = -5))+
        theme(legend.title = element_text(face = "bold"))+
        theme(plot.caption = element_text(size = 5, vjust = 35, hjust = 0.5))
      
      # Makanga Village Compesation payment mode Pie Chart Plot
      df_Makanga <- df_payment %>% 
        filter(Village == "Makanga")
      ggplot(data = df_Makanga, aes(x = "", y = Payment_Mode_Per, fill = Mode.of.Compersation))+
        geom_bar(stat = "identity")+
        coord_polar("y", start = 0)+
        geom_text(aes(label = str_c(Payment_Mode_Per, "%")), position = position_stack(vjust = 0.5), size = 5, colour = "white")+
        labs(title = "Household Prefered Compensation Mode of Payment - Makanga Village", caption = "HBCL RAP 2020", fill = "Mode of Payment")+
        theme_void()+
        theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold", vjust = -5))+
        theme(legend.title = element_text(face = "bold"))+
        theme(plot.caption = element_text(size = 5, vjust = 35, hjust = 0.5))
      
      # Mdindo Village Compesation payment mode Pie Chart Plot
      df_Mdindo <- df_payment %>% 
        filter(Village == "Mdindo")
      ggplot(data = df_Mdindo, aes(x = "", y = Payment_Mode_Per, fill = Mode.of.Compersation))+
        geom_bar(stat = "identity")+
        coord_polar("y", start = 0)+
        geom_text(aes(label = str_c(Payment_Mode_Per, "%")), position = position_stack(vjust = 0.5), size = 5, colour = "white")+
        labs(title = "Household Prefered Compensation Mode of Payment - Mdindo Village", caption = "HBCL RAP 2020", fill = "Mode of Payment")+
        theme_void()+
        theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold", vjust = -5))+
        theme(legend.title = element_text(face = "bold"))+
        theme(plot.caption = element_text(size = 5, vjust = 35, hjust = 0.5))
      
      
      # Nawenge Village Compesation payment mode Pie Chart Plot- 
      df_Nawenge <- df_payment %>% 
        filter(Village == "Nawenge")
      ggplot(data = df_Nawenge, aes(x = "", y = Payment_Mode_Per, fill = Mode.of.Compersation))+
        geom_bar(stat = "identity")+
        coord_polar("y", start = 0)+
        geom_text(aes(label = str_c(Payment_Mode_Per, "%")), position = position_stack(vjust = 0.5),vjust = -8, size = 5, colour = "black")+
        labs(title = "Household Prefered Compensation Mode of Payment - Nawenge Village", caption = "HBCL RAP 2020", fill = "Mode of Payment")+
        theme_void()+
        theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold", vjust = -5))+
        theme(legend.title = element_text(face = "bold"))+
        theme(plot.caption = element_text(size = 5, vjust = 35, hjust = 0.5))
