      # Household Survey data analysis - EDUCATION LEVEL
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
      
      # LEVEL OF EDUCATION ATTAINED BY HEAD OF HOUSEHOLD --------------------------
      # 1. Head of household with Primary education Level  ----------------
      df_Primary <- df %>% 
        select(Village,HofH,Education_level) %>%
        filter(HofH == "Yes") %>% 
        filter(Education_level == "Primary") %>% 
        select(-HofH) %>% 
        group_by(Village, Education_level) %>% 
        summarise(Primary = n()) %>% 
        select(-Education_level) %>% 
        view()
        
      # write.csv(df_Primary, "Primary_Education.csv")
      
      # 2. Head of household did not go School  ----------------
      df_NoEdL<- df %>% 
        select(Village,HofH,Education_level) %>%
        filter(HofH == "Yes") %>% 
        filter(Education_level %in% c("Didn't go to school","No formal education","Did not go to school","Did not finish Primary school")) %>% 
        select(-HofH) %>% 
        mutate(Education_level = case_when(Education_level %in% c("Didn't go to school","No formal education",
                                                                  "Did not go to school","Did not finish Primary school")~"No Education")) %>% 
        group_by(Village, Education_level) %>% 
        summarise(No_Education = n()) %>% 
        select(-Education_level) %>% 
        view()
        
      # write.csv(df_NoEdL, "No_Education.csv")
      
      # 3. Head of household with Primary education Level  ---------------- 
      df_HighEd <- df %>% 
        select(Village,HofH,Education_level) %>%
        filter(HofH == "Yes") %>% 
        filter(Education_level %in% c("University","Secondary","Diploma in Community Development and Social Work ")) %>%
        select(-HofH) %>% 
        mutate(Education_level = case_when(Education_level %in% c("University","Secondary",
                                                                  "Diploma in Community Development and Social Work ")~"High Education")) %>% 
        group_by(Village, Education_level) %>% 
        summarise(High_Education = n()) %>% 
        select(-Education_level) %>% 
        view()
        
      # write.csv(df_HighEd, "High_Education.csv")
      
      # Education level dataframe ---------------------------------------------------------
      df_Education <- df_Primary %>% 
        mutate(No_Education = df_NoEdL$No_Education[match(Village,df_NoEdL$Village)],
               High_Education = df_HighEd$High_Education[match(Village, df_HighEd$Village)],
               High_Education = ifelse(is.na(High_Education), 0, High_Education)) %>% 
        ungroup() %>% 
        mutate(Total = rowSums(.[2:4]))%>% 
        mutate(Primary_Per = round(Primary/Total*100, digits = 2),
               No_Education_Per = round(No_Education/Total*100, digits = 2),
               High_Education_Per = round(High_Education/Total*100, digits = 2)) %>% 
        pivot_longer(cols = 6:8, names_to = "Education", values_to = "Percentages") %>% 
        mutate(Education = ifelse(Education == "Primary_Per", "Primary Education", Education),
               Education = ifelse(Education == "No_Education_Per", "No Education", Education),
               Education = ifelse(Education == "High_Education_Per", "High Education", Education)) %>% 
      view()
      # write.csv(df_Education, "Education.csv")      
      
      # Education Level Kisewe Pie Chat plot
      df_Edu_Kisewe <- df_Education %>% 
        filter(Village == "Kisewe") 
      ggplot(data = df_Edu_Kisewe, aes(x = "", y = Percentages, fill = Education))+
        geom_bar(stat = "identity")+
        coord_polar("y", start = 0)+
        geom_text(aes(label = ifelse(Percentages > 90, str_c(Percentages,"%"), "")), position = position_stack(vjust = 0.5), hjust = 0.5, vjust = -5, size = 5, colour = "black")+
        geom_text(aes(label = ifelse(Percentages < 90, str_c(Percentages,"%"), "")), position = position_stack(vjust = 0.3), size = 5, colour = "black",
                  angle = 100, hjust = -0.5)+
        scale_fill_discrete(breaks = c("No Education", "Primary Education", "High Education"))+
        labs(title = "Respondent Education Level - Kisewe Village", caption = "HBCL RAP 2020", fill = "Education Level")+
        theme_void()+
        theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold", vjust = -5))+
        theme(legend.title = element_text(face = "bold"))+
        theme(plot.caption = element_text(size = 5, vjust = 35, hjust = 0.5))
      
      # Education level Makanga Pie Chat plot
      df_Education %>% 
        filter(Village == "Makanga") %>% 
        ggplot(aes(x = "", y = Percentages, fill = Education))+
        geom_bar(stat = "identity")+
        coord_polar("y", start = 0)+
        geom_text(aes(label = str_c(Percentages,"%")), position = position_stack(vjust = 0.5), hjust = 0.5, vjust = 0.25, size = 5, colour = "black")+
        scale_fill_discrete(breaks = c("No Education", "Primary Education", "High Education"))+
        labs(title = "Respondent Education Level - Makanga Village", caption = "HBCL RAP 2020", fill = "Education Level")+
        theme_void()+
        theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold", vjust = -5))+
        theme(legend.title = element_text(face = "bold"))+
        theme(plot.caption = element_text(size = 5, vjust = 35, hjust = 0.5))
      
      # Education level Mdindo Pie Chat plot
      df_Education %>% 
        filter(Village == "Mdindo") %>% 
        ggplot(aes(x = "", y = Percentages, fill = Education))+
        geom_bar(stat = "identity")+
        coord_polar("y", start = 0)+
        geom_text(aes(label = ifelse(Percentages > 80, str_c(Percentages,"%"), "")), position = position_stack(vjust = 0.5), hjust = 0.5, vjust = -5, size = 5, colour = "black")+
        geom_text(aes(label = ifelse(Percentages < 80, str_c(Percentages,"%"), "")), position = position_stack(vjust = 0.3), size = 5, colour = "black",
                  angle = 100, hjust = -0.5)+
        scale_fill_discrete(breaks = c("No Education", "Primary Education", "High Education"))+
        labs(title = "Respondent Education Level - Mdindo Village", caption = "HBCL RAP 2020", fill = "Education Level")+
        theme_void()+
        theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold", vjust = -5))+
        theme(legend.title = element_text(face = "bold"))+
        theme(plot.caption = element_text(size = 5, vjust = 35, hjust = 0.5))
      
      # Education level Nawenge Pie Chat plot
      df_Education %>% 
        filter(Village == "Nawenge") %>% 
        filter(Percentages > 0) %>% 
        ggplot(aes(x = "", y = Percentages, fill = Education))+
        geom_bar(stat = "identity")+
        coord_polar("y", start = 0)+
        geom_text(aes(label = str_c(Percentages,"%")), position = position_stack(vjust = 0.5), hjust = 0.5, vjust = 0.25, size = 5, colour = "black")+
        scale_fill_discrete(breaks = c("No Education", "Primary Education", "High Education"))+
        labs(title = "Respondent Education Level - Nawenge Village", caption = "HBCL RAP 2020", fill = "Education Level")+
        theme_void()+
        theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold", vjust = -5))+
        theme(legend.title = element_text(face = "bold"))+
        theme(plot.caption = element_text(size = 5, vjust = 35, hjust = 0.5))
      
      
      
