      # Household Survey data analysis - HOUSEHOLD CHARACTERISTICS
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
      
      
      # POPULATION CHARACTERISTICS -------------------------------------------------
      
      # 1.Percentage of population ratio for PAPS in each of the affected village
      
      df_pop<-df %>%
        select(Village,HoH_N) %>% 
        group_by(Village) %>% 
        summarise(freq = n()) %>% 
        mutate(Percentage_ratio = freq/sum(freq)*100,
               Percentage_ratio = str_c(round(Percentage_ratio,2),"%")) %>% 
        view()
      
      # Population 
      df_pop %>% 
        ggplot(aes(x = "", y = Percentage_ratio, fill = Village))+
        geom_bar(stat = "identity")+
        coord_polar("y", start = 0)+
        geom_text(aes(label = str_c(Percentage_ratio, "%")), position = position_stack(vjust = 0.5))+
        labs(title = "Percentage of Population Ratio in Each Affected  Village", caption = "HBCL RAP 2020")+
        theme_void()+
        theme(plot.title = element_text(size = 15, face = "bold", hjust = 0.5, vjust = -6))+
        theme(plot.caption = element_text(size = 5, hjust = 0.5, vjust = 25))+
        theme(legend.title = element_text(face = "bold"))
      
      # 2.	Gender ratio among the surveyed households -----------------------
      # {in each of the affected villages }
      dfHoH <- df %>% 
        select(Village, Respondent_Gender,contains(c("HM1_G", "HM2_G", "HM3_G", "HM4_G", "HM5_G","HM6_G", "HM7_G", "HM8_G" ))) %>%
        #rename(HM7_G = HM6_G.1, HM8_G = HM_G) %>% 
        mutate_all(na_if,"-") %>% 
        mutate_all(na_if,"") %>% 
        mutate(HM2_G = ifelse(HM2_G=="Male ", "Male",HM2_G))
         view()
      
      # Household Member - (Combine dataframe)
      dfHoH_gender <- dfHoH %>% 
        pivot_longer(cols = c(2:10), names_to = "HouseMember", values_to = "Gender") %>% 
        na.omit(dfHoH_gender) %>% 
        group_by(Village, Gender) %>% 
        summarise(GendeCount=n()) %>% 
        pivot_wider(names_from = "Gender", values_from = "GendeCount") %>% 
        ungroup() %>% 
        mutate(Total = rowSums(.[2:3])) %>% 
        mutate(FemaleRatio=round(Female/Total*100, digits = 2),
               MaleRatio=round(Male/Total*100, digits = 2)) %>% 
        rownames_to_column("ID") %>% 
        pivot_longer(cols = 6:7, names_to = "Gender", values_to = "Percentage") %>% 
        mutate(Gender = ifelse(Gender == "FemaleRatio", "Female", "Male")) %>% 
        view()
        
      # Kisewe Pie Chart Gender Ratio ------------------
      dfHoH_gender %>% 
        filter(Village == "Kisewe") %>% 
        ggplot(aes(x = "", y= Percentage , fill = Gender))+
        geom_bar(stat = "identity")+
        coord_polar("y", start = 0)+
        geom_text(aes(label = str_c(Percentage, "%")), position = position_stack(vjust = 0.5))+
        labs(title = "Household Gender Ration - Kisewe Village", caption = "HBCL RAP 2020")+
        theme_void()+
        theme(plot.title = element_text(size = 15, hjust = 0.5, face = "bold", vjust = -5))+
        theme(plot.caption = element_text(size = 5, hjust = 0.5, vjust = 25))+
        theme(legend.title = element_text(face = "bold"))
      
      # Makanga Pie Chart Gender Ratio ------------------
      dfHoH_gender %>% 
        filter(Village == "Makanga") %>% 
        ggplot(aes(x = "", y= Percentage , fill = Gender))+
        geom_bar(stat = "identity")+
        coord_polar("y", start = 0)+
        geom_text(aes(label = str_c(Percentage, "%")), position = position_stack(vjust = 0.5))+
        labs(title = "Household Gender Ration - Makanga Village", caption = "HBCL RAP 2020")+
        theme_void()+
        theme(plot.title = element_text(size = 15, hjust = 0.5, face = "bold", vjust = -5))+
        theme(plot.caption = element_text(size = 5, hjust = 0.5, vjust = 25))+
        theme(legend.title = element_text(face = "bold"))
      
      # Mdindo Pie Chart Gender Ratio ------------------
      dfHoH_gender %>% 
        filter(Village == "Mdindo") %>% 
        ggplot(aes(x = "", y= Percentage , fill = Gender))+
        geom_bar(stat = "identity")+
        coord_polar("y", start = 0)+
        geom_text(aes(label = str_c(Percentage, "%")), position = position_stack(vjust = 0.5))+
        labs(title = "Household Gender Ration - Mdindo Village", caption = "HBCL RAP 2020")+
        theme_void()+
        theme(plot.title = element_text(size = 15, hjust = 0.5, face = "bold", vjust = -5))+
        theme(plot.caption = element_text(size = 5, hjust = 0.5, vjust = 25))+
        theme(legend.title = element_text(face = "bold"))
      
      # NawengePie Chart Gender Ratio ------------------
      dfHoH_gender %>% 
        filter(Village == "Nawenge") %>% 
        ggplot(aes(x = "", y= Percentage , fill = Gender))+
        geom_bar(stat = "identity")+
        coord_polar("y", start = 0)+
        geom_text(aes(label = str_c(Percentage, "%")), position = position_stack(vjust = 0.5))+
        labs(title = "Household Gender Ration - Nawenge Village", caption = "HBCL RAP 2020")+
        theme_void()+
        theme(plot.title = element_text(size = 15, hjust = 0.5, face = "bold", vjust = -5))+
        theme(plot.caption = element_text(size = 5, hjust = 0.5, vjust = 25))+
        theme(legend.title = element_text(face = "bold"))
     
      # 3.Average size of the household   ----------------------- 
      df_AHS <- df %>% 
        select(Village) %>% 
        group_by(Village) %>% 
        summarise(VillageCount = n()) %>% 
        rownames_to_column("ID") %>% 
        view()
      
      # Join two dataframe dfHoH_gender & df_AHS
      dfcombine <- df_AHS %>% 
        left_join(dfHoH_gender, by= c("ID", "Village")) %>% 
        mutate(HouseholdSize=round(Total/VillageCount, digits = 1))%>% 
        view()
      
      # 4.Categorize age group for PAPS in to 8 groups from each of the affected population 
      # (0 – 5, 5 – 18, 19 – 24, 25 – 34, 35 – 44, 45 – 54, 55- 64, 65 & above) -------------------------------
      
      # Rename column for the household member 
      Age_group <- c("HM1_A", "HM2_A", "HM3_A", "HM4_A", "HM5_A","HM6_A", "HM7_A", "HM8_A")
      
      df_AGC <- df %>% 
        select(Village,Respondent_Age, HM1_A, HM2_A,HM3_A,  HM4_A, HM5_A, HM6_A,HM7_A, HM8_A) %>% 
        mutate_at(Age_group, as.integer) %>% 
        mutate_all(na_if,"-") %>% 
        mutate_all(na_if,"") %>% 
        pivot_longer(cols = c(2:10), names_to = "Members_Age", values_to = "Age") %>% 
        mutate(AgeGroup = case_when(Age%in%c(0:4)~"0-4",
                                    Age%in%c(5:9)~"5-9",
                                    Age%in%c(10:19)~"10-19",
                                    Age%in%c(20:44)~"20-44",
                                    Age%in%c(45:64)~"45-64",
                                    Age >= 65~"65 & Above")) %>%
        filter(!is.na(Age)) %>% 
        select(-Age,-Members_Age) %>% 
        group_by(Village, AgeGroup) %>% 
        summarise(AgeGroupCount = n()) %>% 
        view()
      
      # write.csv(df_AGC, "Age Structure.csv")
      
      # Bar Plot - Age Strucuture ---------------------------------------------------------------------------
      ggplot(data = df_AGC, aes(x = Village, y = AgeGroupCount, fill = AgeGroup))+
        geom_col(position = position_dodge2(preserve = "single",width = 0.9))+
        scale_y_continuous(name = "PAPS Population", limits = c(0,550), seq(0,550,25))+
        labs(title = "Age Structure of Affected Population", fill = "Age Groups", caption = "HBCL RAP 2020")+
        scale_fill_discrete(breaks = c("0-4","5-9", "10-19", "20-44", "45-64", "65 & Above"))+
        theme_light()+
        theme(plot.title = element_text(hjust = 0.5,face = "bold"))+
        theme(plot.caption = element_text(size = 5))+
        theme(axis.title.x = element_text(face = "bold"))+
        theme(axis.title.y = element_text(face = "bold"))+
        theme(legend.title = element_text(face = "bold"))
      
      # Respondent Characteristics ----------------------------
      Respondend <- df %>% 
        select(Village, HofH) %>% 
        group_by(Village) %>% 
        summarise(Total_Household = n()) %>% 
        rownames_to_column("ID") %>% 
        view()
      
      # a.The surveyed households headed by men in % -----------------------------
      Male_HoH <- df %>% 
        select(Village, HofH, Respondent_Gender) %>% 
        filter(HofH=="Yes" & Respondent_Gender=="Male") %>% 
        group_by(Village, Respondent_Gender) %>% 
        summarise(Male_Resp = n()) %>% 
        mutate(Total_Household = Respondend$Total_Household[match(Village,Respondend$Village)], 
               HofH_Male = round(Male_Resp/Total_Household*100, digits = 1)) %>%
        view()
      
      # b.The surveyed households headed by Female in % -----------------------------
      Female_HoH <- df %>% 
        select(Village, HofH, Respondent_Gender) %>% 
        filter(HofH=="Yes" & Respondent_Gender=="Female") %>% 
        group_by(Village, Respondent_Gender) %>% 
        summarise(Female_Resp = n()) %>% 
        mutate(Total_Household = Respondend$Total_Household[match(Village,Respondend$Village)], 
               HofH_Female = round(Female_Resp/Total_Household*100, digits = 1))%>% 
        view()
      
      # c.The surveyed households headed by None in % -----------------------------
      None_HoH <- df %>% 
        select(Village, HofH) %>% 
        filter(HofH=="No") %>% 
        group_by(Village) %>% 
        summarise(None_Resp = n()) %>% 
        mutate(Total_Household = Respondend$Total_Household[match(Village, Respondend$Village)],
               None_HoH = round(None_Resp/Total_Household*100, digits = 1)) %>% 
        view()
      # Join two dataframe Male_HoH & Female_HoH -----------
      HofH_df <- Male_HoH %>% 
        left_join(Female_HoH, by = c("Village", "Total_Household")) %>% 
        left_join(None_HoH, by = c("Village", "Total_Household")) %>% 
        select(-Respondent_Gender.x, -Respondent_Gender.y) %>% 
        select(Village, Male_Resp, Female_Resp, None_Resp, Total_Household, HofH_Male, HofH_Female, None_HoH) %>% 
        pivot_longer(cols = c(6:8), names_to = "HeadHousehold_Perc", values_to = "Values") %>%
        mutate(HeadHousehold_Perc = ifelse(HeadHousehold_Perc== "HofH_Female", "Male", HeadHousehold_Perc),
               HeadHousehold_Perc = ifelse(HeadHousehold_Perc== "HofH_Male", "Female", HeadHousehold_Perc),
               HeadHousehold_Perc = ifelse(HeadHousehold_Perc== "None_HoH", "Neither", HeadHousehold_Perc)) %>% 
        filter(!is.na(Values)) %>% 
        mutate(HeadHousehold_Perc = fct_reorder(HeadHousehold_Perc,Values)) %>% 
        view()
      #write.csv(HofH_df, "Household_Head.csv")
      
      # Bar Chart -----------
      ggplot(data = HofH_df, aes(x = Village, y = Values, fill =  HeadHousehold_Perc))+
        geom_col(position = position_dodge2(preserve = "single", width = 0.9))+
        geom_text(aes(label = str_c(Values, "%")), position = position_dodge2(preserve = "single", width = 0.9), vjust = 2, size = 4)+
        scale_y_continuous(name = "Percentage (%)", breaks = breaks_width(5))+
        labs(title = "Head of Household", fill ="Gender", caption = "HBCL RAP 2020")+
        theme_light()+
        theme(plot.title = element_text(hjust = 0.5,face = "bold"))+
        theme(plot.caption = element_text(size = 5))+
        theme(axis.title.x = element_text(face = "bold"))+
        theme(axis.title.y = element_text(face = "bold"))+
        theme(legend.title = element_text(face = "bold"))
