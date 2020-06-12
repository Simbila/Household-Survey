 # Household Survey data analysis - Houses Construction Materials
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
      
      # Houses Construction Materilas --------------
      
      df_HouseConstruction <- df %>% 
        select(Village,Roof_constr, Walls_constr) %>%
        mutate(Roof_constr = ifelse(Roof_constr == "5. Grass / leaves", "Grass / leaves" , Roof_constr)) %>%
        mutate(Walls_constr = ifelse(Walls_constr == "2. Poles and mud", "Poles and mud", Walls_constr),
               Walls_constr = ifelse(Walls_constr == "8. Other", "Other", Walls_constr)) %>% 
        view()
      
      # 1.Burnt bricks wall, roofed with iron sheets %?
      df_CISB <- df_HouseConstruction %>% 
        filter(Roof_constr == "Iron Sheets" & Walls_constr == "Baled bricks") %>%
        group_by(Village, Walls_constr, Roof_constr) %>% 
        summarise(House_constr = n()) %>% 
        mutate(Total_Household = Respondend$Total_Household[match(Village,Respondend$Village)]) %>%
        ungroup() %>% 
        mutate(Iron_Bailed_Perc = round(House_constr/Total_Household*100, digits = 2)) %>% 
        view()
      
      # write.csv(df_CISB, "Burntbricks_IronSheet.csv")
      
      # 2. Burnt bricks wall roofed with grass %?
      df_GRASSB <- df_HouseConstruction %>% 
        filter(Roof_constr == "Grass / leaves" & Walls_constr == "Baled bricks") %>%
        group_by(Village, Walls_constr, Roof_constr) %>% 
        summarise(House_constr = n()) %>% 
        mutate(Total_Household = Respondend$Total_Household[match(Village,Respondend$Village)]) %>%
        ungroup() %>% 
        mutate(Grass_Bailed_Perc = round(House_constr/Total_Household*100, digits = 2)) %>% 
        view()
        
      # write.csv(df_GRASSB, "BurntBricks_Grass.csv")
      
      # 3.Poles and mud Grass roofed %
      df_PolMud <- df_HouseConstruction %>% 
        filter(Roof_constr == "Grass / leaves" & Walls_constr == "Poles and mud") %>%
        group_by(Village, Walls_constr, Roof_constr) %>% 
        summarise(House_constr = n()) %>% 
        mutate(Total_Household = Respondend$Total_Household[match(Village,Respondend$Village)]) %>%
        ungroup() %>% 
        mutate(Mud_Poles_Perc = round(House_constr/Total_Household*100, digits = 2)) %>% 
        view()
        
      # write.csv(df_PolMud, "Poles_Grass.csv")
      
      # 4.Mud bricks wall roofed with grasses %?
      df_MudGrass <- df_HouseConstruction %>% 
        filter(Roof_constr == "Grass / leaves" & Walls_constr == "Sun-dried bricks") %>% 
        group_by(Village, Walls_constr, Roof_constr) %>% 
        summarise(House_constr = n()) %>% 
        mutate(Total_Household = Respondend$Total_Household[match(Village,Respondend$Village)]) %>%
        ungroup() %>% 
        mutate(Mud_Grass_Perc = round(House_constr/Total_Household*100, digits = 2)) %>% 
        view()
        
      # write.csv(df_MudGrass, "Mudbricks_Grass.csv")
      
      # Join data 4 frame to have one dataframe containing all construction material
      df_roof_wall <- df_PolMud %>% 
        mutate(BBricks_IS = df_CISB$House_constr[match(Village,df_CISB$Village)],
               BBricks_Grass = df_GRASSB$House_constr[match(Village, df_GRASSB$Village)],
               MudBricks_Grass = df_MudGrass$House_constr[match(Village, df_MudGrass$Village)]) %>% 
        rename(PoleMud = House_constr) %>% 
        select(-Walls_constr, -Roof_constr, -Total_Household, -Mud_Poles_Perc) %>%
        replace(is.na(.), 0) %>% 
        mutate(Total = rowSums(.[2:5])) %>% 
        mutate(PoleMud_Perc = round(PoleMud/Total*100, digits = 2),
               BBricks_IS_Perc = round(BBricks_IS/Total*100, digits = 2),
               BBricks_Grass_Perc = round(BBricks_Grass/Total*100, digits = 2),
               MudBricks_Grass_Perc = round(MudBricks_Grass/Total*100, digits = 2)) %>% 
        pivot_longer(cols = 7:10, names_to = "Materials", values_to = "Percentages") %>% 
        mutate(Materials = ifelse(Materials == "PoleMud_Perc", "Poles and Mud, Grass/Leaves", Materials),
               Materials = ifelse(Materials == "BBricks_IS_Perc", "Baled Bricks, Iron Sheet", Materials),
               Materials = ifelse(Materials == "BBricks_Grass_Perc", "Baled Bricks, Grass/Leaves", Materials),
               Materials = ifelse(Materials == "MudBricks_Grass_Perc", "Sun-dried bricks, Grass/Leaves", Materials)) %>% 
        view()
      
      # Kisewe - Pie Chat
      df_roof_wall %>% 
        filter(Village == "Kisewe") %>% 
        ggplot( aes(x= "", y= Percentages, fill = Materials))+
        geom_bar(stat = "Identity")+
        coord_polar("y", start = 0)+
        geom_text(aes(label = ifelse(Percentages > 10, str_c(Percentages, "%"), "")), position = position_stack(vjust = 0.5))+
        geom_label_repel(aes(label = ifelse(Percentages < 10, str_c(Percentages, "%"), "")), size = 3, show.legend = F, nudge_x = 0.5, hjust = 2)+
        labs(title = "Houses Walls & Roofs Construction Materials - Kisewe Village", fill = "Construction Materials \n (Walls, Roofs)", 
             caption = "HBCL RAP 2020")+
        theme_void()+
        theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 10, vjust = 0.1))+
        theme(legend.title = element_text(face = "bold"))+
        theme(plot.caption = element_text(vjust = 25, face = "bold", size = 5, hjust = 0.5))
      
      
      # Makanga - Pie Chat
      df_roof_wall %>% 
        filter(Village == "Makanga") %>% 
        ggplot( aes(x= "", y= Percentages, fill = Materials))+
        geom_bar(stat = "Identity")+
        coord_polar("y", start = 0)+
        geom_text(aes(label = ifelse(Percentages > 10, str_c(Percentages, "%"), "")), position = position_stack(vjust = 0.5))+
        # geom_label_repel(aes(label = ifelse(Percentages < 10, str_c(Percentages, "%"), "")), size = 3, show.legend = F, nudge_x = 0.5, hjust = 2)+
        labs(title = "Houses Walls & Roofs Construction Materials - Makanga Village", fill = "Construction Materials \n (Walls, Roofs)", 
             caption = "HBCL RAP 2020")+
        theme_void()+
        theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 10, vjust = 0.1))+
        theme(legend.title = element_text(face = "bold"))+
        theme(plot.caption = element_text(vjust = 25, face = "bold", size = 5, hjust = 0.5))
      
      # Mdindo - Pie Chat
      df_roof_wall %>% 
        filter(Village == "Mdindo") %>% 
        ggplot( aes(x= "", y= Percentages, fill = Materials))+
        geom_bar(stat = "Identity")+
        coord_polar("y", start = 0)+
        geom_text(aes(label = ifelse(Percentages > 10, str_c(Percentages, "%"), "")), position = position_stack(vjust = 0.5))+
        geom_label_repel(aes(label = ifelse(Percentages < 10, str_c(Percentages, "%"), "")), size = 3, show.legend = F, nudge_x = 0.5, hjust = -2)+
        labs(title = "Houses Walls & Roofs Construction Materials - Mdindo Village", fill = "Construction Materials \n (Walls, Roofs)", 
             caption = "HBCL RAP 2020")+
        theme_void()+
        theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 10, vjust = 0.1))+
        theme(legend.title = element_text(face = "bold"))+
        theme(plot.caption = element_text(vjust = 25, face = "bold", size = 5, hjust = 0.5))
      
      # Nawenge - Pie Chat
      df_roof_wall %>% 
        filter(Village == "Nawenge") %>% 
        filter(Percentages > 0) %>% 
        ggplot( aes(x= "", y= Percentages, fill = Materials))+
        geom_bar(stat = "Identity")+
        coord_polar("y", start = 0)+
        geom_text(aes(label = ifelse(Percentages > 10, str_c(Percentages, "%"), "")), position = position_stack(vjust = 0.5), vjust = -10)+
        #geom_label_repel(aes(label = ifelse(Percentages < 10, str_c(Percentages, "%"), "")), size = 3, show.legend = F, nudge_x = 0.5, hjust = -2)+
        labs(title = "Houses Walls & Roofs Construction Materials - Mdindo Village", fill = "Construction Materials \n (Walls, Roofs)", 
             caption = "HBCL RAP 2020")+
        theme_void()+
        theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 10, vjust = 0.1))+
        theme(legend.title = element_text(face = "bold"))+
        theme(plot.caption = element_text(vjust = 25, face = "bold", size = 5, hjust = 0.5))
      
