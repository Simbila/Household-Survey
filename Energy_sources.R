 # Household Survey data analysis
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
      
      
      # 1. MAIN SOURCE OF ENERGY FOR LIGHTING --------------------------------------------------------
      # create data frame
      df_LightEnergy <- df %>% 
        select(Village, Lighting_enrg) %>% 
        separate(Lighting_enrg, into = c("Number", "Lighting_enrg"), sep = "[0-9]. ") %>% 
        mutate(Lighting_enrg = ifelse(is.na(Lighting_enrg), Number, Lighting_enrg)) %>%
        select(-Number) %>% 
        view()
      
      df_lumbesa <- df_LightEnergy %>% 
        filter(str_detect(Lighting_enrg, pattern = "Lumbesa")| str_detect(Lighting_enrg, pattern = "lumbesa")|str_detect(Lighting_enrg, pattern = "Local")) %>%
        mutate(Lighting_enrg = ifelse(str_detect(Lighting_enrg, pattern = "Lumbesa")| str_detect(Lighting_enrg, pattern = "lumbesa")|str_detect(Lighting_enrg, pattern = "Local"),
                                      "Lumbesa", Lighting_enrg)) %>% 
        group_by(Village) %>% 
        summarise(Lumbesa = n()) %>% 
        view()
      
      df_flashlight <- df_LightEnergy %>% 
        filter(str_detect(Lighting_enrg, pattern = "Flash light")| str_detect(Lighting_enrg, pattern = "Tourch")|str_detect(Lighting_enrg, pattern = "Torch")|
                 str_detect(Lighting_enrg, pattern = "Charg")|str_detect(Lighting_enrg, pattern = "flash light")|str_detect(Lighting_enrg, pattern = "Battery powered lights")|
                 str_detect(Lighting_enrg, pattern = "Flash lamp")|str_detect(Lighting_enrg, pattern = "Flashlight")) %>% 
        mutate(Lighting_enrg = ifelse(str_detect(Lighting_enrg, pattern = "Flash light")| str_detect(Lighting_enrg, pattern = "Tourch")|str_detect(Lighting_enrg, pattern = "Torch")|
                                        str_detect(Lighting_enrg, pattern = "Charg")|str_detect(Lighting_enrg, pattern = "flash light")|str_detect(Lighting_enrg, pattern = "Battery powered lights")|
                                        str_detect(Lighting_enrg, pattern = "Flash lamp")|str_detect(Lighting_enrg, pattern = "Flashlight"), "Flash light", Lighting_enrg)) %>% 
        group_by(Village) %>% 
        summarise(Flash_Light = n()) %>% 
        view()
      
      df_solar <- df_LightEnergy %>% 
        filter(str_detect(Lighting_enrg, pattern = "Solar")) %>% 
        mutate(Lighting_enrg = ifelse(str_detect(Lighting_enrg, pattern = "Solar"), "Solar", Lighting_enrg)) %>% 
        group_by(Village) %>% 
        summarise(Solar = n()) %>% 
        view()
      
      df_wicklamp <- df_LightEnergy %>% 
        filter(str_detect(Lighting_enrg, pattern = "Wick lamp")|str_detect(Lighting_enrg, pattern = "Wicklamp")) %>% 
        mutate(Lighting_enrg= ifelse(str_detect(Lighting_enrg, pattern = "Wick lamp")|str_detect(Lighting_enrg, pattern = "Wicklamp"), "Wick Lamp", Lighting_enrg)) %>% 
        group_by(Village) %>% 
        summarise(Wicklamp = n()) %>% 
        view()
      
      
      df_pressurelamp <- df_LightEnergy %>% 
        filter(str_detect(Lighting_enrg, pattern = "Pressure lamp")|str_detect(Lighting_enrg, pattern = "Hurricane lamp")) %>% 
        mutate(Lighting_enrg = ifelse(str_detect(Lighting_enrg, pattern = "Pressure lamp")|str_detect(Lighting_enrg, pattern = "Hurricane lamp"), "Pressure Lamp", Lighting_enrg)) %>%
        group_by(Village) %>% 
        summarise(Pressurelamp= n()) %>% 
        view()
      
      df_candle <- df_LightEnergy %>% 
        filter(Lighting_enrg == "Candles") %>%
        group_by(Village) %>% 
        summarise(Candle = n()) %>% 
        view()
      
      df_generator <- df_LightEnergy %>% 
        filter(str_detect(Lighting_enrg, pattern = "generator")) %>% 
        mutate(Lighting_enrg = ifelse(str_detect(Lighting_enrg, pattern = "generator"), "Generator", Lighting_enrg)) %>%
        group_by(Village) %>% 
        summarise(generator = n()) %>% 
        view()
      
      
      df_biomass <- df_LightEnergy %>% 
        filter(str_detect(Lighting_enrg, pattern = "biomass")) %>% 
        mutate(Lighting_enrg = ifelse(str_detect(Lighting_enrg, pattern = "biomass"), "Firewood", Lighting_enrg)) %>% 
        group_by(Village) %>% 
        summarise(Firewood = n()) %>% 
        view()
      
      
      # Join all Energy Source for Lighting dataframe
      df_lighting <- df_biomass %>% 
        mutate(Lumbesa = df_lumbesa$Lumbesa[match(Village,df_lumbesa$Village)],
               Flash_Light = df_flashlight$Flash_Light[match(Village,df_flashlight$Village)],
               Solar = df_solar$Solar[match(Village,df_solar$Village)],
               Wicklamp= df_wicklamp$Wicklamp[match(Village,df_wicklamp$Village)],
               Pressulamp = df_pressurelamp$Pressurelamp[match(Village,df_pressurelamp$Village)],
               Candle = df_candle$Candle[match(Village,df_candle$Village)],
               generator = df_generator$generator[match(Village,df_generator$Village)],
               Firewood = df_biomass$Firewood[match(Village, df_biomass$Village)])%>% 
        replace(is.na(.),0) %>% 
        mutate(Total = rowSums(.[2:9])) %>% 
        mutate(Firewood_P = round(Firewood/Total*100, digits = 2),
               Lumbesa_P = round(Lumbesa/Total*100, digits = 2),
               Flash_light_P = round(Flash_Light/Total*100, digits = 2),
               Solar_P = round(Solar/Total*100, digits = 2),
               Wicklamp_P = round(Wicklamp/Total*100, digits = 2),
               Pressurelamp_P = round(Pressulamp/Total*100, digits = 2),
               Candle_p = round(Candle/Total*100, digits = 2),
               Generator_P = round(generator/Total*100, digits = 2)) %>% 
        pivot_longer(cols = 11:18, names_to = "Energy", values_to = "Percentages") %>% 
        mutate(Energy = ifelse(Energy == "Firewood_P", "Firewood(biomass)", Energy),
               Energy= ifelse(Energy == "Lumbesa_P", "Local Made (Lumbesa)", Energy),
               Energy = ifelse(Energy == "Flash_light_P", "Flash Light", Energy),
               Energy = ifelse(Energy == "Solar_P", "Solar", Energy),
               Energy = ifelse(Energy == "Wicklamp_P", "Wick lamp", Energy),
               Energy = ifelse(Energy == "Pressurelamp_P", "Pressure Lamp", Energy),
               Energy = ifelse(Energy == "Candle_p", "Candle", Energy),
               Energy = ifelse(Energy == "Generator_P", "Generator", Energy)) %>% 
        
      # BAR CHART - Energy for Lighting
      df_lighting %>% 
        filter(Percentages > 0) %>% 
        mutate(Energy = as.factor(Energy)) %>% 
        
        ggplot(aes(x = Village, y = Percentages, fill = Energy))+
        geom_col(position = position_dodge2(width = 0.9, preserve = "single"))+
        geom_text(aes(label = ifelse(Percentages >= 5, str_c(Percentages, "%"), "")), position = position_dodge2(width = 0.9, preserve = "single"), 
                      size =4, angle = 90, hjust = 1)+
        geom_text(aes(label = ifelse(Percentages < 5, str_c(Percentages, "%"), "")), position = position_dodge2(width = 0.9, preserve = "single"), 
                      size =4, angle = 90, hjust = -0.1)+
        scale_y_continuous(breaks = breaks_width(5))+
        labs(title = "Energy Sources for Lighting",caption = "HBCL RAP 2020")+
        theme_light()+
        theme(plot.title = element_text(size = 15, hjust = 0.5, face = "bold"))+
        theme(plot.caption = element_text(size = 5))+
        theme(legend.title = element_text(face = "bold"))+
        theme(axis.title.x = element_text(face = "bold"), axis.title.y = element_text(face = "bold"))
      
      # PIE CHART - Energy for Lighting      
      # Kisewe Lighting Energy Sources 
      df_lighting %>% 
        filter(Village == "Kisewe") %>% 
        filter(Percentages > 0) %>% 
        ggplot(aes(x = "", y = Percentages, fill = Energy))+
        geom_bar(stat = "identity")+
        coord_polar("y", start = 0)+
        geom_text(aes(label = ifelse(Percentages > 4, str_c(Percentages, "%"), "")), position = position_stack(vjust = 0.5))+
        geom_text(aes(label = ifelse(Percentages == 2.1, str_c(Percentages, "%"), "")), position = position_stack(vjust = 0.5), angle = 90, hjust = -1.5)+
        geom_text(aes(label = ifelse(Percentages < 2, str_c(Percentages, "%"), "")), position = position_stack(vjust = 0.5), angle = 70, hjust = 2, vjust = -0.25)+
        labs(title = "Energy Source for Lighting - Kisewe Village", caption = "HBCL RAP 2020")+
        theme_void()+
        theme(plot.title = element_text(size = 15, hjust = 0.5, vjust = -10, face = "bold"))+
        theme(plot.caption = element_text(size = 5, hjust = 0.5, vjust = 25))+
        theme(legend.title = element_text(face = "bold", size = 10))
      
      # Makanga Lighting Energy Sources
      df_lighting %>% 
        filter(Village == "Makanga") %>% 
        filter(Percentages > 0) %>% 
        ggplot(aes(x = "", y = Percentages, fill = Energy))+
        geom_bar(stat = "identity")+
        coord_polar("y", start = 0)+
        geom_text(aes(label = ifelse(Percentages > 4, str_c(Percentages, "%"), "")), position = position_stack(vjust = 0.5))+
        geom_text(aes(label = ifelse(Percentages == 2.1, str_c(Percentages, "%"), "")), position = position_stack(vjust = 0.5), angle = 90, hjust = -1.5)+
        geom_text(aes(label = ifelse(Percentages < 2, str_c(Percentages, "%"), "")), position = position_stack(vjust = 0.5), angle = 70, hjust = 2, vjust = -0.25)+
        labs(title = "Energy Source for Lighting - Makanga Village", caption = "HBCL RAP 2020")+
        theme_void()+
        theme(plot.title = element_text(size = 15, hjust = 0.5, vjust = -10, face = "bold"))+
        theme(plot.caption = element_text(size = 5, hjust = 0.5, vjust = 25))+
        theme(legend.title = element_text(face = "bold", size = 10))
      
      # Mdindo Lighting Energy Sources 
      df_lighting %>% 
        filter(Village == "Mdindo") %>% 
        filter(Percentages > 0) %>% 
        ggplot(aes(x = "", y = Percentages, fill = Energy))+
        geom_bar(stat = "identity")+
        coord_polar("y", start = 0)+
        geom_text(aes(label = ifelse(Percentages > 4, str_c(Percentages, "%"), "")), position = position_stack(vjust = 0.5), vjust = -2)+
        geom_text(aes(label = ifelse(Percentages == 2.54, str_c(Percentages, "%"), "")), position = position_stack(vjust = 0.5), hjust = 0.5, angle = 100)+
        geom_text(aes(label = ifelse(Percentages < 2, str_c(Percentages, "%"), "")), position = position_stack(vjust = 0.5), hjust = -1.5, vjust = 0.5, angle = 90)+
        labs(title = "Energy Source for Lighting - Mdindo Village", caption = "HBCL RAP 2020")+
        theme_void()+
        theme(plot.title = element_text(size = 15, hjust = 0.5, vjust = -10, face = "bold"))+
        theme(plot.caption = element_text(size = 5, hjust = 0.5, vjust = 25))+
        theme(legend.title = element_text(face = "bold", size = 10))
      
      # Nawenge Lighting Energy Sources
      df_lighting %>% 
        filter(Village == "Nawenge") %>% 
        filter(Percentages > 0) %>% 
        ggplot(aes(x = "", y = Percentages, fill = Energy))+
        geom_bar(stat = "identity")+
        coord_polar("y", start = 0)+
        geom_text(aes(label =str_c(Percentages, "%")), position = position_stack(vjust = 0.5), vjust = -15)+
        labs(title = "Energy Source for Lighting - Nawenge Village", caption = "HBCL RAP 2020")+
        theme_void()+
        theme(plot.title = element_text(size = 15, hjust = 0.5, vjust = -10, face = "bold"))+
        theme(plot.caption = element_text(size = 5, hjust = 0.5, vjust = 25))+
        theme(legend.title = element_text(face = "bold", size = 10))
      
      
      # 2. MAIN SOURCE OF ENERGY FOR COOKING--------------------------------------------------------
      # Create data frame for cooking energy
      df_CookingEnergy <- df %>% 
        select(Village, Cooking_enrg) %>% 
        mutate(Cooking_enrg = ifelse(Cooking_enrg == "7. Firewood (biomass)", "Firewood (biomass)", Cooking_enrg),
               Cooking_enrg = ifelse(Cooking_enrg == "Torch" ,"Firewood (biomass)",  Cooking_enrg),
               Cooking_enrg = ifelse(Cooking_enrg == "Lumbesa (battery powered) and flashlight", "Firewood (biomass)", Cooking_enrg),
               Cooking_enrg = ifelse(Cooking_enrg == "Solar" ,"Gas (biogas)",  Cooking_enrg)) %>%
        mutate_all(na_if,"-") %>%
        group_by(Village, Cooking_enrg) %>% 
        summarise(CookE_Per = n()) %>% 
        filter(!is.na(Cooking_enrg)) %>%
        mutate(CookE_Perc = round(CookE_Per/sum(CookE_Per)*100, digits = 2)) %>% 
        
      
      # BAR CHART - Energy for Cooking
      df_CookingEnergy %>% 
        ggplot(aes(x = Village, y = CookE_Perc, fill = Cooking_enrg))+
        geom_col(position = position_dodge2(width = 0.9, preserve = "single"))+
        geom_text(aes(label = ifelse(CookE_Perc >= 5, str_c(CookE_Perc, "%"), "")), position = position_dodge2(width = 0.9, preserve = "single"), 
                  size =4, angle = 90, hjust = 1)+
        geom_text(aes(label = ifelse(CookE_Perc < 5, str_c(CookE_Perc, "%"), "")), position = position_dodge2(width = 0.9, preserve = "single"), 
                  size =4, angle = 90, hjust = -0.1)+
        scale_y_continuous(name = "Percentages", breaks = breaks_width(5))+
        labs(title = "Energy Sources for Cooking",caption = "HBCL RAP 2020", fill = "Energy Sources")+
        theme_light()+
        theme(plot.title = element_text(size = 15, hjust = 0.5, face = "bold"))+
        theme(plot.caption = element_text(size = 5))+
        theme(legend.title = element_text(face = "bold"))+
        theme(axis.title.x = element_text(face = "bold"), axis.title.y = element_text(face = "bold"))
      
      # 1.Kisewe Energy Source for Cooking - Pie Chart 
      df_CookingEnergy %>% 
        filter(Village == "Kisewe") %>% 
        mutate(Cooking_enrg = fct_reorder(Cooking_enrg, -CookE_Perc)) %>% 
        ggplot(aes(x = "", y = CookE_Perc, fill = Cooking_enrg))+
        geom_bar(stat = "identity")+
        coord_polar("y", start = 0)+
        geom_text(aes(label = ifelse(CookE_Perc > 2, str_c(CookE_Perc, "%"), "")), position = position_stack(vjust = 0.5), size = 5, vjust = -8, hjust = 0.1, colour = "black")+
        geom_label_repel(aes(label = ifelse(CookE_Perc < 2, str_c(CookE_Perc, "%"), "")), size = 5,vjust = -8, hjust = 0.7, nudge_x = -0.05, show.legend = F)+
        labs(title = "Household Energy Sources for Cooking - Kisewe Village", caption = "HBCL RAP 2020", fill = "Energy Sources")+
        theme_void()+
        theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold", vjust = -5))+
        theme(legend.title = element_text(face = "bold"))+
        theme(plot.caption = element_text(size = 5, vjust = 35, hjust = 0.5))
      
      
      # 2.Makanga Energy Source for Cooking - Pie Chart
      df_CookingEnergy %>% 
        filter(Village == "Makanga") %>% 
        ggplot(aes(x = "", y = CookE_Perc, fill = Cooking_enrg))+
        geom_bar(stat = "identity")+
        coord_polar("y", start = 0)+
        geom_text(aes(label = str_c(CookE_Perc, "%")), position = position_stack(vjust = 0.5), size = 5, colour = "white")+
        labs(title = "Household Energy Sources for Cooking - Makanga Village", caption = "HBCL RAP 2020", fill = "Energy Sources")+
        theme_void()+
        theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold", vjust = -5))+
        theme(legend.title = element_text(face = "bold"))+
        theme(plot.caption = element_text(size = 5, vjust = 35, hjust = 0.5))
      
      
      # 3.Mdindo Energy Source for Cooking - Pie Chart 
      df_CookingEnergy %>% 
        filter(Village == "Mdindo") %>% 
        mutate(Cooking_enrg = fct_reorder(Cooking_enrg, -CookE_Perc)) %>% 
        ggplot(aes(x = "", y = CookE_Perc, fill = Cooking_enrg))+
        geom_bar(stat = "identity")+
        coord_polar("y", start = 0)+
        geom_text(aes(label = ifelse(CookE_Perc == 2.04, str_c(CookE_Perc, "%"), "")), position = position_stack(vjust = 0.5), size = 5, vjust = 1, hjust =-1, colour = "black",
                  angle = 85)+
        geom_text(aes(label = ifelse(CookE_Perc > 4, str_c(CookE_Perc, "%"), "")), position = position_stack(vjust = 0.5), size = 5, vjust = -7, hjust =- 0.3, colour = "black")+
        geom_label_repel(aes(label = ifelse(CookE_Perc < 2, str_c(CookE_Perc, "%"), "")), size = 5,vjust = -8, hjust = 1.5, nudge_x = -0.45, show.legend = F)+
        labs(title = "Household Energy Sources for Cooking - Mdindo Village", caption = "HBCL RAP 2020", fill = "Energy Sources")+
        theme_void()+
        theme(plot.title = element_text(hjust = 0.5, size = 15, face = "bold", vjust = -10))+
        theme(legend.title = element_text(face = "bold"))+
        theme(plot.caption = element_text(size = 5, vjust = 35, hjust = 0.5))
      
      
      # 4.Nawenge Energy Source for Cooking
      df_CookingEnergy %>% 
        filter(Village == "Nawenge") %>% 
        ggplot(aes(x = "", y = CookE_Perc, fill = Cooking_enrg))+
        geom_bar(stat = "identity")+
        coord_polar("y", start = 0)+
        geom_text(aes(label = str_c(CookE_Perc, "%")), position = position_stack(vjust = 0.5), vjust = -10,size = 5, colour = "black")+
        labs(title = "Household Energy Sources for Cooking - Nawenge Village", caption = "HBCL RAP 2020", fill = "Energy Sources")+
        theme_void()+
        theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold", vjust = -5))+
        theme(legend.title = element_text(face = "bold"))+
        theme(plot.caption = element_text(size = 5, vjust = 35, hjust = 0.5))
