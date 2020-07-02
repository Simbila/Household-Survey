      # Household Survey data analysis - WATER SORCES
      # Harmonic Biosphere Company Limited 
      # W. J. Simbila
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
      
      # WATER SERVICES AND DISTANCE 
      # 1 (a).Show percentage of each sources from each of the affected village - Dry Season
      df_Water_dryseason <- df %>% 
        select(Village, Water_Dryseason, Dryseason_dist_km) %>%
        mutate_all(na_if,"-") %>% 
        separate(Water_Dryseason,into = c("No_code","Water_Dryseason"),sep = "[0-9].") %>%
        mutate(Water_Dryseason = ifelse(is.na(Water_Dryseason), No_code, Water_Dryseason)) %>%
        select(-No_code) %>% 
        filter(!is.na(Water_Dryseason)) %>% 
        group_by(Village, Water_Dryseason) %>% 
        summarise(Dry_Season = n()) %>% 
        mutate(Dry_Season_Perc = round(Dry_Season/sum(Dry_Season)*100, digits = 2)) %>%
        mutate(Water_Dryseason = ifelse(Water_Dryseason ==" Unprotected spring", "Unprotected spring", Water_Dryseason),
               Water_Dryseason = ifelse(Water_Dryseason =="Surface water (lake/dam/river/stream)", "Surface Water", Water_Dryseason),
               Water_Dryseason = ifelse(Water_Dryseason ==" Surface water (lake/dam/river/stream)", "Surface Water", Water_Dryseason)) %>% 
        view()
      
      # Bar Chart Water Sources Dry Season
      ggplot(data = df_Water_dryseason, aes(x = Village, y = Dry_Season_Perc, fill = Water_Dryseason))+
        geom_col(position = position_dodge2(preserve = "single", width = 0.9))+
        #geom_text(aes(y = -1, label = Water_Dryseason), position = position_dodge(width = 0.9), size = 3, angle = 90, va = "top")+
        #geom_text(aes(label = ifelse(Dry_Season_Perc > 2, str_c(Dry_Season_Perc, "%"), "")), position = position_dodge2(width= 0.9, preserve = "single"),
                  #angle = 90, hjust = 1, size = 4)+
        #geom_text(aes(label = ifelse(Dry_Season_Perc < 2, str_c(Dry_Season_Perc, "%"), "")), position = position_dodge2(width= 0.9, preserve = "single"), 
                  #angle = 90, hjust = -0.1, size = 4)+
        scale_y_continuous(name = "Percentages", breaks = breaks_width(5))+
        labs(title = "Water Sources - Dry Season", caption = "HBCL RAP 2020", fill = "Water Sources")+
        theme_light()+
        theme(plot.title = element_text(size = 15, face = "bold", hjust = 0.5))+
        theme(plot.caption = element_text(size = 5, hjust = 1))+
        theme(legend.title = element_text(face = "bold"))+
        theme(axis.title.x = element_text(face = "bold"), axis.title.y = element_text(face = "bold"))
      
      
      # PIE CHART
      # (i). Pie Chat plot Kisewe water Sources - Dry Season -------------------------------------------------------
      df_Water_dryseason %>% 
        filter(Village == "Kisewe") %>% 
        ggplot(aes(x = "", y = Dry_Season_Perc, fill = Water_Dryseason))+
        geom_bar(stat = "identity")+
        coord_polar("y", start = 0)+
        geom_text(aes(label = str_c(Dry_Season_Perc, "%")), position = position_stack(vjust = 0.1), size = 5, colour = "black")+
        labs(title = "Household Water Sources During Dry season - Kisewe Village", caption = "HBCL RAP 2020", fill = "Water Sources")+
        theme_void()+
        theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold", vjust = -5))+
        theme(legend.title = element_text(face = "bold"))+
        theme(plot.caption = element_text(size = 5, vjust = 35, hjust = 0.5))
      
      # (ii). Makanga Pie Chat plot Makanga water Sources - Dry Season -------------------------------------------------------
      df_Water_dryseason %>% 
        filter(Village == "Makanga") %>% 
        ggplot(aes(x = "", y = Dry_Season_Perc, fill = Water_Dryseason))+
        geom_bar(stat = "identity")+
        coord_polar("y", start = 0)+
        geom_text(aes(label = str_c(Dry_Season_Perc, "%")), position = position_stack(vjust = 0.5), size = 5, colour = "black")+
        labs(title = "Household Water Sources During Dry season - Makanga Village", caption = "HBCL RAP 2020", fill = "Water Sources")+
        theme_void()+
        theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold", vjust = -8))+
        theme(legend.title = element_text(face = "bold"))+
        theme(plot.caption = element_text(size = 5, vjust = 35, hjust = 0.5))
      
      # (ii). Mdindo Pie Chat plot Mdindo water Sources - Dry Season -------------------------------------------------------
      df_Water_dryseason %>% 
        filter(Village == "Mdindo") %>% 
        mutate(Water_Dryseason = fct_reorder(Water_Dryseason, -Dry_Season_Perc)) %>% 
        ggplot( aes(x = "", y = Dry_Season_Perc, fill =Water_Dryseason))+
        geom_bar(stat = "identity")+
        coord_polar("y", start = 0)+
        geom_text(aes(label = ifelse(Dry_Season_Perc > 2, str_c(Dry_Season_Perc, "%"), "")), position = position_stack(vjust = 0.5))+
        geom_label_repel(aes(label = ifelse(Dry_Season_Perc < 2, str_c(Dry_Season_Perc, "%"), ""), y = Dry_Season_Perc), size = 4, show.legend = F, nudge_x = 0.6)+
        labs(title = "Household Water Sources During Dry season - Mdindo Village", caption = "HBCL RAP 2020", fill = "Water Sources")+
        theme_void()+
        theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold", vjust = 0.5))+
        theme(legend.title = element_text(face = "bold"))+
        theme(plot.caption = element_text(size = 5, vjust = 35, hjust = 0.5))
      
      # (iv). Pie Chat plot Nawenge water Sources - Dry Season -------------------------------------------------------
      df_Water_dryseason %>% 
        filter(Village == "Nawenge") %>% 
        ggplot(aes(x = "", y = Dry_Season_Perc, fill = Water_Dryseason))+
        geom_bar(stat = "identity")+
        coord_polar("y", start = 0)+
        geom_text(aes(label = str_c(Dry_Season_Perc, "%")), position = position_stack(vjust = 0.5), size = 5, colour = "black")+
        labs(title = "Household Water Sources During Dry season - Nawenge Village", caption = "HBCL RAP 2020", fill = "Water Sources")+
        theme_void()+
        theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold", vjust = -8))+
        theme(legend.title = element_text(face = "bold"))+
        theme(plot.caption = element_text(size = 5, vjust = 35, hjust = 0.5))
      
      # write.csv(df_Water, "water_dryseason.csv")
      
      # 1 (b).Distance variation to the houses - Dry Season --------------------------------- 
      df_Water <- df %>% 
        select(Village, Water_Dryseason, Dryseason_dist_km) %>%
        mutate_all(na_if,"-") %>% 
        separate(Water_Dryseason,into = c("No_code","Water_Dryseason"),sep = "[0-9].") %>%
        mutate(Water_Dryseason = ifelse(is.na(Water_Dryseason), No_code, Water_Dryseason)) %>%
        select(-No_code) %>% 
        mutate(Dryseason_dist_km = as.numeric(Dryseason_dist_km)) %>% 
        filter(!is.na(Water_Dryseason)) %>% 
        group_by(Village, Water_Dryseason) %>% 
        summarise(Ave_dist = mean(Dryseason_dist_km, na.rm = TRUE)) %>% 
        view()
      
      
      # 2 (a).Show percentage of each sources from each of the affected village - Wet Season
      df_Water_wetseason <- df %>% 
        select(Village, Water_Wetseason, Wetseason_dist_km) %>%
        mutate_all(na_if,"-") %>% 
        separate(Water_Wetseason,into = c("No_code","Water_Wetseason"),sep = "[0-9].") %>%
        mutate(Water_Wetseason = ifelse(is.na(Water_Wetseason), No_code, Water_Wetseason)) %>%
        select(-No_code) %>% 
        filter(!is.na(Water_Wetseason)) %>% 
        group_by(Village, Water_Wetseason) %>% 
        summarise(Wet_Season = n()) %>% 
        mutate(Wet_Season_Perc = round(Wet_Season/sum(Wet_Season)*100, digits = 2)) %>%
        mutate(Water_Wetseason = ifelse(Water_Wetseason ==" Unprotected spring", "Unprotected spring", Water_Wetseason),
               Water_Wetseason = ifelse(Water_Wetseason =="Surface water (lake/dam/river/stream)", "Surface Water", Water_Wetseason),
               Water_Wetseason = ifelse(Water_Wetseason ==" Surface water (lake/dam/river/stream)", "Surface Water", Water_Wetseason)) %>% 
        view()
      
      
      # Bar Chart Water Sources Wet Season
      ggplot(data = df_Water_wetseason, aes(x = Village, y = Wet_Season_Perc, fill = Water_Wetseason))+
        geom_col(position = position_dodge2(preserve = "single", width = 0.9))+
        #geom_text(aes(y = -1, label = Water_Dryseason), position = position_dodge(width = 0.9), size = 3, angle = 90, va = "top")+
        #geom_text(aes(label = ifelse(Wet_Season_Perc > 2, str_c(Wet_Season_Perc, "%"), "")), position = position_dodge2(width= 0.9, preserve = "single"), 
                  #angle = 90, hjust = 1, size = 4)+
        #geom_text(aes(label = ifelse(Wet_Season_Perc < 2, str_c(Wet_Season_Perc, "%"), "")), position = position_dodge2(width= 0.9, preserve = "single"), 
                  #angle = 90, hjust = -0.1, size = 4)+
        scale_y_continuous(name = "Percentages", breaks = breaks_width(5))+
        labs(title = "Water Sources - Wet Season", caption = "HBCL RAP 2020", fill = "Water Sources")+
        theme_light()+
        theme(plot.title = element_text(size = 15, face = "bold", hjust = 0.5))+
        theme(plot.caption = element_text(size = 5, hjust = 1))+
        theme(legend.title = element_text(face = "bold"))+
        theme(axis.title.x = element_text(face = "bold"), axis.title.y = element_text(face = "bold"))
      
      
      # (i). Pie Chat plot Kisewe water Sources - Wet Season -------------------------------------------------------
      df_Water_wetseason %>% 
        filter(Village == "Kisewe") %>%
        mutate(Water_Wetseason = fct_reorder(Water_Wetseason, -Wet_Season_Perc)) %>% 
        ggplot(aes(x = "", y = Wet_Season_Perc, fill = Water_Wetseason))+
        geom_bar(stat = "identity")+
        coord_polar("y", start = 0)+
        geom_text(aes(label = ifelse(Wet_Season_Perc >1, str_c( Wet_Season_Perc, "%"),"")), position = position_stack(vjust = 0.5), size = 5, colour = "black")+
        geom_label_repel(aes(label = ifelse(Wet_Season_Perc < 1, str_c(Wet_Season_Perc, "%"), "")), size = 4, nudge_x = 0.55, show.legend = F)+
        labs(title = "Household Water Sources During Wet season - Kisewe Village", caption = "HBCL RAP 2020", fill = "Water Sources")+
        theme_void()+
        theme(plot.title = element_text(hjust = 0.5, size = 15, face = "bold", vjust = -2))+
        theme(legend.title = element_text(face = "bold"))+
        theme(plot.caption = element_text(size = 5, vjust = 35, hjust = 0.5))
      
      # (ii). Pie Chat plot Makanga water Sources - Wet Season -------------------------------------------------------
      df_Water_wetseason %>% 
        filter(Village == "Makanga") %>% 
        ggplot(aes(x = "", y = Wet_Season_Perc, fill = Water_Wetseason))+
        geom_bar(stat = "identity")+
        coord_polar("y", start = 0)+
        geom_text(aes(label = str_c( Wet_Season_Perc, "%")), position = position_stack(vjust = 0.5), size = 5, colour = "black")+
        labs(title = "Household Water Sources During Wet season - Makanga Village", caption = "HBCL RAP 2020", fill = "Water Sources")+
        theme_void()+
        theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold", vjust = -8))+
        theme(legend.title = element_text(face = "bold"))+
        theme(plot.caption = element_text(size = 5, vjust = 35, hjust = 0.5))
      
      
      # (iii). Mdindo Pie Chat plot Mdindo water Sources - Wet Season -------------------------------------------------------
      df_Water_wetseason %>% 
        filter(Village == "Mdindo") %>% 
        ggplot(aes(x = "", y = Wet_Season_Perc, fill = reorder(Water_Wetseason, -Wet_Season_Perc)))+
        geom_bar(stat = "identity")+
        coord_polar("y", start = 0)+
        geom_text(aes(label = ifelse (Wet_Season_Perc > 14, str_c( Wet_Season_Perc, "%"), "")), position = position_stack(vjust = 0.5), size = 5, colour = "black")+
        geom_label_repel(aes(label = ifelse(Wet_Season_Perc < 14, str_c(Wet_Season_Perc, "%"), "")), size = 3, nudge_x = 1.5, show.legend = F)+
        labs(title = "Household Water Sources During Wet season - Mdindo Village", caption = "HBCL RAP 2020", fill = "Water Sources")+
        theme_void()+
        theme(plot.title = element_text(hjust = 0.5, size = 15, face = "bold", vjust = 2))+
        theme(legend.title = element_text(face = "bold"))+
        theme(plot.caption = element_text(size = 5, vjust = 35, hjust = 0.5))
      
      
      # (iiv).Pie Chat plot Nawenge water Sources - Wet Season -------------------------------------------------------
      df_Water_wetseason %>% 
        filter(Village == "Nawenge") %>% 
        ggplot(aes(x = "", y = Wet_Season_Perc, fill = Water_Wetseason))+
        geom_bar(stat = "identity")+
        coord_polar("y", start = 0)+
        geom_text(aes(label = str_c( Wet_Season_Perc, "%")), position = position_stack(vjust = 0.5), size = 5, colour = "black")+
        labs(title = "Household Water Sources During Wet season - Nawenge Village", caption = "HBCL RAP 2020", fill = "Water Sources")+
        theme_void()+
        theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold", vjust = -8))+
        theme(legend.title = element_text(face = "bold"))+
        theme(plot.caption = element_text(size = 5, vjust = 35, hjust = 0.5))
      
      # write.csv(df_Water, "water_wetseason.csv")
      
      
      # b.(i). Pie Chat plot Nawenge water Sources - Wet Season -------------------------------------------------------
      df_Nawenge_Drywater <- df_Water %>% 
        filter(Village == "Nawenge") %>% 
        ggplot(data = df_Nawenge_Drywater, aes(x = "", y = Dry_Season_Perc, fill = Water_Dryseason))+
        geom_bar(stat = "identity")+
        coord_polar("y", start = 0)+
        geom_text(aes(label = str_c(Dry_Season_Perc, "%")), position = position_stack(vjust = 0.5), size = 5, colour = "white")+
        labs(title = "Household Water Sources During Dry season - Nawenge Village", caption = "HBCL RAP 2020", fill = "Water Sources")+
        theme_void()+
        theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold", vjust = -5))+
        theme(legend.title = element_text(face = "bold"))+
        theme(plot.caption = element_text(size = 5, vjust = 35, hjust = 0.5))
      
      # 2 (b).Distance variation to the houses - Wet Season  
      df_Water_wetseason <- df %>% 
        select(Village, Water_Wetseason, Wetseason_dist_km) %>%
        mutate_all(na_if,"-") %>% 
        separate(Water_Wetseason,into = c("No_code","Water_Wetseason"),sep = "[0-9].") %>%
        mutate(Water_Wetseason = ifelse(is.na(Water_Wetseason), No_code, Water_Wetseason)) %>%
        select(-No_code) %>% 
        mutate( Wetseason_dist_km = as.numeric(Wetseason_dist_km)) %>% 
        filter(!is.na(Water_Wetseason)) %>% 
        group_by(Village, Water_Wetseason) %>% 
        summarise(Ave_dist = mean(Wetseason_dist_km, na.rm = TRUE)) %>% 
        view()
