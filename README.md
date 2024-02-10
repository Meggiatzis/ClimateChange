# ClimateChange
Analysis code examining the relationship between one's political affiliation and their intention to participate in climate change related activism

    ## load libraries and import data files
    climate_data <- read_csv("Climate Change Data.csv")

    ## calculate descriptive stats
    summarise(climate_data, mean_age = mean(age))

    filtered_climate_data <- climate_data %>%
    select(political_party, activism_intentions_1, activism_intentions_2, activism_intentions_3,   activism_intentions_4, activism_intentions_5, activism_intentions_6)
    climate_data_means <- filtered_climate_data %>%

    filter(political_party %in% c("1", "3")) %>% 
    mutate(political_party = recode(political_party, "1" = "Republican",
                                   "3" = "Democrat"))
    climate_means <- climate_data_means %>%
    mutate(party_m = activism_intentions_1 + activism_intentions_2 + activism_intentions_3 +   activism_intentions_4 + activism_intentions_5 + activism_intentions_6/6)
  
    party_means <- climate_means %>%
    group_by(political_party) %>%
    summarise(mean_intention = mean(party_m), sd = sd(party_m), sum = n())

    ## data visualizations
    ggplot(party_means, aes(x = political_party, y = mean_intention, fill = political_party)) +
    geom_col(show.legend = FALSE, position = position_dodge(.9)) +
    geom_errorbar(aes(x = political_party, ymin = mean_intention-sd, ymax = mean_intention+sd), width = .2, colour = "black", alpha = .9, size= .3) + 
    labs(x = "Political Party", y = "Mean Activism Intentions", title = "Mean Activism Intentions by Political Party")

    ggplot(climate_means, aes(x = political_party, y = party_m, fill = political_party)) +
    geom_violin(trim = FALSE, show.legend = FALSE, alpha = .4) +
    geom_boxplot(width = .2, show.legend = FALSE, alpha = .7) +
    scale_x_discrete(name = "Political Party", 
                   labels = c("Democrat", "Republican")) +
    scale_y_continuous(name = "Mean Activism Intentions") +
    labs(title = "Mean Activism Intentions by Political Party") +
    theme_minimal() +
    scale_fill_viridis_d()

  
