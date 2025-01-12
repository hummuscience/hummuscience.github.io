+++
title = "Simulating dipping MDMA"
author = ["muad abd el hay"]
lastmod = 2025-01-12T19:45:22+01:00
draft = true
+++

## Pharmacological models {#pharmacological-models}


### Plasma concentrations {#plasma-concentrations}

The benchmark dataset is a dataset digitzed from (NO_ITEM_DATA:straumann2023) where they gave participants 100mg MDMA orally.

Two additional datasets from (NO_ITEM_DATA:kirkpatrick2014) where 0.75 and 1.5mg/kg MDMA were given. This is around 50 and 100mg for a 70kg person

<a id="table--straumann2023-mdma-100mg-plasma"></a>

| time_h                | conc_ngml          |
|-----------------------|--------------------|
| -0.045145825009988094 | 0.696247835930194  |
| 0.41323744839525833   | 22.198778132907194 |
| 0.9129045145825012    | 101.07079837528298 |
| 1.367492342522306     | 168.58935943534425 |
| 1.8692235983486478    | 205.32985417498998 |
| 2.3821414302836574    | 216.97046877080834 |
| 2.8850046610733786    | 213.6710946863763  |
| 3.387867891863098     | 210.3717206019443  |
| 3.8566387002263953    | 208.5672193367958  |
| 4.867026235184444     | 191.51018777467036 |
| 5.892129444666399     | 178.93486149953387 |
| 6.8859368757491       | 161.57943800772404 |
| 7.910374217605539     | 150.49815221733917 |
| 8.885737115461446     | 137.02765015314952 |
| 9.910307630842986     | 125.64755626581433 |
| 10.916433612997732    | 118.15238380609932 |
| 11.922692768677585    | 110.35840324943399 |
| 23.98714875482754     | 40.7352843254761   |

<!--list-separator-->

-  Modelling plasma levels

    ```R

    ##install.packages("linpk")

    library(linpk)
    library(tidyverse)

    max_first  <- max(data$conc_ngml)   # Specify max of first y axis
    max_second <- max(dataeffects$effect_perc) # Specify max of second y axis
    min_first  <- min(data$conc_ngml)   # Specify min of first y axis
    min_second <- min(dataeffects$effect_perc) # Specify min of second y axis

    # scale and shift variables calculated based on desired mins and maxes
    scale = (max_second - min_second)/(max_first - min_first)
    shift = min_first - min_second

    # Function to scale secondary axis
    scale_function <- function(x, scale, shift){
      return ((x)*scale - shift)
    }

    # Function to scale secondary variable values
    inv_scale_function <- function(x, scale, shift){
      return ((x + shift)/scale)
    }

    data %>%
      mutate(time = plyr::round_any(time_h, 0.5)) %>%
      full_join(dataeffects %>% mutate(time = plyr::round_any(time_h, 0.5)) %>% select(time,effect_perc)) %>%
      ggplot(aes(x = time, y = conc_ngml)) +
      geom_path(aes(color = "Drug Concentration")) +
      geom_path(aes(y = inv_scale_function(effect_perc, scale, shift), color = "Effect (%)")) +
      scale_y_continuous(sec.axis = sec_axis(~scale_function(., scale, shift), name="Effect (%)")) +
      scale_color_manual(values = c("orange2", "gray30")) +
      labs(x = "Time (h)", y = "Concentration (ng/ml)", color = " ")+
      ggpubr::theme_pubr()+
          theme(
        axis.title.y = element_text(color = "gray30"),
        axis.title.y.right = element_text(color = "orange3")
      )



    max_first  <- max(redose$conc_ngml, na.rm = TRUE)   # Specify max of first y axis
    max_second <- max(redoseeffects$effect_score, na.rm = TRUE) # Specify max of second y axis
    min_first  <- min(redose$conc_ngml, na.rm = TRUE)   # Specify min of first y axis
    min_second <- min(redoseeffects$effect_score, na.rm = TRUE) # Specify min of second y axis

    # scale and shift variables calculated based on desired mins and maxes
    scale = (max_second - min_second)/(max_first - min_first)
    shift = min_first - min_second

    redose %>%
      mutate(time = plyr::round_any(time_h, 0.1)) %>%
      full_join(redoseeffects %>% mutate(time = plyr::round_any(time_h, 0.1)) %>% select(time,effect_score)) %>%
      ggplot(aes(x = time, y = conc_ngml)) +
      geom_path(aes(color = "Drug Concentration")) +
      geom_path(aes(y = inv_scale_function(effect_score, scale, shift), color = "Effect")) +
      scale_y_continuous(sec.axis = sec_axis(~scale_function(., scale, shift), name="Effect")) +
      scale_color_manual(values = c("orange2", "gray30")) +
      labs(x = "Time (h)", y = "Concentration (ng/ml)", color = " ")+
      geom_vline(xintercept = 2)+
      ggpubr::theme_pubr()+
          theme(
        axis.title.y = element_text(color = "gray30"),
        axis.title.y.right = element_text(color = "orange3")
      )







    t.obs <- seq(0, 24, 0.1)

    y <- pkprofile(t.obs, cl=21, vc=250, ka=1, dose=list(amt=100, f = 0.75))


    tibble(time_h = t.obs, conc_ngml = as.numeric(y*1000)) %>%
      mutate(type = "Simulation") %>%
      bind_rows(data %>% mutate(type = "Straumann2023")) %>%
      ggplot(aes(x = time_h, y = conc_ngml, color = type, group = type)) +
      geom_line() +
      geom_vline(xintercept = 2, color = "black")+
      labs(x = "Time (h)", y = "Concentration (ng/ml)") +
      ggpubr::theme_pubr()


    y2 <- pkprofile(t.obs, cl=21, vc=250, ka=1, dose=list(t.dose = c(0,2), amt=c(50,100), f = c(0.75, 0.75)))

    tibble(time_h = t.obs, conc_ngml = as.numeric(y2*1000)) %>%
      mutate(type = "Simulation") %>%
      bind_rows(redose %>% mutate(type = "Farre2012")) %>%
      ggplot(aes(x = time_h, y = conc_ngml, color = type, group = type)) +
      geom_line() +
      geom_vline(xintercept = 2, color = "black")+
      labs(x = "Time (h)", y = "Concentration (ng/ml)") +
      ggpubr::theme_pubr()

    ```


### Subjective effects {#subjective-effects}


#### Data {#data}

<a id="table--straumann2023-effects"></a>

| time                   | time_unit | effect_perc          | sex   | n_participants | origin        | dose | dose_unit | n_doses | dose_timing | measurment_point | datapoint_type | comment |
|------------------------|-----------|----------------------|-------|----------------|---------------|------|-----------|---------|-------------|------------------|----------------|---------|
| -0.48752407356457983   | h         | -0.3225681290695803  | mixed | 24             | straumann2023 | 100  | mg        | 1       | 1           | 0                | mean           |         |
| -0.0015661044210704134 | h         | -0.10123242541604327 | mixed | 24             | straumann2023 | 100  | mg        | 1       | 1           | 1                | mean           |         |
| 0.4774036196888751     | h         | 6.255918127571647    | mixed | 24             | straumann2023 | 100  | mg        | 1       | 1           | 2                | mean           |         |
| 0.946307292149255      | h         | 33.846554894100024   | mixed | 24             | straumann2023 | 100  | mg        | 1       | 1           | 3                | mean           |         |
| 1.4813026321616238     | h         | 54.832171678432      | mixed | 24             | straumann2023 | 100  | mg        | 1       | 1           | 4                | mean           |         |
| 1.9771291860242357     | h         | 57.84214122099438    | mixed | 24             | straumann2023 | 100  | mg        | 1       | 1           | 5                | mean           |         |
| 2.4654428839861575     | h         | 52.48965137240922    | mixed | 24             | straumann2023 | 100  | mg        | 1       | 1           | 6                | mean           |         |
| 2.961821158380178      | h         | 44.006505879531126   | mixed | 24             | straumann2023 | 100  | mg        | 1       | 1           | 7                | mean           |         |
| 3.4870504112919765     | h         | 28.783729269728937   | mixed | 24             | straumann2023 | 100  | mg        | 1       | 1           | 8                | mean           |         |
| 3.9857077521279614     | h         | 16.432501565630176   | mixed | 24             | straumann2023 | 100  | mg        | 1       | 1           | 9                | mean           |         |
| 4.98971095861949       | h         | 12.04830126701684    | mixed | 24             | straumann2023 | 100  | mg        | 1       | 1           | 10               | mean           |         |
| 5.9994350869959465     | h         | 8.511147902782326    | mixed | 24             | straumann2023 | 100  | mg        | 1       | 1           | 11               | mean           |         |
| 6.989779127879379      | h         | 6.196141633093944    | mixed | 24             | straumann2023 | 100  | mg        | 1       | 1           | 12               | mean           |         |
| 8.01790842895326       | h         | 3.091488230178129    | mixed | 24             | straumann2023 | 100  | mg        | 1       | 1           | 13               | mean           |         |
| 9.007095977160413      | h         | 2.7393070305094085   | mixed | 24             | straumann2023 | 100  | mg        | 1       | 1           | 14               | mean           |         |
| 10.058563519797456     | h         | 2.251362175311826    | mixed | 24             | straumann2023 | 100  | mg        | 1       | 1           | 15               | mean           |         |
| 11.016232460688663     | h         | 0.2462029022313601   | mixed | 24             | straumann2023 | 100  | mg        | 1       | 1           | 16               | mean           |         |
| 12.018052527988825     | h         | -0.06507798776743812 | mixed | 24             | straumann2023 | 100  | mg        | 1       | 1           | 17               | mean           |         |
| -0.5000000000000009    | h         | 1.748192851865923    | mixed | 24             | straumann2023 | 100  | mg        | 1       | 1           | 0                | sem            |         |
| -8.881784197001252e-16 | h         | 1.90302062205717     | mixed | 24             | straumann2023 | 100  | mg        | 1       | 1           | 1                | sem            |         |
| 0.5                    | h         | 8.268854702721924    | mixed | 24             | straumann2023 | 100  | mg        | 1       | 1           | 2                | sem            |         |
| 0.9913017713910817     | h         | 39.781345015625774   | mixed | 24             | straumann2023 | 100  | mg        | 1       | 1           | 3                | sem            |         |
| 1.4504031660705596     | h         | 60.58425571240114    | mixed | 24             | straumann2023 | 100  | mg        | 1       | 1           | 4                | sem            |         |
| 1.9640430890351528     | h         | 63.82316423638299    | mixed | 24             | straumann2023 | 100  | mg        | 1       | 1           | 5                | sem            |         |
| 2.4825717974222776     | h         | 58.76471750156962    | mixed | 24             | straumann2023 | 100  | mg        | 1       | 1           | 6                | sem            |         |
| 3                      | h         | 49.9775014670815     | mixed | 24             | straumann2023 | 100  | mg        | 1       | 1           | 7                | sem            |         |
| 3.4838839389642544     | h         | 34.315499495601514   | mixed | 24             | straumann2023 | 100  | mg        | 1       | 1           | 8                | sem            |         |
| 4                      | h         | 20.59999982995666    | mixed | 24             | straumann2023 | 100  | mg        | 1       | 1           | 9                | sem            |         |
| 5                      | h         | 16.232274841080425   | mixed | 24             | straumann2023 | 100  | mg        | 1       | 1           | 10               | sem            |         |
| 6.000719561490762      | h         | 12.686151263112592   | mixed | 24             | straumann2023 | 100  | mg        | 1       | 1           | 11               | sem            |         |
| 6.999999999999998      | h         | 9.12020950741723     | mixed | 24             | straumann2023 | 100  | mg        | 1       | 1           | 12               | sem            |         |
| 7.999999999999998      | h         | 4.1737325363158675   | mixed | 24             | straumann2023 | 100  | mg        | 1       | 1           | 13               | sem            |         |
| 8.999999999999998      | h         | 3.549321140632003    | mixed | 24             | straumann2023 | 100  | mg        | 1       | 1           | 14               | sem            |         |
| 9.999999999999998      | h         | 2.659975590121661    | mixed | 24             | straumann2023 | 100  | mg        | 1       | 1           | 15               | sem            |         |
| 10.999999999999998     | h         | 1.6662118875244403   | mixed | 24             | straumann2023 | 100  | mg        | 1       | 1           | 16               | sem            |         |
| 12                     | h         | 1.624274934525559    | mixed | 24             | straumann2023 | 100  | mg        | 1       | 1           | 17               | sem            |         |

<a id="table--kilpatrick2014-effects"></a>

| time                  | time_unit | effect_perc        | sex   | n_participants | origin         | dose | dose_unit | n_doses | dose_timing | measurment_point | datapoint_type | comment                    |
|-----------------------|-----------|--------------------|-------|----------------|----------------|------|-----------|---------|-------------|------------------|----------------|----------------------------|
| -0.024590163934425924 | h         | 0.6235011990407742 | mixed | 40             | kilpatrick2014 | 100  | mg        | 1       | 1           | 0                | mean           | actual dose was 0.75 mg/kg |
| -0.032786885245901454 | h         | 2.302158273381295  | mixed | 40             | kilpatrick2014 | 100  | mg        | 1       | 1           | 0                | sem            | actual dose was 0.75 mg/kg |
| 0.4918032786885247    | h         | 9.976019184652273  | mixed | 40             | kilpatrick2014 | 100  | mg        | 1       | 1           | 1                | mean           | actual dose was 0.75 mg/kg |
| 0.4836065573770494    | h         | 11.414868105515566 | mixed | 40             | kilpatrick2014 | 100  | mg        | 1       | 1           | 1                | sem            | actual dose was 0.75 mg/kg |
| 0.9918032786885247    | h         | 77.60191846522781  | mixed | 40             | kilpatrick2014 | 100  | mg        | 1       | 1           | 2                | mean           | actual dose was 0.75 mg/kg |
| 0.9918032786885247    | h         | 80.71942446043164  | mixed | 40             | kilpatrick2014 | 100  | mg        | 1       | 1           | 2                | sem            | actual dose was 0.75 mg/kg |
| 1.483606557377049     | h         | 76.88249400479614  | mixed | 40             | kilpatrick2014 | 100  | mg        | 1       | 1           | 3                | mean           | actual dose was 0.75 mg/kg |
| 1.483606557377049     | h         | 80.2398081534772   | mixed | 40             | kilpatrick2014 | 100  | mg        | 1       | 1           | 3                | sem            | actual dose was 0.75 mg/kg |
| 1.983606557377049     | h         | 66.33093525179855  | mixed | 40             | kilpatrick2014 | 100  | mg        | 1       | 1           | 4                | mean           | actual dose was 0.75 mg/kg |
| 1.9672131147540983    | h         | 69.44844124700239  | mixed | 40             | kilpatrick2014 | 100  | mg        | 1       | 1           | 4                | sem            | actual dose was 0.75 mg/kg |
| 3.4754098360655727    | h         | 33.95683453237409  | mixed | 40             | kilpatrick2014 | 100  | mg        | 1       | 1           | 5                | mean           | actual dose was 0.75 mg/kg |
| 3.483606557377049     | h         | 36.115107913669064 | mixed | 40             | kilpatrick2014 | 100  | mg        | 1       | 1           | 5                | sem            | actual dose was 0.75 mg/kg |
| 3.9918032786885242    | h         | 16.930455635491597 | mixed | 40             | kilpatrick2014 | 100  | mg        | 1       | 1           | 6                | mean           | actual dose was 0.75 mg/kg |
| 3.983606557377049     | h         | 18.848920863309345 | mixed | 40             | kilpatrick2014 | 100  | mg        | 1       | 1           | 6                | sem            | actual dose was 0.75 mg/kg |
| -0.032786885245901454 | h         | 0.8633093525179731 | mixed | 40             | kilpatrick2014 | 50   | mg        | 1       | 1           | 0                | mean           | actual dose was 1.5 mg/kg  |
| -0.032786885245901454 | h         | 2.0623501199040675 | mixed | 40             | kilpatrick2014 | 50   | mg        | 1       | 1           | 0                | sem            | actual dose was 1.5 mg/kg  |
| 0.4918032786885247    | h         | 9.976019184652273  | mixed | 40             | kilpatrick2014 | 50   | mg        | 1       | 1           | 1                | mean           | actual dose was 1.5 mg/kg  |
| 0.4918032786885247    | h         | 11.414868105515566 | mixed | 40             | kilpatrick2014 | 50   | mg        | 1       | 1           | 1                | sem            | actual dose was 1.5 mg/kg  |
| 0.9918032786885247    | h         | 41.39088729016785  | mixed | 40             | kilpatrick2014 | 50   | mg        | 1       | 1           | 2                | mean           | actual dose was 1.5 mg/kg  |
| 1                     | h         | 44.98800959232614  | mixed | 40             | kilpatrick2014 | 50   | mg        | 1       | 1           | 2                | sem            | actual dose was 1.5 mg/kg  |
| 1.4918032786885242    | h         | 54.820143884892076 | mixed | 40             | kilpatrick2014 | 50   | mg        | 1       | 1           | 3                | mean           | actual dose was 1.5 mg/kg  |
| 1.5                   | h         | 58.177458033573124 | mixed | 40             | kilpatrick2014 | 50   | mg        | 1       | 1           | 3                | sem            | actual dose was 1.5 mg/kg  |
| 1.9918032786885247    | h         | 46.18705035971222  | mixed | 40             | kilpatrick2014 | 50   | mg        | 1       | 1           | 4                | mean           | actual dose was 1.5 mg/kg  |
| 1.9918032786885247    | h         | 49.06474820143883  | mixed | 40             | kilpatrick2014 | 50   | mg        | 1       | 1           | 4                | sem            | actual dose was 1.5 mg/kg  |
| 3.4754098360655727    | h         | 22.925659472422055 | mixed | 40             | kilpatrick2014 | 50   | mg        | 1       | 1           | 5                | mean           | actual dose was 1.5 mg/kg  |
| 3.4918032786885242    | h         | 25.563549160671457 | mixed | 40             | kilpatrick2014 | 50   | mg        | 1       | 1           | 5                | sem            | actual dose was 1.5 mg/kg  |
| 3.9918032786885242    | h         | 11.175059952038367 | mixed | 40             | kilpatrick2014 | 50   | mg        | 1       | 1           | 6                | mean           | actual dose was 1.5 mg/kg  |
| 3.9918032786885242    | h         | 13.093525179856115 | mixed | 40             | kilpatrick2014 | 50   | mg        | 1       | 1           | 6                | sem            | actual dose was 1.5 mg/kg  |

<a id="table--vizeli2017-effects"></a>

| time                   | time_unit | effect_perc            | sex  | n_participants | origin     | dose | dose_unit | n_doses | dose_timing | measurment_point | datapoint_type | comment |
|------------------------|-----------|------------------------|------|----------------|------------|------|-----------|---------|-------------|------------------|----------------|---------|
| -0.02460460595728753   | h         | 0.158521436775942      | male | 15             | vizeli2017 | 125  | mg        | 1       | 1           | 0                | mean           |         |
| -0.007359552605728625  | h         | 2.3811998773407765     | male | 15             | vizeli2017 | 125  | mg        | 1       | 1           | 0                | sem            |         |
| 0.299059785970073      | h         | 2.7067665265094547     | male | 15             | vizeli2017 | 125  | mg        | 1       | 1           | 1                | mean           |         |
| 0.2987791250656173     | h         | 4.135330530189236      | male | 15             | vizeli2017 | 125  | mg        | 1       | 1           | 1                | sem            |         |
| 0.5937225511036726     | h         | 32.87329199648654      | male | 15             | vizeli2017 | 125  | mg        | 1       | 1           | 2                | mean           |         |
| 0.5930053065700633     | h         | 36.52406667255708      | male | 15             | vizeli2017 | 125  | mg        | 1       | 1           | 2                | sem            |         |
| 0.9841218692016231     | h         | 55.7407628779177       | male | 15             | vizeli2017 | 125  | mg        | 1       | 1           | 3                | mean           |         |
| 0.9892673191166459     | h         | 59.55042281045513      | male | 15             | vizeli2017 | 125  | mg        | 1       | 1           | 3                | sem            |         |
| 1.4953612989402445     | h         | 63.53206550833406      | male | 15             | vizeli2017 | 125  | mg        | 1       | 1           | 4                | mean           |         |
| 1.482981034599252      | h         | 66.54761100398642      | male | 15             | vizeli2017 | 125  | mg        | 1       | 1           | 4                | sem            |         |
| 1.991445039838256      | h         | 58.46582433745834      | male | 15             | vizeli2017 | 125  | mg        | 1       | 1           | 5                | mean           |         |
| 1.9907277953046472     | h         | 62.116599013528884     | male | 15             | vizeli2017 | 125  | mg        | 1       | 1           | 5                | sem            |         |
| 2.4945453033476603     | h         | 47.685483074588234     | male | 15             | vizeli2017 | 125  | mg        | 1       | 1           | 6                | mean           |         |
| 2.4996907532626826     | h         | 51.49514300712565      | male | 15             | vizeli2017 | 125  | mg        | 1       | 1           | 6                | sem            |         |
| 2.991626949683736      | h         | 37.5399032239622       | male | 15             | vizeli2017 | 125  | mg        | 1       | 1           | 7                | mean           |         |
| 2.9909720742400063     | h         | 40.87321923254834      | male | 15             | vizeli2017 | 125  | mg        | 1       | 1           | 7                | sem            |         |
| 3.997297339438573      | h         | 18.677619371839313     | male | 15             | vizeli2017 | 125  | mg        | 1       | 1           | 8                | mean           |         |
| 3.996954309444238      | h         | 20.42364204300347      | male | 15             | vizeli2017 | 125  | mg        | 1       | 1           | 8                | sem            |         |
| 4.995670545677562      | h         | 6.957999615390605      | male | 15             | vizeli2017 | 125  | mg        | 1       | 1           | 9                | mean           |         |
| 5.001221394676797      | h         | 8.704178209279462      | male | 15             | vizeli2017 | 125  | mg        | 1       | 1           | 9                | sem            |         |
| 5.99273400102909       | h         | 1.9050118761142016     | male | 15             | vizeli2017 | 125  | mg        | 1       | 1           | 10               | mean           |         |
| 5.998471957297962      | h         | 2.69881446754988       | male | 15             | vizeli2017 | 125  | mg        | 1       | 1           | 10               | sem            |         |
| -0.0009979054380648833 | h         | 0.00041579393251822694 | male | 15             | vizeli2017 | 75   | mg        | 1       | 1           | 0                | mean           |         |
| -0.018960203323233005  | h         | 1.4285120294381954     | male | 15             | vizeli2017 | 75   | mg        | 1       | 1           | 0                | sem            |         |
| 0.2931970915214419     | h         | 2.547881270042552      | male | 15             | vizeli2017 | 75   | mg        | 1       | 1           | 1                | mean           |         |
| 0.2930723533416837     | h         | 3.1827986050113566     | male | 15             | vizeli2017 | 75   | mg        | 1       | 1           | 1                | sem            |         |
| 0.6108740508204136     | h         | 5.572158438277967      | male | 15             | vizeli2017 | 75   | mg        | 1       | 1           | 2                | mean           |         |
| 0.5995540610073644     | h         | 3.1909065866956325     | male | 15             | vizeli2017 | 75   | mg        | 1       | 1           | 2                | sem            |         |
| 1.000150725300541      | h         | 34.15388533442824      | male | 15             | vizeli2017 | 75   | mg        | 1       | 1           | 3                | mean           |         |
| 1.0075414624512091     | h         | 26.535033237527472     | male | 15             | vizeli2017 | 75   | mg        | 1       | 1           | 3                | sem            |         |
| 1.4925858744406266     | h         | 47.65897621138964      | male | 15             | vizeli2017 | 75   | mg        | 1       | 1           | 4                | mean           |         |
| 1.4999766115912947     | h         | 40.04012411448886      | male | 15             | vizeli2017 | 75   | mg        | 1       | 1           | 4                | sem            |         |
| 1.9939709879783578     | h         | 45.60874830434035      | male | 15             | vizeli2017 | 75   | mg        | 1       | 1           | 5                | mean           |         |
| 1.9955302152253345     | h         | 37.672281617230496     | male | 15             | vizeli2017 | 75   | mg        | 1       | 1           | 5                | sem            |         |
| 2.5014059032343567     | h         | 42.7650296513048       | male | 15             | vizeli2017 | 75   | mg        | 1       | 1           | 6                | mean           |         |
| 2.4969153287630643     | h         | 35.622053710181234     | male | 15             | vizeli2017 | 75   | mg        | 1       | 1           | 6                | sem            |         |
| 2.9933420996554103     | h         | 28.809789868141337     | male | 15             | vizeli2017 | 75   | mg        | 1       | 1           | 7                | mean           |         |
| 2.99468303508781       | h         | 21.98442851722686      | male | 15             | vizeli2017 | 75   | mg        | 1       | 1           | 7                | sem            |         |
| 4.015820959132653      | h         | 14.392395128974073     | male | 15             | vizeli2017 | 75   | mg        | 1       | 1           | 8                | mean           |         |
| 4.004906368403819      | h         | 9.947661938743153      | male | 15             | vizeli2017 | 75   | mg        | 1       | 1           | 8                | sem            |         |
| 4.99579528385732       | h         | 6.323082280421801      | male | 15             | vizeli2017 | 75   | mg        | 1       | 1           | 9                | mean           |         |
| 5.002063377390164      | h         | 4.418486198240146      | male | 15             | vizeli2017 | 75   | mg        | 1       | 1           | 9                | sem            |         |
| 5.998440772753023      | h         | 2.8575438012920813     | male | 15             | vizeli2017 | 75   | mg        | 1       | 1           | 10               | mean           |         |
| 5.998440772753023      | h         | 2.8575438012920813     | male | 15             | vizeli2017 | 75   | mg        | 1       | 1           | 10               | sem            |         |


#### Simple model {#simple-model}

```python

import pandas as pd
import matplotlib.pyplot as plt
from scipy.optimize import curve_fit
import numpy as np
import re

# Initialize an empty list to hold the corrected DataFrames
corrected_dfs = []

# Iterate over all items in the global namespace
for var_name, var_value in list(globals().items()):
    # Check if the variable name matches the pattern (df1, df2, df3)
    if re.match(r'df[123]$', var_name):
        # Apply the correction
        corrected_df = pd.DataFrame(var_value[1:], columns=var_value[0])
        # Append the corrected DataFrame to the list
        corrected_dfs.append(corrected_df)

# Concatenate all corrected DataFrames into a single DataFrame
combined_df = pd.concat(corrected_dfs)

combined_df['time'] = combined_df.groupby(['origin', 'measurment_point'])['time'].transform(lambda x: round(x.mean(), 1))


wider_df = combined_df.pivot_table(index=['time', 'n_participants','time_unit', 'sex', 'origin', 'dose', 'dose_unit', 'n_doses', 'dose_timing', 'measurment_point', 'comment'],
                                   columns='datapoint_type',
                                   values='effect_perc').reset_index()


# Here, taking the absolute of the SEM as it's understood to be positive
wider_df['sem'] = abs(wider_df['mean'] - wider_df['sem'])

# Step 2: Calculate the true SD using the (corrected) 'sem'
wider_df['true_sd'] = wider_df['sem'] * np.sqrt(wider_df['n_participants'])


# Prepare a dictionary to store parameters for each group
group_params = {}


def expanded_biphasic_model(t, dose, beta0, beta1, k1, k2, t0):
    E_max = beta0 + beta1 * dose
    return (E_max / (1 + np.exp(-k1 * (t - t0)))) * np.exp(-k2 * (t - t0))

# Group by both 'dose' and 'origin'
for (dose, origin), group in wider_df.groupby(['dose', 'origin']):
    print(dose, origin)
    # Extract the time, effect data, and true SD for the current group
    time = group['time'].values
    effect = group['mean'].values  # Assuming 'mean' represents the effect measurements
    true_sd = group['true_sd'].values  # Use the calculated true SD

    # Update the initial guess based on the current group's data
    initial_guess = [max(effect), 0.1, 0.1, min(time)]

    try:
        # Fit the model to the group data, using true SD values as weights
        params, cov = curve_fit(biphasic_model, time, effect, sigma = true_sd, absolute_sigma=True, p0=initial_guess,  maxfev=10000)

        # Store the parameters for the current group, identified by both dose and origin
        group_params[(dose, origin)] = params

        # Plotting adjustments for visualization
        t_sim = np.linspace(min(time), max(time), 100)
        effect_sim = biphasic_model(t_sim, *params)

        plt.figure(figsize=(10, 6))
        plt.errorbar(time, effect, yerr=true_sd, fmt='o', capsize=5, label=f'Data with True SD - {origin}', color='black')
        plt.plot(t_sim, effect_sim, label=f'Model Fit - Dose {dose}mg, {origin}', color='red')
        plt.xlabel('Time (hours)')
        plt.ylabel('Average Effect')
        plt.title(f'Fit of Biphasic Model - Dose {dose}mg, {origin}')
        plt.legend()
        plt.show()
    except RuntimeError as e:
        print(f"Could not fit the model for the group {dose}mg, {origin}: {e}")



# Define your model as before
def dose_dependent_biphasic_model(time, dose, E_max_base, E_max_slope, k1, k2, t0):
    E_max = E_max_base + E_max_slope * dose
    effect = (E_max / (1 + np.exp(-k1 * (time - t0)))) * np.exp(-k2 * (time - t0))
    return effect

# Wrapper function for curve_fit
def model_wrapper(X, *params):
    # Unpack time and dose from the first argument
    time, dose = X
    # Call the actual model function
    return dose_dependent_biphasic_model(time, dose, *params)

# Prepare your data
time = wider_df['time'].values
dose = wider_df['dose'].values
effect = wider_df['mean'].values
true_sd = wider_df['true_sd'].values

# Stack time and dose for curve_fit's xdata
X = np.vstack((time, dose))

# Initial guesses for the parameters
initial_guesses = [1, 0.1, 0.1, 0.1, 0]

# Use the wrapper function for fitting
params, cov = curve_fit(model_wrapper, X, effect, p0=initial_guesses, sigma=true_sd, absolute_sigma=True)

print("Fitted parameters:", params)

wider_df = wider_df.query('origin != "kilpatrick2014"')

# Unique doses for plotting
unique_doses = wider_df['dose'].unique()

# Plot settings
colors = plt.cm.viridis(np.linspace(0, 1, len(unique_doses)))  # Color map for different doses

# Plot the fitted results for each dose
plt.figure(figsize=(12, 8))

for i, dose in enumerate(unique_doses):
    # Filter data for the current dose
    dose_group = wider_df[wider_df['dose'] == dose]

    # Prepare data for curve fitting
    time = dose_group['time'].values
    effect = dose_group['mean'].values
    true_sd = dose_group['true_sd'].values
    n_participants = dose_group['n_participants'].values  # If needed for SEM to SD conversion

    # Combine time and dose for the model's input
    X = np.vstack((time, np.full_like(time, fill_value=dose)))

    # Fit the model (repeat the fitting process here if necessary, or use previously fitted params if applicable)
    # For demonstration, assuming previously fitted parameters are used:
    # params = [E_max_base, E_max_slope, k1, k2, t0] for this specific dose
    # This step is conceptual; in practice, you would fit once outside the loop or use a model that includes dose directly

    # Simulate the model with the fitted parameters
    t_sim = np.linspace(time.min(), time.max(), 100)
    dose_sim = np.full_like(t_sim, fill_value=dose)
    X_sim = np.vstack((t_sim, dose_sim))
    effect_sim = model_wrapper(X_sim, *params)  # Use your actual fitted params

    # Plot original data
    plt.errorbar(time, effect, yerr=true_sd, fmt='o', color=colors[i], capsize=5, label=f'Dose {dose}mg - Data')

    # Plot model fit
    plt.plot(t_sim, effect_sim, color=colors[i], linestyle='--', label=f'Dose {dose}mg - Fit')

plt.xlabel('Time (hours)')
plt.ylabel('Effect')
plt.title('Model Fit to Data by Dose')
plt.legend()
plt.show()


from plotnine import *

(
    ggplot(wider_df, aes(x = 'time', y = 'mean', color = 'dose',group=list(zip(wider_df.dose, wider_df.origin)))) +
    geom_point()+
    geom_line()+
    geom_errorbar(aes(ymax = 'mean+sem', ymin = 'mean-sem'))
    )
```


### Repeated dosing {#repeated-dosing}

The redosung dataset is from (NO_ITEM_DATA:peiro2013) where 50mg and 100mg of MDMA were givent one after the other with 2 hours in-between. An additional dataet comes from (NO_ITEM_DATA:farre2015) where two doses of 100mg were given in succession.


#### Datasets {#datasets}

<a id="table--peiro2013-plasma1"></a>

| time                 | time_unit | plasma_conc          | sex  | n_participants | origin    | dose | dose_unit | n_doses | dose_timing | measurment_point | datapoint_type | comment |
|----------------------|-----------|----------------------|------|----------------|-----------|------|-----------|---------|-------------|------------------|----------------|---------|
| -0.00434965802835352 | h         | 0.7583173377709045   | male | 10             | peiro2013 | 50   | mg        | 2       | 1           | 0                | mean           |         |
| -0.00434965802835352 | h         | 0.7583173377709045   | male | 10             | peiro2013 | 50   | mg        | 2       | 1           | 0                | sem            |         |
| 0.3191763739514215   | h         | -0.20780044347787907 | male | 10             | peiro2013 | 50   | mg        | 2       | 1           | 1                | mean           |         |
| 0.3191763739514215   | h         | -0.20780044347787907 | male | 10             | peiro2013 | 50   | mg        | 2       | 1           | 1                | sem            |         |
| 0.672971259060013    | h         | 44.75232059788647    | male | 10             | peiro2013 | 50   | mg        | 2       | 1           | 2                | mean           |         |
| 0.672971259060013    | h         | 44.75232059788647    | male | 10             | peiro2013 | 50   | mg        | 2       | 1           | 2                | sem            |         |
| 0.9978251709858226   | h         | 62.252033316167285   | male | 10             | peiro2013 | 50   | mg        | 2       | 1           | 3                | mean           |         |
| 0.9978251709858226   | h         | 62.252033316167285   | male | 10             | peiro2013 | 50   | mg        | 2       | 1           | 3                | sem            |         |
| 1.493498921097543    | h         | 80.21512080728291    | male | 10             | peiro2013 | 50   | mg        | 2       | 1           | 4                | mean           |         |
| 1.493498921097543    | h         | 80.21512080728291    | male | 10             | peiro2013 | 50   | mg        | 2       | 1           | 4                | sem            |         |
| 2.0057881946365566   | h         | 79.23783095918043    | male | 10             | peiro2013 | 100  | mg        | 2       | 2           | 5                | mean           |         |
| 2.0057881946365566   | h         | 79.23783095918043    | male | 10             | peiro2013 | 100  | mg        | 2       | 2           | 5                | sem            |         |
| 2.320700031068985    | h         | 83.48055634764916    | male | 10             | peiro2013 | 100  | mg        | 2       | 2           | 6                | mean           |         |
| 2.320700031068985    | h         | 83.48055634764916    | male | 10             | peiro2013 | 100  | mg        | 2       | 2           | 6                | sem            |         |
| 2.706227842067406    | h         | 194.72667804444137   | male | 10             | peiro2013 | 100  | mg        | 2       | 2           | 7                | mean           |         |
| 2.689816607862581    | h         | 216.50795238358702   | male | 10             | peiro2013 | 100  | mg        | 2       | 2           | 7                | sem            |         |
| 3.018790352441468    | h         | 291.29908793374216   | male | 10             | peiro2013 | 100  | mg        | 2       | 2           | 8                | mean           |         |
| 3.0295495848247143   | h         | 315.9196632632649    | male | 10             | peiro2013 | 100  | mg        | 2       | 2           | 8                | sem            |         |
| 3.522295189414412    | h         | 293.1632270887509    | male | 10             | peiro2013 | 100  | mg        | 2       | 2           | 9                | mean           |         |
| 3.523452828341723    | h         | 309.2616434216742    | male | 10             | peiro2013 | 100  | mg        | 2       | 2           | 9                | sem            |         |
| 4.025221206923702    | h         | 286.97815807729785   | male | 10             | peiro2013 | 100  | mg        | 2       | 2           | 10               | mean           |         |
| 4.026549086869736    | h         | 305.44398857682756   | male | 10             | peiro2013 | 100  | mg        | 2       | 2           | 10               | sem            |         |
| 5.021641889505068    | h         | 268.4532752243989    | male | 10             | peiro2013 | 100  | mg        | 2       | 2           | 11               | mean           |         |
| 5.0227654802286335   | h         | 284.07820872400094   | male | 10             | peiro2013 | 100  | mg        | 2       | 2           | 11               | sem            |         |
| 6.035563348811079    | h         | 243.29856869863502   | male | 10             | peiro2013 | 100  | mg        | 2       | 2           | 12               | mean           |         |
| 6.018709487957574    | h         | 258.9245662046041    | male | 10             | peiro2013 | 100  | mg        | 2       | 2           | 12               | sem            |         |
| 8.037802018207275    | h         | 211.93006498950885   | male | 10             | peiro2013 | 100  | mg        | 2       | 2           | 13               | mean           |         |
| 8.029732593919842    | h         | 224.71463349236672   | male | 10             | peiro2013 | 100  | mg        | 2       | 2           | 13               | sem            |         |
| 10.031017913611194   | h         | 180.08861045024486   | male | 10             | peiro2013 | 100  | mg        | 2       | 2           | 14               | mean           |         |
| 10.013789522516502   | h         | 190.5062967896799    | male | 10             | peiro2013 | 100  | mg        | 2       | 2           | 14               | sem            |         |
| 12.03346087222986    | h         | 151.56100374104625   | male | 10             | peiro2013 | 100  | mg        | 2       | 2           | 15               | mean           |         |
| 12.016300577542655   | h         | 162.92565574712387   | male | 10             | peiro2013 | 100  | mg        | 2       | 2           | 15               | sem            |         |

<a id="table--peiro2013-plasma2"></a>

| time                  | time_unit | plasma_conc          | sex  | n_participants | origin    | dose | dose_unit | n_doses | dose_timing | measurment_point | datapoint_type | comment |
|-----------------------|-----------|----------------------|------|----------------|-----------|------|-----------|---------|-------------|------------------|----------------|---------|
| -0.013483146067414964 | h         | -0.09451795841209787 | male | 10             | peiro2013 | 0    | mg        | 2       | 0           | 0                | mean           |         |
| -0.013483146067414964 | h         | -0.09451795841209787 | male | 10             | peiro2013 | 0    | mg        | 2       | 0           | 0                | sem            |         |
| 0.32808988764045033   | h         | 0.35789384252666423  | male | 10             | peiro2013 | 0    | mg        | 2       | 0           | 1                | mean           |         |
| 0.32808988764045033   | h         | 0.35789384252666423  | male | 10             | peiro2013 | 0    | mg        | 2       | 0           | 1                | sem            |         |
| 0.6426966292134839    | h         | -0.13328094135636093 | male | 10             | peiro2013 | 0    | mg        | 2       | 0           | 2                | mean           |         |
| 0.6426966292134839    | h         | -0.13328094135636093 | male | 10             | peiro2013 | 0    | mg        | 2       | 0           | 2                | sem            |         |
| 0.9842696629213483    | h         | -0.15345893247803133 | male | 10             | peiro2013 | 0    | mg        | 2       | 0           | 3                | mean           |         |
| 0.9842696629213483    | h         | -0.15345893247803133 | male | 10             | peiro2013 | 0    | mg        | 2       | 0           | 3                | sem            |         |
| 1.487640449438202     | h         | -0.6557847114547144  | male | 10             | peiro2013 | 0    | mg        | 2       | 0           | 4                | mean           |         |
| 1.487640449438202     | h         | -0.6557847114547144  | male | 10             | peiro2013 | 0    | mg        | 2       | 0           | 4                | sem            |         |
| 2                     | h         | -0.6860516981372484  | male | 10             | peiro2013 | 100  | mg        | 2       | 1           | 5                | mean           |         |
| 2                     | h         | -0.6860516981372484  | male | 10             | peiro2013 | 100  | mg        | 2       | 1           | 5                | sem            |         |
| 2.332584269662922     | h         | -0.23310889743208918 | male | 10             | peiro2013 | 100  | mg        | 2       | 1           | 6                | mean           |         |
| 2.332584269662922     | h         | -0.23310889743208918 | male | 10             | peiro2013 | 100  | mg        | 2       | 1           | 6                | sem            |         |
| 2.6831460674157306    | h         | 73.94277946517707    | male | 10             | peiro2013 | 100  | mg        | 2       | 1           | 7                | mean           |         |
| 2.656179775280899     | h         | 102.29975998810568   | male | 10             | peiro2013 | 100  | mg        | 2       | 1           | 7                | sem            |         |
| 3.0067415730337093    | h         | 144.3395424906014    | male | 10             | peiro2013 | 100  | mg        | 2       | 1           | 8                | mean           |         |
| 2.988764044943821     | h         | 168.44268388521914   | male | 10             | peiro2013 | 100  | mg        | 2       | 1           | 8                | sem            |         |
| 3.510112359550562     | h         | 188.7332469573714    | male | 10             | peiro2013 | 100  | mg        | 2       | 1           | 9                | mean           |         |
| 3.510112359550562     | h         | 217.08863448100087   | male | 10             | peiro2013 | 100  | mg        | 2       | 1           | 9                | sem            |         |
| 3.9955056179775292    | h         | 196.2660096429558    | male | 10             | peiro2013 | 100  | mg        | 2       | 1           | 10               | mean           |         |
| 3.9955056179775292    | h         | 226.98434612688774   | male | 10             | peiro2013 | 100  | mg        | 2       | 1           | 10               | sem            |         |
| 4.9932584269662925    | h         | 182.97455449119607   | male | 10             | peiro2013 | 100  | mg        | 2       | 1           | 11               | mean           |         |
| 4.9932584269662925    | h         | 217.00101951955145   | male | 10             | peiro2013 | 100  | mg        | 2       | 1           | 11               | sem            |         |
| 5.991011235955057     | h         | 171.57345850767825   | male | 10             | peiro2013 | 100  | mg        | 2       | 1           | 12               | mean           |         |
| 5.991011235955057     | h         | 203.7095643677917    | male | 10             | peiro2013 | 100  | mg        | 2       | 1           | 12               | sem            |         |
| 8.004494382022473     | h         | 148.77020454111      | male | 10             | peiro2013 | 100  | mg        | 2       | 1           | 13               | mean           |         |
| 7.99550561797753      | h         | 176.65353327244537   | male | 10             | peiro2013 | 100  | mg        | 2       | 1           | 13               | sem            |         |
| 10.000000000000002    | h         | 122.65988402965098   | male | 10             | peiro2013 | 100  | mg        | 2       | 1           | 14               | mean           |         |
| 9.991011235955058     | h         | 143.92695567213957   | male | 10             | peiro2013 | 100  | mg        | 2       | 1           | 14               | sem            |         |
| 12.004494382022472    | h         | 104.58305898345407   | male | 10             | peiro2013 | 100  | mg        | 2       | 1           | 15               | mean           |         |
| 11.986516853932585    | h         | 123.01512287334589   | male | 10             | peiro2013 | 100  | mg        | 2       | 1           | 15               | sem            |         |

<a id="table--peiro2013-effect1"></a>

| time                  | time_unit | effect_perc          | sex  | n_participants | origin    | dose | dose_unit | n_doses | dose_timing | measurment_point | datapoint_type | comment |
|-----------------------|-----------|----------------------|------|----------------|-----------|------|-----------|---------|-------------|------------------|----------------|---------|
| 0.0012360939431397266 | h         | 0.020492473924960564 | male | 10             | peiro2013 | 50   | mg        | 2       | 1           | 0                | mean           |         |
| 0.0012360939431397266 | h         | 0.020492473924960564 | male | 10             | peiro2013 | 50   | mg        | 2       | 1           | 0                | sem            |         |
| 0.3374536464771323    | h         | 0.011932963931727159 | male | 10             | peiro2013 | 50   | mg        | 2       | 1           | 1                | mean           |         |
| 0.3374536464771323    | h         | 0.011932963931727159 | male | 10             | peiro2013 | 50   | mg        | 2       | 1           | 1                | sem            |         |
| 0.6637824474660072    | h         | 1.327454124802685    | male | 10             | peiro2013 | 50   | mg        | 2       | 1           | 2                | mean           |         |
| 0.6637824474660072    | h         | 1.327454124802685    | male | 10             | peiro2013 | 50   | mg        | 2       | 1           | 2                | sem            |         |
| 1.004944375772559     | h         | 20.2597056032063     | male | 10             | peiro2013 | 50   | mg        | 2       | 1           | 3                | mean           |         |
| 1.0098887515451174    | h         | 29.01721720260109    | male | 10             | peiro2013 | 50   | mg        | 2       | 1           | 3                | sem            |         |
| 1.5092707045735474    | h         | 16.58087855817574    | male | 10             | peiro2013 | 50   | mg        | 2       | 1           | 4                | mean           |         |
| 1.5191594561186652    | h         | 21.468610514602787   | male | 10             | peiro2013 | 50   | mg        | 2       | 1           | 4                | sem            |         |
| 2.003708281829419     | h         | 11.069309373418704   | male | 10             | peiro2013 | 100  | mg        | 2       | 2           | 5                | mean           |         |
| 2.008652657601978     | h         | 16.059000198882742   | male | 10             | peiro2013 | 100  | mg        | 2       | 2           | 5                | sem            |         |
| 2.34487021013597      | h         | 12.4862859027388     | male | 10             | peiro2013 | 100  | mg        | 2       | 2           | 6                | mean           |         |
| 2.349814585908529     | h         | 17.67964271598288    | male | 10             | peiro2013 | 100  | mg        | 2       | 2           | 6                | sem            |         |
| 2.676143386897405     | h         | 38.64893169762777    | male | 10             | peiro2013 | 100  | mg        | 2       | 2           | 7                | mean           |         |
| 2.676143386897405     | h         | 48.526732104959756   | male | 10             | peiro2013 | 100  | mg        | 2       | 2           | 7                | sem            |         |
| 2.9925834363411625    | h         | 56.15615063730589    | male | 10             | peiro2013 | 100  | mg        | 2       | 2           | 8                | mean           |         |
| 3.0024721878862795    | h         | 65.8300333065639     | male | 10             | peiro2013 | 100  | mg        | 2       | 2           | 8                | sem            |         |
| 3.4969097651421506    | h         | 48.60766982445455    | male | 10             | peiro2013 | 100  | mg        | 2       | 2           | 9                | mean           |         |
| 3.4919653893695926    | h         | 58.79109508860354    | male | 10             | peiro2013 | 100  | mg        | 2       | 2           | 9                | sem            |         |
| 3.9913473423980226    | h         | 26.395489641734173   | male | 10             | peiro2013 | 100  | mg        | 2       | 2           | 10               | mean           |         |
| 3.9962917181705806    | h         | 31.48701346108824    | male | 10             | peiro2013 | 100  | mg        | 2       | 2           | 10               | sem            |         |
| 5                     | h         | 10.280198077131274   | male | 10             | peiro2013 | 100  | mg        | 2       | 2           | 11               | mean           |         |
| 5.004944375772558     | h         | 15.982719859825451   | male | 10             | peiro2013 | 100  | mg        | 2       | 2           | 11               | sem            |         |
| 5.993819530284302     | h         | 4.55224951475131     | male | 10             | peiro2013 | 100  | mg        | 2       | 2           | 12               | mean           |         |
| 6.008652657601978     | h         | 8.93069062658131     | male | 10             | peiro2013 | 100  | mg        | 2       | 2           | 12               | sem            |         |

<a id="table--peiro2013-effect2"></a>

| time                 | time_unit | effect_perc           | sex  | n_participants | origin    | dose | dose_unit | n_doses | dose_timing | measurment_point | datapoint_type | comment |
|----------------------|-----------|-----------------------|------|----------------|-----------|------|-----------|---------|-------------|------------------|----------------|---------|
| 0.006180469715698411 | h         | -0.0814663951120167   | male | 10             | peiro2013 | 0    | mg        | 2       | 0           | 0                | mean           |         |
| 0.006180469715698411 | h         | -0.0814663951120167   | male | 10             | peiro2013 | 0    | mg        | 2       | 0           | 0                | sem            |         |
| 0.3374536464771323   | h         | -0.1917330238483146   | male | 10             | peiro2013 | 0    | mg        | 2       | 0           | 1                | mean           |         |
| 0.3374536464771323   | h         | -0.1917330238483146   | male | 10             | peiro2013 | 0    | mg        | 2       | 0           | 1                | sem            |         |
| 0.6687268232385661   | h         | -0.30199965258459827  | male | 10             | peiro2013 | 0    | mg        | 2       | 0           | 2                | mean           |         |
| 0.6687268232385661   | h         | -0.30199965258459827  | male | 10             | peiro2013 | 0    | mg        | 2       | 0           | 2                | sem            |         |
| 1.004944375772559    | h         | -0.10689317479777571  | male | 10             | peiro2013 | 0    | mg        | 2       | 0           | 3                | mean           |         |
| 1.004944375772559    | h         | -0.10689317479777571  | male | 10             | peiro2013 | 0    | mg        | 2       | 0           | 3                | sem            |         |
| 1.49938195302843     | h         | -0.017647695603685065 | male | 10             | peiro2013 | 0    | mg        | 2       | 0           | 4                | mean           |         |
| 1.49938195302843     | h         | -0.017647695603685065 | male | 10             | peiro2013 | 0    | mg        | 2       | 0           | 4                | sem            |         |
| 2.003708281829419    | h         | -0.030486960593520962 | male | 10             | peiro2013 | 100  | mg        | 2       | 1           | 5                | mean           |         |
| 2.003708281829419    | h         | -0.030486960593520962 | male | 10             | peiro2013 | 100  | mg        | 2       | 1           | 5                | sem            |         |
| 2.3399258343634117   | h         | -0.14087946447676813  | male | 10             | peiro2013 | 100  | mg        | 2       | 1           | 6                | mean           |         |
| 2.3399258343634117   | h         | -0.14087946447676813  | male | 10             | peiro2013 | 100  | mg        | 2       | 1           | 6                | sem            |         |
| 2.671199011124845    | h         | 23.170442501491635    | male | 10             | peiro2013 | 100  | mg        | 2       | 1           | 7                | mean           |         |
| 2.671199011124845    | h         | 32.0299129699234      | male | 10             | peiro2013 | 100  | mg        | 2       | 1           | 7                | sem            |         |
| 2.9975278121137205   | h         | 52.897368957678275    | male | 10             | peiro2013 | 100  | mg        | 2       | 1           | 8                | mean           |         |
| 2.9975278121137205   | h         | 62.673336371120236    | male | 10             | peiro2013 | 100  | mg        | 2       | 1           | 8                | sem            |         |
| 3.4969097651421506   | h         | 45.14534803219385     | male | 10             | peiro2013 | 100  | mg        | 2       | 1           | 9                | mean           |         |
| 3.4969097651421506   | h         | 54.004818500625625    | male | 10             | peiro2013 | 100  | mg        | 2       | 1           | 9                | sem            |         |
| 3.9962917181705806   | h         | 35.865832198359115    | male | 10             | peiro2013 | 100  | mg        | 2       | 1           | 10               | mean           |         |
| 4.00123609394314     | h         | 45.03067577331399     | male | 10             | peiro2013 | 100  | mg        | 2       | 1           | 10               | sem            |         |
| 4.995055624227442    | h         | 19.44529340238006     | male | 10             | peiro2013 | 100  | mg        | 2       | 1           | 11               | mean           |         |
| 5                    | h         | 29.01746895289503     | male | 10             | peiro2013 | 100  | mg        | 2       | 1           | 11               | sem            |         |
| 5.99876390605686     | h         | 8.72727638909518      | male | 10             | peiro2013 | 100  | mg        | 2       | 1           | 12               | mean           |         |
| 6.00370828182942     | h         | 16.873790025149873    | male | 10             | peiro2013 | 100  | mg        | 2       | 1           | 12               | sem            |         |

<a id="table--farre2015-effect"></a>

| time                 | time_unit | effect_perc          | sex  | n_participants | origin    | dose | dose_unit | n_doses | dose_timing | measurment_point | datapoint_type | comment |
|----------------------|-----------|----------------------|------|----------------|-----------|------|-----------|---------|-------------|------------------|----------------|---------|
| 0.015691039608604296 | h         | -0.31565145159849806 | male | 10             | farre2015 | 100  | mg        | 2       | 1           | 0                | mean           |         |
| 0.015691039608604296 | h         | -0.31565145159849806 | male | 10             | farre2015 | 100  | mg        | 2       | 1           | 0                | sem            |         |
| 0.354820698350804    | h         | -0.31118921924662857 | male | 10             | farre2015 | 100  | mg        | 2       | 1           | 1                | mean           |         |
| 0.354820698350804    | h         | -0.31118921924662857 | male | 10             | farre2015 | 100  | mg        | 2       | 1           | 1                | sem            |         |
| 0.7097489479429107   | h         | 8.85137562614274     | male | 10             | farre2015 | 100  | mg        | 2       | 1           | 2                | mean           |         |
| 0.6915155800558801   | h         | 13.693240976565278   | male | 10             | farre2015 | 100  | mg        | 2       | 1           | 2                | sem            |         |
| 1.0108008906158106   | h         | 27.80270525696737    | male | 10             | farre2015 | 100  | mg        | 2       | 1           | 3                | mean           |         |
| 1.001629286889504    | h         | 30.539426683234133   | male | 10             | farre2015 | 100  | mg        | 2       | 1           | 3                | sem            |         |
| 1.5157699867963692   | h         | 24.230402218759238   | male | 10             | farre2015 | 100  | mg        | 2       | 1           | 4                | mean           |         |
| 1.4981224299257891   | h         | 25.703854224589882   | male | 10             | farre2015 | 100  | mg        | 2       | 1           | 4                | sem            |         |
| 2.0044645206761538   | h         | 14.236832409994491   | male | 10             | farre2015 | 100  | mg        | 2       | 1           | 5                | mean           |         |
| 2.003933629442496    | h         | 17.2894570035309     | male | 10             | farre2015 | 100  | mg        | 2       | 1           | 5                | sem            |         |
| 2.9886453349077002   | h         | 5.197150578602802    | male | 10             | farre2015 | 100  | mg        | 2       | 1           | 6                | mean           |         |
| 2.996956528703596    | h         | 7.407786252205369    | male | 10             | farre2015 | 100  | mg        | 2       | 1           | 6                | sem            |         |
| 4.006162457294147    | h         | 4.473696856528946    | male | 10             | farre2015 | 100  | mg        | 2       | 1           | 7                | mean           |         |
| 4.014510264278572    | h         | 6.4738066960945275   | male | 10             | farre2015 | 100  | mg        | 2       | 1           | 7                | sem            |         |
| 4.3541158944716365   | h         | 3.7414330859654825   | male | 10             | farre2015 | 100  | mg        | 2       | 2           | 8                | mean           |         |
| 4.362536927833117    | h         | 5.320491257457093    | male | 10             | farre2015 | 100  | mg        | 2       | 2           | 8                | sem            |         |
| 4.672430955535571    | h         | 23.429831968347912   | male | 10             | farre2015 | 100  | mg        | 2       | 2           | 9                | mean           |         |
| 4.698096800693818    | h         | 25.85122230841577    | male | 10             | farre2015 | 100  | mg        | 2       | 2           | 9                | sem            |         |
| 5.007697922888049    | h         | 45.64476969160254    | male | 10             | farre2015 | 100  | mg        | 2       | 2           | 10               | mean           |         |
| 5.0158260507413015   | h         | 48.90803453539008    | male | 10             | farre2015 | 100  | mg        | 2       | 2           | 10               | sem            |         |
| 5.511605236601289    | h         | 48.177715840467194   | male | 10             | farre2015 | 100  | mg        | 2       | 2           | 11               | mean           |         |
| 5.502451939469244    | h         | 50.809174349715455   | male | 10             | farre2015 | 100  | mg        | 2       | 2           | 11               | sem            |         |
| 6.001929057370578    | h         | 28.81574641705626    | male | 10             | farre2015 | 100  | mg        | 2       | 2           | 12               | mean           |         |
| 6.001526312296768    | h         | 31.131530591463182   | male | 10             | farre2015 | 100  | mg        | 2       | 2           | 12               | sem            |         |
| 7.022704753536033    | h         | 9.355493465690017    | male | 10             | farre2015 | 100  | mg        | 2       | 2           | 13               | mean           |         |
| 7.022210475490903    | h         | 12.197592225189425   | male | 10             | farre2015 | 100  | mg        | 2       | 2           | 13               | sem            |         |
| 8.015434747288909    | h         | 1.1580293866604592   | male | 10             | farre2015 | 100  | mg        | 2       | 2           | 14               | mean           |         |
| 8.015434747288909    | h         | 1.1580293866604592   | male | 10             | farre2015 | 100  | mg        | 2       | 2           | 14               | sem            |         |
| 9.989544646350923    | h         | 0.02610978006917719  | male | 10             | farre2015 | 100  | mg        | 2       | 2           | 15               | mean           |         |
| 9.99791075992961     | h         | 1.9209567026162517   | male | 10             | farre2015 | 100  | mg        | 2       | 2           | 15               | sem            |         |
| 11.972185418340006   | h         | -0.15832915714149465 | male | 10             | farre2015 | 100  | mg        | 2       | 2           | 16               | mean           |         |
| 11.97180097986046    | h         | 2.052192100246927    | male | 10             | farre2015 | 100  | mg        | 2       | 2           | 16               | sem            |         |

<a id="table--farre2015-plasma"></a>

| time                  | time_unit | plasma_conc          | sex  | n_participants | origin    | dose | dose_unit | n_doses | dose_timing | measurment_point | datapoint_type | comment |
|-----------------------|-----------|----------------------|------|----------------|-----------|------|-----------|---------|-------------|------------------|----------------|---------|
| -0.006518904823989757 | h         | 0.7920792079207786   | male | 10             | farre2015 | 100  | mg        | 2       | 1           | 0                | mean           |         |
| 0.013037809647977738  | h         | -0.9893244865555744  | male | 10             | farre2015 | 100  | mg        | 2       | 1           | 0                | sem            |         |
| 0.3063885267275088    | h         | -0.38364723043366666 | male | 10             | farre2015 | 100  | mg        | 2       | 1           | 1                | mean           |         |
| 0.3063885267275088    | h         | -0.38364723043366666 | male | 10             | farre2015 | 100  | mg        | 2       | 1           | 1                | sem            |         |
| 0.6584093872229451    | h         | 39.43227438780377    | male | 10             | farre2015 | 100  | mg        | 2       | 1           | 2                | mean           |         |
| 0.6779661016949134    | h         | 50.72017762402055    | male | 10             | farre2015 | 100  | mg        | 2       | 1           | 2                | sem            |         |
| 1.0299869621903515    | h         | 100.04104973730745   | male | 10             | farre2015 | 100  | mg        | 2       | 1           | 3                | mean           |         |
| 1.0299869621903515    | h         | 114.29847547988174   | male | 10             | farre2015 | 100  | mg        | 2       | 1           | 3                | sem            |         |
| 1.518904823989569     | h         | 168.3772445041114    | male | 10             | farre2015 | 100  | mg        | 2       | 1           | 4                | mean           |         |
| 1.4993481095175998    | h         | 184.41607394116204   | male | 10             | farre2015 | 100  | mg        | 2       | 1           | 4                | sem            |         |
| 2.027379400260756     | h         | 185.62510488336972   | male | 10             | farre2015 | 100  | mg        | 2       | 1           | 5                | mean           |         |
| 2.0078226857887866    | h         | 201.06987491447973   | male | 10             | farre2015 | 100  | mg        | 2       | 1           | 5                | sem            |         |
| 3.0247718383311586    | h         | 188.0408431977487    | male | 10             | farre2015 | 100  | mg        | 2       | 1           | 6                | mean           |         |
| 3.005215123859191     | h         | 203.4856132288587    | male | 10             | farre2015 | 100  | mg        | 2       | 1           | 6                | sem            |         |
| 4.022164276401565     | h         | 175.01103695767227   | male | 10             | farre2015 | 100  | mg        | 2       | 1           | 7                | mean           |         |
| 4.022164276401565     | h         | 189.26846270024657   | male | 10             | farre2015 | 100  | mg        | 2       | 1           | 7                | sem            |         |
| 4.3350717079530625    | h         | 195.81550853911983   | male | 10             | farre2015 | 100  | mg        | 2       | 2           | 8                | mean           |         |
| 4.3350717079530625    | h         | 211.85511249951588   | male | 10             | farre2015 | 100  | mg        | 2       | 2           | 8                | sem            |         |
| 4.687092568448499     | h         | 333.05717273161474   | male | 10             | farre2015 | 100  | mg        | 2       | 2           | 9                | mean           |         |
| 4.628422425032593     | h         | 370.4805917358359    | male | 10             | farre2015 | 100  | mg        | 2       | 2           | 9                | sem            |         |
| 5.0195567144719675    | h         | 373.46637923244737   | male | 10             | farre2015 | 100  | mg        | 2       | 2           | 10               | mean           |         |
| 5.0195567144719675    | h         | 407.9218247770018    | male | 10             | farre2015 | 100  | mg        | 2       | 2           | 10               | sem            |         |
| 5.528031290743153     | h         | 390.7142396117056    | male | 10             | farre2015 | 100  | mg        | 2       | 2           | 11               | mean           |         |
| 5.528031290743153     | h         | 419.2290910968541    | male | 10             | farre2015 | 100  | mg        | 2       | 2           | 11               | sem            |         |
| 6.016949152542372     | h         | 391.9217215072224    | male | 10             | farre2015 | 100  | mg        | 2       | 2           | 12               | mean           |         |
| 6.016949152542372     | h         | 420.43657299237094   | male | 10             | farre2015 | 100  | mg        | 2       | 2           | 12               | sem            |         |
| 7.014341590612776     | h         | 380.0800340790272    | male | 10             | farre2015 | 100  | mg        | 2       | 2           | 13               | mean           |         |
| 7.014341590612776     | h         | 421.66419249486876   | male | 10             | farre2015 | 100  | mg        | 2       | 2           | 13               | sem            |         |
| 8.031290743155148     | h         | 329.62525978803876   | male | 10             | farre2015 | 100  | mg        | 2       | 2           | 14               | mean           |         |
| 8.031290743155148     | h         | 362.89258652071203   | male | 10             | farre2015 | 100  | mg        | 2       | 2           | 14               | sem            |         |
| 10.026075619295955    | h         | 303.5656473078859    | male | 10             | farre2015 | 100  | mg        | 2       | 2           | 15               | mean           |         |
| 10.006518904823988    | h         | 325.54507080434246   | male | 10             | farre2015 | 100  | mg        | 2       | 2           | 15               | sem            |         |
| 12.020860495436764    | h         | 265.6248467089212    | male | 10             | farre2015 | 100  | mg        | 2       | 2           | 16               | mean           |         |
| 12.040417209908732    | h         | 288.1998786580092    | male | 10             | farre2015 | 100  | mg        | 2       | 2           | 16               | sem            |         |
| 24.04823989569752     | h         | 116.99226767526818   | male | 10             | farre2015 | 100  | mg        | 2       | 2           | 17               | mean           |         |
| 24.04823989569752     | h         | 116.99226767526818   | male | 10             | farre2015 | 100  | mg        | 2       | 2           | 17               | sem            |         |


#### Analysis {#analysis}

<!--list-separator-->

-  Modelling plasma

    ```python

    from scipy.optimize import curve_fit
    import numpy as np
    import pandas as pd
    from scipy.optimize import minimize




    plasma1 = pd.DataFrame(plasma1[1:], columns=plasma1[0])
    plasma1['experiment_id'] = 'peiro2013-1'
    plasma2 = pd.DataFrame(plasma2[1:], columns=plasma2[0])
    plasma2['experiment_id'] = 'peiro2013-2'
    ##plasma2 = plasma2.query("dose_timing == 1")
    plasma3 = pd.DataFrame(plasma3[1:], columns=plasma3[0])
    plasma3['experiment_id'] = 'farre2015'

    data = pd.concat([plasma1, plasma2, plasma3]).query("datapoint_type == 'mean'")


    from plotnine import *

    (
        ggplot(data.query("experiment_id == 'peiro2013-2'"), aes( x  = 'time', y = 'plasma_conc', color = 'experiment_id'))+
        geom_line()
        )


    # Unique experiment IDs
    experiment_ids = data['experiment_id'].unique()

    dose_timings = [[(0,50),(2,100)], [(0,0), (2,100)], [(0,100),(4,100)]]


    def simulate_plasma_concentration(t, dose_time, dose_amount, ka, ke, Vd):
        # Ensure t is an array for vectorized operations
        t = np.array(t)
        # Calculate concentration contribution from a single dose
        concentration = np.where(
            t > dose_time,
            (dose_amount * ka) / (Vd * (ka - ke)) * (np.exp(-ke * (t - dose_time)) - np.exp(-ka * (t - dose_time))),
            0  # Concentration contribution is 0 before dose_time
        )
        return concentration

    ka = 0.1
    ke = 0.01
    Vd = 1.0


    def global_objective(params):
        ka, ke, Vd = params
        weighted_residuals_sq = 0.0

        for i, experiment_id in enumerate(experiment_ids):
            # Extract data for this experiment
            experiment_data = data[data['experiment_id'] == experiment_id]
            times = experiment_data['time'].values
            observed_concs = experiment_data['plasma_conc'].values
            sems = abs(experiment_data['sem'].values - observed_concs)  # SEM for each data point

            # Simulate concentrations for all doses in the regimen
            simulated_concs = np.sum([
                simulate_plasma_concentration(times, dose_time, dose_amount, ka, ke, Vd)
                for dose_time, dose_amount in dose_timings[i]
            ], axis=0)

            # Calculate weighted squared residuals using SEM
            weights = 1 / (sems ** 2)  # Weights are the inverse of SEM squared
            weighted_residuals = weights * (observed_concs - simulated_concs) ** 2
            weighted_residuals_sq += np.sum(weighted_residuals)

        return weighted_residuals_sq


    # Initial parameter guesses
    initial_params = [0.1, 0.01, 1.0]  # Example: ka, ke, Vd

    result = minimize(global_objective, initial_params, method='L-BFGS-B', bounds=[(1e-5, 1), (1e-5, 1), (0.1, 1000)], options={'disp': True})


    print("Optimization Result:", result)



    # Optimized parameters from the output
    ka_opt, ke_opt, Vd_opt = 1.000, 8.275e-2, 3.665e-1

    for i,_ in enumerate(experiment_ids):

        # Select an experiment to visualize, for example, the first experiment
        experiment_id_to_plot =  experiment_ids[i] # Adjust this based on the experiment you wish to plot
        experiment_data = data[data['experiment_id'] == experiment_id_to_plot]
        times = experiment_data['time'].values
        observed_concs = experiment_data['plasma_conc'].values

        # Corresponding dose_timing for the selected experiment
        dose_timing = dose_timings[i]  # Assuming 1-based indexing for experiment IDs

        # Simulate concentrations considering all doses in the selected regimen
        simulated_concs = np.sum([
            simulate_plasma_concentration(times, dose_time, dose_amount, ka_opt, ke_opt, Vd_opt)
            for dose_time, dose_amount in dose_timing
        ], axis=0)

        import matplotlib.pyplot as plt

        plt.figure(figsize=(10, 6))
        plt.plot(times, observed_concs, 'o', label='Observed Concentrations')
        plt.plot(times, simulated_concs, '-', label='Simulated Concentrations', color='red')
        plt.xlabel('Time (hours)')
        plt.ylabel('Plasma Concentration')
        plt.title(f'Observed vs. Simulated Plasma Concentrations for Experiment {experiment_id_to_plot}')
        plt.legend()
        plt.show()


    dose_timing = [(0,30),(3,50),(5,30)]

    simulated_concs = np.sum([
            simulate_plasma_concentration(times, dose_time, dose_amount, ka_opt, ke_opt, Vd_opt)
            for dose_time, dose_amount in dose_timing
        ], axis=0)

    plt.figure(figsize=(10, 6))
    plt.plot(times, simulated_concs, '-', label='Simulated Concentrations', color='red')
    plt.xlabel('Time (hours)')
    plt.ylabel('Plasma Concentration')
    plt.legend()
    plt.show()




    def emax_model_with_tolerance(concentration, time, E0, Emax, EC50_initial, tolerance_rate):
        """
        A modified Emax model that includes tolerance development over time.

        Parameters:
        - concentration: Drug concentration at the time of effect measurement.
        - time: Time since the start of drug exposure.
        - E0: Baseline effect.
        - Emax: Maximum achievable effect.
        - EC50_initial: Initial EC50, at the start of exposure.
        - tolerance_rate: Rate at which EC50 increases over time.
        """
        EC50 = EC50_initial + tolerance_rate * time
        effect = E0 + (Emax * concentration) / (EC50 + concentration)
        return effect

    def emax_model_with_tolerance_combined(X, E0, Emax, EC50_initial, tolerance_rate):
        """
        Adjusted model function that accepts a combined array of independent variables for curve fitting.
        """
        concentration, time = X
        return emax_model_with_tolerance(concentration, time, E0, Emax, EC50_initial, tolerance_rate)

    # Combine independent variables into a single 2D array for curve_fit
    X_combined = np.vstack((concentrations, times))

    # Perform the curve fitting
    opt_params, param_cov = curve_fit(emax_model_with_tolerance_combined, X_combined, observed_effects, p0=[0, 100, 50, 0.1])

    print("Optimized Parameters:", opt_params)


    # Predict effects using the optimized parameters
    predicted_effects = emax_model_with_tolerance_combined(X_combined, *opt_params)

    # Plot observed vs. predicted effects
    plt.figure(figsize=(10, 6))
    plt.plot(times, observed_effects, 'o', label='Observed Effects')
    plt.plot(times, predicted_effects, '-', label='Predicted Effects')
    plt.xlabel('Time')
    plt.ylabel('Effect')
    plt.title('Observed vs. Predicted Drug Effects Over Time')
    plt.legend()
    plt.show()
    ```

<!--list-separator-->

-  Plotting acute tolerance

    ```R
    library(tidyverse)


     colors = c("#332288",
      "#117733",
      "#44AA99",
      "#88CCEE",
      "#DDCC77",
      "#CC6677",
      "#AA4499",
      "#882255")

    plasma = as_tibble(bind_rows(plasma1, plasma2, plasma3))

    effect = as_tibble(bind_rows(effect1, effect2, effect3))

    effect1_corr = effect1 %>%
      group_by(origin, measurment_point, dose) %>%
      mutate(time = round(mean(time), digits=1)) %>%
      ungroup() %>%
      pivot_wider(names_from = datapoint_type, values_from = effect_perc) %>%
      mutate(sem = abs(sem - mean),
             p_sem = mean + sem,
             m_sem = mean - sem) %>%
      mutate(dataset = "1")


    plasma1_corr = plasma1 %>%
      group_by(origin, measurment_point, dose) %>%
      mutate(time = round(mean(time), digits=1)) %>%
      ungroup() %>%
      pivot_wider(names_from = datapoint_type, values_from = plasma_conc) %>%
      mutate(sem = abs(sem - mean),
             p_sem = mean + sem,
             m_sem = mean - sem) %>%
      mutate(dataset = "1")


    max_first  <- max(plasma1_corr$p_sem)   # Specify max of first y axis
    max_second <- max(effect1_corr$p_sem) # Specify max of second y axis
    min_first  <- min(plasma1_corr$m_sem)   # Specify min of first y axis
    min_second <- min(effect1_corr$m_sem) # Specify min of second y axis

    # scale and shift variables calculated based on desired mins and maxes
    scale = (max_second - min_second)/(max_first - min_first)
    shift = min_first - min_second

    # Function to scale secondary axis
    scale_function <- function(x, scale, shift){
      return ((x)*scale - shift)
    }

    # Function to scale secondary variable values
    inv_scale_function <- function(x, scale, shift){
      return ((x + shift)/scale)
    }

    plot1 = ggplot(plasma1_corr, aes(x = time, y = mean)) +
      geom_line(aes(color = "Drug Concentration")) +
      geom_ribbon(aes(x = time, ymin = m_sem, ymax = p_sem, fill = "Drug Concentration"), alpha = 0.3) +
      geom_line(data = effect1_corr, aes(y = inv_scale_function(mean, scale, shift), color = "Blood MDMA (ng/ml)")) +
      geom_ribbon(data = effect1_corr, aes(ymin = inv_scale_function(m_sem, scale, shift),ymax = inv_scale_function(p_sem, scale, shift),  fill = "Blood MDMA (ng/ml)"), alpha = 0.3) +
     ##scale_x_continuous(breaks = seq(0, 336, 24)) +
      scale_y_continuous(limits = c(-Inf, max(plasma1_corr$p_sem)), sec.axis = sec_axis(~scale_function(., scale, shift), name="Effect (%)")) +
      labs(x = "Time (h)", y = "Blood MDMA (ng/ml)") +
      scale_color_manual(values = colors[1:2])+
      scale_fill_manual(values = colors[1:2])+
      coord_cartesian(xlim = c(0,6),ylim = c(-1, max_first),  expand = TRUE) +
      ggpubr::theme_pubr(legend = "none")+
        theme(
        axis.title.y = element_text(color = colors[1]),
        axis.title.y.right = element_text(color = colors[2])
        )



    effect2_corr = effect2 %>%
      group_by(origin, measurment_point, dose) %>%
      mutate(time = round(mean(time), digits=1)) %>%
      ungroup() %>%
      pivot_wider(names_from = datapoint_type, values_from = effect_perc) %>%
      mutate(sem = abs(sem - mean),
             p_sem = mean + sem,
             m_sem = mean - sem) %>%
      mutate(dataset = "2")


    plasma2_corr = plasma2 %>%
      group_by(origin, measurment_point, dose) %>%
      mutate(time = round(mean(time), digits=1)) %>%
      ungroup() %>%
      pivot_wider(names_from = datapoint_type, values_from = plasma_conc) %>%
      mutate(sem = abs(sem - mean),
             p_sem = mean + sem,
             m_sem = mean - sem)%>%
      mutate(dataset = "2")


    max_first  <- max(plasma2_corr$p_sem)   # Specify max of first y axis
    max_second <- max(effect2_corr$p_sem) # Specify max of second y axis
    min_first  <- min(plasma2_corr$m_sem)   # Specify min of first y axis
    min_second <- min(effect2_corr$m_sem) # Specify min of second y axis

    # scale and shift variables calculated based on desired mins and maxes
    scale = (max_second - min_second)/(max_first - min_first)
    shift = min_first - min_second

    # Function to scale secondary axis
    scale_function <- function(x, scale, shift){
      return ((x)*scale - shift)
    }

    # Function to scale secondary variable values
    inv_scale_function <- function(x, scale, shift){
      return ((x + shift)/scale)
    }

    plot2 = ggplot(plasma2_corr, aes(x = time, y = mean)) +
      geom_line(aes(color = "Drug Concentration")) +
      geom_ribbon(aes(x = time, ymin = m_sem, ymax = p_sem, fill = "Drug Concentration"), alpha = 0.3) +
      geom_line(data = effect2_corr, aes(y = inv_scale_function(mean, scale, shift), color = "Blood MDMA (ng/ml)")) +
      geom_ribbon(data = effect2_corr, aes(ymin = inv_scale_function(m_sem, scale, shift),ymax = inv_scale_function(p_sem, scale, shift),  fill = "Blood MDMA (ng/ml)"), alpha = 0.3) +
     ##scale_x_continuous(breaks = seq(0, 336, 24)) +
      scale_y_continuous(limits = c(-Inf, max(plasma2_corr$p_sem)), sec.axis = sec_axis(~scale_function(., scale, shift), name="Effect (%)")) +
      labs(x = "Time (h)", y = "Blood MDMA (ng/ml)") +
      scale_color_manual(values = colors[1:2])+
      scale_fill_manual(values = colors[1:2])+
      coord_cartesian(xlim = c(0,6),ylim = c(-1, max_first),  expand = TRUE) +
      ggpubr::theme_pubr(legend = "none")+
        theme(
        axis.title.y = element_text(color = colors[1]),
        axis.title.y.right = element_text(color = colors[2])
        )






    effect3_corr = effect3 %>%
      group_by(origin, measurment_point, dose) %>%
      mutate(time = round(mean(time), digits=1)) %>%
      ungroup() %>%
      pivot_wider(names_from = datapoint_type, values_from = effect_perc) %>%
      mutate(sem = abs(sem - mean),
             p_sem = mean + sem,
             m_sem = mean - sem) %>%
      mutate(dataset = "3")


    plasma3_corr = plasma3 %>%
      group_by(origin, measurment_point, dose) %>%
      mutate(time = round(mean(time), digits=1)) %>%
      ungroup() %>%
      pivot_wider(names_from = datapoint_type, values_from = plasma_conc) %>%
      mutate(sem = abs(sem - mean),
             p_sem = mean + sem,
             m_sem = mean - sem) %>%
      mutate(dataset = "3")


    max_first  <- max(plasma3_corr$p_sem)   # Specify max of first y axis
    max_second <- max(effect3_corr$p_sem) # Specify max of second y axis
    min_first  <- min(plasma3_corr$m_sem)   # Specify min of first y axis
    min_second <- min(effect3_corr$m_sem) # Specify min of second y axis

    # scale and shift variables calculated based on desired mins and maxes
    scale = (max_second - min_second)/(max_first - min_first)
    shift = min_first - min_second

    # Function to scale secondary axis
    scale_function <- function(x, scale, shift){
      return ((x)*scale - shift)
    }

    # Function to scale secondary variable values
    inv_scale_function <- function(x, scale, shift){
      return ((x + shift)/scale)
    }

    plot3 = ggplot(plasma3_corr, aes(x = time, y = mean)) +
      geom_line(aes(color = "Drug Concentration")) +
      geom_ribbon(aes(x = time, ymin = m_sem, ymax = p_sem, fill = "Drug Concentration"), alpha = 0.3) +
      geom_line(data = effect3_corr, aes(y = inv_scale_function(mean, scale, shift), color = "Blood MDMA (ng/ml)")) +
      geom_ribbon(data = effect3_corr, aes(ymin = inv_scale_function(m_sem, scale, shift),ymax = inv_scale_function(p_sem, scale, shift),  fill = "Blood MDMA (ng/ml)"), alpha = 0.3) +
     ##scale_x_continuous(breaks = seq(0, 336, 24)) +
      scale_y_continuous(limits = c(-Inf, max(plasma3_corr$p_sem)), sec.axis = sec_axis(~scale_function(., scale, shift), name="Effect (%)")) +
      labs(x = "Time (h)", y = "Blood MDMA (ng/ml)") +
      scale_color_manual(values = colors[c(2,6)])+
      scale_fill_manual(values = colors[c(2,6)])+
      coord_cartesian(xlim = c(0,12),ylim = c(-1, max_first),  expand = TRUE) +
      geom_vline(xintercept = c(0, 4), linetype = 3, size = 1) +
      annotate(geom = "label", x = c(0,4), y = c(400,400), label = c("50 mg", "100 mg"), size = 12)+
      ggpubr::theme_pubr(legend = "none", base_size = 24)+
        theme(
        axis.title.y = element_text(color = colors[2]),
        axis.title.y.right = element_text(color = colors[6]),
        )


    ggsave(filename = "./redose-50-100.pdf",plot3, scale = 1)

    plasma = as_tibble(bind_rows(plasma1_corr, plasma2_corr, plasma3_corr))

    effect = as_tibble(bind_rows(effect1_corr, effect2_corr, effect3_corr)) %>%
      select(mean, p_sem, m_sem, measurment_point,dataset, dose) %>%
      rename(mean_effect = mean, p_sem_effect = p_sem, m_sem_effect = m_sem)


    inner_join(plasma,effect, by = c("measurment_point", "dataset", "dose")) %>%
      filter(dose >0) %>%
      group_by(dataset) %>%
      mutate(time = time - first(time)) %>%
      ungroup() %>%
      group_by(dataset, dose_timing) %>%
      mutate(time_since_dose = time - first(time)) %>%
      ungroup() %>%
      ggplot(aes(x = mean, y = mean_effect, color = time_since_dose)) +
      geom_path(size = 2)+
      geom_errorbar(aes(ymin = m_sem_effect, ymax = p_sem_effect)) +
      geom_errorbarh(aes(xmin = m_sem, xmax = p_sem)) +
      cmocean::scale_color_cmocean(name = "phase")+
      facet_wrap(.~dataset, ncol = 2)+
      ggpubr::theme_pubr()
    ```
