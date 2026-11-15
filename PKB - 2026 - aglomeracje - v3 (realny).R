#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ 
#### Usuniêcie wszystkich bibliotek, zmiennych oraz ustalenie seed, by stan by³ odtwarzalny za ka¿dym razem ####
#restartowanie sesji
#.rs.restartR()

#od³¹czeni wszytkich pakietów
detachAllPackages <- function() {
  basic.packages <- c("package:stats","package:graphics","package:grDevices","package:utils","package:datasets","package:methods","package:base")
  package.list <- search()[ifelse(unlist(gregexpr("package:",search()))==1,TRUE,FALSE)]
  package.list <- setdiff(package.list,basic.packages)
  if (length(package.list)>0)  for (package in package.list) detach(package, character.only=TRUE)
}

detachAllPackages() #wywo³anie funkcji 

#usuniêcie zmiennych
gc(reset = TRUE)
rm(list = ls())

#ustalenie seed
set.seed(1)

Sys.setenv(LANGUAGE='en')

#install.packages("eurostat")

library(eurostat)
library(dplyr)
library(stringr) #do stri count
library(ggplot2)
library(viridis)
library(scales)
#library(extrafont) #do czcionek
#font_import()
#loadfonts(device="win")
source("C:/Krzysiek/wrodata/base_color_map/wrodata_colors.R", encoding = "UTF-8") # paleta WroData


# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#### THEME ####

rok_od <- 1990
rok_do <- 2027

#kolory versja
wersja_kolorystyczna <- "v2"
#
if (wersja_kolorystyczna == "wrodata"){
  # paleta kategoryczna WroData dla 7 kategorii (te same barwy co wd_paleta(7));
  # Wrocław w kolorze sygnaturowym, Warszawa w grafitowym
  cmap = c(
    "wrocławska" = wd_kolory[["pomarancz"]],               # #D95F0E
    "górnośląsko-zagłębiowska" = wd_kolory[["czekolada"]], # #3D1A08
    "krakowska" = wd_kolory[["rdza"]],                     # #993404
    "trójmiejska" = wd_kolory[["granat"]],                 # #08519C
    "poznańska" = wd_kolory[["zloty"]],                    # #FEC44F
    "łódzka" = wd_kolory[["szary_jasny"]],                 # #999999
    "warszawska" = wd_kolory[["grafit"]]                   # #4E4D47
  )
} else if (wersja_kolorystyczna == "v1"){
  C11 <- "#FEEEE6"
  C10 <- "#FDCFB9"
  C9  <- "#FCB08D"
  C8  <- "#FB9160"
  C7  <- "#FA7233"
  C6  <- "#F95306"
  C5  <- "#CC4405"
  C4  <- "#993404"
  C3  <- "#722603"
  C2  <- "#461702"
  C1  <- "#190801"
  C0  <- "#000000"
  
  cmap = c(
    "wrocławska" = C0, 
    "górnośląsko-zagłębiowska" = C4, 
    "krakowska" = C6, 
    "trójmiejska" = C8, 
    "poznańska" = C9, 
    "łódzka" = C10,
    "warszawska" = "gray70"
  )
} else if (wersja_kolorystyczna == "v2"){
  C5  <- "#B77B69"
  C4  <- "#D89A4A"
  C3  <- "#E17B43"
  C2  <- "#C4512D"
  C1  <- "#7F3121"
  C0  <- "#2B1710"
  
  cmap = c(
    "wrocławska" = C0, 
    "górnośląsko-zagłębiowska" = C1, 
    "krakowska" = C2, 
    "trójmiejska" = C3, 
    "poznańska" = C4, 
    "łódzka" = C5,
    "warszawska" = "#77736F"
  )
} else if (wersja_kolorystyczna == "v3"){
  C5  <- "#B87B68"
  C4  <- "#D89A43"
  C3  <- "#EA7435"
  C2  <- "#D95628"
  C1  <- "#B63A20"
  C0  <- "#24130D"
  
  cmap = c(
    "wrocławska" = C0, 
    "górnośląsko-zagłębiowska" = C1, 
    "krakowska" = C2, 
    "trójmiejska" = C3, 
    "poznańska" = C4, 
    "łódzka" = C5,
    "warszawska" = "#77736F"
  )
} 



FILL_COL <- "#dedfe0"
TEXT_COL <- "#4e4d47"
TEXT_BASE_SIZE <- 12


THEME <-
  theme(
    axis.line = element_blank(),
    axis.text.x = element_text(family = "Ubuntu", size = TEXT_BASE_SIZE,  hjust = 0.5,  color = TEXT_COL),
    axis.text.y = element_text(family = "Ubuntu", size = TEXT_BASE_SIZE,  hjust = 0.0,  color = TEXT_COL),
    axis.ticks = element_blank(),
    axis.title.x = element_text(family = "Ubuntu", size = TEXT_BASE_SIZE, hjust = 0.5,  color = TEXT_COL),
    axis.title.y = element_text(family = "Ubuntu", size = TEXT_BASE_SIZE, hjust = 0.5,  color = TEXT_COL),
    
    
    #panel.border = element_blank(),
    # panel.grid.major=element_blank(),
    #panel.grid.minor = element_blank(),
    
    #tlo
    plot.background  = element_rect(fill = FILL_COL,  color = NA), 
    panel.background = element_rect(fill = FILL_COL,  color = NA),
    text = element_text(family = "Ubuntu", size = TEXT_BASE_SIZE - 2, color = TEXT_COL),
    
    # legenda
    legend.position = "bottom",# "none",
    legend.key.width = unit(0.9, "cm"),
    legend.key.height = unit(0.3, "cm"),
    legend.title.align = 0.5,
    legend.title = element_text(family = "Ubuntu", size = TEXT_BASE_SIZE, hjust = 0, color = TEXT_COL),#element_blank(),
    legend.background = element_rect(fill = FILL_COL, color = NA),
    legend.key = element_rect(fill = FILL_COL),
    legend.text       = element_text(family = "Ubuntu", size = TEXT_BASE_SIZE, hjust = 0, color = TEXT_COL),
    legend.direction = "horizontal",
    
    strip.text = element_text(size = TEXT_BASE_SIZE, color = TEXT_COL), 
    
    # tytuy
    plot.title    = element_text(family = "Ubuntu", size = TEXT_BASE_SIZE + 2, hjust = 0.0,  color = TEXT_COL, face="bold"),
    plot.subtitle = element_text(family = "Ubuntu", size = TEXT_BASE_SIZE,  hjust = 0.01, face = "italic", color = TEXT_COL),
    plot.caption  = element_text(family = "Ubuntu", size = TEXT_BASE_SIZE - 2,  hjust = 0.99, color = TEXT_COL),
  )  


#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ 

MAIN_PATH <- dirname(rstudioapi::getSourceEditorContext()$path)
IN_PATH <- paste0(MAIN_PATH, "\\Dane\\")
OUT_PATH <- paste0(MAIN_PATH, "\\Wykresy\\")


#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ 
#do?acznei opis?w  NUTS 3 
dane <- read.csv(paste0(IN_PATH, "\\ARDECO-SOVGD.versions_2024-unit_MIO_EUR2020-level_id_3.csv"), stringsAsFactors = F) %>%
  mutate(
    OBS_VALUE_num = as.numeric(VALUE),
    country = substr(TERRITORY_ID, 1, 2)
  ) %>%
  filter(country == "PL") %>%
  filter(VERSIONS == 2024) %>%
  filter(LEVEL_ID == 3) %>%
  rename(
    metric = UNIT,
    region = NAME_HTML ,
    year = YEAR,
    value = OBS_VALUE_num
  ) %>%
  select(region, year, metric, value)  %>%
  mutate(
    region_agg = case_when(
      region %in% c(
        "Miasto Wrocław",
        "Wrocławski",
        "Miasto Wroclaw",
        "Wroclawski"
      ) ~ "wrocławska",
      
      region %in% c(
        "Miasto Warszawa",
        "Warszawski zachodni",
        "Warszawski wschodni"
      ) ~ "warszawska",
      
      region %in% c(
        "Katowicki",  # Zaglebie
        "Gliwicki",   # Zaglebie
        "Bytomski",   # Zaglebie
        "Sosnowiecki", # Zaglebie
        "Tyski"
      ) ~ "górnośląsko-zagłębiowska",
      
      region %in% c(
        "Miasto Kraków",
        "Miasto Krakow",
        "Krakowski"
      ) ~ "krakowska",
      
      region %in% c(
        "Trójmiejski",
        "Trojmiejski",
        "Gdański",
        "Gdanski"
      ) ~ "trójmiejska",
      
      region %in% c(
        "Miasto Poznań",
        "Miasto Poznan",
        "Poznański",
        "Poznanski"
      ) ~ "poznańska",
      
      region %in% c(
        "Miasto Łódź",
        "Łódzki",
        "Miasto Lodz",
        "Lodzki",
        "Miasto Lódz",
        "Lódzki"
      ) ~ "łódzka",
      
      TRUE ~ region
      
    )
  ) 

Kolejnosc <- data.frame(
  region = c(
    "wrocławska", 
    "warszawska",
    "górnośląsko-zagłębiowska",
    "krakowska" , 
    "trójmiejska" , 
    "poznańska"  , 
    "łódzka" 
  ),
  Kolejnosc = 1:7
)


############################################################################
#### % Polski #### 

polska_pkb <- dane %>%
  group_by(year) %>%
  summarise(value = sum(value, na.rm = TRUE)) %>% 
  ungroup() %>% 
  rename(PKB_PL = value) %>%
  select(year, PKB_PL ) 


dane_wykres <- dane %>%
  filter(region_agg %in% Kolejnosc$region) %>%
  group_by(region_agg, year) %>%
  summarise(value = sum(value, na.rm = TRUE)) %>% 
  ungroup() %>% 
  arrange(year, desc(value)) %>% 
  left_join(polska_pkb, by = "year") %>%
  mutate(
    region = factor(region_agg, levels = Kolejnosc$region, ordered = TRUE),
    WRO = if_else(
        region_agg == "wrocławska",
        "Wrocław",
        "Pozostałe"
      ),
    grubosc = case_when(
      region_agg == "wrocławska" ~ 1.2,
      region_agg == "warszawska" ~ 1.0,
      TRUE ~ 0.7
    ),
    PRC_PKB_PL = value / PKB_PL
  ) %>% 
  ungroup()


#########################################################################
#### DANE DO OPISU #### 
  
wzrost_pkb <- dane_wykres %>%
  filter(year %in% c(rok_od, rok_do)) %>%
  select(region_agg, year, value) %>%
  tidyr::pivot_wider(
    names_from = year,
    values_from = value,
    names_prefix = "PKB_"
  ) %>%
  mutate(
    wzrost_proc = (
      .data[[paste0("PKB_", rok_do)]] /
        .data[[paste0("PKB_", rok_od)]] - 1
    ) * 100
  ) %>%
  arrange(desc(wzrost_proc))

wzrost_pkb


####
porownanie <- dane_wykres %>%
  filter(
    region_agg %in% c("wrocławska", "warszawska"),
    year %in% c(rok_od, rok_do)
  ) %>%
  group_by(region_agg) %>%
  summarise(
    PKB_od = value[year == rok_od],
    PKB_do = value[year == rok_do],
    CAGR = (PKB_do / PKB_od)^(1 / (rok_do - rok_od)) - 1,
    .groups = "drop"
  )

wro <- porownanie %>%
  filter(region_agg == "wrocławska")

waw <- porownanie %>%
  filter(region_agg == "warszawska")

if (wro$CAGR > waw$CAGR) {
  
  lata <- log(waw$PKB_do / wro$PKB_do) /
    log((1 + wro$CAGR) / (1 + waw$CAGR))
  
  wynik <- data.frame(
    PKB_Wrocław_do = wro$PKB_do,
    PKB_Warszawa_do = waw$PKB_do,
    CAGR_Wrocław_proc = 100 * wro$CAGR,
    CAGR_Warszawa_proc = 100 * waw$CAGR,
    liczba_lat = lata,
    rok_przegonienia = rok_do + ceiling(lata)
  )
  
} else {
  
  wynik <- data.frame(
    PKB_Wrocław_do = wro$PKB_do,
    PKB_Warszawa_do = waw$PKB_do,
    CAGR_Wrocław_proc = 100 * wro$CAGR,
    CAGR_Warszawa_proc = 100 * waw$CAGR,
    liczba_lat = Inf,
    rok_przegonienia = NA_integer_
  )
}

wynik



porownanie_waw_wro <- dane_wykres %>%
  filter(
    region_agg %in% c("warszawska", "wrocławska"),
    year %in% c(1990, 2026)
  ) %>%
  select(year, region_agg, value) %>%
  tidyr::pivot_wider(
    names_from = region_agg,
    values_from = value
  ) %>%
  mutate(
    ile_razy = warszawska / wrocławska,
    przewaga_proc = (warszawska / wrocławska - 1) * 100
  )

porownanie_waw_wro

#########################################################################
#### wykres - realne #### 

w <- ggplot(data=dane_wykres, aes(x=year, y=value, colour=region)) +
  geom_line(aes(linetype = WRO, linewidth = grubosc))  +
  geom_point() + 
  labs(
    title = "Warszawa ekonomicznie dominuje Polskę, dystans rośnie, a Polska się centralizuje",
    subtitle = "Przy zachowaniu aktualnego modelu rozwoju kraju żaden region nie jest w stanie dogonić stolicy",
    x = "Rok",
    y = "PKB realny [mln EUR, ceny z 2020 r.]",
    caption = paste0(
      "Autor: WroData (Krzysztof Karabon) | Dane: Komisja Europejska, baza ARDECO",
      "\n",   
      "Uwagi metodologiczne: podział na aglomeracje na podstawie NUTS 3:",
      "\n",   
      "wrocławski - Miasto Wrocław i podregion Wrocławski; ", 
      "warszawska - Miasto Warszawa, podregiony Warszawski zachodni i Warszawski wschodni; ",
      "\n",   
      "górnośląsko-zagłębiowska - podregiony Katowicki, Gliwicki, Bytomski, Tyski i Sosnowiecki; ",
      "krakowska - Miasto Kraków i podregion Krakowski; ",
      "\n",  
      "trójmiejska - podregiony Trójmiejski i Gdański; ",
      "poznańska - Miasto Poznań i podregion Poznański; ", 
      "łódzka - Miasto Łódź i podregion Łódzki.",
      ""
    )
  ) + 
  scale_colour_manual(
    values = cmap,
    name = " ",
    drop = FALSE, 
    guide = guide_legend(
      direction = "horizontal",
      override.aes = list(linetype = c("solid", rep("dashed", 6))))
  )   + 
  scale_linewidth_identity() +
  scale_x_continuous(breaks = seq(rok_od, rok_do, 5) ) +
  scale_y_continuous(
    labels = scales::label_number(
      big.mark = " ",
      decimal.mark = ","
  )) +
  coord_cartesian( xlim = c(rok_od, rok_do)) + 
  scale_size(guide = 'none') +
  scale_linetype_manual(
    values = c(
      "Wrocław" = "solid",
      "Pozostałe" = "dashed"
    ),
    guide = "none"
  ) +
  THEME

plot(w)

# czy udział spada?
udzial = dane_wykres %>%
  filter(region != "Warszawski stołeczny") %>%
  group_by(year) %>%
  summarise(PRC_PKB_PL = sum(PRC_PKB_PL))
dane_wykres %>%
  filter(region != "Warszawski stołeczny")  %>%
  filter(year %in% c(rok_od, rok_do)) %>%
  arrange(region, year) 





n <- 1.5
# save
png(filename = paste0(OUT_PATH, "rejony - PKB realne - ",  Sys.Date(), "wersja kolorystyczna ", wersja_kolorystyczna, " .png", sep=""),
    bg = FILL_COL, width = 16 / n, height =  9 / n, units = 'in', res = 500
)
plot(w)
dev.off()



