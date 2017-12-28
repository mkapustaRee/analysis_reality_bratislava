
library(tidyverse)
library(stringr)
library(rebus)
Sys.setlocale(category = "LC_ALL",locale ="de_DE")

# source the data -----------------------------------------------------------
df_old_long <- read_csv("_scraped_data/topreality_results [2017-09-17] [raw].csv")

# filter results ----------------------------------------------------------
df_ads <- df_old_long %>% filter(`Balkón / loggia` %in% c("Áno","Nie"))
# Stav
df_ads$Stav <- ifelse(df_ads$`Stav nehnuteľnosti:` %in% "Novostavba",
                      "kompletná rekonštrukcia",
                      as.character(df_ads$Stav))
df_ads <- df_ads %>%
        drop_na(Cena,Lokalita,`Úžitková plocha`,Ulica,Stav)
# Material
df_ads$Materiál <- ifelse(df_ads$Materiál == is.na(df_ads$Materiál),
                          "panel",
                          as.character(df_ads$Materiál))
# Cena
df_ads$Cena <- df_ads$Cena %>%
        str_replace_all("EUR", "") %>%
        str_replace_all(rebus::space(), "") %>%
        str_replace_all(",", ".") %>%
        as.numeric(df_ads$Cena)
# Plocha
df_ads$`Úžitková plocha` <- df_ads$`Úžitková plocha` %>%
        str_replace_all(" m2", "")  %>%
        as.numeric()
# Kategoria
df_ads$Kategória <- df_ads$Kategória %>%
        str_replace_all("izbový byt / Predaj", "") %>%
        str_replace_all("Dvojgarsónka / Predaj", "1.5") %>%
        str_replace_all("kompletná rekonštrukcia","") %>%
        str_replace_all(rebus::space(), "") %>%
        as.factor()
regex_pat <- group(" /" %R% space() %R% one_or_more(rebus::DIGIT))
# Poschodie
df_poschodie <- df_ads$Poschodie %>%
        unclass() %>%
        str_split("/", simplify = T) %>%
        as.tibble()
colnames(df_poschodie) <- c("Poschodie","Bytovka")
df_poschodie <- map_df(df_poschodie, as.numeric)
df_poschodie$Poschodie_feat <- ifelse(df_poschodie$Poschodie == 1,"prizemie",
                                      ifelse(df_poschodie$Poschodie == df_poschodie$Bytovka, "strop","ine"))
df_ads$Poschodie <- df_poschodie$Poschodie_feat %>% as.factor()
# Lokalita
df_ads$Lokalita <- df_ads$Lokalita %>% as.factor()
regex_pat2 <- group(", časť" %R% space()  %R% one_or_more(rebus::WRD))
df_ads$Lokalita  <- df_ads$Lokalita  %>%
        str_replace_all(regex_pat2, "") %>%
        str_replace_all(" / Kuchajda", "") %>%
        str_replace_all(" hony", "") %>%
        str_replace_all(" diely", "") %>%
        str_replace_all(" háj", "") %>%
        str_replace_all(" jarkami", "") %>%
        str_replace_all(" kolónia", "") %>%
        str_replace_all(" dolina", "") %>%
        str_replace_all(" Stred", "") %>%
        str_replace_all(" hrdlo", "") %>%
        as.factor()
# feature engineering -----------------------------------------------------
df_ads <- df_ads %>%
        group_by(Kategória) %>%
        mutate(Size_mean = mean(`Úžitková plocha`, na.rm = T)) %>%
        ungroup() %>%
        mutate(Size_extra = `Úžitková plocha`-Size_mean,
               Size_bin = ntile(Size_extra,5))
# model statistics --------------------------------------------------------
model <- lm(Cena ~ Kategória +
                    Výťah +
                    Size_bin +
                    Materiál   +
                    Poschodie +
                    Lokalita - 1, data = df_ads)
summary(model)
gln <- broom::glance(model)
tdy <- broom::tidy(model)
agm <- broom::augment(model)

# model statistics --------------------------------------------------------


# create train & predict
df_ads$id <- 1:nrow(df_ads)
df_train   <- df_ads %>% dplyr::sample_frac(size = .8)
df_predict <- dplyr::anti_join(df_ads, df_train, by = 'id') 
df_predict <- df_predict %>% filter(Lokalita != "Rusovce")
model <- lm(Cena ~ Kategória +
                    Výťah +
                    Size_bin +
                    Materiál   +
                    Poschodie +
                    Lokalita - 1, data = df_train)
summary(model)
gln <- broom::glance(model)
tdy <- broom::tidy(model)
agm <- broom::augment(model)

df_prediction <- predict.lm(object = model,newdata = df_predict)

ggplot(data = df_predict, aes(df_predict$Cena, df_prediction)) +
        geom_point() +
        scale_x_continuous(labels = scales::comma)  +
        scale_y_continuous(labels = scales::comma) + 
        geom_smooth(method = "lm",se = F)

