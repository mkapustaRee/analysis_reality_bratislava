
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




# create charts -----------------------------------------------------------


chart_forecast_vs_actual <- ggplot(data = df_predict, aes(df_predict$Cena, df_prediction)) +
        labs(title = "Testing the linear model on test dataset",
             subtitle = "train = 80%, test = 20 % of sample",
             x = "Actual price", y = "Forecasted price") + 
        geom_point() +
        scale_x_continuous(labels = scales::comma)  +
        scale_y_continuous(labels = scales::comma) + 
        geom_smooth(method = "lm",se = F) +
        theme_minimal(base_family = "Georgia")


chart_lm_by_rooms <-agm %>%  
        ggplot(aes(.fitted,Cena)) +
        geom_point(alpha = .3,show.legend = F) +
        theme_minimal() +
        geom_abline()+
        scale_x_continuous(labels = scales::dollar_format(prefix = "€"))+
        scale_y_continuous(labels = scales::dollar_format(prefix = "€")) +
        labs(title = "Regression model of flats in the Bratislava") +
        facet_wrap(~Kategória,scales = "free")


chart_lm_by_Lokalita <-agm %>%  
        ggplot(aes(.fitted,Cena)) +
        geom_point(alpha = .3,show.legend = F) +
        theme_minimal() +
        scale_x_continuous(labels = scales::dollar_format(prefix = "€"))+
        scale_y_continuous(labels = scales::dollar_format(prefix = "€")) +
        labs(title = "Regression model of flats in the Bratislava") +
        geom_abline() + 
        facet_wrap(~Lokalita,scales = "free") 



# create stats -----------------------------------------------------------

sum_by <- function(data,...) {
        gr_var <- quos(...)
        
        data %>% 
                group_by(!!!gr_var) %>% 
                summarize(count = n(),
                          mean = round(median(Cena),0),
                          model_mean = round(mean(.fitted),0),
                          sd = round(sd(Cena),0),
                          min = round(min(Cena),0),
                          max = round(max(Cena),0),
                          dif = round(mean - model_mean,0)) %>% 
                modify_at(c("mean","model_mean","sd","min","max","diff"), scales::dollar_format("€"))
        
}


sum_by(agm, Kategória,Lokalita)
sum_by(agm, Kategória)
sum_by(agm, Lokalita)







