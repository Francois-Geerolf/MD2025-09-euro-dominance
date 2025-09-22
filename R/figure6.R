source("_rinit.R")

## Load Eurostat datasets ------

load(url("https://fgeerolf.com/data/flags/colors.RData"))

# https://fgeerolf.com/data/eurostat/ei_mfir_m_files/figure-html/MF-LTGBY-RT-FR-DE-IT-ES-1995-1.png

## Load Eurostat datasets ------

datasets_eurostat <- c("ei_mfir_m")

for (dataset in datasets_eurostat){
  assign(dataset, 
         get_eurostat(dataset, stringsAsFactors = F, cache = F) |>
           rename(date = TIME_PERIOD)
  )
}

## English -------

Sys.setlocale("LC_TIME", "en_US.UTF-8")

figure6 <- ei_mfir_m |>
  filter(indic %in% c("MF-LTGBY-RT"),
         geo %in% c("FR", "DE", "IT", "ES")) |>
  left_join(get_eurostat_dic("geo"), by = c("geo" = "code_name")) |>
  filter(date >= as.Date("1995-01-01")) |>
  mutate(values = values / 100) |>
  left_join(colors, by = c("full_name" = "country"))


# Build color mapping: each country gets its hex
country_colors <- figure6 |> 
  distinct(full_name, color) |> 
  tibble::deframe()

write.csv(figure6, "csv/figure6.csv")


ggplot(data = figure6) +
  geom_line(aes(x = date, y = values, color = full_name)) +
  scale_color_manual(values = country_colors) + theme_minimal()  +
  scale_x_date(breaks = as.Date(paste0(seq(1960, 2100, 5), "-01-01")),
               labels = date_format("%Y")) +
  xlab("") + ylab("") +
  scale_y_continuous(breaks = 0.01*seq(-30, 30, 2),
                     labels = percent_format(a = 1)) + 
  theme(legend.position = c(0.8, 0.85),
        legend.title = element_blank(),
        legend.direction = "vertical") +
  geom_hline(yintercept = 0, linetype = "dashed",  color = "black")


ggsave("pdf/figure6.pdf", width = 7, height = 4, device = cairo_pdf)
ggsave("png/figure6.png", width = 7, height = 4, bg = "white")
