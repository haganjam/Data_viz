
# Visualise the EU energy data

# load the relevant libraries
library(ggplot2)
library(ggstream)
library(ggtext)
library(tidyr)
library(dplyr)
library(ggimage)
library(countrycode)
library(extrafont)
library(patchwork)
library(ggnewscale)
library(gggibbous)

# load plotting theme
source("02-EU-energy/code/helper-plotting-theme.R")

# choose some fonts
choose_font(c("GillSans",  "Verdana", "sans"), quiet = TRUE)

# load the cleaned data
energy <- readRDS(file = "02-EU-energy/data/EU_energy_data.rds")
head(energy)
summary(energy)

# pull the data into the long format
energy <- 
  energy |>
  tidyr::pivot_longer(cols = c("solid_ff", "oil", "gas", "renew", "other"),
                      names_to = "energy_source",
                      values_to = "proportion") |>
  mutate(share = total*proportion)

# check the countries in the data
unique(energy$eu_member)

# remove Turkey, UK , Croatia and Cyprus
energy <- 
  energy |>
  dplyr::filter(!(eu_member %in% c("Turkey", "United Kingdom", "Croatia", "Cyprus")))

# pick the n EU countries with the highest total CHP
n <- 8
eu_sort <- 
  energy |>
  dplyr::group_by(eu_member) |>
  dplyr::summarise(total = mean(total)) |>
  dplyr::arrange(desc(total)) |>
  dplyr::pull(eu_member)

# get the 10 countries with the highets CHP
energy <- dplyr::filter(energy, eu_member %in% eu_sort[1:n])

# how many countries are left over?
length(unique(energy$eu_member))

# which country year has the most CHP energy production?
energy |>
  filter(total == max(total))

# add values to the start and end for a smoother look
energy_smooth <- 
  energy |> 
  dplyr::group_by(eu_member) |> 
  dplyr::slice(1:10) |>
  dplyr::ungroup() |>
  dplyr::mutate(
    year = rep(c(rep(2003, 5),  rep(2021, 5)), n), 
    share = rep(rep(c(0, 0.001, 0.001, 0.001, 0), 2), n)
  )

# bind the two datasets
energy <- 
  dplyr::bind_rows(energy, energy_smooth) |>
  dplyr::arrange(eu_member, year)

# colour palette
energy$energy_source_fac <- 
  factor(energy$energy_source,
         levels = c("solid_ff", "oil", "gas", "renew", "other"))

# make a shade layer for the colours to make it look cooler
energy2 <- 
  energy |>
  dplyr::mutate(share = share/2)

# make energy source a factor
energy2$energy_source_fac <- factor(paste0(energy2$energy_source_fac, "2"))

# combine energy and energy2
energy <- 
  dplyr::bind_rows(energy, energy2) |>
  dplyr::arrange(eu_member, year)

# make sure the energy source is a factor
energy$energy_source_fac <- factor(energy$energy_source_fac,
                                   levels = c("solid_ff", "solid_ff2", 
                                              "oil", "oil2",
                                              "gas", "gas2",
                                              "renew", "renew2",
                                              "other", "other2"))

# make a new shading variable
energy <- 
  energy |>
  dplyr::mutate(shading = ifelse(grepl(pattern = "2", x = energy_source_fac),"yes", "no" ))

# convert original energy_source to a factor
energy$energy_source <- 
  factor(energy$energy_source,
         levels = c("solid_ff", "oil", "gas", "renew", "other"))

# colour palette
cols <- c("brown", "black", "#CC9966", "#228b22", "grey")

# convert the country to a factor
energy$eu_member_fac <- factor(energy$eu_member)

# label the countries
country_labels <- 
  dplyr::tibble(year = 2002,
                share = 0,
                eu_member_fac = factor(levels(energy$eu_member_fac), levels(energy$eu_member_fac)),
                country = unique(energy$eu_member_fac),
                country_code = countrycode(unique(energy$eu_member_fac), "country.name", "iso2c"))

# label the vertical lines
vert_labs <- 
  dplyr::tibble(year = rep(c(2005, 2019), n),
                eu_member_fac = factor(rep(levels(energy$eu_member_fac), each = 2), levels(energy$eu_member_fac)),
                label = c("Start", "End", rep(c(NA, NA), n-1)))

# reference values
ref_vals <- 
  dplyr::tibble(year = c(2006, 2013, NA, NA, NA,NA, 2007, NA),
                eu_member_fac = factor(c(levels(energy$eu_member_fac)), levels(energy$eu_member_fac)),
                share_min = c(-350, -450, NA, NA, NA, NA, -1450, NA),
                share_max = c(350, 450, NA, NA, NA, NA, 1450, NA))

# reference value labels
ref_vals_lab <- 
  dplyr::tibble(year = c(2006, 2013, NA, NA, NA, NA, 2007, NA),
                eu_member_fac = factor(c(levels(energy$eu_member_fac)), levels(energy$eu_member_fac)),
                share_pos = c( 0, 0, NA, NA, NA, NA, 0, NA),
                label = c("371 PJ",
                          "411 PJ",
                          NA, NA, NA, NA,
                          "1508 PJ", 
                          NA),
                hjust = c(0.3, 0.3, NA, NA, NA, NA, -0.1, NA),
                vjust = c(3, -2.2, NA, NA, NA, NA, -0.4, NA))

# make a stream plot
p1 <- 
  ggplot(data = energy,
       mapping = aes(x = year, y = share, colour = energy_source, fill = energy_source, alpha = shading)) +
  geom_stream(
    geom = "contour",
    size = 0,
    bw = 0.75,
    extra_span = .2, 
    true_range = "none"
  ) +
  geom_stream(
    geom = "polygon",
    bw = 0.75,
    size = 0,
    extra_span = .2, 
    true_range = "none"
  ) +
  scale_fill_manual(values = cols) +
  scale_colour_manual(values = cols) +
  scale_alpha_manual(values = c(0.9, 0.75)) +
  geom_flag(
    data = country_labels,
    inherit.aes = FALSE,
    mapping = aes(x = year, y = share, image = country_code), 
    size = 0.4, alpha = 1) +
  geom_text(
    data = country_labels,
    mapping = aes(x = year, y = share-950, label = country),
    inherit.aes = F,
    size = 3.5,
    color = "grey25",
    fontface = "plain",
    lineheight = .85,
    hjust = 0.5
  ) +
  geom_vline(
    data = dplyr::tibble(x = c(2005, 2019)),
    mapping = aes(xintercept = x),
    color = "#CC9966", 
    size = .75,
    linetype = "dotted") +
  geom_errorbar(
    data = ref_vals,
    mapping = aes(x = year, ymin = share_min, ymax = share_max),
    inherit.aes = FALSE,
    color = "grey25",
    size = 0.5,
    width = 0.2
  ) +
  geom_text(
    data = ref_vals_lab,
    mapping = aes(x = year, y = share_pos, 
                  label = label, hjust = hjust, vjust = vjust),
    inherit.aes = FALSE,
    colour = "grey25",
    size = 3.5,
    fontface = "bold"
  ) +
  geom_label(
    data = vert_labs,
    mapping = aes(x = year, y = 1100, label = label),
    inherit.aes = F,
    family = "Tahoma",
    size = 3.5,
    color = "grey25",
    fill = "#FDEBD0",
    label.size = NA,
    fontface = "plain",
    lineheight = .85
  ) +
  facet_grid( ## needs facet_grid for space argument
    eu_member_fac ~ ., 
    scales = "fixed", 
    space = "free"
  ) +
  ggtitle(label = "Top 8 CHP fuel-users in the EU") +
  scale_x_continuous(limits = c(2001.5, 2028), breaks = seq(2005, 2019, by = 2)) +
  theme(legend.position = "none",
        axis.text.x = element_text(colour = "black", size = 12),
        plot.title = element_text(size = 15,vjust = 2, hjust = 0.3),
        plot.margin = margin(c(5, 5, 5, 5)))
# plot(p1)

# add text boxes
texts <-
  tibble(
    year = rep(c(2025), n),
    share = rep(0, n),
    eu_member_fac = factor(levels(energy$eu_member_fac), levels(energy$eu_member_fac)),
    energy_source = factor(c("gas", "renew", "oil", "oil", "oil", "oil", "solid_ff", "oil"),
                           levels = c("solid_ff", "oil", "gas", "renew", "other")),
    text = c(
      NA,
      "More than 90% of **Finland's** apartment blocks use CHP systems. Many are powered by renewable fuels, mostly wood.",
      NA,
      "**Germany** produces the most energy with CHP systems. Moreover, Germany's CHP systems are the most diversified and use all the different fuel sources.",
      NA,
      NA,
      "**Poland** uses solid fossil fuels (e.g. coal) for the majority of its CHP systems.",
      NA),
    vjust = 0.5
  )

p1 <- 
  p1 +
  geom_textbox(
    data = texts,
    mapping = aes(
      x = year,
      y = share,
      label = text,
      colour = energy_source,
      vjust = vjust
    ),
    inherit.aes = FALSE,
    size = 2.75,
    fill = "grey95",
    width = unit(125, "pt"),
    hjust = 0.4
  )
# plot(p1)

# export the plot to test the output
# ggsave( "02-EU-energy/figures-tables/figtest.pdf", p1,
      # width = 7.2, height = 9, device = cairo_pdf)

  
# use moon plots for the legends
leg_dat <- dplyr::tibble(
  x = rep(c(1.1, 2, 2.9, 1.1, 2), 2),
  y = rep(c(rep(2, 3), rep(1.5, 2)), 2),
  ratio = c(rep(0.3, 5), rep(0.7, 5)),
  right = rep(c(FALSE, TRUE), each = 5),
  energy_source = factor(rep(c("solid_ff", "oil", "gas", "renew", "other"),2),
                         levels = c("solid_ff", "oil", "gas", "renew", "other")))

leg_text <- dplyr::tibble(
  x = c(1.1, 2, 2.9, 1.1, 2),
  y = rep(c(rep(2, 3), rep(1.5, 2))),
  label = c("Solid fossil fuels", 
            "Oil products",
            "Natural gas",
            "Renewables",
            "Other"))

leg <- 
  ggplot() +
  gggibbous::geom_moon(
    data = leg_dat,
    mapping = aes(
      x = x, y = y, ratio = ratio, right = right, fill = energy_source, alpha = right),
    size = 20,
    colour = NA) +
  geom_text(
    data = leg_text,
    mapping = aes(x = x, y = y, label = label),
    size = 3.5,
    color = "black",
    vjust = -3.6) +
  scale_fill_manual(values = cols) +
  scale_alpha_manual(values = c(0.75, 0.9)) +
  scale_x_continuous(limits = c(0.75, 3.25)) +
  scale_y_continuous(limits = c(1.25, 2.25)) +
  theme(legend.position = "none",
        plot.margin = margin(c(10, 10, 50, 10)),
        axis.text.x = element_blank())
plot(leg)

# text on the left
text_plot <- 
  ggplot() +
  geom_textbox(
  data = dplyr::tibble(
    x = 0,
    y = c(1.45, 1.3125),
    label = c(
      "<b style='font-size:14pt'>**What fuels are European countries using for Combined Heat and Power (CHP)?**</b><br><br>Combined Heat and Power (CHP), also known as cogeneration, is a potentially crucial aspect of EU net-zero strategies. These systems use power stations or heat engines to generate both electricity and heat simultaneously. For example, some CHP systems use high-temperature heat to power a turbine and generate electricity and then distribute the excess heat to heat water or air in homes, offices etc. These systems can be highly efficient and, therefore, could play an important role in the climate transition.",
      "Many European Union countries already use these systems but they tend to differ in the fuels that are used. **Eurostat** recognises five major fuel types: solid fossil fuels (e.g. coal), oil products, natural gas, renewables (e.g. wood, organic waste) and other fuels (e.g. industrial waste). The countries that use the most fuel for CHP are: **Austria**, **Denmark**, **Finland**, **France**, **Germany**, **Italy**, **Netherlands**, **Poland**, **Spain** and **Sweden**. Poland and Germany are the biggest CHP fuel users in Europe. The Nordic countries are strong represented and also use significant amounts of renewable fuels.")
  ),
  mapping = aes(
    x = x, 
    y = y, 
    label = label),
  width = unit(4, "inch"),
  color = "black",
  lineheight = 1.7,
  size = 2.7,
  fill = NA,
  box.colour = NA
  ) +
  coord_cartesian(clip = "off") +
  scale_y_continuous(limits = c(1.25, 1.5)) +
  theme(axis.text.x = element_blank(),
        plot.margin = margin(c(10, 10, 10, 10)))
# plot(text_plot)

text_ref <- 
  ggplot() +
  geom_textbox(
    data = dplyr::tibble(
      x = 0,
      y = c(0),
      label = c(
        "<span>**Notes:** Fuel use reported in Peta Joules (PJ) as Net calorific values.<br> 
        Visualization by James G. Hagan.<br> 
        Data from *Eurostat*."),
    ),
    mapping = aes(
      x = x, 
      y = y, 
      label = label),
    width = unit(4, "inch"),
    color = "black",
    lineheight = 1.7,
    size = 3,
    fill = NA,
    box.colour = NA,
  ) +
  coord_cartesian(clip = "off") +
  theme(axis.text.x = element_blank(),
        plot.margin = margin(c(-5, 10, -5, 10)))
# plot(text_ref)


# combine left-margin plots
g <- ( (text_plot/leg/text_ref) ) + plot_layout(heights = c(1, 0.6, 0.1))

# export the plot to test the output
# ggsave( "02-EU-energy/figures-tables/figtest2.pdf", g,
        # width = 4, height = 9, device = cairo_pdf)

# combine with the main plot
h <- ( (g | p1)  + plot_layout(widths = c(0.55, 1)))
# plot(g)

# export the plot
ggsave(filename = "02-EU-energy/figures-tables/fig1.pdf", h,
       width = 12, height = 9, device = cairo_pdf)

### END
