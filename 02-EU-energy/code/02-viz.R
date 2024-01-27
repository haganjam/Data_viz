
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
n <- 10
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

# make a stream plot
p1 <- 
  ggplot(data = energy,
       mapping = aes(x = year, y = share, colour = energy_source, fill = energy_source, alpha = shading)) +
  geom_stream(
    geom = "contour",
    size = 0,
    bw = .7
  ) +
  geom_stream(
    geom = "polygon",
    bw = .7,
    size = 0
  ) +
  geom_flag(
    data = country_labels,
    inherit.aes = FALSE,
    mapping = aes(x = year, y = share, image = country_code), 
    size = 0.4) +
  geom_text(
    data = country_labels,
    mapping = aes(x = year, y = share-500, label = country),
    inherit.aes = F,
    family = "Tahoma",
    size = 2.5,
    color = "grey25",
    fontface = "plain",
    lineheight = .85,
    hjust = 0.4
  ) +
  geom_vline(
    data = dplyr::tibble(x = c(2005, 2019)),
    mapping = aes(xintercept = x),
    color = "#CC9966", 
    size = .75,
    linetype = "dotted") +
  geom_label(
    data = vert_labs,
    mapping = aes(x = year, y = 700, label = label),
    inherit.aes = F,
    family = "Tahoma",
    size = 3,
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
  ggtitle(label = "Top 10 CHP generators in the EU") +
  scale_x_continuous(limits = c(2000, 2030), breaks = seq(2005, 2019, by = 2)) +
  scale_fill_manual(values = cols) +
  scale_colour_manual(values = cols) +
  scale_alpha_manual(values = c(1, 0.9)) +
  theme(legend.position = "none",
        axis.text.x = element_text(colour = "black"),
        plot.title = element_text(size = 14,vjust = 2, hjust = 0.3))
plot(p1)

# add text boxes
texts <-
  tibble(
    year = rep(c(2025), n),
    share = rep(0, n),
    eu_member_fac = factor(levels(energy$eu_member_fac), levels(energy$eu_member_fac)),
    energy_source = factor(c("other", "gas", "renew", "oil", "oil", "oil", "oil", "solid_ff", "oil", "renew"),
                           levels = c("solid_ff", "oil", "gas", "renew", "other")),
    text = c(
      NA,
      NA,
      "Much of **Finland's** building stock is connected to a district heating facility with more than 90% of apartment blocks using CHP systems. Many of these systems are powered by renewable fuels, mostly wood and various waste products",
      NA,
      "**Germany** produces the most energy with CHP systems. Moreover, Germany's CHP systems are the most diversified and use all the different fuel sources.",
      NA,
      NA,
      "**Poland** uses solid fossil fuels (e.g. coal) for the majority of its CHP systems. However, the power and heat generated from CHP systems in Poland has declined substantially since 2008",
      NA,
      "Almost all of **Sweden's** CHP systems rely on renewable energy"),
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
    family = "Tahoma",
    size = 2.7,
    fill = "grey95",
    width = unit(200, "pt"),
    hjust = 0.4
  )
  
# text on the left
p2 <- 
  ggplot() +
  geom_textbox(
  data = dplyr::tibble(
    x = 0,
    y = c(1.5, 0.9, -0.5),
    label = c(
      "<b style='font-size:14pt'>What fuels are European countries using for Combined Heat and Power (CHP)?</b><br><br>Combined Heat and Power (CHP), also known as cogeneration, is a potentially crucial aspect of EU net-zero strategies. These systems use power stations or heat engines to generate both electricity and heat simultaneously. For example, some CHP systems use high-temperature heat to power a turbine and generate electricity and then distribute the excess heat to heat water or air in homes, offices etc. These systems can be highly efficient and, therefore, could play an important role in the climate transition.",
      "Many European Union countries already use these systems but they tend to differ in the fuels that are used. More specifically, there are ",
      "<span style='color:#656565'>For reference: The maximum fuel used for CHP by a country in a year was Poland in 2006 (1525 Net Calorific).<br>*Visualization by James G. Hagan • Data from Eurostat*</span>"),
    v = c(0.5, .5, 1.3)
  ),
  mapping = aes(
    x = x, 
    y = y, 
    label = label, 
    vjust = v),
  width = unit(5, "inch"),
  color = "black",
  family = "Tahoma",
  lineheight = 1.7,
  size = 3,
  fill = NA,
  box.colour = NA,
  hjust = 0
  ) +
  coord_cartesian(clip = "off") +
  scale_x_continuous(limits = c(0, 1)) +
  scale_y_continuous(limits = c(-0.6, 1.5)) +
  theme(axis.text.x = element_blank(),
        plot.margin = margin(c(-5, 5, 5, 5)))
plot(p2)

# combine these plots
((p2 | p1)  + plot_layout(widths = c(0.5, 1)))



