
# Visualise the EU energy data

# load the relevant libraries
library(ggplot2)
library(ggstream)
library(ggtext)
library(tidyr)
library(dplyr)
library(ggimage)
library(countrycode)

# load plotting theme
source("02-EU-energy/code/helper-plotting-theme.R")

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

# add values to the start and end for a smoother look
energy_smooth <- 
  energy |> 
  dplyr::group_by(eu_member) |> 
  dplyr::slice(1:10) |>
  dplyr::ungroup() |>
  dplyr::mutate(
    year = rep(c(rep(2003, 5),  rep(2021, 5)), n), 
    share = rep(rep(0, 10), n)
  )

# bind the two datasets
energy <- 
  dplyr::bind_rows(energy, energy_smooth) |>
  dplyr::arrange(eu_member, year)

# colour palette
energy$energy_source_fac <- 
  factor(energy$energy_source,
         levels = c("solid_ff", "oil", "gas", "renew", "other"))

# refactor the levels
levels(energy$energy_source_fac) <- 
  c("Solid fossil fuel", 
    "Oil", 
    "Gas",
    "Renewables",
    "Other")

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

# make a stream plot
ggplot(data = energy,
       mapping = aes(x = year, y = share, colour = energy_source_fac, fill = energy_source_fac)) +
  geom_stream(
    geom = "contour",
    color = "black",
    size = 0.75,
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
    family = "Times New Roman",
    size = 2.5,
    color = "grey25",
    fontface = "bold",
    lineheight = .85,
    hjust = 0.4
  ) +
  geom_vline(data = dplyr::tibble(x = seq(2005, 2019, by = 4)),
             mapping = aes(xintercept = x),
             color = "grey88", 
             size = .75,
             linetype = "dotted") +
  facet_grid( ## needs facet_grid for space argument
    eu_member_fac ~ ., 
    scales = "fixed", 
    space = "free"
  ) +
  scale_x_continuous(limits = c(2000, 2021), breaks = seq(2005, 2019, by = 2)) +
  scale_fill_manual(values = cols) +
  scale_colour_manual(values = cols) +
  theme(legend.position = "none",
        axis.text.x = element_text(colour = "black"))



# text on the left
ggplot() +
  geom_textbox(
  data = dplyr::tibble(
    x = 0,
    y = c(1.45, .7, -.34, -.6),
    label = c(
      "<b style='font-size:18pt'>What fuels are European countries using for Combined Heat and Power (CHP)?</b><br><br>Combined Heat and Power (CHP), also known as cogeneration, is a potentially crucial aspect of EU net-zero strategies. These systems use power stations or heat engines to generate both electricity and heat simultaneously. For example, some CHP systems use high-temperature heat to power a turbine and generate electricity and then distribute the excess heat to heat water or air in homes, offices etc. These systems can be highly efficient and, therefore, could play an important role in the climate transition.",
      "Many European Union countries already use these systems. ",
      "**Norway** had an electricity production almost entirely made up of renewable energy (98%). This makes Norway the second largest producer of this energy type in Europe. Interestingly, most of the renewable energy is produced by hydro power that take up 95% and only 3% by wind. In contrast, twelve European countries were reported to produce less than 20% of their energy with renewable resources: **Malta** (0%), **Hungary** (5%), **Estonia** (6%), **Czechia** (7%), **Cyprus** (9%), **Ukraine** (9%), **Poland** (10%), **Netherlands** (13%), **Bulgaria** (17%), **Belgium** (18%), **Slovakia** (19%), and **France** (19%).",
      "<span style='color:#656565'>Note: Energy production is mapped to the area of the circles.<br>*Visualization by Cédric Scherer • Data by Eurostat*</span>"),
    v = c(.75, .5, .5, 1.3)
  ),
  aes(x = x, y = y, label = label, vjust = v),
  width = unit(4, "inch"),
  color = "black",
  family = "Arial",
  lineheight = 1.7,
  size = 3,
  fill = NA,
  box.colour = NA,
  hjust = 0
) +
  coord_cartesian(clip = "off") +
  scale_x_continuous(limits = c(0, 1)) +
  scale_y_continuous(limits = c(-.6, 1.5)) +
  scale_color_manual(values = c("#228b22", "#45B145", "#929292", "black", "#871a1a", "#228b22"), guide = F) +
  scale_size_area(max_size = 39 / 4, guide = F) +
  theme(axis.text.x = element_blank())






