
# shiny source code

# load relevant libraries
library(shiny)
library(shinyjs)
library(scrollytell)

# function to switch different text blocks
text <- function(num){
  p(
    switch(num,
           text1,
           text2,
           text3,
           text4,
           text5,
           text6,
           text7,
           text8
    )
  )
}

# write relevant texts for the app
text0 <- HTML("<span style='font-size:20px'> How well have past climate models predicted future changes in global temperature? </span>
              <br><br> 
              <p>A claim I often hear repeated in the conservative media space (don't ask we why I sometimes frequent conservative media on YouTube, we all have our guilty pleasures)
              is that the climate models have consistently *overestimated* the amount of warming that has occurred in the 20th and 21st centuries. For example,
              I heard Ben Shapiro say in a debate: 'the modelling has been wrong for 20-30 years and has been overestimating the amount of climate change that has actually taken place'.
              Personally, I'm not sure where he or anyone else gets such a claim from but I thought it would be fun to look at climate models that have been published at some time (e.g. 1970)
              and compare how well their future predictions matched the actual warming that occurred. Fortunately, an excellent paper from Hausfather et al. (2019, Geophysical Research Letters)
              did most of this work for me by collating predictions from climate models that had been made at different times since the first major climate model was published in 1970 and collating
              several observational datasets of global temperature. All I have done here is visualise their data in a more intuitive way to try and give people a sense for how good even old models are.<p>
              <br><br>
              <p>Before we get into this, I want to stress an important point. The future predictions of climate models can be wrong for at least two reasons. First, they model the physics of the
              climate system incorrectly which means that the predictions are wrong. Second, the future scenarios of climate forcing (e.g. CO2 emissions) are wrong which means that the future predictions
              are wrong. This second reason has nothing to do with how good the models are at simulating the earth's climate but rather are based on the vagaries of human population growth, economic cycles
              etc. which are, arguably, more difficult to predict. So, for the purposes of this visualisation, I only show predictions from scenarios that used climate forcing scenarios that are at least vaguely
              in line with what actually happened as this is a better test of whether the models can accurately simulate the climate.
              <br><br>
              To start with, let's look at the level of warming that occurred between 1880 and 2019 based on five different data products namely: National Aeronautics and Space Administration *GISTEMP* (Lenssen et al., 2019), 
              National Oceanic and Atmospheric Administration *GlobalTemp* (Vose et al., 2012), Hadley/UEA *HadCRUT4* (Morice et al., 2012), *Berkeley Earth* (Rohde et al., 2013), and *Cowtan and Way* (Cowtan & Way, 2014).
              What is abundantly clear from all these different data products is that there has been a notable increase in the global temperature since 1880."
              )

text1 <- HTML("<H2> No education credentials </H2>
              <br> <p> Workers with <font color='#A00042'>no formal education credential</font> have a median income of $25,636.
              <br> On average, those occupations have a <b>90% chance</b> of job automation.
              <br><br> There are 23,765,700 workers with <font color='#A00042'>no formal education credential</font>.<p>")

text2 <- HTML("<H2> High school diplomas </H2>
              <br> <p>Workers with <font color='#F56C42'>high school diplomas</font> have a median income of $25,636.
              <br> On average, those occupations have a <b>60% chance</b> of job automation.
              <br><br> There are 33,129,910 workers with a <font color='#F56C42'>high school diploma</font>.<p>")

text3 <- HTML("<H2> Postsecondary nondegree awards </H2>
              <br> <p>Workers with <font color='#008640'>postsecondary nondegree awards</font> (e.g. actors) have a median income of $39,990.
              <br> On average, those occupations have a <b>52% chance</b> of job automation.
              <br><br> There are 5,904,150 workers with a <font color='#008640'>postsecondary nondegree award</font>.<p>")

text4 <- HTML("<H2> Associate's degrees </H2>
              <br> <p>Workers with <font color='#3487BD'>associate's degrees</font> have a median income of $41,496.
              <br> On average, those occupations have a <b>50% chance</b> of job automation.
              <br><br> There are 1,869,840 workers with an <font color='#3487BD'>associate's degree</font>.<p>")

text5 <- HTML("<H2> Bachelor's degrees </H2>
              <br> <p>Workers with <font color='#C71C7E'>bachelor's degrees</font> have a median income of $59,124.
              <br> On average, those occupations have a <b>20% chance</b> of job automation.
              <br><br> There are 18,399,270 workers with a <font color='#C71C7E'>bachelor's degree</font>.<p>")

text6 <- HTML("<H2> Master's degrees </H2>
              <br> <p>Workers with <font color='#5E4FA2'>master's degrees</font> have a median income of $69,732.
              <br> On average, those occupations have a <b>10% chance</b> of job automation.
              <br><br> There are 1,281,710 workers with a <font color='#5E4FA2'>master's degree</font>.<p>")

text7 <- HTML("<H2> Doctoral degrees </H2>
              <br> <p>Workers with <b>doctoral degrees</b> have a median income of $84,396.
              <br> On average, those occupations have a <b>3% chance</b> of job automation.
              <br><br> There are 1,386,850 workers with a <b>doctoral degree</b>.<p>")

text8 <- HTML("<H2> In Sum </H2>
              <br> <p>All things considered, the nominal median income of an average US worker is <b>$31,786</b>.
              <br>
              <br> 47% of jobs are expected to face a high risk of automatization in the near future.<sup>1</sup><p>
              <br><br><br>
              <span style='font-size:11px'><sup>1</sup><a href='https://www.oxfordmartin.ox.ac.uk/downloads/academic/The_Future_of_Employment.pdf' target='_blank'>Frey and Osborne (2013)</a>
               write that 'associated occupations are potentially automatable over
              some unspecified number of years, <i>perhaps a decade or two.'</i></span>")

concludingtext <- HTML("<p><span style='font-size:24px'><b>The Risk of Automation</b></span>
                        <br>
                            <span style='font-size:18px'>This data led researchers Carl Frey and Michael Osborne to predict that 47% of jobs are at serious risk of automation over the next couple decades.
                        <br>
                            <br>The visuals above suggest that the ills of automation may not be evenly distributed across jobs.
                            Less educated workers are more likely to face job loss as a product of automation. Those with high school diplomas or less find themself concentrated near the top of the y-axis, while those with bachelor’s degrees or higher face a lower risk of automation.
                        <br>
                            <br>A job’s salary is also predictive of automation probability. As the median income of a profession increases, the likelihood of automation displacing its workers decreases.
                            This could suggest that automation will increasingly bifurcate the already divided labor market, making those at the top wealthier at the expense of the worse-off.
                        <br>
                            <br>Automation’s impact on work necessitates a policy response. The fact that automation will have different effects on different industries and different workers is a reminder that this public policy will have to be strategic and thoughtful.</span></p>")

technicalnotes <- HTML("<p>
                <span style='font-size:18px'><i>Technical Notes</i></span><br>
                <br>
                To learn more about how I made this app, please see the <a href='https://connorrothschild.github.io/r/automation-scrollytell/' target='_blank'>accompanying blog post</a>
                <br>
                Employment and education data comes from the
                <a href='https://www.bls.gov/emp/documentation/education-training-system.htm' target='_blank'>Bureau of Labor Statistics</a>. 
                <br>
                Employment and income data also comes from the <a href='https://www.bls.gov/oes/current/oes_nat.htm#11-0000' target='_blank'>BLS</a>.
                <br>
                Data on occupation and the risk of automation comes from <a href='https://www.oxfordmartin.ox.ac.uk/downloads/academic/The_Future_of_Employment.pdf' target='_blank'>Frey and Osborne (2013)</a>. 
                <br>
                <br>
                Education is coded as typical education, meaning that the coded variable corresponds to the level of education that is most prevalent within a given occupation.
                If 51% of accountants hold a bachelor's degree, their typical education will be coded as such.
                Summary statistics for each level of education are calculated via the weighted mean of each occupation given its number of workers.
                <br>
                <br>
                For more information on the technical details of this analysis, please see the <a href='https://connorrothschild.github.io/r/automation/' target='_blank'>accompanying blog post</a>. 
                <br>
                <br>
                The R packages powering this site include 
                <a href='https://www.tidyverse.org/' target='_blank'>tidyverse</a>,
                <a href='http://shiny.rstudio.com/' target='_blank'>shiny</a>,
                <a href='https://github.com/ropensci/plotly' target='_blank'>plotly</a>, and 
                <a href='https://github.com/statistiekcbs/scrollytell' target='_blank'>scrollytell</a>.
                </span>
                </p>")


# make relevant plots for the app

# load the model data
ts_pub <- readRDS("07-climate-models/data/ts_pub.rds")
head(ts_pub)
summary(ts_pub)

# get the start and end dates of the models
ts_box <- 
  ts_pub |>
  dplyr::select(model_id, author, year_start, year_end) |>
  dplyr::distinct() |>
  dplyr::mutate(date_start = lubridate::make_date(year = year_start),
                date_end = lubridate::make_date(year = year_end)) |>
  dplyr::arrange(year_start) |>
  dplyr::mutate(label = paste0(author, " ", year_start),
                date_mid = date_start + floor((date_end-date_start)/2))

# load the observed data
ts_obs <- readRDS("07-climate-models/data/ts_obs.rds")
head(ts_obs)
summary(ts_obs)

# add a factor with different names for the sources
ts_obs$source_fac <- factor(ts_obs$source)
levels(ts_obs$source_fac) <- c("Berkeley Earth", "Cowtan and Way", "GISTEMP", "HadCRUT4", "NOAA")

# colours for the data products
# helpers for all plots:
cols <- c('#A00042','#F56C42','#008640','#3487BD', '#C71C7E') 

intro_plot <- 
  ggplot(data = ts_obs,
       mapping = aes(x = date, y = temp_anom_C, colour = source_fac)) +
  geom_line(alpha = 0.5, linewidth = 0.2) +
  ylab("Temperature anomaly (\u00B0C)") +
  xlab(NULL) +
  guides(color = guide_legend(override.aes = list(linewidth = 1, 
                                                  alpha = 0.75))) +
  scale_colour_manual(values = cols) +
  scale_x_date() +
  scale_y_continuous(limits = c(-1, 1.4), expand = c(0, 0)) +
  coord_cartesian(clip = "off") +
  theme(legend.position = "top",
        legend.title = element_blank())

# make a base observational data plot to highlight with the different boxes
base_obs_plot <-
  ggplot(data = ts_obs,
         mapping = aes(x = date, y = temp_anom_C, group = source_fac)) +
  geom_line(alpha = 0.2, linewidth = 0.1) +
  ylab("Temperature anomaly (\u00B0C)") +
  xlab(NULL) +
  scale_x_date() +
  scale_y_continuous(limits = c(-1, 1.4), expand = c(0, 0)) +
  coord_cartesian(clip = "off") +
  theme(plot.margin = margin(c(40, 10, 10, 10)))
plot(base_obs_plot)

# select a model
mod_id <- "Benson_1970_T"

# add a box to this
p_a1 <- 
  base_obs_plot +
  geom_rect(
    data = dplyr::filter(ts_box, model_id == mod_id),
    mapping = aes(xmin = date_start, xmax = date_end, ymin = -1, ymax = 1.4),
    inherit.aes = FALSE,
    alpha = 0.15, fill = "red") +
  geom_text(
    data = dplyr::filter(ts_box, model_id == mod_id),
    mapping = aes(x = date_mid, y = 1.4, label = label),
    inherit.aes = FALSE,
    family = "Dosis",
    size = 6,
    vjust = -0.5)
plot(p_a1)

# make an animate plot
pub_in <- 
  ts_pub |> 
  dplyr::filter(model_id == "Benson_1970_T")

obs_in <- 
  ts_obs |>
  dplyr::filter(date >= min(pub_in$date), date < max(pub_in$date))

p_a2 <- 
  ggplot() +
  geom_line(data = pub_in,
            mapping = aes(x = date, y = temp_anom_C)) +
  geom_line(data = obs_in,
            mapping = aes(x = date, y = temp_anom_C, group = source),
            alpha = 0.5, linewidth = 0.1) +
  ylab("Temperature anomaly (\u00B0C)") +
  xlab(NULL) +
  scale_x_date() +
  scale_y_continuous(expand = c(0, 0)) +
  coord_cartesian(clip = "off") +
  theme(plot.margin = margin(c(40, 10, 10, 10))) 
plot(p_a2)

# fit this to a gganimate
p_a2 +
  transition_reveal(date) +
  theme(axis.title.y = element_text(vjust = 0.5,
                                    margin = margin(c(10, 30, 10, 10))),
        plot.margin = margin(c(40, 10, 20, 20)))

legend_ord <- levels(with(data, reorder(typicaled, reveal)))

## Intro plot
# Intro static ggplot
introggPlot <- data %>% 
  filter(typicaled != "Some college, no degree") %>%
  ggplot() +
  geom_point(mapping=aes(x=A_MEDIAN, y=probability, size=TOT_EMP,
                         alpha= 1/7, col=typicaled,
                         text = glue::glue('<span style = "font-size:1.5em">{occupation}</span><br>
                                                <i>Probability of Automation</i>: {probability}%
                                                <i>Median Income</i>: ${comma(A_MEDIAN, digits = 0)}
                                                <i>Number of Workers</i>: {comma(TOT_EMP, digits = 0)}'))) +
  scale_size(range = c(1, 20), guide = 'none') +
  xlab("\nMedian Income") +
  ylab("Probability of Automation") +
  # ggtitle("Likelihood of Job Automation vs Median Income") +
  labs(size= "", col= "", alpha = "") + 
  scale_color_manual(values = cols, breaks = legend_ord) +
  scale_x_continuous(labels=scales::dollar_format(prefix="$"), limits = c(25000,200000)) +
  scale_y_continuous(labels=scales::number_format(suffix="%"), limits = c(0,100)) +
  # theme(legend.position = "top", legend.direction = "horizontal") +
  # legend.text = element_text(colour = ifelse(add == reveal, "black", "grey"))) +
  # legend.text = element_text(colour="black", size = ifelse(add == reveal, 20, 12))) +
  # cr::drop_axis(axis = "y")
  theme(axis.line.x = ggplot2::element_line(colour = NULL, 
                                            size = NULL, linetype = NULL, lineend = NULL), 
        axis.line.y = ggplot2::element_blank(),
        panel.grid.major.x = element_blank()) 

# Convert into ggplotly
introPlot <- ggplotly(introggPlot, tooltip = 'text') %>%
  layout(
    title = element_blank(),
    legend = list(x = 0.65, y = 0.925),
    font = list(family = 'Lato'),
    margin = list(t=50),
    hoverlabel = list(bgcolor = 'whitesmoke', color = 'DarkGray')) %>% 
  config(displaylogo = F, showSendToCloud = F, displayModeBar = F)