
# shiny source code

# load relevant libraries
library(ggplot2)

# load the plotting theme
source("helper-plotting-theme.R")

# functions taken from: https://github.com/connorrothschild/shiny-scrollytell/blob/master/scripts/source_code_for_shiny.R

longdiv <- function(...){
  div(
    ...,
    class = "container",
    style = "height:100vh"
  )
}

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
           text8,
           text9,
           text10
    )
  )
}

render_text <- function(num){
  
  div(
    text(num), class = "text"
  )
  
}

# write relevant texts for the app
text0a <- HTML("<span style='font-size:20px'> How well have past climate models predicted future changes in global temperature? </span>
              <br><br> 
              <p>A claim I often hear repeated in the conservative media space (don't ask we why I sometimes frequent conservative media on YouTube, we all have our guilty pleasures)
              is that the climate models have consistently <i>overestimated</i> the amount of warming that has occurred in the 20th and 21st centuries. For example,
              I heard Ben Shapiro say in a debate: 'the modelling has been wrong for 20-30 years and has been overestimating the amount of climate change that has actually taken place'.
              Personally, I'm not sure where he or anyone else gets such a claim from but I thought it would be fun to look at climate models that have been published at some time (e.g. 1970)
              and compare how well their future predictions matched the actual warming that occurred. Fortunately, an excellent paper from Hausfather et al. (2019, Geophysical Research Letters)
              did most of this work for me by collating predictions from climate models that had been made at different times since the first major climate model was published in 1970 and collating
              several observational datasets of global temperature. All I have done here is visualise their data in a more intuitive way to try and give people a sense for how good even old models are.<p>
              <p>To start with, let's look at the level of warming that occurred between 1880 and 2019 based on five different data products namely: National Aeronautics and Space Administration GISTEMP (Lenssen et al., 2019), 
              National Oceanic and Atmospheric Administration, GlobalTemp (Vose et al., 2012), Hadley/UEA, HadCRUT4 (Morice et al., 2012), Berkeley Earth (Rohde et al., 2013), and Cowtan and Way (Cowtan & Way, 2014).
              What is abundantly clear from all these different data products is that there has been a notable increase in the global temperature since 1880.<p>"
              )

text0b <- HTML("<p>So, how well have the different climate models over the years predicted this increase. The idea behind this visualisation is that you can scroll through
               the predictions that the different models have made and compare these predictions to the observation record. Each model was published in some year (e.g. 1970) and
               made predictions about how the global temperature should change. At the time the predictions were made, the models of course had no idea how the global temperature 
               would change. Yet, in many cases, these models made very good future predictions of the temperature. See for yourself.<p>"
)

text1 <- HTML("<H3> Manabe (1970) </H3>
              <br> <p> In 1970, Manable published one of the first climate models with projections of warming for the future. The model was a simple energy balance model with CO2 concentrations as the main driver of global temperatures.
              <br> Despite its simplicity, Manabe's model still made relatively accurate predictions of the observed warming trend of the global temperature between 1970 and 2000")

text2 <- HTML("<H3> Benson (1970) </H3>
              <br> <p> In 1970, Manable published one of the first climate models with projections of warming for the future. The model was a simple energy balance model with CO2 concentrations as the main driver of global temperatures.
              <br> Despite its simplicity, Manabe's model still made relatively accurate predictions of the observed warming trend of the global temperature between 1970 and 2000")

text3 <- HTML("<H3> Mitchell (1970) </H3>
              <br> <p> In 1970, Manable published one of the first climate models with projections of warming for the future. The model was a simple energy balance model with CO2 concentrations as the main driver of global temperatures.
              <br> Despite its simplicity, Manabe's model still made relatively accurate predictions of the observed warming trend of the global temperature between 1970 and 2000")

text4 <- HTML("<H3> Sawyer (1972) </H3>
              <br> <p> In 1970, Manable published one of the first climate models with projections of warming for the future. The model was a simple energy balance model with CO2 concentrations as the main driver of global temperatures.
              <br> Despite its simplicity, Manabe's model still made relatively accurate predictions of the observed warming trend of the global temperature between 1970 and 2000")

text5 <- HTML("<H3> Broecker (1975) </H3>
              <br> <p> In 1970, Manable published one of the first climate models with projections of warming for the future. The model was a simple energy balance model with CO2 concentrations as the main driver of global temperatures.
              <br> Despite its simplicity, Manabe's model still made relatively accurate predictions of the observed warming trend of the global temperature between 1970 and 2000")

text6 <- HTML("<H3> Nordhaus (1977) </H3>
              <br> <p> In 1970, Manable published one of the first climate models with projections of warming for the future. The model was a simple energy balance model with CO2 concentrations as the main driver of global temperatures.
              <br> Despite its simplicity, Manabe's model still made relatively accurate predictions of the observed warming trend of the global temperature between 1970 and 2000")

text7 <- HTML("<H3> Hansen et al. (1981) </H3>
              <br> <p> In 1970, Manable published one of the first climate models with projections of warming for the future. The model was a simple energy balance model with CO2 concentrations as the main driver of global temperatures.
              <br> Despite its simplicity, Manabe's model still made relatively accurate predictions of the observed warming trend of the global temperature between 1970 and 2000")

text8 <- HTML("<H3> Schneider and Thompson (1981) </H3>
              <br> <p> In 1970, Manable published one of the first climate models with projections of warming for the future. The model was a simple energy balance model with CO2 concentrations as the main driver of global temperatures.
              <br> Despite its simplicity, Manabe's model still made relatively accurate predictions of the observed warming trend of the global temperature between 1970 and 2000")

text9 <- HTML("<H3> IPCC (1995) </H3>
              <br> <p> In 1970, Manable published one of the first climate models with projections of warming for the future. The model was a simple energy balance model with CO2 concentrations as the main driver of global temperatures.
              <br> Despite its simplicity, Manabe's model still made relatively accurate predictions of the observed warming trend of the global temperature between 1970 and 2000")

text10 <- HTML("<H3> IPCC (2007) </H3>
              <br> <p> In 1970, Manable published one of the first climate models with projections of warming for the future. The model was a simple energy balance model with CO2 concentrations as the main driver of global temperatures.
              <br> Despite its simplicity, Manabe's model still made relatively accurate predictions of the observed warming trend of the global temperature between 1970 and 2000")

concludingtext <- HTML("<p><span style='font-size:24px'><b>Are climate models accurate?</b></span>
                        <br>
                        <span style='font-size:18px'>The data from Hausfather et al. (2019) show clearly that predictions made by climate models about future global temperatures have been relatively accurate. This holds even for some very simple models published in the 1970's and, therefore, it seems reasonable to think that the much more sophisticated general circulation models that are currently in use are giving us reasonable projections of our climate future.
                        <br>
                        <br>The take hom message here is basically not to trust random media commentators who claim that the climate models are bad. I hope this visualisation makes this point clear.
                        <br>
                        <br>Of course, there climate models do have their limitations. Specifically, even if a climate model can accurately predict changes in global temperatures, that does not mean that it can predict all the consequences of climate change (e.g. sea-level rise, changes in precipitation, changes in extreme weather etc.). However, it does give us confidence that these models are telling us something useful about the evolution of the earth's climate and that, perhaps, we should heed their warnings.")

technicalnotes <- HTML("<p>
                <span style='font-size:18px'><i>Technical Notes</i></span><br>
                <br>
                <p>Note that the predictions of climate models can be wrong for at least two reasons. First, they model the physics of the
                climate system incorrectly. Second, the future scenarios of climate forcing (e.g. CO2 emissions) are wrong. This second reason has nothing to do with how good the models are at simulating the earth's climate but rather are based on the vagaries of human population growth, economic cycles
                etc. Here, I only showed predictions from models and scenarios that used climate forcing scenarios that are
                in line with what actually happened.<p>
                For more information on the technical details of this analysis, please refer to the original paper that compiled these predictions: Hausfather et al. (2021). 
                <br>
                <br>
                Visualisation by me (James G. Hagan). To see more of my work, I can refer you to my <a href='https://haganjam.github.io' target='_blank'>personal website</a>
                <br>
                <br>
                Creating these visualisations would not be possible without R and a range of useful packages, namely:
                <a href='https://www.tidyverse.org/' target='_blank'>tidyverse</a>,
                <a href='http://shiny.rstudio.com/' target='_blank'>shiny</a>,
                <a href='https://gganimate.com' target='_blank'>gganimate</a>, and 
                <a href='https://github.com/statistiekcbs/scrollytell' target='_blank'>scrollytell</a>.
                </span>
                </p>")


# load the observed data
ts_obs <- readRDS("data/ts_obs.rds")
# head(ts_obs)
# summary(ts_obs)

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
        legend.title = element_blank(),
        legend.text = element_text(size = 18, color = "grey40"),
        axis.text = element_text(size = 18),
        axis.title = element_text(size = 22))
