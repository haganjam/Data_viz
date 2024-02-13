
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
text0a <- HTML("<span style='font-size:20px'><b>How well have past climate models predicted future changes in global temperature?</b></span>
              <br><br> 
              <p>A claim I often hear repeated in the conservative media space (don't ask we why I sometimes frequent conservative media on YouTube, we all have our guilty pleasures)
              is that the climate models have consistently <i>overestimated</i> the amount of warming that has occurred in the 20th and 21st centuries. For example,
              I heard Ben Shapiro say in a debate: 'the modelling has been wrong for 20-30 years and has been overestimating the amount of climate change that has actually taken place'.
              Personally, I'm not sure where he or anyone else gets such a claim from but I thought it would be fun to look at climate models that have been published at some time (e.g. 1970)
              and compare how well their future predictions matched the actual warming that occurred. Fortunately, an excellent paper from Hausfather et al. (2019, <i>Geophysical Research Letters</i>)
              did most of this work for me by collating predictions from climate models that had been made at different times since the first major climate model was published in 1970 and collating
              several observational datasets of global temperature. All I have done here is visualise their data in a more intuitive way to try and give people a sense for how good even old models are.<p>
              <p>To start with, let's look at the level of warming that occurred between 1880 and 2019 based on five different data products namely: National Aeronautics and Space Administration GISTEMP (Lenssen et al., 2019), 
              National Oceanic and Atmospheric Administration, GlobalTemp (Vose et al., 2012), Hadley/UEA, HadCRUT4 (Morice et al., 2012), Berkeley Earth (Rohde et al., 2013), and Cowtan and Way (Cowtan & Way, 2014).
              What is abundantly clear from all these different data products is that there has been a notable increase in the global temperature since 1880.<p>"
              )

text0b <- HTML("<p>So, how well have the different climate models over the years predicted this increase. The idea behind this visualisation is that you can scroll through
               the predictions that the different models have made and compare these predictions to the observational record. Each model was published in some year (e.g. 1970) and
               made predictions about how the global temperature would change. At the time the predictions were made, the models of course had no idea how the global temperature 
               would change. Yet, in many cases, these models made very good future predictions of the temperature. See for yourself.<p>"
)

text1 <- HTML("<br> 
              <H3> Model 1: Manabe (1970) </H3>
              <br> <p> How to read these graphs? For each of the following models, the figures have the same structure. The left graph shows the observational global temperature record from five different data products. The vertical dashed line shows when the model was published and the pink rectangle shows the window of time that the model made global temperature predictions into the future. The right graph zooms into this prediction window and overlays the model predictions (solid black line).<p>
              <p> In 1970, Manabe published one of the first climate models with projections of warming for the future. The model was a simple energy balance model with CO2 concentrations as the main driver of global temperatures.<p>
              <p> Despite its simplicity, Manabe's model still made relatively accurate predictions of the observed warming trend of the global temperature between 1970 and 2000<p>")

text2 <- HTML("<H3> Model 2: Benson (1970) </H3>
              <br> <p> Benson's (1970) model was relatively similar to Manabe's (1970) model but it assumed linearly increasing C02 concentrations. <p>")

text3 <- HTML("<H3> Model 3: Mitchell (1970) </H3>
              <br> <p> This model is similar to Manabe (1970) and Benson (1970) but uses slightly different predictions of C02 concentrations. Indeed, comparing the predictions across these three models shows that they are relatively similar. <p>")

text4 <- HTML("<H3> Model 4: Sawyer (1972) </H3>
              <br> <p> In 1972, Sawyer also came to similar predictions as Manabe (1970), Benson (1970) and Mitchell (1970). <p>")

text5 <- HTML("<H3> Model 5: Broecker (1975) </H3>
              <br> <p> Wally Broecker is a renowned geochemist who actually popularised the term 'global warming'. Moreover, he was one of the fiercest proponents for the idea of climate tipping points, something that is crucial to our understanding of the climate but which models generally capture quite poorly. <p>")

text6 <- HTML("<H3> Model 6: Nordhaus (1977) </H3>
              <br> <p> William Nordhaus is now a nobel prize-winning economist due to his work which delves into the economics of climate change. In this model, he examined the consequences of substituting energy-use for non-carbon fuels.<p>")

text7 <- HTML("<H3> Model 7: Hansen et al. (1981) </H3>
              <br> <p> Like Nordhaus, Hansen used his model to explore the consequences of different scenarios of economic growth and how this may affect CO2 trajectories and, therefore, the global temperature.")

text8 <- HTML("<H3> Model 8: Schneider and Thompson (1981) </H3>
              <br> <p> All the previous models assumed instantaneous thermal equilibrium such that the global temperature immediately adjusts to the concentration of C02. Schneider and Thompson's (1981) allows the ocean to uptake heat from the atmosphere which reduces the instantaneous global temperature.<p>")

text9 <- HTML("<H3> Model 9: IPCC (1995) </H3>
              <br> <p> In the 1990's, most of the modelling efforts were done in combination with the Intergovernmental Panel on Climate Change (IPCC) reports that aimed to summarise the evidence of climate change. This model is from the IPCC's second assessment report. <p>")

text10 <- HTML("<H3> Model 10: IPCC (2007) </H3>
              <br> <p> Like model 9, this model comes from the IPCC, specifically, its fourth assessment report. This is by far the most advanced model presented here and this can be seen by how it captures variability through time much better than the previous models. <p>")

concludingtext <- HTML("<p><span style='font-size:20px'><b>Are climate models accurate?</b></span>
                        <br><br>
                        <span style='font-size:14px'>The data from Hausfather et al. (2019) show clearly that predictions made by climate models about future global temperatures have been relatively accurate. This holds even for some very simple models published in the 1970's and, therefore, it seems reasonable to think that the much more sophisticated general circulation models used by, for example, the IPCC (models 9 and 10 above) and are currently the standard tool for climate modelling are giving us reasonable projections about our climate future.
                        <br>
                        <br>The take home message here is basically not to trust random media commentators who claim that the climate models are bad. I hope this visualisation makes this point clear.
                        <br>
                        <br>Of course, these climate models do have their limitations. Specifically, even if a climate model can accurately predict changes in global temperatures, that does not mean that it can predict all the consequences of climate change (e.g. sea-level rise, changes in precipitation, changes in extreme weather etc.). However, it does give us confidence that these models are telling us something useful about the evolution of the earth's climate and that, perhaps, we should heed their warnings.")

technicalnotes <- HTML("<p>
                <span style='font-size:14px'><b>Technical Notes</b></span><br>
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
        legend.text = element_text(size = 16, color = "grey40"),
        axis.text = element_text(size = 16),
        axis.title = element_text(size = 16))
