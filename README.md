# Pitch-Effectiveness
Research into pitch effectiveness with MLB data from the 2022 season. This was done as a two year research project during my undergraduate at Elon University.

The following files are all that is needed to create the logistic models and resulting shiny app, which can also be found online at https://evanwu.shinyapps.io/Pitch_Outcome_Model/

This research attempts to predict the effectiveness of Major League Baseball (MLB) pitches using a variety of pitch characteristics. Pitch-by-pitch data from the 2022 MLB season was used in logistic regression models to predict the probability of different outcomes based on pitch characteristics (e.g., speed, spin, movement, location). The results of these models were used to create an interactive applet, allowing users to explore pitch effectiveness. The applet allows a user to input pitch characteristics and to click on a location in a strike zone to predict the likelihood of different outcomes for their described pitch. The applet also includes a series of heatmaps of the likelihood of outcomes based on pitch location and a list of the MLB pitchers with the most similar pitch to the one inputted. The applet is intended to be used as an evaluation tool or to determine potential changes for a pitcher’s arsenal.

The data used for the modeling is 2022 pitch-by-pitch data for every pitcher with at least 1800 pitches thrown that season (297,675 observations). All of the different CSVs are stored in a .zip file due to their large size.
