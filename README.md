#shiny_stepwise
All you have to do to run this visualization is open R and type these three lines:

1. install.packages('shiny')

2. library(shiny)

3. runGitHub('shiny_stepwise', 'justmytwospence')

##Future features:

*Ability to upload a csv.

*Tracking of model F-test p-value in addition to the currently implemented Restricted F-test p-value.

*Toggle for showing the Beta coefficient for the intercept term / the initial null model iteration.

*Isolate reactive endpoints to avoid error messages while page is loading.
