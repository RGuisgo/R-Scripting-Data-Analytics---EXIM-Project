# R-Scripting-Data-Analytics---EXIM-Project

TOPIC:  REVISION OF THE FOREIGN CORPORATE RATING MODEL OF THE HUNGARIAN EXIM BANK

Risk rating models are instruments for calculating the likelihood of a default. The idea of default risk is closely related to the idea of risk rating models, which are important tools in risk management, underwriting, capital allocation, and portfolio management. A vital tool for making loan decisions and managing and building portfolios is a risk rating model. They offer lenders, analysts, and portfolio managers a fairly objective method of classifying borrowers or particular securities according to their creditworthiness and default risk.

Data was collected from the Orbis database. Orbis is a commercially available dataset that includes information on more than 200 million businesses worldwide. It offers financial information about companies (such as operational revenue, employment, and fixed capital) together with specifics about the ownership structure of the organization, industry, and other firm characteristics.

The level that Orbis offers with the most agglomeration is world regions. There are two distinct categories of world regions: the Geographical world regions and the political world regions. Here, concentration was given to the world region, which includes Middle East, Balkans, and Eastern Europe, Far East and Central Asia, South and Central America, Africa and Turkey for the years 2013–2021. However, due to high storage issue, the codes here are linited to only Africa.

All the dataset for all indicators were then merged into one data frame. The attribute column of the data frame was then split into date and variable columns. Companies operating in the ‘Banking, Insurance and Financial Services’ and ‘Public Administration, Education, Health Social Services’ sectors were excluded from the dataset because these sectors do not have normal financial ratios due to substantially differences in balance sheets and profit and loss statements. The resulting data frame was then reshaped into a long and wide format and duplicates were removed.

# Methods

Descriptive statistics summarize the basic characteristics of the dataset including measures of central tendency, variability and frequency.
Boxplots are particularly useful for quickly assessing the location, dispersion, and symmetry or skewness of a set of data, and for making comparisons of these features in multiple data sets. A histogram categorizes or group the range of potential values in a data collection. A rectangle with a base length equal to the range of values in each group and a length equal to the number of observations falling into that group is built for each group. Scores are then assigned to each company under each indicator. Scoring was done by assigning a score from 1 to 7 to each company. This is called the septile. Any of the quantiles that divide an ordered sample population into seven equal parts are called septiles.


