Fastest Way to Get Around in Manhattan
================
Kaitlyn Wang (kw2961), Yuchen Zheng(yz4183), Fengjia Chen (fc2691),
Keming Zhang(kz2411), Junzhe Shao(js5959)

#### Motivation

According to the INRIX scorecard, NYC was ranked No. 1 for the worst
traffic in the United States, and the NYC subway on time performance has
always been an issue as well. In this project, we were interested in
whether bicycles can outcompete other transportations in terms of travel
time in Manhattan. We focused on the time in a day, weekday versus
weekend and seasonal variation to investigate the best time-saving way
to commute, if not bicycle.

#### The Intended Final Products

To the end, we are expected to create several interactive dashboards of
the relationship between the travel time differences and comparisons of
the public traffic (citibike/bus/subway/taxi) within daytime across the
Manhattan area in New York. Analyzing the factors that might influence
the actual travel time could reflect the road congestion situation in
New York.

#### The Anticipated Data Sources

-   Main dataset - NYC Taxi and Limousine Commission Trip Record Data:
    <https://www1.nyc.gov/site/tlc/about/tlc-trip-record-data.page>  
-   Dataset on Citi Bike Trip Histories:
    <https://ride.citibikenyc.com/system-data>  
-   Neighborhood Tabulation Areas:
    <https://data.cityofnewyork.us/City-Government/2010-Neighborhood-Tabulation-Areas-NTAs-/cpf4-rkhqs>

#### The Planned Analyses / Visualizations / Coding Challenges

-   Identify the original destination pairs around the city across
    different time periods. Divide four time periods for our study.
-   Exogenous variables should be adjusted. Add MTA attributes and bus
    station attributes.
-   Adapt panel Mixed Multinomial Logit Model(MMNL).
-   Barplots compare the travel time of trips by four means of
    transportation during different time periods of a day, weekend
    vs. weekday, seasonal variations, etc.
-   Map of available routes and average travel time for each mean of
    transportation from one neighborhood to another in nyc
-   Code challenges: Need to properly divide data into subgroups based
    on different times of the day, day types, etc. Panel mixed
    multinomial logit model may be difficult to deploy.

#### The Planned Timeline

Nov. 13: Create shared Github repository, submit project proposal  
Nov. 13-20: Narrow dataset, decide data visualizations, data cleaning  
Nov. 21-27: Create Visualizations (interactive descriptives, interactive
plots)  
Nov. 28-Dec. 4: Final integrate code, proofread and test for glitches,
two-minute screencast  
Dec. 5-Dec. 10: Finishing report for detailed information of the
projects.  
Dec. 11: Final project due
