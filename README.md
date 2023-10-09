1.	Summary
Cyclistic is a bike-share company in Chicago. It has more than 5800 bicycles and 600 docking station. Cyclistic sets itself apart by also offering reclining bikes, hand tricycles, and cargo bikes, making bike-share more inclusive to people with disabilities and riders who can’t use a standard two-wheeled bike. The majority of riders opt for traditional bikes; about 8% of riders use the assistive options. Cyclistic users are more likely to ride for leisure, but about 30% use them to commute to work each day. The director of marketing believes the company’s future success depends on maximizing the number of annual memberships.
In 2016, Cyclistic launched a successful bike-share offering. Since then, the program has grown to a fleet of 5,824 bicycles that are geotracked and locked into a network of 692 stations across Chicago. The bikes can be unlocked from one station and returned to any other station in the system anytime.
Until now, Cyclistic’s marketing strategy relied on building general awareness and appealing to broad consumer segments. One 2 approach that helped make these things possible was the flexibility of its pricing plans: single-ride passes, full-day passes, and annual memberships. Customers who purchase single-ride or full-day passes are referred to as casual riders. Customers who purchase annual memberships are Cyclistic members.
Cyclistic’s finance analysts have concluded that annual members are much more profitable than casual riders. Although the pricing flexibility helps Cyclistic attract more customers, the manager believes that maximizing the number of annual members will be key to future growth. Rather than creating a marketing campaign that targets all-new customers, the manager believes there is a very good chance to convert casual riders into members. She notes that casual riders are already aware of the Cyclistic program and have chosen Cyclistic for their mobility needs.
First thing, which I want to see how casual riders and annual members use Cyclistic bikes differently. From these insights, my team will design a new marketing strategy to convert casual riders into annual members. But first, Cyclistic executives expect my recommendations and after they want to create own decision.

2.	Ask
1. How do annual members and casual riders use Cyclistic bikes differently?
2. Why would casual riders buy Cyclistic annual memberships?
3. How can Cyclistic use digital media to influence casual riders to become members?
My business task is to analyze the behavior of casual customer and find the way how they can become members.

3.	Prepare
3.1.	 Data used
For this case study I used data from
https://divvy-tripdata.s3.amazonaws.com/index.html 
This database contains csv files for each month during more than 10 years. I decided use only last year. The data has been made available by Motivate International Inc. under this license. I will use only last two years to understand historical trends.
3.2.	Data Credibility and Integrity
This dataset contains all data which was gathered within a lot of years operating this company and contains information about all users I think that I don’t have a lot of possibility of encountering a sampling bias. These data is not integrity, because doesn’t contain name of every station, customer’s name or customer ID and cost of each ride. Because of it it’s not possible to analyze path and cost which every customer pays during the year (it could give additional information how the casual riders can become member.

4.	Process
To work with data I chose R, because it allow to work with big data and I can create my own variable with which I can work without changing original data. And R allows to create great visualization.
First, I upload all data in one data frame. I used 12 csv files and I got 6,547,094 rows. Using the skim_without_charts() I checked the data and found that a lot of name and address of station is missing and I decided to delete these columns from database which I will analyze. I found that this data contains only unique rows, and I don’t need to remove duplicates. I checked text columns and found that data is correct. But when I checked the date I found that some rows have date/time of ending trip less than date/time of starting trip. I had 787 rows and I deleted them, because I can’t check them and fix. And I found that some trip time less than 1 minute. I decided to removed them too (170371 rows).

5.	Analyze
I added 4 columns: time_trip – duration of trip, month_trip – month of trip, day_week – day of week of trip, year_trip – year of trip. I analyzed mean of trip duration by customer type and month and found that casual customers have longer mean duration of trip than members. Analysis by customer type and days of week showed same tendency. But when I analyzed number of trips by customer type by month and day of week, I found that number of trips of casual customer less than members. Also, I analyzed what kind of type of bikes different types of customer use. And found that members don’t use at all docked bike. Casual customers have very long duration using docked bike – 146.4 minutes. Electric and classic bikes they use very similar time, but duration of trip for casual customer more than members and number of trips less than members. I created 5 visualizations in R: average duration of trips by months and days of week for different types of customers. And numbers of trips by months and days of week for different types of customers. And mode for days of week to see in what days the different types of customers use bike different.
To create some visualizations, I used Tableau. I create csv file from R (after cleaning data and adding new columns) and uploaded it in Tableau public. I created 3 pie charts to see average duration of trips, numbers of trips and summary duration of trips during whole year for different types of bikes and customers. And created histogram for trip duration to see how.

6.	Share
First question was about difference between casual customers and member. I create this visualization to show what I found. First thing which I found is that casual customers use bike less amount of time (have a smaller number of trips than members) but very close to time which members use bike. But average duration of their trip more than twice compared with members.
Second thing which I found that casual customers use bike more during weekend while members use bike more during week days. During weekend all customers use the bike very similar.
Third thing which I found that members have a lot of very short trips more than twice compared with casual customers, which use bike longer. 
Forth thing – casual customers use a lot of docked bike and have long trip duration using it – average is 146 minutes. And summary time during the year very similar to time when they use electric or classic bike. Members don’t use docked bike at all.
   
7.	Act
I found on this website Divvy For Everyone (divvybikes.com) that for members first 45 minutes of each ride are free and after they pay 0.5$/minute for classic bike. For electric bike members pay 0.05$/minute. And scooter or docked bike cost 0.10$/minute.
If we have goal to have more members, I think that we can use digital media to show the casual customers the benefits using the membership – because they use bike in average more than members during one trip. I think also we can offer them membership because of using docked bike. And third recommendation we can offer them membership using the information that they use bike during weekend.
The narrow place of this analysis that I don’t have real information about real customers. If I have customer ID or any other identification of customers, I can analyze customer behavior for each customer. Also, if I have the real membership offer which the Cyclistic company has for members and costs of each trip, I can analyze how we can create valuable offer for casual customers when they could spend less money using bike more time.
