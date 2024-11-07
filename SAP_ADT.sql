-- Table Creation
-- Creating the main table dailyActivity with all the parameters and constraints- Abhigna
CREATE TABLE IF NOT EXISTS public."dailyActivity"
(
    "Id" bigint NOT NULL,
    "ActivityDate" date NOT NULL,
    "TotalSteps" bigint NOT NULL,
    "TotalDistance" double precision NOT NULL,
    "TrackerDistance" double precision NOT NULL,
    "LoggedActivitiesDistance" double precision NOT NULL,
    "VeryActiveDistance" double precision NOT NULL,
    "ModeratelyActiveDistance" double precision NOT NULL,
    "LightActiveDistance" double precision NOT NULL,
    "SedentaryActiveDistance" double precision NOT NULL,
    "VeryActiveMinutes" bigint NOT NULL,
    "FairlyActiveMinutes" bigint NOT NULL,
    "LightlyActiveMinutes" bigint NOT NULL,
    "SedentaryMinutes" bigint NOT NULL,
    "Calories" bigint NOT NULL,
    CONSTRAINT "Primary Key" PRIMARY KEY ("Id", "ActivityDate")
)

-- Loading the data form csv file to the table created above - Abhigna
COPY public."dailyActivity"
FROM 'C:\Users\shiva\Documents\ADT\project\dailyActivity_merged.csv'
DELIMITER ','
CSV HEADER

-- Normalising the tables created the daily calories table making the Id, activity date to be the primary keys - Preetham
CREATE TABLE IF NOT EXISTS public.daily_calories
(
    "Id" bigint NOT NULL,
    "ActivityDate" date NOT NULL,
    "Calories" bigint NOT NULL,
    CONSTRAINT daily_calories_pkey PRIMARY KEY ("Id", "ActivityDate")
)
-- Created the steps table. In this activitydate and Id are the foreign key constraints referencing 
-- from the daily_calories table - preetham
CREATE TABLE IF NOT EXISTS public.steps
(
    "Id" bigint NOT NULL,
    "ActivityDate" date NOT NULL,
    "TotalSteps" bigint NOT NULL,
    CONSTRAINT "steps_Id_ActivityDate_fkey" FOREIGN KEY ("ActivityDate", "Id")
        REFERENCES public.daily_calories ("ActivityDate", "Id") MATCH SIMPLE
        ON UPDATE NO ACTION
        ON DELETE NO ACTION
)

-- Minutes table also below also has two columns referencing the activity_date, Id from the daily calories.- Abhigna
CREATE TABLE IF NOT EXISTS public.minutes
(
    "Id" bigint NOT NULL,
    "ActivityDate" date NOT NULL,
    "VeryActiveMinutes" bigint NOT NULL,
    "FairlyActiveMinutes" bigint NOT NULL,
    "LightlyActiveMinutes" bigint NOT NULL,
    "SedentaryMinutes" bigint NOT NULL,
    CONSTRAINT "minutes_Id_ActivityDate_fkey" FOREIGN KEY ("ActivityDate", "Id")
        REFERENCES public.daily_calories ("ActivityDate", "Id") MATCH SIMPLE
        ON UPDATE NO ACTION
        ON DELETE NO ACTION
)

-- Distances table with all the constraints is created below. This table references the activitydate, 
-- Id (foriegn key constraint) from the daily calories -- shivani
CREATE TABLE IF NOT EXISTS public.distances
(
    "Id" bigint NOT NULL,
    "ActivityDate" date NOT NULL,
    "TotalDistance" double precision NOT NULL,
    "TrackerDistance" double precision NOT NULL,
    "LoggedActivitiesDistance" double precision NOT NULL,
    "VeryActiveDistance" double precision NOT NULL,
    "ModeratelyActiveDistance" double precision NOT NULL,
    "LightActiveDistance" double precision NOT NULL,
    "SedentaryActiveDistance" double precision NOT NULL,
    CONSTRAINT "distances_Id_ActivityDate_fkey" FOREIGN KEY ("ActivityDate", "Id")
        REFERENCES public.daily_calories ("ActivityDate", "Id") MATCH SIMPLE
        ON UPDATE NO ACTION
        ON DELETE NO ACTION
)

-- Following sleep table has been created initially and later data was imported from csv.
-- even in this table we are referencing the columns from daily_calories -- shivani
Create Table sleeping(
	"Id" bigint NOT NULL,
    "SleepDay" date NOT NULL,
	"TotalSleepRecords" int NOT NULL,
	"TotalMinutesAsleep" int NOT NULL,
	"TotalTimeInBed" int NOT NULL,
	foreign key ("Id","SleepDay") references daily_calories("Id","ActivityDate")
);

-- loading data from the csv file-- shivani
COPY public."sleeping"
FROM 'C:\Users\shiva\Documents\ADT\project\sleepday_data.csv'
DELIMITER ','
CSV HEADER

-- Following is the weight_log table. after creating the table we inserted the data from the csv
-- preetham
Create Table weight_Log(
	"Id" bigint NOT NULL,
    "Date" date NOT NULL,
	"WeightKg" double precision NOT NULL,
	"WeightPounds" double precision NOT NULL,
	"Fat" double precision,
	"BMI" double precision NOT NULL,
	"IsManualReport" boolean NOT NULL,
	foreign key ("Id","Date") references daily_calories("Id","ActivityDate")
);
-- Loading data from csv-preetham
COPY public."weight_log"
FROM 'C:\Users\shiva\Documents\ADT\project\weightLogInfo_data.csv'
DELIMITER ','
CSV HEADER


-- Application Functionality Design - Part 3

-- Finding the time spent and calories spent by user on a specific date -- Abhigna
create or replace function personTimeSpent(id_1 int)
     returns table(id int, date_activity date, timeSpent int, caloriesBurnt int) as
     $$
          select distinct d."Id", d."ActivityDate", d."Calories", mi."VeryActiveMinutes" + mi."FairlyActiveMinutes"+ mi."LightlyActiveMinutes"
		  from public."daily_calories" as d, public."minutes" as mi 
		  where d."Id" = mi."Id" and d."ActivityDate" = mi."ActivityDate" and d."Id" = id_1;
     $$ LANGUAGE SQL;
	 
select * from personTimeSpent(1503960366);

-- Finding the time spent and steps taken spent by user on a specific date -- Preetham
create or replace function personStepsCalories(id_1 int)
     returns table(id int, date_activity date, totalSteps int, caloriesBurnt int) as
     $$
          select distinct d."Id", d."ActivityDate", d."Calories", s."TotalSteps"
		  from public."daily_calories" as d, public."steps" as s 
		  where d."Id" = s."Id" and d."ActivityDate" = s."ActivityDate" and d."Id" = id_1;
     $$ LANGUAGE SQL;
	 
select * from personStepsCalories(1503960366);

-- Finding the time spent and distance travelled by user on a specific date -- Shivani
create or replace function personTotalDistance(id_1 int)
     returns table(id int, date_activity date, distance double precision, caloriesBurnt int) as
     $$
          select distinct d."Id", d."ActivityDate", d."Calories", dis."TotalDistance"
		  from public."daily_calories" as d, public."distances" as dis 
		  where d."Id" = dis."Id" and d."ActivityDate" = dis."ActivityDate" and d."Id" = id_1;
     $$ LANGUAGE SQL;
	 
select * from personTotalDistance(1503960366);

-- Finding average steps taken by all people -- Shivani
create view average_steps as
select s."Id",avg(dis."TotalSteps") from public."steps" as s group by (s."Id");

select * from average_steps;

-- Finding average distance travelled by all people -- Abhigna
create view average_distance as
select dis."Id",avg(dis."TotalDistance") from public."distances" as dis group by (dis."Id");

select * from average_steps;

-- Finding average calories burnt by all people -- Preeetham & Shivani
create view average_calories as
select d."Id",avg(d."Calories") from public."daily_calories" as d group by (d."Id");

select * from average_calories;

-- Finding average sleep taken by all people -- Abhigna
create view average_sleep as
select s."Id",avg(s."TotalMinutesAsleep") from public."sleeping" as s group by (s."Id");

select * from average_sleep;


