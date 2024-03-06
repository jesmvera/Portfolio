/*
I imported and bulk insert data to the tables deaths and vaccination from flat files retrieved from https://ourworldindata.org/covid-deaths
I needed to do some alterations in the tables designs because some new values overflow the capacity of the previous data types.

--ALTER TABLE PortfolioProjectCOVID..covid_deaths ALTER COLUMN new_cases BIGINT;
--ALTER TABLE PortfolioProjectCOVID..covid_deaths ALTER COLUMN new_deaths BIGINT;

BULK INSERT PortfolioProjectCOVID..covid_deaths
FROM 'G:\Other computers\My Laptop (1)\OneDrive\Documents\COURSES\DATA ANALYST BOOTCAMP\datasets\CovidDeaths_202105-2024.csv'
WITH
(
	FIRSTROW = 2, -- The first row is a header
	FIELDTERMINATOR = ',',
	ROWTERMINATOR = '\n'
);

SELECT min(date), max(date)
  FROM PortfolioProjectCOVID..covid_deaths;

--ALTER TABLE PortfolioProjectCOVID..covid_vaccinations ALTER COLUMN total_vaccinations BIGINT;
--ALTER TABLE PortfolioProjectCOVID..covid_vaccinations ALTER COLUMN people_vaccinated BIGINT;
--ALTER TABLE PortfolioProjectCOVID..covid_vaccinations ALTER COLUMN people_fully_vaccinated BIGINT;
--ALTER TABLE PortfolioProjectCOVID..covid_vaccinations ALTER COLUMN new_vaccinations BIGINT;

BULK INSERT PortfolioProjectCOVID..covid_vaccinations
FROM 'G:\Other computers\My Laptop (1)\OneDrive\Documents\COURSES\DATA ANALYST BOOTCAMP\datasets\CovidVaccinations_202105-2024.csv'
WITH
(
	FIRSTROW = 2, -- The first row is a header
	FIELDTERMINATOR = ',',
	ROWTERMINATOR = '\n'
);

SELECT min(date), max(date)
  FROM PortfolioProjectCOVID..covid_vaccinations;

*/

-------- #### PROJECT NAME: EXPLORATION COVID-19 IN SQL

-- Let's check if our datasets have been correctly imported
SELECT TOP 1000 *
  FROM PortfolioProjectCOVID..covid_deaths;

SELECT TOP 1000 *
  FROM PortfolioProjectCOVID..covid_vaccinations;

-- We added new variables to store the first date of report, the initial population and a correction in the number of cases and deaths.
CREATE VIEW vw_deaths_edited AS
WITH cte_first_report AS (
SELECT *, 
MIN(date) OVER (PARTITION BY location) AS first_date_report
FROM PortfolioProjectCOVID..covid_deaths
)
SELECT 
	de1.*, 
	de2.population AS initial_population,
	de3.real_total_cases,
	de3.real_total_deaths
FROM cte_first_report AS de1
LEFT JOIN PortfolioProjectCOVID..covid_deaths AS de2
ON de1.location = de2.location
AND de1.first_date_report = de2.date
LEFT JOIN (
SELECT location, MAX(total_cases) AS real_total_cases, MAX(total_deaths) AS real_total_deaths
FROM (SELECT 
	location, 
	SUM(new_cases) AS total_cases, 
	SUM(new_deaths) AS total_deaths
FROM PortfolioProjectCOVID..covid_deaths
GROUP BY location
UNION
SELECT 
	location, 
	MAX(total_cases) AS total_cases,
	MAX(total_deaths) AS total_deaths
FROM PortfolioProjectCOVID..covid_deaths
GROUP BY location
) AS union_max
GROUP BY location) AS de3
ON de1.location = de3.location;


------------- Summarizing total cases, total deaths and death rate per location

-- Creating a view that we'll use often later about deaths per location
CREATE VIEW vw_deaths_per_location AS
WITH cte_deaths_location AS (
SELECT 
	location,
	continent,
	--SUM(new_cases) AS new_cases_sum,
	--SUM(new_deaths) AS new_deaths_sum,
	--MAX(population) AS population,
	MAX(real_total_cases) AS total_cases, 
	MAX(real_total_deaths) AS total_deaths,
	MAX(initial_population) AS initial_population,
	MIN(date) AS first_date_reported,
	MAX(date) AS last_date_reported
FROM PortfolioProjectCOVID..vw_deaths_edited
GROUP BY location, continent
HAVING MAX(real_total_cases) IS NOT NULL AND MAX(real_total_deaths) IS NOT NULL AND 
	MAX(initial_population) IS NOT NULL AND MAX(real_total_cases) > 0 AND MAX(initial_population) > 0
)
SELECT *, 
	ROUND(100 * total_cases/initial_population, 2) AS perc_cases_per_pop, -- percentage of cases per population
	ROUND(100 * total_deaths/initial_population, 2) AS perc_deaths_per_pop, -- percentage of deaths per population
	1000 * total_deaths / total_cases AS death_rate_1000, -- death rate per 100 people
	DATEDIFF(DD, first_date_reported, last_date_reported) AS days_reported,
	total_deaths/DATEDIFF(DD, first_date_reported, last_date_reported) AS avg_dead_people_daily
FROM cte_deaths_location
WHERE DATEDIFF(DD, first_date_reported, last_date_reported) >= 365; -- only locations that reported for longer than a year

-- Top 10 of locations with the highest percentage of people dead of covid-19
SELECT 
	TOP 10 location,
	CAST(perc_deaths_per_pop AS float) perc_deaths_per_pop,
	total_deaths,
	total_cases, 
	initial_population
FROM vw_deaths_per_location
WHERE location NOT IN ('World', 'Europe', 'South America', 'North America', 'European Union', 'Asia', 'Africa') AND
	location NOT LIKE '%income'
ORDER BY perc_deaths_per_pop DESC;

/*
Peru was the location with the highest percentage of loss due to covid-19, with about 0.67% of its population dead,
followed by 'Bulgaria', 'Hungary', 'Bosnia and Herzegovina', 'North Macedonia', 'Slovenia', 'Croatia', 'Montenegro', 'Georgia', 'Czechia',
all of them with of percentage people dead of covid-19 between 0.41% and 0.56%, relative to their population.
*/

-- Top 10 of locations with the highest amount of people dead of covid-19
SELECT 
	TOP 10 location, 
	total_deaths,
	total_cases, 
	initial_population,
	CAST(perc_deaths_per_pop AS float) perc_deaths_per_pop,
	avg_dead_people_daily
FROM vw_deaths_per_location
WHERE location NOT IN ('World', 'Europe', 'South America', 'North America', 'European Union', 'Asia', 'Africa') AND
	location NOT LIKE '%income'
ORDER BY total_deaths DESC;

/*
However, when it is about amount of people, The United States is the location with the highest number of people dead of covid-19, 
about 1.1 million of people. This amout stood for the 0.35% of the American population.
The daily average of people dead of covid-19 in this location was 787.
The next locations with the highest number of people who died of covid-19 were 
'Brazil', 'India', 'Russia', 'Mexico', 'United Kingdom', 'Peru', 'Italy', 'Germany', 'France'. 
All of them lost between 1.6 and 7.1 thousand of people of covid-19, and the daily death average was higher than 110 people.
*/

-- Top 10 of locations with the highest covid-19 death rate per thousand
SELECT 
	TOP 10 location, 
	death_rate_1000,
	CAST(perc_deaths_per_pop AS float) perc_deaths_per_pop,
	total_deaths,
	total_cases, 
	initial_population,
	avg_dead_people_daily
FROM vw_deaths_per_location
WHERE location NOT IN ('World', 'Europe', 'South America', 'North America', 'European Union', 'Asia', 'Africa') AND
	location NOT LIKE '%income'
ORDER BY death_rate_1000 DESC;

/*
Regarding death rate per thousand, which means deaths per 1000 cases of covid-19, 
Yemen reported the highest rate with 181 people dead out of 1000 that was positive to covid-19.
'Sudan', 'Syria', 'Somalia', 'Egypt', 'Peru', 'Mexico', 'Bosnia and Herzegovina', 'Liberia', 'Afghanistan'
*/

-- Top 10 of locations with the highest percentage of covid-19 cases
SELECT 
	TOP 10 location, 
	CAST(perc_cases_per_pop AS float) perc_cases_per_pop,
	total_cases, 
	total_deaths,
	death_rate_1000,
	initial_population,
	avg_dead_people_daily
FROM vw_deaths_per_location
WHERE location NOT IN ('World', 'Europe', 'South America', 'North America', 'European Union', 'Asia', 'Africa') AND
	location NOT LIKE '%income'
ORDER BY perc_cases_per_pop DESC;

/*
According to the reports, Brunei, Cyprus, San Marino and Faeroe Islands got at least 70% of their population infected, 
but the death rate was less than 10 people out of 1000.
*/

-- Top 10 of locations with the highest number of people infected with covid-19 cases
SELECT 
	TOP 10 location, 
	total_cases, 
	CAST(perc_cases_per_pop AS float) perc_cases_per_pop,
	total_deaths,
	death_rate_1000,
	initial_population,
	avg_dead_people_daily
FROM vw_deaths_per_location
WHERE location NOT IN ('World', 'Europe', 'South America', 'North America', 'European Union', 'Asia', 'Africa') AND
	location NOT LIKE '%income'
ORDER BY total_cases DESC;

/*
As for number of infected people, The US reported the highest amount, with over 100 millon of people infected and a death rate of 11 out 1000.
Followed by 'China', 'India', 'France', 'Germany', 'Brazil', 'South Korea', 'Japan', 'Italy', 'United Kingdom'
*/

-- Top 10 of locations with the lowest percentage of people dead of covid-19
SELECT 
	TOP 10 location, 
	CAST(perc_deaths_per_pop AS float) perc_deaths_per_pop,
	total_deaths,
	total_cases, 
	initial_population
FROM vw_deaths_per_location
ORDER BY perc_deaths_per_pop;

/*
'Falkland Islands', 'Mali', 'Democratic Republic of Congo', 'Uzbekistan', 'Nigeria', 'Chad', 'Sierra Leone', 'Nicaragua', 'Vatican', 'Saint Helena' were
the location with the lowest percentage of loss due to covid-19, with less than 0.01% of its population dead.
Most of these also reported the lowest amount od dead people and cases.
We also observed six locations with no death of covid-19, namely, Falkland Islands, Vatican, Saint Helena, Pitcairn, Tokelau, and Niue.
However, some of these reached even 50% of cases per population.
*/

-- Continents covid-19 death and cases
SELECT 
	location, 
	CAST(perc_deaths_per_pop AS float) AS perc_deaths_per_pop, 
	ROW_NUMBER() OVER (ORDER BY perc_deaths_per_pop DESC) AS rank_perc_deaths_desc,
	CAST(perc_cases_per_pop AS float) AS perc_cases_per_pop, 
	ROW_NUMBER() OVER (ORDER BY perc_cases_per_pop DESC) AS rank_perc_cases_desc,
	total_deaths,
	ROW_NUMBER() OVER (ORDER BY total_deaths DESC) AS rank_total_deaths_desc,
	total_cases,
	ROW_NUMBER() OVER (ORDER BY total_cases DESC) AS rank_total_cases_desc,
	death_rate_1000,
	ROW_NUMBER() OVER (ORDER BY death_rate_1000 DESC) AS rank_death_rate_desc,
	avg_dead_people_daily,
	ROW_NUMBER() OVER (ORDER BY avg_dead_people_daily DESC) AS rank_daily_deaths_desc,
	initial_population
FROM vw_deaths_per_location
WHERE location IN ('Europe', 'South America', 'North America', 'Asia', 'Africa')
ORDER BY perc_deaths_per_pop DESC;

/*
South America was the continent with the highest percentage of deaths and death rate, 19 out of 1000 infected people, while
Europe was the continent with the highest percentage of cases and number of deaths, but the second highest in total cases and the death rate was low.
The highest number of cases was reported in Asia, where was also reported the second highest number of deaths.
Although Africa was one of the countries with the lowest cases, it displayed the highest death rate along with South America, 
where per each thousand of infected people, about 19 died.
*/


----- Now, what was the impact of covid-19 on the globe?
SELECT 
	location, 
	CAST(perc_cases_per_pop AS float) perc_cases_per_pop,
	CAST(perc_deaths_per_pop AS float) perc_deaths_per_pop,
	total_deaths,
	total_cases, 
	avg_dead_people_daily,
	death_rate_1000,
	initial_population
FROM vw_deaths_per_location
WHERE location = 'World';
 
/*According to the reports, about 780 million people or 10% of the global population people was infected with covid-19,
and about 7 million or 0.09% of the population died of covid-19, which means the death rate was 9 per thousand and
the average daily deaths of this virus was 4.5 thousand of people since 2020
*/


--Now, let's see the global trend per year.
SELECT
	year,
	total_cases,
	total_deaths,
	ROUND(CAST(100 * total_deaths/initial_population AS float), 2) AS perc_deaths_per_pop,
	ROUND(CAST(100 * total_cases/initial_population AS float),2) AS perc_cases_per_pop,
	ROUND(CAST(1000 * total_deaths/ total_cases AS float), 2) AS death_rate_1000
FROM (
SELECT
	YEAR(DATE) AS year,
	SUM(new_cases) AS total_cases, 
	SUM(new_deaths) AS total_deaths,
	MAX(initial_population) AS initial_population
FROM vw_deaths_edited
WHERE location = 'World'
GROUP BY YEAR(DATE)
) AS cte_deaths_location
ORDER BY year;

/*From 2020 through 2022, the number of cases displayed an increasing trend and then the tendency started to decrease, while deaths started to decrease from 2022 on.*/

--Now, let's see what was the most critical month worlwide, in terms of death.
SELECT
	TOP 1 CONCAT(year, '-', month) month,
	total_cases,
	total_deaths,
	ROUND(CAST(100 * total_deaths/initial_population AS float), 2) AS perc_deaths_per_pop,
	ROUND(CAST(100 * total_cases/initial_population AS float),2) AS perc_cases_per_pop,
	ROUND(CAST(1000 * total_deaths/ total_cases AS float), 2) AS death_rate_1000
FROM (
SELECT
	YEAR(DATE) AS year,
	MONTH(date) AS month,
	SUM(new_cases) AS total_cases, 
	SUM(new_deaths) AS total_deaths,
	MAX(initial_population) AS initial_population
FROM vw_deaths_edited
WHERE location = 'World'
GROUP BY YEAR(DATE), MONTH(date)
) AS cte_deaths_location
ORDER BY perc_deaths_per_pop DESC;

--January 2021 was the most critical month since the covid outbreak around the globe, when about 411 thousand people or 0.25% of the population died of covid-19.

/*
Vaccinations and deaths.
*/

-- Creating a view to use often
CREATE VIEW vaccination_edited AS
SELECT *,
	MIN(date) OVER(PARTITION BY location) AS first_vaccination_date,
	MAX(total_vaccinations) OVER(PARTITION BY location) AS final_total_vaccinations,
	MAX(people_vaccinated) OVER(PARTITION BY location) AS total_people_vaccinated,
	MAX(people_fully_vaccinated) OVER(PARTITION BY location) AS total_fully_vaccinated
FROM PortfolioProjectCOVID..covid_vaccinations
WHERE total_vaccinations IS NOT NULL

-- When the vaccination started per location?
SELECT location, MIN(date) first_vaccination_day, MAX(date) last_vaccination_day
FROM PortfolioProjectCOVID..covid_vaccinations
WHERE total_vaccinations IS NOT NULL AND continent IS NOT NULL AND 
	location NOT IN ('World', 'Europe', 'South America', 'North America', 'European Union', 'Asia', 'Africa') AND
	location NOT LIKE '%income'
GROUP BY location
ORDER BY first_vaccination_day;

/* The first country starting the vaccination against COVID-19 was Canada, followed by Russia and China in the mid December 2020,
while locations like Haiti, Tanzania, Bonaire Sint Eustatius and Saba, Burundi and Turkmenistan, 
started the vaccination between the last half of 2021 and and early 2022*/

-- Adding deaths and cases before and after vaccination started
CREATE VIEW vw_deaths_vaccination AS
SELECT vac.*,
	dea.initial_population,
	dea.total_cases AS bf_total_cases,
	(dea.real_total_cases - dea.total_cases) AS af_total_cases,
	dea.real_total_cases AS overall_total_cases,
	dea.total_deaths AS bf_total_deaths,
	(dea.real_total_deaths - dea.total_deaths) AS af_total_deaths,
	dea.real_total_deaths AS overall_total_deaths
FROM vaccination_edited AS vac
LEFT JOIN vw_deaths_edited AS dea
ON vac.location = dea.location
AND vac.first_vaccination_date = dea.date;

-- Creating a view that summarizes vaccinations and deaths per location
CREATE VIEW vw_vaccine_death_per_location AS
SELECT 
	iso_code,
	continent,
	location,
	MAX(final_total_vaccinations) AS total_vaccinations,
	MAX(total_people_vaccinated) AS people_vaccinated,
	MAX(total_fully_vaccinated) AS people_fully_vaccinated,
	MAX(initial_population) AS initial_population,
	100*MAX(total_people_vaccinated)/MAX(initial_population) AS perc_people_vaccinated,
	100*MAX(total_fully_vaccinated)/MAX(initial_population) AS perc_fully_vaccinated,
	MAX(bf_total_cases) AS bf_total_cases,
	MAX(af_total_cases) AS af_total_cases,
	--(MAX(af_total_cases)/MAX(bf_total_cases))-1.00 AS diff_total_cases,
	MAX(overall_total_cases) AS total_cases,
	MAX(bf_total_deaths) AS bf_total_deaths,
	MAX(af_total_deaths) AS af_total_deaths,
	--(MAX(af_total_deaths)/MAX(bf_total_deaths))-1.00 AS diff_total_deaths,
	MAX(overall_total_deaths) AS total_deaths,
	1000*MAX(bf_total_deaths)/MAX(bf_total_cases) AS bf_death_rate_1000,
	1000*MAX(af_total_deaths)/MAX(af_total_cases) AS af_death_rate_1000,
	--((MAX(af_total_deaths)*MAX(bf_total_cases))/(MAX(bf_total_deaths)*MAX(af_total_cases)))-1.00 AS diff_death_rate_1000,
	MAX(median_age) AS median_age,
	MAX(aged_65_older) AS perc_aged_65_older,
	MAX(extreme_poverty) AS perc_extreme_poverty,
	MAX(cardiovasc_death_rate) AS cardiovasc_death_rate_1000,
	MAX(diabetes_prevalence) AS diabetes_prevalence,
	MAX(female_smokers + male_smokers) perc_smokers,
	MAX(hospital_beds_per_thousand) AS hospital_beds_1000,
	MAX(handwashing_facilities) AS perc_handwashing_facilities
FROM vw_deaths_vaccination
WHERE total_vaccinations IS NOT NULL AND people_vaccinated IS NOT NULL AND people_fully_vaccinated IS NOT NULL AND continent IS NOT NULL AND
	initial_population IS NOT NULL AND bf_total_cases IS NOT NULL AND af_total_cases IS NOT NULL AND 
	initial_population > 0 AND bf_total_cases > 0 AND af_total_cases > 0
GROUP BY iso_code, continent, location;

/*
-- Creating a temporary table for cases and deaths BEFORE December 2020 (vaccination started in the world)
USE PortfolioProjectCOVID
CREATE TABLE tb_bfr_vaccine (
	iso_code nvarchar(50),
	continent nvarchar(50),
	location nvarchar(50),
	population numeric(18, 0),
	total_cases numeric(18, 0),
	total_deaths numeric(18, 0),
	icu_patients numeric(18, 0),
	hosp_patients numeric(18, 0)
);

-- Filling the temporary table
INSERT INTO tb_bfr_vaccine
SELECT
	iso_code,
	continent,
	location,
	MAX(population) AS population,
	SUM(CAST(new_cases AS BIGINT)) AS total_cases, 
	SUM(CAST(new_deaths AS BIGINT)) AS total_deaths,
	MAX(icu_patients) AS icu_patients,
	MAX(hosp_patients) AS hosp_patients
FROM PortfolioProjectCOVID..covid_deaths
WHERE new_cases IS NOT NULL AND new_deaths IS NOT NULL AND population IS NOT NULL AND
	YEAR(date) <= 2020 AND MONTH(date) <= 12 AND continent IS NOT NULL
GROUP BY iso_code, continent, location;

SELECT * FROM PortfolioProjectCOVID..tb_bfr_vaccine;

-- Creating a temporary table for cases and deaths AFTER December 2020 (vaccination started in the world)
USE PortfolioProjectCOVID
CREATE TABLE tb_after_vaccine (
	iso_code nvarchar(50),
	continent nvarchar(50),
	location nvarchar(50),
	population numeric(18, 0),
	total_cases numeric(18, 0),
	total_deaths numeric(18, 0),
	icu_patients numeric(18, 0),
	hosp_patients numeric(18, 0)
);

-- Filling the temporary table
INSERT INTO tb_after_vaccine
SELECT
	iso_code,
	continent,
	location,
	MAX(population) AS population,
	SUM(CAST(new_cases AS BIGINT)) AS total_cases, 
	SUM(CAST(new_deaths AS BIGINT)) AS total_deaths,
	MAX(icu_patients) AS icu_patients,
	MAX(hosp_patients) AS hosp_patients
FROM PortfolioProjectCOVID..covid_deaths
WHERE new_cases IS NOT NULL AND new_deaths IS NOT NULL AND population IS NOT NULL AND
	YEAR(date) > 2020 AND continent IS NOT NULL
GROUP BY iso_code, continent, location;

SELECT * FROM PortfolioProjectCOVID..#temp_after_vaccine;

-- Creating a temporary table for vaccines aggregated by location (vaccination started in the world)
USE PortfolioProjectCOVID
CREATE TABLE tb_vaccine_agg (
	iso_code nvarchar(50),
	continent nvarchar(50),
	location nvarchar(50),
	total_vaccinations numeric(18, 0),
	people_vaccinated numeric(18, 0),
	people_fully_vaccinated numeric(18, 0),
	median_age float,
	perc_aged_65_older float,
	perc_extreme_poverty float,
	cardiovasc_death_rate_1000 float,
	perc_diabetes_prevalence float,
	perc_smokers float,
	hospital_beds_1000 float,
	perc_handwashing_facilities float
);

-- Filling the temporary table
INSERT INTO tb_vaccine_agg
SELECT 
	iso_code,
	continent,
	location,
	MAX(CAST(total_vaccinations AS BIGINT)) AS total_vaccinations,
	MAX(CAST(people_vaccinated AS BIGINT)) AS people_vaccinated,
	MAX(CAST(people_fully_vaccinated AS BIGINT)) AS people_fully_vaccinated,
	ROUND(MAX(CAST(median_age AS float)),3) AS median_age,
	ROUND(MAX(CAST(aged_65_older AS float)),3) AS perc_aged_65_older,
	ROUND(MAX(CAST(extreme_poverty AS float)),3) AS perc_extreme_poverty,
	ROUND(MAX(CAST(cardiovasc_death_rate AS float)),3) AS cardiovasc_death_rate_1000,
	ROUND(MAX(CAST(diabetes_prevalence AS float)),3) AS diabetes_prevalence,
	ROUND(MAX(CAST(female_smokers + male_smokers AS float)),3) perc_smokers,
	ROUND(MAX(CAST(hospital_beds_per_thousand AS float)),3) AS hospital_beds_1000,
	ROUND(MAX(CAST(handwashing_facilities AS float)),3) AS perc_handwashing_facilities
FROM PortfolioProjectCOVID..covid_vaccinations
WHERE total_vaccinations IS NOT NULL AND people_vaccinated IS NOT NULL AND people_fully_vaccinated IS NOT NULL AND continent IS NOT NULL
GROUP BY iso_code, continent, location;

SELECT * FROM PortfolioProjectCOVID..#temp_vaccine_agg;
*/

SELECT * FROM vw_vaccine_death_per_location

SELECT -- Death rate before and after vaccination
	location,
	bf_death_rate_1000,
	af_death_rate_1000,
	bf_total_deaths,
	af_total_deaths,
	bf_total_cases,
	af_total_cases,
	perc_people_vaccinated,
	perc_fully_vaccinated
FROM vw_vaccine_death_per_location
WHERE bf_death_rate_1000 > 0 AND bf_death_rate_1000 IS NOT NULL AND bf_total_deaths > 0 AND bf_total_deaths IS NOT NULL AND
	bf_total_cases > 0 AND bf_total_cases IS NOT NULL
ORDER BY bf_death_rate_1000 DESC;

/*
In Yemen, where the highest death rate before vaccination was reported, after vaccination, it decreased from 196 to 164.
Although, in Yemen, only 3.5% of population got vaccinated and only 2.7% got fully vaccinated.
In Mexico, where about 75% of people got the vaccine, the death rate per thousand decreased from 88 to 33, which is more than 50%. */

SELECT -- the death rate of the locations with the highest percent of people vaccinated
	location,
	bf_death_rate_1000,
	af_death_rate_1000,
	bf_total_deaths,
	af_total_deaths,
	bf_total_cases,
	af_total_cases,
	perc_people_vaccinated,
	perc_fully_vaccinated
FROM vw_vaccine_death_per_location
WHERE bf_death_rate_1000 > 0 AND bf_death_rate_1000 IS NOT NULL AND bf_total_deaths > 0 AND bf_total_deaths IS NOT NULL AND
	bf_total_cases > 0 AND bf_total_cases IS NOT NULL
ORDER BY perc_people_vaccinated DESC;

/*
There are some countries that got a decrease in the death rate, but others didn't, e.g., Peru and Nepal.
So let's see in other analysis if the vaccination got an impact in the death rate.
*/

