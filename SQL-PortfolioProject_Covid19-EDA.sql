/*
PROJECT NAME: EXPLORATION COVID-19 IN SQL
*/

-- I imported the the table contents from flat files retrieved from https://ourworldindata.org/covid-deaths

-- Let's check if our datasets have been correctly imported
SELECT TOP 1000 *
  FROM PortfolioProjectCOVID..covid_deaths;

SELECT TOP 1000 *
  FROM PortfolioProjectCOVID..covid_vaccinations;

-- Now, let's select the columns we are going to use
SELECT location, date, total_cases, new_cases, total_deaths, population
FROM PortfolioProjectCOVID..covid_deaths
ORDER BY 1, 2;

-- Summarizing total cases, total deaths and death rate per location
WITH cte_deaths_location AS (
SELECT 
	location, 
	SUM(CAST(new_cases AS BIGINT)) AS total_cases, 
	SUM(CAST(new_deaths AS BIGINT)) AS total_deaths,
	MAX(population) AS population,
	MIN(date) AS first_date_reported,
	MAX(date) AS last_date_reported
FROM PortfolioProjectCOVID..covid_deaths
GROUP BY location
)
SELECT *, 
	CAST(ROUND(100 * total_cases/population, 2) AS float) AS perc_cases_per_pop, -- percentage of cases per population
	CAST(ROUND(100 * total_deaths/population, 2) AS float) AS perc_deaths_per_pop, -- percentage of deaths per population
	CAST(100 * ROUND(CAST(total_deaths AS numeric)/ CAST(total_cases AS numeric), 4) AS float) AS death_rate_100, -- death rate per 100 people
	DATEDIFF(DD, first_date_reported, last_date_reported) AS days_reported
FROM cte_deaths_location
WHERE total_cases IS NOT NULL AND total_deaths IS NOT NULL AND population IS NOT NULL AND 
	DATEDIFF(DD, first_date_reported, last_date_reported) >= 365
ORDER BY perc_deaths_per_pop DESC, death_rate_100 DESC;

/*
Hungary was the location with the highest percentage of deaths per population in 423 days, 0.3%, according to the reports.
Also, from each 100 cases, about 3 ended up in death.
Czechia was the second location with a high percetage of deaths for covid-19, with about 2 people death per 100 cases. 

The top 10 most affected locations were: 'Hungary', 'Czechia', 'San Marino', 'Bosnia and Herzegovina', 'Bulgaria', 'Montenegro', 'North Macedonia', 'Slovakia', 'Belgium', 'Italy'.
*/

WITH cte_deaths_location AS (
SELECT 
	location, 
	SUM(CAST(new_cases AS BIGINT)) AS total_cases, 
	SUM(CAST(new_deaths AS BIGINT)) AS total_deaths,
	MAX(population) AS population,
	MIN(date) AS first_date_reported,
	MAX(date) AS last_date_reported
FROM PortfolioProjectCOVID..covid_deaths
GROUP BY location
)
SELECT *, 
	CAST(ROUND(100 * total_cases/population, 2) AS float) AS perc_cases_per_pop, -- percentage of cases per population
	CAST(ROUND(100 * total_deaths/population, 2) AS float) AS perc_deaths_per_pop, -- percentage of deaths per population
	CAST(100 * ROUND(CAST(total_deaths AS numeric)/ CAST(total_cases AS numeric), 4) AS float) AS death_rate_100, -- death rate per 100 people
	DATEDIFF(DD, first_date_reported, last_date_reported) AS days_reported
FROM cte_deaths_location
WHERE total_cases IS NOT NULL AND total_deaths IS NOT NULL AND population IS NOT NULL AND 
	DATEDIFF(DD, first_date_reported, last_date_reported) >= 365
ORDER BY perc_deaths_per_pop, death_rate_100;

/*
We also observed that Singapore and Bhutan had reported less than 1% of deaths per covid-19, and less than 5% cases.
The top 10 less affected locations that reported for at least one year were: 'Singapore', 'Bhutan', 'Timor', 'Burundi', 'Eritrea', 'Thailand', 'Mongolia', 'Malaysia', 'Cote d'Ivoire', 'Sri Lanka'
*/


/*
Which continent had the worst condition in terms of deaths
*/
WITH cte_deaths_location AS (
SELECT 
	continent, 
	SUM(CAST(new_cases AS BIGINT)) AS total_cases, 
	SUM(CAST(new_deaths AS BIGINT)) AS total_deaths,
	MAX(population) AS population,
	MIN(date) AS first_date_reported,
	MAX(date) AS last_date_reported
FROM PortfolioProjectCOVID..covid_deaths
WHERE location NOT IN ('World', 'Europe', 'South America', 'North America', 'European Union', 'Asia', 'Africa')
GROUP BY continent
)
SELECT *, 
	CAST(ROUND(100 * total_cases/population, 2) AS float) AS perc_cases_per_pop, -- percentage of cases per population
	CAST(ROUND(100 * total_deaths/population, 2) AS float) AS perc_deaths_per_pop, -- percentage of deaths per population
	CAST(100 * ROUND(CAST(total_deaths AS numeric)/ CAST(total_cases AS numeric), 4) AS float) AS death_rate_100, -- death rate per 100 people
	DATEDIFF(DD, first_date_reported, last_date_reported) AS days_reported
FROM cte_deaths_location
WHERE total_cases IS NOT NULL AND total_deaths IS NOT NULL AND population IS NOT NULL AND 
	DATEDIFF(DD, first_date_reported, last_date_reported) >= 365
ORDER BY perc_deaths_per_pop DESC, death_rate_100 DESC;

/*
Europe was the continent that covid-19 hit the most, with about 30% of cases reported, where about 2 out 100 ended up in death. 
About 1% of the European people died of covid-19 in 462 days.
The second most hit continent was South America, with the same death rate and 12% of population infected.
Oceania was the continent less affected by covid-19.
Across the continents, we observed that the aproximate death rate of covid-19 is 2 out 100.
*/

/*
Now, let's see what location did not report deaths for covid-19 for a year or longer
*/

WITH cte_no_report AS (
SELECT
	location, 
	COUNT(location) AS days_no_report,
	MAX(population) AS population,
	SUM(new_cases) total_cases,
	SUM(new_deaths) total_deaths
FROM PortfolioProjectCOVID..covid_deaths
WHERE new_cases IS NULL OR population IS NULL OR new_deaths IS NULL
GROUP BY location
)
SELECT *
FROM cte_no_report
WHERE days_no_report >= 365
ORDER BY days_no_report DESC;

/*
There were 7 locations that had no completely reported about covid-19 for a year or longer.
In this sense, we have no data reported for the population in the International location. 
Also, we have no total_deaths reported for longer than a year in the locations Vatican, Cambodia, Dominica, Laos, Saint Kitts and Nevis, and Timor.
*/

/*
Now, let's see what was the most critical month worlwide, in terms of death.
*/

SELECT
	TOP 1 CONCAT(year, '-', month) month,
	ROUND(CAST(100 * total_deaths/population AS float), 2) AS perc_deaths_per_pop,
	ROUND(CAST(100 * total_cases/population AS float),2) AS perc_cases_per_pop,
	ROUND(CAST(100 * CAST(total_deaths AS numeric)/ CAST(total_cases AS numeric) AS float), 2) AS death_rate_100--, -- death rate per 100 people
FROM (
SELECT
	YEAR(DATE) AS year,
	MONTH(date) AS month,
	SUM(CAST(new_cases AS BIGINT)) AS total_cases, 
	SUM(CAST(new_deaths AS BIGINT)) AS total_deaths,
	MAX(population) AS population
FROM PortfolioProjectCOVID..covid_deaths
WHERE location = 'World'
GROUP BY YEAR(DATE), MONTH(date)
) AS cte_deaths_location
ORDER BY perc_deaths_per_pop DESC;

/*
January 2021 was the most critical month since the covid outbreak around the globe, when about 0.25% of people died of covid-19.
*/

/*
Now, let's see the top 10 locations mostly affected in this month in terms of percentage of deaths per population
*/

WITH cte_deaths_20211 AS (
SELECT 
	location, 
	--DATENAME(month, DATEADD(month, MONTH(date), 0) - 1) AS month_name,
	SUM(CAST(new_cases AS BIGINT)) AS total_cases, 
	SUM(CAST(new_deaths AS BIGINT)) AS total_deaths,
	MAX(population) AS population
FROM PortfolioProjectCOVID..covid_deaths
WHERE YEAR(DATE) = 2021 AND MONTH(date) = 4 AND location NOT IN ('World', 'Europe', 'South America', 'North America', 'European Union', 'Asia', 'Africa')
GROUP BY location
)
SELECT 
	TOP 10 *,
	ROUND(CAST(100 * total_deaths/population AS float), 2) AS perc_deaths_per_pop,
	ROUND(CAST(100 * total_cases/population AS float),2) AS perc_cases_per_pop,
	ROUND(CAST(100 * CAST(total_deaths AS numeric)/ CAST(total_cases AS numeric) AS float), 2) AS death_rate_100--, -- death rate per 100 people
FROM cte_deaths_20211
ORDER BY perc_deaths_per_pop DESC, total_deaths DESC;

/*
The most affected locations in January 2021 were Hungary, Bosnia and Herzegovina, Bulgaria, Uruguay, North Macedonia, Brazil, Poland, Slovakia, Montenegro and Ukraine,
where the percentage of deaths of covid-19 was between 0.03% and 0.07% and the death rate was greater than the general rate, reaching 9 out 100 people infected.
*/

/*
Now, let's see the top 10 locations less affected in this month in terms of percentage of deaths per population
*/

WITH cte_deaths_20211 AS (
SELECT 
	location, 
	--DATENAME(month, DATEADD(month, MONTH(date), 0) - 1) AS month_name,
	SUM(CAST(new_cases AS BIGINT)) AS total_cases, 
	SUM(CAST(new_deaths AS BIGINT)) AS total_deaths,
	MAX(population) AS population
FROM PortfolioProjectCOVID..covid_deaths
WHERE YEAR(DATE) = 2021 AND MONTH(date) = 4 AND location NOT IN ('World', 'Europe', 'South America', 'North America', 'European Union', 'Asia', 'Africa')
GROUP BY location
)
SELECT 
	TOP 10 *,
	ROUND(CAST(100 * total_deaths/population AS float), 2) AS perc_deaths_per_pop,
	ROUND(CAST(100 * total_cases/population AS float),2) AS perc_cases_per_pop,
	ROUND(CAST(100 * CAST(total_deaths AS numeric)/ CAST(total_cases AS numeric) AS float), 2) AS death_rate_100--, -- death rate per 100 people
FROM cte_deaths_20211
WHERE total_cases IS NOT NULL AND total_deaths IS NOT NULL AND population IS NOT NULL
ORDER BY perc_deaths_per_pop ASC, total_deaths ASC;
/*
The less affected locations in January 2021 were Liberia, Vietnam, New Zealand, Iceland, Comoros, Sierra Leone, Fiji, Brunei, Burundi and Grenade,
where no deaths were reported, even where cases reached up to 1185 people.
*/


/*
Vaccinations and deaths.
*/

-- Cases and date report period
SELECT MIN(date), MAX(date)
FROM PortfolioProjectCOVID..covid_deaths
WHERE continent = 'Europe' --2020-01 until 2021-04

-- When the vaccination started?
SELECT MIN(date), MAX(date)
FROM PortfolioProjectCOVID..covid_vaccinations
WHERE continent = 'Europe' AND total_vaccinations IS NOT NULL -- 2020-12 until 2021-04

-- Creating a temporary table for cases and deaths BEFORE December 2020 (vaccination started in the world)
USE PortfolioProjectCOVID
CREATE TABLE #temp_bfr_vaccine (
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
INSERT INTO #temp_bfr_vaccine
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

SELECT * FROM PortfolioProjectCOVID..#temp_bfr_vaccine;

-- Creating a temporary table for cases and deaths AFTER December 2020 (vaccination started in the world)
USE PortfolioProjectCOVID
CREATE TABLE #temp_after_vaccine (
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
INSERT INTO #temp_after_vaccine
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
CREATE TABLE #temp_vaccine_agg (
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
INSERT INTO #temp_vaccine_agg
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

-- Analyzing the effect vaccines per location
SELECT 
	bf.location,
	ROUND(CAST(1000 * bf.total_deaths / bf.total_cases AS FLOAT), 1) AS bf_death_rate_1000,
	ROUND(CAST(1000 * af.total_deaths / af.total_cases AS FLOAT), 1) AS af_death_rate_1000,
	ROUND(CAST(100 * bf.total_deaths / bf.population AS FLOAT), 1) AS bf_perc_deaths_per_pop,
	ROUND(CAST(100 * af.total_deaths / af.population AS FLOAT), 1) AS af_perc_deaths_per_pop,
	ROUND(CAST(100 * bf.total_cases / bf.population AS FLOAT), 1) AS bf_perc_cases_per_pop,
	ROUND(CAST(100 * af.total_cases / af.population AS FLOAT), 1) AS af_perc_cases_per_pop,
	ROUND(CAST(100 * bf.icu_patients / bf.population AS FLOAT), 1) AS bf_perc_icu_per_pop,
	ROUND(CAST(100 * af.icu_patients / af.population AS FLOAT), 1) AS af_perc_icu_per_pop,
	ROUND(CAST(100 * bf.hosp_patients / bf.population AS FLOAT), 1) AS bf_perc_hosp_per_pop,
	ROUND(CAST(100 * af.hosp_patients / af.population AS FLOAT), 1) AS af_perc_hosp_per_pop,
	ROUND(CAST(100 * va.people_vaccinated / af.population AS FLOAT), 1) AS vaccinated_per_pop,
	ROUND(CAST(100 * va.people_fully_vaccinated / af.population AS FLOAT), 1) AS full_vaccinated_per_pop,
	va.perc_aged_65_older,
	va.median_age,
	va.cardiovasc_death_rate_1000,
	va.perc_diabetes_prevalence,
	va.perc_smokers,
	va.perc_extreme_poverty,
	va.perc_handwashing_facilities,
	va.hospital_beds_1000
FROM PortfolioProjectCOVID..#temp_bfr_vaccine AS bf
	INNER JOIN PortfolioProjectCOVID..#temp_after_vaccine AS af
	ON bf.iso_code = af.iso_code
	INNER JOIN PortfolioProjectCOVID..#temp_vaccine_agg AS va
	ON bf.iso_code = va.iso_code
ORDER BY bf_death_rate_1000 DESC;

/*
In Mexico, the death rate and percetage of deaths of covid-19 increased after the vaccination period started.
However, only 5% of people got fully vaccinated and about 10% got at least one dose of the vaccine. 
We also observed that the percentage of elderly is about 6% and the median age is about 30 years old.
Nonetheless, cardiovascular diseases are one of the main causes of death and diabetes is prevalent in Mexico.
Also the percentage of smokers is about 30%.
On the other hand, in Ecuador after after the vaccination period started, the death rated and percentage of deaths of covid-19 decreased significantly.
However, only 1% of people got fully vaccinated and about 4% got at least one dose of the vaccine. 
In comparison to Mexico, the diabetes prevalence and the percentage of smokers were significantly lower.
*/

SELECT 
	bf.location,
	ROUND(CAST(100 * va.people_vaccinated / af.population AS FLOAT), 1) AS vaccinated_per_pop,
	ROUND(CAST(100 * va.people_fully_vaccinated / af.population AS FLOAT), 1) AS full_vaccinated_per_pop,
	ROUND(CAST(1000 * bf.total_deaths / bf.total_cases AS FLOAT), 1) AS bf_death_rate_1000,
	ROUND(CAST(1000 * af.total_deaths / af.total_cases AS FLOAT), 1) AS af_death_rate_1000,
	ROUND(CAST(100 * bf.total_deaths / bf.population AS FLOAT), 1) AS bf_perc_deaths_per_pop,
	ROUND(CAST(100 * af.total_deaths / af.population AS FLOAT), 1) AS af_perc_deaths_per_pop,
	ROUND(CAST(100 * bf.total_cases / bf.population AS FLOAT), 1) AS bf_perc_cases_per_pop,
	ROUND(CAST(100 * af.total_cases / af.population AS FLOAT), 1) AS af_perc_cases_per_pop,
	ROUND(CAST(100 * bf.icu_patients / bf.population AS FLOAT), 1) AS bf_perc_icu_per_pop,
	ROUND(CAST(100 * af.icu_patients / af.population AS FLOAT), 1) AS af_perc_icu_per_pop,
	ROUND(CAST(100 * bf.hosp_patients / bf.population AS FLOAT), 1) AS bf_perc_hosp_per_pop,
	ROUND(CAST(100 * af.hosp_patients / af.population AS FLOAT), 1) AS af_perc_hosp_per_pop,
	va.perc_aged_65_older,
	va.median_age,
	va.cardiovasc_death_rate_1000,
	va.perc_diabetes_prevalence,
	va.perc_smokers,
	va.perc_extreme_poverty,
	va.perc_handwashing_facilities,
	va.hospital_beds_1000
FROM PortfolioProjectCOVID..#temp_bfr_vaccine AS bf
	INNER JOIN PortfolioProjectCOVID..#temp_after_vaccine AS af
	ON bf.iso_code = af.iso_code
	INNER JOIN PortfolioProjectCOVID..#temp_vaccine_agg AS va
	ON bf.iso_code = va.iso_code
ORDER BY vaccinated_per_pop DESC;

/*
Israel was the location where most people getting vaccinated, about 60% of people got fully vaccinated. 
However, there was no much difference in the death rates and percentages
*/










