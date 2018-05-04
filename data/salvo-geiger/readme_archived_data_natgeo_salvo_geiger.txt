Reduction in Local Ozone Levels in Urban São Paulo Due to a Shift from Ethanol to Gasoline Use
Alberto Salvo* and Franz M. Geiger
Department of Economics, National University of Singapore and Department of Chemistry, Northwestern University

****************************************************************************

Notes on the data archived at https://drive.google.com/folderview?id=0B9o5r_FpA4LfNGdnX25XYVkxaXc&usp=sharing
or via the url shortener http://bit.do/salvo_geiger_data

Please cite: 

Salvo, A. & F. M. Geiger. Reduction in Local Ozone Levels in Urban São Paulo Due to a Shift from Ethanol to Gasoline Use. Nature Geoscience (2014). DOI 10.1038/ngeo2144

These notes are intended to complement or reference, not substitute for, information provided in the Supplementary Information to the above-referenced Nature Geoscience article. The notes may subsequently be edited to increase ease of use of the dataset hereby provided to other researchers. For convenience, the data are assembled into one Stata file (approximately 500 MB, Stata v13), though not all the data vary at the date-hour-station level. 

Alternatively, two text (csv) files are provided, one file with gasoline shares estimated for each of 200 bootstrap samples (see below), which vary only with date and not across air monitoring stations, and another text file with all other variables.

Please see the published article and the accompanying Supplementary Information for data sources and treatment. We gratefully acknowledge numerous people from CETESB, INMET, CET, and ANP for generously sharing their data.

****************************************************************************

Date of this version: April 28, 2014

****************************************************************************

Number of observations: 1003*24*22 = 529,584 (number of days between "1-Nov-08" and "31-Jul-11" * number of hours per day * number of CETESB air monitoring stations in the São Paulo metropolitan area, including stations with high proportions of missing values).

****************************************************************************

Brief description of variable or reference to Supplementary Information (SI)

year, monthofyear, dayofmonth: Identifiers for date.

hour: Time of day.

stationno: Identifier for air monitoring station. See Table SII and Table A-S1 (in the Appendix to the SI). Stations with high proportions of missing values for pollutant concentrations are included in the dataset for completeness (see, in particular, Table A-S1 and its accompanying notes).

weekofyear: Week of year, which is generated from date.

dayofweek: Day of week (Sunday = 0), which is generated from date.

(Other variables that are generated from date or hour, such as trends and hour-of-day fixed effects, are omitted for brevity.)

dv_week_reg: Indicator variable for non-holiday (and non yearend school vacation) weekdays.

dv_pubhol: Indicator variable for public holidays, including public holidays that fell on weekends (Saturday and Sunday) and "pontes" (e.g., a Friday subsequent to a public-holiday Thursday). See SI Part E.

dv_sun_or_pubhol: Indicator variable for Sundays or public holidays.

dv_yearendvacation: Indicator variable for days that fell on the yearend school vacation fortnight. See SI Part E. 

O3, NO, NO2, NOX, CO, SO2, PM10: Measured stationno-date-hour specific pollutant concentrations, as reported by CETESB, in the originally reported units, namely micrograms per cubic meter for all concentrations except NOX (parts per billion) and CO (parts per million). See SI Part C.

share_gas: The estimated gasoline share (the proportion of bi-fuel vehicles burning gasoline E25 over ethanol E100). See SI Part A.

dv_e20: Indicator variable for the three-month period during which the government mandated the distribution of gasoline as E20 rather than the usual E25 (assuming a 5-day lag to account for fuel stored in retailers' and consumers' tanks). See SI Part A.

pp, rd, ws, tp, hm, pr, wd: Measured date-hour specific meteorological parameters, respectively, precipitation (mm), radiation (W/m2), wind speed (m/s), temperature (degrees Celsius), relative humidity (%), atmospheric pressure (hPa), and wind direction (degrees). See SI Part D and, in particular, Table SIII. Wind direction varies not only over time but also across stationno, based on the wind monitor that is closest to stationno (wind monitors are located in Ibirapuera, Osasco, Pinheiros and Santana, see Table SIII). 

sumcongestcity, sumcongestregion, sumcongestidist: Measured stationno-date-hour specific extension of road traffic congestion (all in km): across the city (sumcongestcity); in the traffic-monitored region where stationno is located or the region closest to it (i.e., north, east, south, west, or center, as indicated in Fig. 2) (sumcongestregion); and along local roads inverse-distance weighted (sumcongestidist). See SI Parts E and F, particularly E1 and F2.

angle_*_north angle_*_east angle_*_south angle_*_west angle_*_center: These variables vary at the stationno-"other region" level, as defined on p. S28 (namely, theta superscripted with 'station, other region' and subscripted with 'endpoint 1' or 'endpoint 2'). The variables are used along with wd and sumcongestregion (not the "own" region to a station but the other regions) to construct the "wind direction & traffic in other regions" interactions included in specification VII. These interactions are set equal to zero when wind speed is low, namely less than 0.5 m/s.

medianinvspeed_am: See robustness specification (e) on p. S33 and SI Part E.

pindex_diesel: See robustness specification (h) on p. S33.

pubtransp_weekday: See robustness specification (i) and Fig. S8 on, respectively, pp. S33 and S44.

industrial_activity: See robustness specification (j) and Fig. S9 on, respectively, pp. S33 and S45.

active_population: See robustness specification (k) on p. S33.

working_population: See robustness specification (l) on p. S33. 

mean_real_wage: See robustness specification (m) on p. S34.

share_g_wholesale: See robustness specification (o) on p. S34.

share_gas_bs1 to share_gas_bs200: The gasoline share estimated from each of 200 consumer-level ("first-step") bootstrap samples. See SI Part A. These estimates vary weekly -- they were not linearly interpolated to vary by day within week. (Point estimates for the original sample are share_gas.)