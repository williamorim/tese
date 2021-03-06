Reduced Ultrafine Particle Levels in São Paulo's Atmosphere During Shifts from Gasoline to Ethanol Use

Alberto Salvo, 1* Joel Brito, 2 Paulo Artaxo, 3 Franz M. Geiger 4
1 Department of Economics, National University of Singapore, 10 Kent Ridge Crescent, Singapore 119260
2 Laboratory for Meteorological Physics (LaMP), University Clermont Auvergne, Clermont-Ferrand, France
3 Institute of Physics, University of São Paulo, Rua do Matão, Travessa R, 187, 05508-090, São Paulo, SP, Brazil
4 Department of Chemistry, Northwestern University, 2145 Sheridan Road, Evanston, IL, 60208, USA
* Corresponding Author, email-address albertosalvo@nus.edu.sg

****************************************************************************

Notes on data archived at https://drive.google.com/drive/folders/0B9o5r_FpA4LfcERYd0xnT0x5czg?usp=sharing
or via the url shortener https://goo.gl/9tNzvj

Please cite:

Salvo, A., J. Brito, P. Artaxo & F. M. Geiger. Reduced Ultrafine Particle Levels in São Paulo's Atmosphere During Shifts from Gasoline to Ethanol Use. Nature Communications (2017). DOI: 10.1038/s41467-017-00041-5

These notes are intended to complement information provided in the published article and the accompanying Supplementary Information. Please see the published article and the accompanying Supplementary Information for data sources and treatment. The notes may subsequently be edited to increase ease of use of the dataset hereby provided to other researchers. We gratefully acknowledge numerous people from ANP, CET, CETESB, INMET, and SPTrans for generously sharing their data.

For convenience, the data are assembled as a balanced panel into one Stata file (524 MB, Stata v13), though not all the data vary at the date-hour-site level. Alternatively, two text (csv) files are provided, one file with gasoline shares estimated for each of 200 bootstrap samples (see below), which vary only with date and not across air monitoring sites, and another text file with all the other variables.

****************************************************************************

Date of this version: May 23, 2017

****************************************************************************

Number of observations: 1673*24*14 = 562,128 (number of days between "1-Nov-08" and "31-May-13" * number of hours per day * number of air monitoring sites (see next).

The panel unit is an air monitoring site (location). It is indexed by the variable siteid. It takes on one of the following possible values:

siteid (source)	Name of site		Monitored pollutant at this site (and data frequency)
--------------------------------------------------------------------------------------------------------------
1 (CETESB)	Parque Dom Pedro II	Ozone (hourly frequency)
2 (CETESB)	Santana			Ozone (hourly frequency)
3 (CETESB)	Moóca			Ozone (hourly frequency)
5 (CETESB)	Ibirapuera		PM2.5 (24-hour mean, every six days), Ozone (hourly frequency)
6 (CETESB)	Nossa Senhora do Ó	Ozone (hourly frequency)
7 (CETESB)	São Caetano do Sul	Ozone (hourly frequency)
8 (CETESB)	Congonhas		PM2.5 (hourly frequency)
10 (CETESB)	Cerqueira César		PM2.5 (24-hour mean, every six days)
15 (CETESB)	Diadema			Ozone (hourly frequency)
18 (CETESB)	Santo André-Capuava	Ozone (hourly frequency)
22 (CETESB)	Mauá			Ozone (hourly frequency)
27 (CETESB)	Pinheiros		PM2.5 (24-hour mean, every six days), PM2.5 (hourly frequency), Ozone (hourly frequency)
29 (CETESB)	Parelheiros		Ozone (hourly frequency)
31 (CETESB)	USP			PM2.5 (hourly frequency), Ozone (hourly frequency)
31 (Own)	USP			Submicron particles (hourly frequency), Black carbon (hourly frequency)
--------------------------------------------------------------------------------------------------------------

Time is indexed by variable date (or variables year, month, day) and variable hour.


****************************************************************************

Description of variable:

pm25_24h_mass_conc	PM2.5 mass concentration, in ug/m3 (24-hour mean, every six days).

pm25_mass_conc		PM2.5 mass concentration, in ug/m3 (hourly frequency).

bc_mass_conc		Black carbon mass concentration, in ug/m3 (hourly frequency).

nm_7_100		Ultrafine particle number concentration 7‐100 nm, in cm-3 (hourly frequency).

nm_100_800		PM 100‐800 nm number concentration, in cm-3 (hourly frequency).

(Several additional variables, denoted nm_7_ub, for particle number concentrations in varying size bins as the 7-20 nm size bin is opened towards 800 nm, where ub denotes an upper bound.)

nucleation		Nucleation mode, dN/dlogDp in cm‐3 (hourly frequency).

aitken			Aitken mode, dN/dlogDp in cm‐3 (hourly frequency).

accumulation		Accumulation mode, dN/dlogDp in cm‐3 (hourly frequency).

particle_count		Particle number concentration in cm‐3 (hourly frequency, from the CPC).

o3_mass_conc		Ozone mass concentration, in ug/m3 (hourly frequency).

share_gas		Gasoline share in the flex-fuel light-vehicle fleet, estimated from a first-step consumer demand model, defined as the proportion of flex-fuel vehicles burning gasoline E25/E20 rather than ethanol E100. This variable varies by date and is common across sites.

share_gas_bs1-
share_gas_bs200 	The gasoline share estimated from each of 200 consumer-level first-step bootstrap samples. These estimates vary weekly; they were not linearly interpolated to vary by date within week. Point estimates for the original sample are share_gas.

share_gas_aggr		Gasoline share in the aggregate light vehicle and motorcycle fleet, calculated from monthly wholesaler shipments for the state of São Paulo, defined as the proportion of gasoline E25/E20 among combined gasoline E25/E20 and ethanol E100 shipments. Prior to computing aggregate shares, gasoline and ethanol quantities in cubic meters are converted to light-vehicle distance traveled. This variable varies by month and is common across sites.

pe_pg_ratio		The consumer ethanol-to-gasoline price ratio. This is the price ratio for one liter of regular-grade ethanol E100 to one liter of regular-grade gasoline E25/E20, based on the median ratio in each large weekly sample of São Paulo city retailers. This variable varies by date and is common across sites.

year
quarter
month
week
day			Year (2008 to 2013), Quarter of the year (1 to 4), Month of the year (1 to 12), Week of the year (1 to 52), Day of the month (1 to 31), all of which are generated from date.

dayofweek		Day of the week (Sunday = 0, Monday = 1, etc), which is generated from date.

dv_weekday_regular	Indicator (dummy) variable for a non-holiday (and non yearend school vacation) weekday.

dv_publicholiday	Indicator (dummy) variable for a public holiday, including a public holiday that falls on a weekend (Saturday or Sunday), or on a Monday or a Friday that lies between a public holiday and a weekend, e.g., a Friday subsequent to a public-holiday Thursday is effectively a public holiday.

dv_yearendvacation	Indicator (dummy) variable for a day that falls on a yearend school vacation fortnight.

(Other variables that are generated from time, such as trends or an indicator for a Monday that is not a public holiday and falls outside the yearend school vacation fortnight, are omitted for brevity.)

rd			Measured date-hour specific solar radiation, in W/m2.

tp			Measured date-hour specific ground temperature, in degrees Celsius.

hm			Measured date-hour specific relative humidity, in percent.

ws			Measured date-hour specific wind speed, in m/s.

wd			Wind direction at the nearest CETESB wind station, in degrees from north, clockwise. Wind direction varies not only over time but also across siteid, based on the wind station that is closest to siteid (wind stations are located in Ibirapuera, Osasco, Pinheiros and Santana).

wd_n_e
wd_s_e
wd_s_w
wd_n_w			A set of four wind direction indicator (dummy) variables that indicate if wind is blowing to a site from one of four quadrants (North-East = clockwise 0 to 90 degrees, South-East = 90 to 180 degrees, etc) and at a minimum wind speed of 0.5 m/s; thus, the omitted category indicates still air.

pp			Measured date-hour specific precipitation, in mm/h.

dv_ti_0to199m		Indicator (dummy) variable for a thermal inversion with base 0 to 199 meters from the surface observed at 09:00 or 21:00 local time, whichever time is closest to hour. An additional variable, dv_ti_0to199m_9am, fixes the reading at 09:00 for all hours within date.

dv_ti_200to499m		Indicator (dummy) variable for a thermal inversion with base 200 to 499 meters from the surface observed at 09:00 or 21:00 local time, whichever time is closest to hour. An additional variable, dv_ti_200to499m_9am, fixes the reading at 09:00 for all hours within date.

congestion_city		Measured date-hour specific extension of road traffic congestion, in km, across the city.

congestion_region	Measured siteid-date-hour specific extension of road traffic congestion, in km, in the traffic-monitored region of São Paulo city where siteid is located (or the region closest to siteid).

flight_departures	Measured date-hour specific number of flight departures from the inner city airport Congonhas, in hour-1. Entered as missing for all siteid other than 8, Congonhas.

priceindex_diesel	Real price index for diesel oil at the pump in the São Paulo metropolis, in index points. This variable varies by month and is common across sites.

publictransp_ridership	Measured ridership on diesel buses in the public transport system in the São Paulo metropolis, in million passengers/day. This variable varies by month and is common across sites.

bus_freq_campus		Measured frequency of public transit diesel buses passing through the university campus where the submicron particle sampling site was located, within a horizontal distance of 400 m on a weekday morning in vehicles/h. Entered as missing for all siteid other than 31, USP. This variable varies by month and is available between October 2010 and May 2011.

dv_beltway_open		Indicator (dummy) variable for a date after the opening of the southern section of the Greater São Paulo beltway, on March 31, 2010.


