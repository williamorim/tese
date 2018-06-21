capture log close
log using NatComm_Ozone_William_FromPublishedData, text replace

use "F:\Dropbox\fuel mix and air quality follow on\data_repository\archived_data_natcomm_salvo_brito_artaxo_geiger.dta", clear

global hoursforreg "(hour>=12&hour<=16)"
global stations "1 2 3 5 6 7 15 18 22 27 29 31"

* restrict sample according to selected stations by pollutant
gen dv_keepstation=0
foreach var_st in $stations {
	replace dv_keepstation=1 if siteid==`var_st'
	}
keep if dv_keepstation
	
* public holidays by year
gen dv_pubhol=(date==date("2-Nov-08","DM20Y")|date==date("15-Nov-08","DM20Y")|date==date("20-Nov-08","DM20Y")|date==date("24-Dec-08","DM20Y")|date==date("25-Dec-08","DM20Y")|date==date("26-Dec-08","DM20Y")|date==date("31-Dec-08","DM20Y"))
replace dv_pubhol=1 if (date==date("1-Jan-09","DM20Y")|date==date("2-Jan-09","DM20Y")|date==date("25-Jan-09","DM20Y")|date==date("23-Feb-09","DM20Y")|date==date("24-Feb-09","DM20Y")|date==date("25-Feb-09","DM20Y")|date==date("10-Apr-09","DM20Y")|date==date("20-Apr-09","DM20Y")|date==date("21-Apr-09","DM20Y")|date==date("1-May-09","DM20Y")|date==date("11-Jun-09","DM20Y")|date==date("12-Jun-09","DM20Y")|date==date("9-Jul-09","DM20Y")|date==date("7-Sep-09","DM20Y")|date==date("12-Oct-09","DM20Y")|date==date("2-Nov-09","DM20Y")|date==date("15-Nov-09","DM20Y")|date==date("20-Nov-09","DM20Y")|date==date("24-Dec-09","DM20Y")|date==date("25-Dec-09","DM20Y")|date==date("31-Dec-09","DM20Y"))
replace dv_pubhol=1 if (date==date("1-Jan-10","DM20Y")|date==date("25-Jan-10","DM20Y")|date==date("15-Feb-10","DM20Y")|date==date("16-Feb-10","DM20Y")|date==date("17-Feb-10","DM20Y")|date==date("2-Apr-10","DM20Y")|date==date("21-Apr-10","DM20Y")|date==date("1-May-10","DM20Y")|date==date("3-Jun-10","DM20Y")|date==date("4-Jun-10","DM20Y")|date==date("9-Jul-10","DM20Y")|date==date("6-Sep-10","DM20Y")|date==date("7-Sep-10","DM20Y")|date==date("11-Oct-10","DM20Y")|date==date("12-Oct-10","DM20Y")|date==date("1-Nov-10","DM20Y")|date==date("2-Nov-10","DM20Y")|date==date("15-Nov-10","DM20Y")|date==date("20-Nov-10","DM20Y")|date==date("24-Dec-10","DM20Y")|date==date("25-Dec-10","DM20Y")|date==date("31-Dec-10","DM20Y"))
replace dv_pubhol=1 if (date==date("1-Jan-11","DM20Y")|date==date("24-Jan-11","DM20Y")|date==date("25-Jan-11","DM20Y")|date==date("7-Mar-11","DM20Y")|date==date("8-Mar-11","DM20Y")|date==date("9-Mar-11","DM20Y")|date==date("21-Apr-11","DM20Y")|date==date("22-Apr-11","DM20Y")|date==date("1-May-11","DM20Y")|date==date("23-Jun-11","DM20Y")|date==date("24-Jun-11","DM20Y")|date==date("9-Jul-11","DM20Y"))
* continued August 2011 on
replace dv_pubhol=1 if (date==date("7-Sep-2011","DMY")|date==date("12-Oct-2011","DMY")|date==date("2-Nov-2011","DMY")|date==date("14-Nov-2011","DMY")|date==date("15-Nov-2011","DMY")|date==date("20-Nov-2011","DMY")|date==date("24-Dec-2011","DMY")|date==date("25-Dec-2011","DMY")|date==date("31-Dec-2011","DMY"))
replace dv_pubhol=1 if (date==date("1-Jan-2012","DMY")|date==date("25-Jan-2012","DMY")|date==date("20-Feb-2012","DMY")|date==date("21-Feb-2012","DMY")|date==date("22-Feb-2012","DMY")|date==date("6-Apr-2012","DMY")|date==date("21-Apr-2012","DMY")|date==date("30-Apr-2012","DMY")|date==date("1-May-2012","DMY")|date==date("7-Jun-2012","DMY")|date==date("8-Jun-2012","DMY")|date==date("9-Jul-2012","DMY")|date==date("7-Sep-2012","DMY")|date==date("12-Oct-2012","DMY")|date==date("2-Nov-2012","DMY")|date==date("15-Nov-2012","DMY")|date==date("16-Nov-2012","DMY")|date==date("20-Nov-2012","DMY")|date==date("24-Dec-2012","DMY")|date==date("25-Dec-2012","DMY")|date==date("31-Dec-2012","DMY"))
replace dv_pubhol=1 if (date==date("1-Jan-2013","DMY")|date==date("25-Jan-2013","DMY")|date==date("11-Feb-2013","DMY")|date==date("12-Feb-2013","DMY")|date==date("13-Feb-2013","DMY")|date==date("29-Mar-2013","DMY")|date==date("21-Apr-2013","DMY")|date==date("1-May-2013","DMY")|date==date("30-May-2013","DMY")|date==date("31-May-2013","DMY"))
* yearend vacations for 2008/09, 2009/10 and 2010/11:
gen dvyearendvacations=((date>=date("24-Dec-08","DM20Y")&date<=date("9-Jan-09","DM20Y"))|(date>=date("24-Dec-09","DM20Y")&date<=date("8-Jan-10","DM20Y"))|(date>=date("24-Dec-10","DM20Y")&date<=date("7-Jan-11","DM20Y"))) & dv_pubhol~=1
* continued 2011/12 and 2012/13
replace dvyearendvacations=1 if ((date>=date("24-Dec-11","DM20Y")&date<=date("6-Jan-12","DM20Y"))|(date>=date("24-Dec-12","DM20Y")&date<=date("4-Jan-13","DM20Y"))) & dv_pubhol~=1

* more time variables or indicators
gen trend=(date-date("1-Nov-08","DM20Y"))/365.25
foreach var of numlist 2(1)52 {
	gen dv_week_`var'=(week==`var')
	}
capture gen dv_beltway_open=(date>=date("31-Mar-10","DM20Y"))
capture gen dayofweek=dow(date)
* "regular" weekdays, i.e., weekdays that are not public holidays nor do they fall on yearend vacation period
* "vacation" weekdays, i.e., weekdays that are not public holidays but do fall on yearend vacation period
* I do not consider July to be a "(school) vacation" period since traffic does not seem to change much according to the traffic authority; the last time the Rodizio was lifted for certain weeks of July was 2007
gen dv_week_reg=(dayofweek>=1 & dayofweek<=5 & dv_pubhol==0 & dvyearendvacations==0)
gen dv_mon_reg=(dv_week_reg==1 & dayofweek==1)
gen dv_tue_reg=(dv_week_reg==1 & dayofweek==2)
gen dv_wed_reg=(dv_week_reg==1 & dayofweek==3)
gen dv_thu_reg=(dv_week_reg==1 & dayofweek==4)
gen dv_fri_reg=(dv_week_reg==1 & dayofweek==5)
assert dv_mon_reg+dv_tue_reg+dv_wed_reg+dv_thu_reg+dv_fri_reg==dv_week_reg
gen dv_week_vac=(dayofweek>=1 & dayofweek<=5 & dv_pubhol==0 & dvyearendvacations==1)
gen dv_sat_reg=(dayofweek==6 & dv_pubhol==0 & dvyearendvacations==0)
gen dv_sat_vac=(dayofweek==6 & dv_pubhol==0 & dvyearendvacations==1)
gen dv_sun_reg=(dayofweek==0 & dv_pubhol==0 & dvyearendvacations==0)
gen dv_sun_vac=(dayofweek==0 & dv_pubhol==0 & dvyearendvacations==1)
assert dv_week_reg+dv_week_vac+dv_sat_reg+dv_sat_vac+dv_sun_reg+dv_sun_vac+dv_pubhol==1
gen dv_sun_or_pubhol=(dayofweek==0 | dv_pubhol==1)
assert dv_mon_reg+dv_tue_reg+dv_wed_reg+dv_thu_reg+dv_fri_reg+dv_week_vac+dv_sat_reg+dv_sat_vac+dv_sun_or_pubhol==1

* use traffic in the morning rush hours 07:00 to 11:00; also thermal inversion data recorded at 9am
foreach var in congestion_city congestion_region {
	capture drop temp
	gen temp=`var' if hour>=7&hour<=11
	bysort siteid date: egen `var'_am=mean(temp)
	}
/*	
foreach var in dv_ti_0to199m dv_ti_200to499m {
	capture drop temp
	gen temp=`var' if hour==9
	bysort siteid date: egen `var'_9am=mean(temp)
	}
*/

gen dv_congestion_city_am_0_20=(congestion_city_am>=0&congestion_city_am<20)
gen dv_congestion_city_am_20_50=(congestion_city_am>=20&congestion_city_am<50)
gen dv_congestion_city_am_50_80=(congestion_city_am>=50&congestion_city_am<80)
gen dv_congestion_city_am_80_200=(congestion_city_am>=80&congestion_city_am<200)
assert dv_congestion_city_am_0_20+dv_congestion_city_am_20_50+dv_congestion_city_am_50_80+dv_congestion_city_am_80_200==1 if (month>=10|month<=5) & o3_mass_conc~=.

gen dv_congestion_region_am_0_4=(congestion_region_am>=0&congestion_region_am<4)
gen dv_congestion_region_am_4_11=(congestion_region_am>=4&congestion_region_am<11)
gen dv_congestion_region_am_11_18=(congestion_region_am>=11&congestion_region_am<18)
gen dv_congestion_region_am_18_60=(congestion_region_am>=18&congestion_region_am<60)
assert dv_congestion_region_am_0_4+dv_congestion_region_am_4_11+dv_congestion_region_am_11_18+dv_congestion_region_am_18_60==1 if (month>=10|month<=5) & o3_mass_conc~=.

keep if $hoursforreg

collapse (mean) o3_mass_conc share_gas trend dv_week_2-dv_week_52 dv_mon_reg dv_tue_reg dv_wed_reg dv_thu_reg dv_fri_reg dv_week_vac dv_sat_reg dv_sat_vac dv_sun_vac dv_pubhol ///
	 rd tp hm ws pp dv_ti_0to199m_9am dv_ti_200to499m_9am dv_congestion_*_am_*_* dv_beltway_open month, by(date siteid)

gen dv_pp_0_0=(pp==0) if pp~=.
gen dv_pp_0_5=(pp>0&pp<.5) if pp~=.
gen dv_pp_5_20=(pp>=.5&pp<2) if pp~=.
gen dv_pp_20_150=(pp>=2&pp<15) if pp~=.

reg o3_mass_conc share_gas i.siteid##c.trend dv_week_2-dv_week_52 dv_mon_reg dv_tue_reg dv_wed_reg dv_thu_reg dv_fri_reg dv_week_vac dv_sat_reg dv_sat_vac dv_sun_vac dv_pubhol ///
	rd tp hm ws dv_pp_*_* dv_ti_0to199m_9am dv_ti_200to499m_9am dv_congestion_city_am_*_* dv_congestion_region_am_*_* i.siteid##i.dv_beltway_open if (month>=10|month<=5), robust cluster(date)
summ o3_mass_conc if e(sample)
	
* impact of share of Gasoline E20/E25 in the flex fleet rising from 30 to 80% (point estimate and standard error)
disp _b[share_gas]*(.8-.3)
disp _se[share_gas]*(.8-.3)

* note to William: these standard errors are too low; you need to account for sampling variation in the predicted gasoline share using the published bootstrap samples
	

log close

