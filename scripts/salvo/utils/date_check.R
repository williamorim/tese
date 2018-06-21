date2 <- function(x, ph) {
  
  lubridate::dmy(x)
  
}

date_ <- df_salvo$date

pubhol <- ifelse(
  date_ == date2("2-Nov-08","DM20Y") | date_ == date2("15-Nov-08","DM20Y") |  
    date_ == date2("20-Nov-08","DM20Y") | date_ == date2("24-Dec-08","DM20Y") |  
    date_ == date2("25-Dec-08","DM20Y") | date_ == date2("26-Dec-08","DM20Y") |  
    date_ == date2("31-Dec-08","DM20Y") | date_ == date2("1-Jan-09","DM20Y") | 
    date_ == date2("2-Jan-09","DM20Y") | date_ == date2("25-Jan-09","DM20Y") | 
    date_ == date2("23-Feb-09","DM20Y") | date_ == date2("24-Feb-09","DM20Y") | 
    date_ == date2("25-Feb-09","DM20Y") | date_ == date2("10-Apr-09","DM20Y") | 
    date_ == date2("20-Apr-09","DM20Y") | date_ == date2("21-Apr-09","DM20Y") |
    date_ == date2("1-May- 09","DM20Y") | date_ == date2("11-Jun-09","DM20Y") | 
    date_ == date2("12-Jun-09","DM20Y") | date_ == date2("9-Jul-09","DM20Y") | 
    date_ == date2("7-Sep-09","DM20Y") | date_ == date2("12-Oct-09","DM20Y") | 
    date_ == date2("2-Nov-09","DM20Y") | date_ == date2("15-Nov-09","DM20Y") | 
    date_ == date2(" 20-Nov-09","DM20Y") | date_ == date2("24-Dec-09","DM20Y") | 
    date_ == date2("25-Dec-09","DM20Y") | date_ == date2("31-Dec-09","DM20Y") | 
    date_ == date2("1-Jan-11","DM20Y") | date_ == date2("24-Jan-11","DM20Y") | 
    date_ == date2("25-Jan-11","DM20Y") | date_ == date2("7-Mar-11","DM20Y") | 
    date_ == date2("8-Mar-11","DM20Y") | date_ == date2("9-Mar-11","DM20Y") | 
    date_ == date2("21-Apr-11","DM20Y") | date_ == date2("22-Apr-11","DM20Y") | 
    date_ == date2("1-May-11","DM20Y") | date_ == date2("23-Jun-11","DM20Y") | 
    date_ == date2("24-Jun-11","DM20Y") | date_ == date2("9-Jul-11","DM20Y") |
    date_ == date2("1-Jan-2012","DMY") | date_ == date2("25-Jan-2012","DMY") | 
    date_ == date2("20-Feb-2012","DMY") | date_ == date2("21-Feb-2012","DMY") | 
    date_ == date2("22-Feb-2012","DMY") | date_ == date2("6-Apr-2012","DMY") | 
    date_ == date2("21-Apr-2012","DMY") | date_ == date2("30-Apr-2012","DMY") | 
    date_ == date2("1-May-2012","DMY") | date_ == date2("7-Jun-2012","DMY") | 
    date_ == date2("8-Jun-2012","DMY") | date_ == date2("9-Jul-2012","DMY") | 
    date_ == date2("7-Sep-2012","DMY") | date_ == date2("12-Oct-2012","DMY") | 
    date_ == date2("2-Nov-2012","DMY") | date_ == date2("15-Nov-2012","DMY") |
    date_ == date2("16-Nov-2012","DMY") | date_ == date2("20-Nov-2012","DMY") | 
    date_ == date2("24-Dec-2012","DMY") | date_ == date2("25-Dec-2012","DMY") | 
    date_ == date2("31-Dec-2012","DMY") | date_ == date2("1-Jan-2013","DMY") | 
    date_ == date2("25-Jan-2013","DMY") | date_ == date2("11-Feb-2013","DMY") | 
    date_ == date2("12-Feb-2013","DMY") | date_ == date2("13-Feb-2013","DMY") | 
    date_ == date2("29-Mar-2013","DMY") | date_ == date2("21-Apr-2013","DMY") | 
    date_ == date2("1-May-2013","DMY") | date_ == date2("30-May-2013","DMY") | 
    date_ == date2("31-May-2013","DMY") | date_ == date2("1-Jan-10","DM20Y") | 
    date_ == date2("25-Jan-10","DM2 0Y") | date_ == date2("15-Feb-10","DM20Y") | 
    date_ == date2("16-Feb-10","DM20Y") | date_ == date2("17-Feb-10","DM20Y") | 
    date_ == date2("2-Apr-10","DM20Y") | date_ == date2("21-Apr-10","DM20Y") | 
    date_ == date2("1-May-10","DM20Y") | date_ == date2("3-Jun-10","DM20Y") | 
    date_ == date2("4-Jun-10","DM20Y") | date_ == date2("9-Jul-10","DM20Y") | 
    date_ == date2("6-Sep-10","DM20Y") | date_ == date2("7-Sep-10","DM20Y") | 
    date_ == date2("11-Oct-10","DM20Y") | date_ == date2("12-Oct-10","DM20Y") | 
    date_ == date2("1-Nov-10","DM20Y") | date_ == date2("2-Nov-10","DM20Y") |
    date_ == date2("15-Nov-10","DM20Y") | date_ == date2("20-Nov-10","DM20Y") | 
    date_ == date2("24-Dec-10","DM20Y") | date_ == date2("25-Dec-10","DM20Y") |
    date_ == date2("31-Dec-10","DM20Y") | date_ == date2("7-Sep-2011","DMY") | 
    date_ == date2("12-Oct-2011","DMY") | date_ == date2("2-Nov-2011","DMY") | 
    date_ == date2("14-Nov-2011","DMY") | date_ == date2("15-Nov-2011","DMY") | 
    date_ == date2("20-Nov-2011","DMY") | date_ == date2("24-Dec-2011","DMY") | 
    date_ == date2("25-Dec-2011","DMY") | date_ == date2("31-Dec-2011","DMY")
  , yes = 1, no = 0
)

all(pubhol == df_salvo$dv_publicholiday)
date_[!pubhol == df_salvo$dv_publicholiday]


vac <- ifelse(
  ((date_>= date2("24-Dec-08","DM20Y") & date_ <= date2("9-Jan-09","DM20Y")) | 
     (date_>= date2("24-Dec-09","DM20Y") & date_ <= date2("8-Jan-10","DM20Y")) |
     (date_>= date2("24-Dec-10","DM20Y") & date_ <= date2("7-Jan-11","DM20Y")) | 
     (date_>= date2("24-Dec-11","DM20Y") & date_ <= date2("6-Jan-12","DM20Y")) | 
     (date_>= date2("24-Dec-12","DM20Y") & date_ <= date2("4-Jan-13","DM20Y"))) &
    df_salvo$dv_publicholiday != 1, 1, 0
)

all(vac == df_salvo$dv_yearendvacation)
date_[!vac == df_salvo$dv_yearendvacation]


belt <- ifelse(date_ >= date2("31-Mar-10", 1), 1, 0)
all(belt == df_salvo$dv_beltway_open)
