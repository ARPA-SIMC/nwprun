BASEURL=https://opendata.dwd.de/weather/nwp/icon-eu/grib
# setup for remote import
IMPORT_THREAD=input_bc
#unset ARKI_IMPDIR
ARKI_DLDIR=$WORKDIR_BASE/download

# quanti secondi aspetto un determinato file prima di passare al
# successivo
#NWPWAITELAPS=1800
# quanti secondi aspetto dopo il tempo nominale per cominciare a
# cercare un certo dato
NWPWAITSOLAR_RUN=10800
# quanti secondi aspetto dopo il tempo nominale per rinunciare a
# cercare un certo dato e passare al successivo
# abbassare considerando che lo scarico ogni 3h? d'altra parte serve ogni 24
NWPWAITSOLAR=36000
# durata di ogni ciclo di attesa
NWPWAITWAIT=60
# intervallo di disponibilita` dei dati in s
PROC_STEP=10800

