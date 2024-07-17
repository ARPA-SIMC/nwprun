FTPDIR=/PRD/SRI
TEMPLATE_LIST=(icon_2I_radar.grib)

# quanti secondi aspetto un determinato file prima di passare al
# successivo
#NWPWAITELAPS=1800
# quanti secondi aspetto dopo il tempo nominale per cominciare a
# cercare un certo dato
NWPWAITSOLAR_RUN=300
# quanti secondi aspetto dopo il tempo nominale per rinunciare a
# cercare un certo dato e passare al successivo
NWPWAITSOLAR=2400
# durata di ogni ciclo di attesa
NWPWAITWAIT=30
# intervallo di disponibilita` dei dati in s
PROC_STEP=600
# resta constantemente attivo
DAEMON=Y

