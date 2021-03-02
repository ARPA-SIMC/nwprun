GTS_BUFR_WORKDIR=$WORKDIR/gts_bufr
case "$HPC_SYSTEM" in
  *.metarpa | maial* ) # Arpae
	FTPDIR="BUFR/"
    ;;
  lami* | galileo | meucci ) # Cineca cloud
	FTPDIR=""
    ;;
esac

# quanti secondi aspetto un determinato file prima di passare al
# successivo
#NWPWAITELAPS=1800
# quanti secondi aspetto dopo il tempo nominale per cominciare a
# cercare un certo dato
NWPWAITSOLAR_RUN=1980 # 1/2h+3'
# quanti secondi aspetto dopo il tempo nominale per rinunciare a
# cercare un certo dato e passare al successivo
NWPWAITSOLAR=3600
# durata di ogni ciclo di attesa
NWPWAITWAIT=60
# intervallo di disponibilita` dei dati in h
GTS_BUFR_STEP=1

