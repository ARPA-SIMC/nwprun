case "$HPC_SYSTEM" in
  *.metarpa | maial* ) # Arpae
	FTPDIR=""
    ;;
  lami* | galileo | meucci | g100 ) # Cineca cloud
	FTPDIR=""
    ;;
esac

IMPORT_THREAD=input_obs
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
# intervallo di disponibilita` dei dati in s
PROC_STEP=3600

