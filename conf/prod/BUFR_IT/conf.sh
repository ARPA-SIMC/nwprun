BUFR_IT_WORKDIR=$WORKDIR/bufr_it
case "$HPC_SYSTEM" in
  *.metarpa | maial* ) # Arpae
	FTPDIR="BUFR/"
	OBSTYPES="AIRC AMDA AMDN B002 B004 OCEA PILO SHIP SYNN SYNO TEMP"
    ;;
  lami* | galileo | meucci ) # Cineca cloud
	FTPDIR=""
	OBSTYPES="AIRC AMDA AMDN B002 B004 OCEA PILO SHIP SYNN SYNO TEMP"
    ;;
esac
#obstypes="AIRC AMDA AMDN B002 B004 BUON OCEA PILN PILO RAOB SHIN SHIP SYNN SYNO TEMP"

# quanti secondi aspetto un determinato file prima di passare al
# successivo
#NWPWAITELAPS=1800
# quanti secondi aspetto dopo il tempo nominale per cominciare a
# cercare un certo dato
NWPWAITSOLAR_RUN=3600
# quanti secondi aspetto dopo il tempo nominale per rinunciare a
# cercare un certo dato e passare al successivo
NWPWAITSOLAR=10800
# durata di ogni ciclo di attesa
NWPWAITWAIT=60
# intervallo di disponibilita` dei dati in s
BUFR_IT_STEP=3

