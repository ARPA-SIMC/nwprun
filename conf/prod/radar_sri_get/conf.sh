# better way?
case "$HPC_SYSTEM" in
  *.metarpa | maial* | nodo* ) # Arpae
    TEMPLATE_NAME=cosmo_5I_radar.grib
    ;;
  g100 ) # Cineca HPC galileo 100
    TEMPLATE_NAME=cosmo_2I_radar.grib
    ;;
esac

#SRI_URL=https://radar.protezionecivile.it/wide-api/wide/product/downloadProduct
SRI_URL=https://radar-api.protezionecivile.it/wide/product/downloadProduct

# quanti secondi aspetto un determinato file prima di passare al
# successivo
#NWPWAITELAPS=1800
# quanti secondi aspetto dopo il tempo nominale per cominciare a
# cercare un certo dato
NWPWAITSOLAR_RUN=1200
# quanti secondi aspetto dopo il tempo nominale per rinunciare a
# cercare un certo dato e passare al successivo
NWPWAITSOLAR=2400
# durata di ogni ciclo di attesa
NWPWAITWAIT=30
# intervallo di disponibilita` dei dati in s
PROC_STEP=600
# resta constantemente attivo
DAEMON=Y

