FTPDIR=bceps

COSMO_AM_ENDA_WORKDIR=$WORKDIR/cosmo_am_enda
COSMO_AM_ENDA_G2_WORKDIR=$WORKDIR/cosmo_am_enda_g2
#ARKI_SYNCDIR=$WORKDIR_BASE/import/sync_galileo

# quanti secondi aspetto un determinato file prima di passare al
# successivo
#NWPWAITELAPS=1800
# quanti secondi aspetto dopo il tempo nominale per cominciare a
# cercare un certo dato
NWPWAITSOLAR_RUN=12600
# quanti secondi aspetto dopo il tempo nominale per rinunciare a
# cercare un certo dato e passare al successivo
# questo dovrebbe essere consistente con NWPWAITSOLAR e MODEL_DELTABD
# impostati in cosmo_2I/enda/conf.sh
NWPWAITSOLAR=57600
# durata di ogni ciclo di attesa
NWPWAITWAIT=60
# intervallo di disponibilita` dei dati in h
COSMO_AM_ENDA_STEP=3

