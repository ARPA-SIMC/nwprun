ARKI_DLDIR=$WORKDIR_BASE/download
# quanti secondi aspetto un determinato file prima di passare al
# successivo (accordare con NWPWAITSOLAR)
NWPWAITELAPS=36000
# quanti secondi aspetto dopo il tempo nominale per cominciare a
# cercare un certo dato
NWPWAITSOLAR_RUN=5400
# quanti secondi aspetto dopo il tempo nominale per rinunciare a
# cercare un certo dato e passare al successivo
# questo dovrebbe essere consistente con NWPWAITSOLAR e MODEL_DELTABD
# impostati in cosmo_2I/enda/conf.sh
NWPWAITSOLAR=57600
# durata di ogni ciclo di attesa
NWPWAITWAIT=60

# caratteristiche della suite
MODEL_FREQINI=24
MODEL_BACK=0
MODEL_DELTABD=6
MODEL_STOP=72
