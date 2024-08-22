MODEL_SIGNAL=cosmo_2I
ARKI_DLDIR=$WORKDIR_BASE/download
if [ -n "$CINECA_MAIN_SYSTEM" ]; then
    ARKI_SYNCDIR=$WORKDIR_BASE/import/sync.lami
fi
unset ARKI_IMPDIR
# modificare
LAMI_CINECA_WORKDIR=$WORKDIR_BASE/download/work/$MODEL_SIGNAL
CROSS_COORD_FILE=$WORKDIR_BASE/nwprun/conf/cross.shp
CROSS_NETWORK=cosmo_2I_fcast_c
VPROF_NETWORK=cosmo_2I_fcast_v
POSTPROC_LIST=(lami_make_nit lami_make_vprof lami_make_cross)


# $TIME deve essere protetto perche' non e' ancora noto, lo sviluppo con eval
CINECA_SUITEDIR_TMPL=$CINECA_RUN/'${TIME:0:2}/lm2.2/ope/forecast'

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

# caratteristiche della suite
MODEL_FREQINI=12
MODEL_BACK=0
MODEL_DELTABD=0
MODEL_STOP=48
