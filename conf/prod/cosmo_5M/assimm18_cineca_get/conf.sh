MODEL_SIGNAL=cosmo_5M_assimm18
ARKI_DLDIR=$WORKDIR_BASE/download
if [ -n "$CINECA_MAIN_SYSTEM" ]; then
    ARKI_SYNCDIR=$WORKDIR_BASE/import/sync_lami
fi
# modificare
LAMI_CINECA_WORKDIR=$WORKDIR_BASE/download/work/$MODEL_SIGNAL
POSTPROC_LIST=(lami_make_itr lami_make_medl lami_make_vprof)

# $TIME deve essere protetto perche' non e' ancora noto, lo sviluppo con eval
CINECA_SUITEDIR_TMPL=$CINECA_RUN/'${TIME:0:2}/lm5/ope/analysis_m18h'

# quanti secondi aspetto un determinato file prima di passare al
# successivo
#NWPWAITELAPS=1800
# quanti secondi aspetto dopo il tempo nominale per cominciare a
# cercare un certo dato
NWPWAITSOLAR_RUN=7200
# quanti secondi aspetto dopo il tempo nominale per rinunciare a
# cercare un certo dato e passare al successivo
# questo dovrebbe essere consistente con NWPWAITSOLAR e MODEL_DELTABD
# impostati in cosmo_2I/enda/conf.sh
NWPWAITSOLAR=57600
# durata di ogni ciclo di attesa
NWPWAITWAIT=60

# caratteristiche della suite
MODEL_FREQINI=12
MODEL_BACK=18
MODEL_DELTABD=0
MODEL_STOP=6
