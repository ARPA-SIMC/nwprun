#!/usr/bin/env python3
# encoding: utf-8

import os, sys, numpy as np, subprocess, glob
import pandas as pd

import matplotlib.pyplot as plt
from matplotlib import colors
from datetime import datetime,timedelta

from eps_utils import macro, get_args, heatmap


def cumula_membri(nmemb, path_in):
    for i in range(1, nmemb+1):
        dir_in = "{}/cosmo.{}/data/lfff????0000".format( path_in, str(i) )
        #dir_in = "{}/2021100321.{}/lfff????0000".format( path_in, f"{i:03d}" )

        for f in glob.glob(dir_in):
            # Estraggo i campi di TP
            grib_copy = "grib_copy -w shortName=tp {} tp_{}.grib".format(f, os.path.basename(f))
            subprocess.call(grib_copy.split(),shell=False)
        # Unisco i file e calcolo le cumulate
        os.system("cat tp_*.grib > tp.grib")

        tobedeleted = glob.glob( "tp_lfff*" )
        for f in tobedeleted:
            os.remove(f)

        grib_cum = "vg6d_transform --comp-stat-proc=1 --comp-step='00 03' --comp-full-steps " \
            "tp.grib tp3h_membro{}.grib".format(str(i),)
        os.system( grib_cum )
        subprocess.call( [ "rm", "tp.grib" ] )


#------------------------------------------------------------------------
# Creazione della scacchiera di probabilità di superamento delle soglie
# delle precipitazione media(massima) sulle macroaree.
#------------------------------------------------------------------------
# - La precipitazione considerata è cumulata su 3h (ne caso si vogliano
#   cumulate diverse, va modificato il programma).
# - Il programma prevede un ensemble di 20 membri.
#------------------------------------------------------------------------
if __name__ == '__main__':

    nmemb = 20
    args = get_args()
    path_in = args.path_in

    if "S" in args.operations:
        aree = args.aree
        # Estraggo la sigla della Regione dal nome dello shapefile
        dumaree = os.path.basename( aree )
        regione = dumaree[len("macroaree_"):]
        regione = regione.split(".")[0] # rimuovo qualunque suffisso
        print( "Post-processing per la Regione {}".format(regione) )

        fold_out = args.fold_out
        # Creo la directory di output, "fold_out", se non esiste
        if not os.path.exists( "{}".format(fold_out) ):
            os.makedirs( "{}".format(fold_out) )

        if len(args.subtype) > 1:
            print("Per questo post-processing è previsto l'uso di un sub_type alla volta, esco")
            sys.exit(1)
        else:
            sub_type = args.subtype[0]
            if sub_type == 'average':
                thresh = [ 1, 2, 5, 10, 20 ]
            elif sub_type == 'max':
                thresh = [ 10, 20, 30, 50, 70 ]
            else:
                print("Sub_type non definito, esco")
                sys.exit(2)

        units = '[%]'

    # Estrazione di TP trioraria per ciascun membro e calcolo della media
    # sulle macroaree 
    if "C" in args.operations:
        tobedeleted = glob.glob("tp3h_membro*.grib")
        for f in tobedeleted:
            os.remove(f)
        cumula_membri(nmemb, path_in)
    if not "S" in args.operations: quit()
    
    val = []
    for i in range(1, nmemb+1):
        # per velocizzare la procedura, aree puo' ossere un file grib
        # generato una-tantum ad hoc a partire da un singolo grib di esempio e
        # dallo shapefile desiderato con il comando:
        # vg6d_transform --trans-type=maskgen --sub-type=poly
        #  --coord-format=shp --coord-file=aree.shp
        #  esempio.grib aree.grib
        if aree.endswith( ("grib", "grib1", "grib2", "grb", "grb1", "grb2") ):
            c_format = "grib_api"
            trans_type = "maskinter"
        else:
            c_format = "shp"
            trans_type = "polyinter"

        vg6d_getpoint = "vg6d_getpoint --coord-file={} " \
            "--coord-format={} --trans-type={} --sub-type={} " \
            "--output-format=native tp3h_membro{}.grib pre.v7d".format( aree, c_format, trans_type, sub_type, str(i))
        subprocess.call(vg6d_getpoint.split(),shell=False)

        csvname = "tp3h_membro{}_{}.csv".format( str(i), regione )
        v7d_trans = "v7d_transform --input-format=native --output-format=csv --csv-header=0 " \
	    "pre.v7d {}".format( csvname )
        subprocess.call(v7d_trans.split(), shell=False)

        # Elimino il file dati non necessari
        subprocess.call( ["rm", "pre.v7d"] )

        csvname = "tp3h_membro{}_{}.csv".format( str(i), regione ) #da cancellare
        val.append( pd.read_csv( csvname, delimiter=',',
                                 names=[ 'Date', 'Time range', 'P1', 'P2',
                                         'Longitude', 'Latitude', 'Level1',
                                         'L1', 'Level2', 'L2', 'Report',
                                         'B01192', 'B13011' ],
                                 skiprows=0) )

    df = pd.concat(val).sort_values(by=['Date', 'B01192'])
    pd.set_option( 'display.max_rows', df.shape[0]+1 )

    # Aggiungo colonne al dataframe contenenti i superamenti di soglia
    y=[]     # Etichette superamento soglia
    lista=[] # Intestazione colonne aggiuntive
    for i in reversed(thresh):
        df[ "Soglia{}".format( str(i) ) ] = np.where( (df["B13011"]>=i), 1, 0 )
        lista.append( "Soglia{}".format( str(i) ) )
        y.append( 'TP>'+str(i)+'mm' )
    
    #Calcolo la media sull'ens del superamento di soglia
    dati = df.groupby( ['Date', 'B01192'], as_index=False )[lista].agg(np.mean) 

    # Definisco le etichette con le ore dei forecast
    orafcst = []
    ore = dati['Date'].unique()
    for i in ore:
        ora = datetime.strptime( i, '%Y-%m-%d %H:%M:%S' ).strftime( '%H' )
        if ora == '00':
            orafcst.append( datetime.strptime( i, '%Y-%m-%d %H:%M:%S' ).strftime( '%d/%m\n%H' ) )
        else:
            orafcst.append( datetime.strptime( i, '%Y-%m-%d %H:%M:%S' ).strftime( '%H' ) )
    # Aggiungo l'ora di inizio del fcst
    orazero = datetime.strptime( ore[0], '%Y-%m-%d %H:%M:%S' ) - timedelta( hours=3 )
    if orazero.strftime( '%H' ) == '00':
        orafcst.insert( 0, orazero.strftime( '%d/%m\n%H' ) )
    else:
        orafcst.insert( 0, orazero.strftime( '%H' ) )        
    #print(orafcst)

    # Definisco date/time del run per il nome del file in output
    inizio = datetime.strptime( ore[0], '%Y-%m-%d %H:%M:%S' ) - timedelta(hours=3)
    fine = datetime.strptime( ore[-1], '%Y-%m-%d %H:%M:%S' )

    bounds = [ 0, 5, 10, 25, 50, 75, 90, 100 ]
    #orig
    #cmap=colors.ListedColormap(['#cccccc','#fee08b',
    #                            '#d9ef8b','#67a9cf',
    #                            '#2166ac','#f768a1'])
    cmap = colors.ListedColormap( [ '#cccccc', '#fecc5c', '#41ab5d', '#006837',
                                    '#1e90ff', '#00008b', '#dd3497' ] )
    
    norm = colors.BoundaryNorm(bounds, cmap.N)
    
    """
    #-----------------------------------------
    # Immagine contenente tutte le scacchiere
    #-----------------------------------------
    fig, ax = plt.subplots( len(macro[regione]), figsize=(70, 20), dpi=50 ) # ORIG - figsize=(70, 20)
   
    # Scacchiera separata per macroarea
    for i in range(len(macro[regione])):
        # Seleziono la macroarea
        area = dati[ dati['B01192'] == (i+1) ]
        #print(area)
        a = area[ area.columns[-(len(thresh)):] ] * 100
        #print(a[1:])
    
        im, cbar = heatmap( a[1:].T, y, ore[1:], ax=ax[i], cmap=cmap, norm=norm, cbarlabel=units )
        ax[i].set_xticks( np.arange( -0.5, len(ore[1:])+0.5, 1 ) )
        ax[i].set_xticklabels( orafcst[1:] )
        ax[i].set_yticklabels( y )
        ax[i].grid( which="major", color="w", axis='x', linestyle='-', linewidth=2 )
        
        ax[i].set_title( "Macroarea {}".format( macro[regione][i]) )
        fig.tight_layout( pad=1.8 )


    # Titolo della figura    
    if sub_type == 'average':
        tipo = 'media'
    elif sub_type == 'max':
        tipo = 'massima'
    fig.suptitle( "Corsa di COSMO-2I-EPS del {}\n" \
                  "Probabilità di superamento soglie della precipitazione {} sulle macroaree "
                  .format( inizio.strftime('%d/%m/%Y %H:%M'), tipo ),
                  y=1.02 )    

    # Creo il nome di output secondo la tassonomia di infomet.
    # I campi "periodo di cumulazione" e "scadenza" sono fissati.
    fileout = "{}/MTG_FC_LENS_PR_0_TPPC_GRND_NULL_NULL_NULL_NULL_{}_{}_003_048_scacchiera_{}.png".format(fold_out, inizio.strftime('%Y%m%d%H'), fine.strftime('%Y%m%d%H'), regione )

    fig.savefig( fileout, bbox_inches='tight' )
    plt.close()
    """
    
    #--------------------------------------------------
    # Immagine contenente una scacchiera per macroarea
    #--------------------------------------------------   
    for i in range(len(macro[regione])):
        #fig = plt.subplot( 1, figsize=(4, 10) ) # ORIG - figsize=(70, 20)
        fig = plt.figure( figsize=(7, 3) )
        ax = fig.add_subplot(111) # ORIG - figsize=(70, 20)
        # Seleziono la macroarea
        area = dati[ dati['B01192'] == (i+1) ]

        a = area[ area.columns[-(len(thresh)):] ] * 100
    
        im, cbar = heatmap( a[1:].T, y, ore[1:], ax=ax, cmap=cmap, norm=norm, cbarlabel=units )
        ax.set_xticks( np.arange( -0.5, len(ore[1:])+0.5, 1 ) )
        ax.set_xticklabels( orafcst[1:] )
        ax.set_yticklabels( y )
        ax.grid( which="major", color="w", axis='x', linestyle='-', linewidth=2 )
        
        ax.set_title( "Macroarea {}".format( macro[regione][i]) )
        #fig.tight_layout( pad=1.8 )
        fig.tight_layout( )

        # Titolo della figura    
        if sub_type == 'average':
            tipo = 'media'
        elif sub_type == 'max':
            tipo = 'massima'
        fig.suptitle( "Corsa di COSMO-2I-EPS del {}\n" \
                      "Probabilità di superamento soglie della precipitazione {} sulle macroaree "
                      .format( inizio.strftime('%d/%m/%Y %H:%M'), tipo ),
                      y=1.02 )    

        # Creo il nome di output secondo la tassonomia di infomet.
        # I campi "periodo di cumulazione" e "scadenza" sono fissati.
        fileout = "{}/MTG_FC_LENS_PR_0_TPPC_GRND_NULL_NULL_NULL_NULL_{}_{}_003_048_scacchiera_{}_macro{}.png".format(fold_out, inizio.strftime('%Y%m%d%H'), fine.strftime('%Y%m%d%H'), regione, macro[regione][i] )

        fig.savefig( fileout, bbox_inches='tight' )
        plt.close()
    
    
    tobedeleted=glob.glob( "tp3h_membro*{}.csv".format( regione ) )
    for f in tobedeleted:
        os.remove(f)

    quit()
