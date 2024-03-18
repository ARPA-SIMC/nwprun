#!/usr/bin/env python3
# encoding: utf-8

import os, sys, numpy as np, subprocess, glob
import pandas as pd

import matplotlib.pyplot as plt
from matplotlib import colors
from datetime import datetime,timedelta

from eps_utils import macro, get_args, estrai_campi_su_macroaree, heatmap

#------------------------------------------------------------------------
# Creazione delle scacchiere contenenti i campi medi e massimi della
# media dell'ensemble sulle macroaree delle Regioni.
#------------------------------------------------------------------------

if __name__ == '__main__':

    args = get_args()
    
    aree = args.aree # default = Emilia-Romagna

    # Estraggo la sigla della Regione dal nome dello shapefile
    dumaree = os.path.basename( aree )
    regione = dumaree[ len("macroaree_"):-len(".shp") ]
    print( "Post-processing per la Regione {}".format(regione) )

    path_in = args.path_in # default = "/autofs/scratch-rad/vpoli/FCST_PROB/fxtr/data" 

    fold_out = args.fold_out # default = tmp
    # Creo la directory di output, "fold_out", se non esiste
    if not os.path.exists( "{}".format(fold_out) ):
        os.makedirs( "{}".format(fold_out) )
    
    sub_type = [ 'average', 'max' ]
    
    cumulate = args.cumulate # default = ['tpp01h', 'tpp03h']
        
    valore = 'ensmean'
    units = '[mm]'

    # Calcolo dei campi di media e massimo sulle macroaree
    for subtype in sub_type: #[ 'average', 'max' ]:
        for j in cumulate:
            # Lista dei file per scadenza 
            search = "{}/{}_??????????????.grib".format(path_in, j)
            lista = glob.glob(search)
            
            if j == 'tpp01h':
                filelist = sorted(lista)[3:len(lista)]
            elif j == 'tpp03h':
                filelist = sorted(lista)[1:len(lista)]
            elif j == 'tpp24h':
                filelist = sorted(lista)
            else:
                sys.exit( "\nERRORE! Questa cumulata non è gestita\n" )

            # Estrazione campi sulle macroaree usando le soglie opportune
            for fname in filelist:
                #print(fname)
                csvname = estrai_campi_su_macroaree( fname, valore, subtype, aree, regione )

    # Lettura dei csv prodotti per la generazione delle scacchiere.
    # Per gestire il secondo giorno per tpp01h ed ottenere due figure
    # differenti, devo dividere i dati in 2 parti: divido in 2 la
    # lista dei file e rinomino il campo delle cumulate (da tpp01h a
    # tpp01hday2); contestualmente l'aggiungo all'array cumulate
    # perchè venga analizzato e plottato.
    if 'tpp01h' in cumulate:
        for subtype in sub_type: #['average', 'max']:
            search = "{}_{}_tpp01h_*{}.csv".format( subtype, valore, regione )
            lista = sorted( glob.glob(search) )
            day2 = lista[len(lista)//2:]
            for i in day2:
                os.rename( i, i.replace( 'tpp01h', 'tpp01hday2' ) )

        cumulate.append('tpp01hday2')

    for j in cumulate:
        # Preparo le figure
        n = 0
        if j != 'tpp24h':
#            fig, ax = plt.subplots(2, figsize=(20, 8))
            fig, ax = plt.subplots(2, figsize=(9, 8))
            fontsize = 10
        else:
#            fig, ax = plt.subplots(figsize=(12, 6), nrows=1, ncols=2) #orig
            fig, ax = plt.subplots(figsize=(7, 4), nrows=1, ncols=2) 
            fontsize = 10
        
        for subtype in sub_type: #['average', 'max']:

            val  = []
            run  = []
            lead = []
            cum  = []

            # Lista dei file da leggere
            search = "{}_{}_{}_*{}.csv".format( subtype, valore, j, regione )
            lista = glob.glob(search)
            
            for f in sorted(lista):
                #print("Leggo file: ",f)
                df = pd.read_csv( f.strip(), delimiter=',',
                                  names = ['Date', 'Time range', 'P1', 'P2',
                                           'Longitude', 'Latitude', 'Level1',
                                           'L1', 'Level2', 'L2', 'Report',
                                           'B01192', 'B13011'],
                                  skiprows = 0 )
                val.append(df['B13011'])
                run.append(df['Date'])
                lead.append(df['P1'])
                cum.append(df['P2'])
            
            df = pd.concat(val,axis=1)
            data = pd.concat(run,axis=1)
            scad = pd.concat(lead,axis=1)
            icum = pd.concat(cum,axis=1)

            # Trasformo date, scadenze e cumulate in liste
            dum = data.iloc[0].tolist()
            run = [s for s in dum]
            dum = scad.iloc[0].astype(int).tolist()
            x = [s for s in dum]
            dum = icum.iloc[0].astype(int).tolist()
            dt = [s for s in dum]

            orafcst=[]
            for i in range(len(run)):
                ora=datetime.strptime(run[i],'%Y-%m-%d %H:%M:%S').strftime('%H')
                if ora=='00':
                    orafcst.append(datetime.strptime(run[i],'%Y-%m-%d %H:%M:%S').strftime('%d/%m\n%H'))
                else:
                    orafcst.append(datetime.strptime(run[i],'%Y-%m-%d %H:%M:%S').strftime('%H'))
            # Aggiungo l'ora di inizio del fcst
            orazero=datetime.strptime(run[0],'%Y-%m-%d %H:%M:%S')-timedelta(seconds=dt[0])
            if orazero.strftime('%H')=='00':
                orafcst.insert(0,orazero.strftime('%d/%m\n%H'))
            else:
                orafcst.insert(0,orazero.strftime('%H'))
            #print(orafcst)

            # Definisco date/time del run (emissione/validità) per il nome del file in output
            inizio = datetime.strptime( run[0], '%Y-%m-%d %H:%M:%S' ) - timedelta( seconds=x[0] )
            fine = datetime.strptime( run[-1], '%Y-%m-%d %H:%M:%S' )
        
            bounds = [ 0, 1, 5, 10, 20, 30, 50, 70, 100, 150, 200, 300, 500 ]
            # orig
            #cmap = colors.ListedColormap( ['#f0f0f0', '#d0d1e6', '#a6bddb',
            #                               '#74a9cf', '#2b8cbe', '#045a8d',
            #                               '#fecc5c', '#fd8d3c', '#e31a1c',
            #                               '#dd3497', '#ae017e', '#7a0177'])
            # NUOVA - colori infomet
            cmap = colors.ListedColormap( ['#f0f0f0', '#00ffff', '#00cdcd',
                                           '#1e90ff', '#0000cd', '#00008b',
                                           '#fecc5c', '#fd8d3c', '#e31a1c',
                                           '#dd3497', '#ae017e', '#7a0177'] )

            norm = colors.BoundaryNorm(bounds, cmap.N)

            if j!='tpp24h':
                im, cbar = heatmap( df, macro[regione], x, ax=ax[n], cmap=cmap, norm=norm,
                                    cbarlabel=units, aspect='auto', cbar_kw=dict(ticks=bounds) )
            else:
                im, cbar = heatmap( df, macro[regione], x, ax=ax[n], cmap=cmap, norm=norm,
                                    cbarlabel=units, aspect='auto',
                                    cbar_kw=dict(ticks=bounds) )
            
            ax[n].set_xticks( np.arange(-0.5, len(x)+0.5, 1) )
            ax[n].set_xticklabels( orafcst, fontsize=fontsize )
            ax[n].grid( which="major", color="w", axis='x', linestyle='-', linewidth=2 )
            if subtype == "average":
                ax[n].set_title( "Precipitazione media della media dell'ensemble",
                                 fontsize=12 )
            elif subtype == "max":
                ax[n].set_title( "Precipitazione massima della media dell'ensemble",
                                 fontsize=12 )
            fig.tight_layout( pad=1.0 )
            n = n+1
                        
        fig.suptitle( "Corsa di COSMO-2I-EPS del {}\n" \
                      .format(inizio.strftime('%d/%m/%Y %H:%M')), y=1.02 )
        # Creo il nome di output secondo la tassonomia di infomet.
        # I campi "periodo di cumulazione" e "scadenza" vengono
        # calcolati in automatico.  
        fileout = "{}/MTG_FC_LENS_PR_0_TPEM_GRND_NULL_NULL_NULL_NULL_{}_{}_{}_{}_scacchiera_{}.png".format( fold_out, inizio.strftime('%Y%m%d%H'), fine.strftime('%Y%m%d%H'), f"{int((x[-1]-x[-2])/3600):03d}", f"{int((x[-1]-x[0]+dt[0])/3600):03d}", regione )
        fig.savefig( fileout, bbox_inches='tight' )
        plt.close()

    # Elimino i file csv
    tobedeleted = glob.glob( "*_{}*.csv".format(valore) )
    for f in tobedeleted:
        os.remove(f)

quit()
