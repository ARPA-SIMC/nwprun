#!/usr/bin/env python3
# encoding: utf-8

import os, sys, numpy as np, subprocess, glob
import pandas as pd

import matplotlib.pyplot as plt
from matplotlib import colors
from datetime import datetime,timedelta

from eps_utils import macro, get_args, soglia, estrai_prob_su_macroaree, heatmap

#------------------------------------------------------------------------
# Creazione delle scacchiere di probabilità sulle macroaree
# delle Regioni.
#------------------------------------------------------------------------

if __name__ == '__main__':

    args = get_args()
    
    aree = args.aree # default = Emilia-Romagna

    # Estraggo la sigla della Regione dal nome dello shapefile
    dumaree = os.path.basename( aree )
    regione = dumaree[len("macroaree_"):]
    regione = regione.split(".")[0] # rimuovo qualunque suffisso
    print( "Post-processing per la Regione {}".format(regione) )

    path_in = args.path_in # default = "/autofs/scratch-rad/vpoli/FCST_PROB/fxtr/data" 

    fold_out = args.fold_out # default = tmp
    # Creo la directory di output, "fold_out", se non esiste
    if not os.path.exists( "{}".format(fold_out) ):
        os.makedirs( "{}".format(fold_out) )
    
    if len(args.subtype) > 1:
        print("Per questo post-processing è previsto l'uso di un sub_type alla volta, esco")
        sys.exit(1)
    else:
        sub_type = args.subtype

    cumulate = args.cumulate # default = ['tpp01h', 'tpp03h']

    valore = 'prob'
    units = '[%]'
    
    thresh = []
    sf = []
    sv = []
    for i in range(len(cumulate)):
        tlist, sflist, svlist =  soglia( cumulate[i] )
        thresh.append( tlist )
        sf.append( sflist )
        sv.append( svlist ) 

    #print("tresh= ", thresh)
    #print("sf= ", sf)
    #print("sv= ", sv)

    # Definisco la soglia per il calcolo del percentile
    percent = 90

    # Calcolo dei campi di media, massimo e 90esimo percentile sulle macroaree
    # 16/02/2021 - Limitiamo il calcolo al max delle probabilità
    for subtype in sub_type:
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
                sys.exit( "\nERRORE! Questa cumulata non è gestita.\n" )

            # Indice dei valori di soglia per la cumulata selezionata
            n = cumulate.index(j)

            # Estrazione campi sulle macroaree usando le soglie opportune
            for fname in filelist:
                #print(fname)
                csvname = estrai_prob_su_macroaree(fname, valore, subtype, aree, regione,
                                                   thresh[n], sf[n], sv[n], percent ) 

        # Lettura dei csv prodotti per la generazione delle scacchiere                
        for j in cumulate:
            # Indice dei valori di soglia per la cumulata selezionata
            n = cumulate.index(j)

            val = [ [] for _ in range(len(thresh[n])) ]
            ind=0
            # Separo i file per soglia
            for i in thresh[n]:
                # Lista dei file per scadenza e soglia 
                search = "{}_{}_{}_*soglia{}_{}.csv".format( subtype, valore, j, str(i),
                                                             regione )
                lista = glob.glob(search)
            
                for f in lista:
                    #print(ind," - Leggo file: ",f)
                    df = pd.read_csv( f.strip(), delimiter=',',
                                      names = [ 'Date', 'Time range', 'P1', 'P2',
                                                'Longitude', 'Latitude', 'Level1',
                                                'L1', 'Level2', 'L2', 'Report',
                                                'B01192', 'B13011' ],
                                      skiprows = 0 )
                    val[ind].append(df)
                ind = ind + 1

            # Inizializzo alcune variabili necessarie
            # a generare l'immagine
            df   = [None] * len(thresh[n])
            dati = [None] * len(thresh[n])

            for k in range(len(thresh[n])):
                # Unisco fisicamente tutti i dati letti ordinandoli
                # per data
                df[k] = pd.concat(val[k]).sort_values( by=['Date'] )
        
                # Raggruppo i dati per macroaree, creando una lista
                # contenente tutte le scadenze
                dati[k] = df[k].groupby('B01192')['B13011'].apply(list)
                # Estraggo la data del run, la scadenza e l'intervallo
                # di cumulata dal primo dataframe poichè i valori sono
                # tutti uguali per le diverse soglie
                run  = df[0].groupby('B01192')['Date'].apply(list)
                lead = df[0].groupby('B01192')['P1'].apply(list)
                cum  = df[0].groupby('B01192')['P2'].apply(list)

            # Creo un unico dataframe contenente TUTTI i dati
            # necessari a generare la scacchiera: ogni riga
            # corrisponde ad una macroarea, ogni colonna ad
            # una soglia.
            # La soglia più bassa verrà visualizzata in basso
            # per le cumulate orarie/triorarie. 
            # Per vedere le colonne: alldata[i]
            # Per vedere le righe: alldata.iloc[i]
            if j != 'tpp24h':
                alldata = pd.DataFrame( zip(dati[4], dati[3], dati[2], dati[1], dati[0]) )
            else:
                alldata = pd.DataFrame( zip(dati[0], dati[1], dati[2], dati[3], dati[4]) )
            #print(alldata)

            # Definisco le etichette date/time
            x = lead.iloc[0]
       
            orafcst = []
            if j == 'tpp24h':
                for i in range(len(run.iloc[0])):
                    ora = datetime.strptime( run.iloc[0][i], '%Y-%m-%d %H:%M:%S' ).strftime( '%H' )
                    orafcst.append( datetime.strptime( run.iloc[0][i], '%Y-%m-%d %H:%M:%S' ).strftime( '%d/%m/%Y %H:%M' ) )
            else:
                for i in range(len(run.iloc[0])):
                    ora = datetime.strptime( run.iloc[0][i], '%Y-%m-%d %H:%M:%S' ).strftime( '%H' )
                    if ora == '00':
                        orafcst.append( datetime.strptime( run.iloc[0][i], '%Y-%m-%d %H:%M:%S' ).strftime( '%d/%m\n%H' ) )
                    else:
                        orafcst.append( datetime.strptime( run.iloc[0][i], '%Y-%m-%d %H:%M:%S' ).strftime( '%H' ) )
                # Se le cumulate sono orarie/triorarie, aggiungo l'ora di inizio del fcst
                orazero = datetime.strptime( run.iloc[0][0], '%Y-%m-%d %H:%M:%S' ) - timedelta( seconds=cum.iloc[0][0] )
                if orazero.strftime('%H') == '00':
                    orafcst.insert( 0, orazero.strftime( '%d/%m\n%H' ) )
                else:
                    orafcst.insert( 0, orazero.strftime( '%H' ) )        
        
            # Definisco date/time del run per il nome del file in output
            inizio = datetime.strptime( run.iloc[0][0], '%Y-%m-%d %H:%M:%S' ) - timedelta( seconds=lead.iloc[0][0] )
            fine = datetime.strptime( run.iloc[0][-1], '%Y-%m-%d %H:%M:%S' )
        
            # Definisco le etichette di superamento soglia
            y = []
            if j != 'tpp24h':
                for i in reversed( thresh[n] ):
                    y.append( 'TP>'+str(i)+'mm' )
            else:
                for i in thresh[n]:
                    y.append('TP>'+str(i)+'mm')
            #print(y)
        
            bounds = [ 0, 5, 10, 25, 50, 75, 90, 100 ]

            cmap = colors.ListedColormap( [ '#cccccc', '#fecc5c', '#41ab5d', '#006837',
                                            '#1e90ff', '#00008b', '#dd3497' ] )   
            norm = colors.BoundaryNorm( bounds, cmap.N )

            # Per cumulate su 3h un unico pannello.
            # Ogni macroarea è un subplot. 
            if j == 'tpp03h' or j == 'tpp01h':
                #fig, ax = plt.subplots(figsize=(14,20), len(macro[regione])) #ER
                
                # Scacchiera separata per macroarea
                for i in range(len(macro[regione])):
                    if j == 'tpp03h':
                        fig, ax = plt.subplots( figsize=(7,3) )
                    else:
                        fig, ax = plt.subplots( figsize=(15,4) ) 
                        
                    a = pd.DataFrame(alldata.iloc[i].to_list(), index=alldata.iloc[i].index)

                    im, cbar = heatmap(a, y, x, ax=ax, cmap=cmap, norm=norm, cbarlabel=units) #, aspect='auto')
                    ax.set_xticks(np.arange(-0.5,len(x)+0.5, 1))
                    ax.set_xticklabels(orafcst)
                    ax.set_yticklabels(y)
                    ax.grid(which="major", color="w", axis='x', linestyle='-', linewidth=2)

                    ax.set_title( "Macroarea {}".format( macro[regione][i]) )
                    fig.tight_layout( pad=1.8 )

                    # Titolo della figura    
                    if subtype == 'average':
                        tipo = 'Media'
                    elif subtype == 'max':
                        tipo = 'Massimo'
                    elif subtype == 'percentile':
                        tipo = '%sesimo percentile'%percent
                    fig.suptitle( "Corsa di COSMO-2I-EPS del {}\n" \
                                  "{} delle probabilità sulle macroaree".format( inizio.strftime( '%d/%m/%Y %H:%M' ), tipo ),
                                  y = 1.02 )    
                    if subtype == 'percentile':    
                        fileout = "{}/{}{}_{}_{}_{}.png".format( fold_out, subtype, str(percent),
                                                                 valore, j,
                                                                 inizio.strftime( '%Y%m%d%H' ) )
                    else:
                        # Creo il nome di output secondo la tassonomia di infomet.
                        # I campi "periodo di cumulazione" e "scadenza" vengono
                        # calcolati in automatico.  
                        fileout = "{}/MTG_FC_LENS_PR_0_TPPR_GRND_NULL_NULL_NULL_NULL_{}_{}_{}_{}_{}_scacchiera_{}_macro{}.png".format( fold_out, inizio.strftime('%Y%m%d%H'), fine.strftime('%Y%m%d%H'), f"{int((x[-1]-x[-2])/3600):03d}", f"{int((x[-1]-x[0]+cum.iloc[0][0])/3600):03d}", subtype, regione, macro[regione][i] )

                    fig.savefig( fileout, bbox_inches='tight' )
                    plt.close()

            else:
        
                fig, ax = plt.subplots( figsize=(14,6), nrows=1, ncols=2 )
                a0 = []
                a1 = []
                for i in range(len(thresh[n])):
                    dum = pd.DataFrame( alldata[i].to_list(), index=alldata[i].index )
                
                    a0.append(dum[0])
                    a1.append(dum[1])

                im, cbar = heatmap( pd.concat(a0,axis=1), macro[regione], y, ax=ax[0],
                                    cmap=cmap, norm=norm, aspect='auto',
                                    cbarlabel=units )
                ax[0].set_title( ( datetime.strptime( orafcst[0], '%d/%m/%Y %H:%M' ) - timedelta(days=1) ).strftime( '%d/%m/%Y' ) )
            
                im, cbar = heatmap( pd.concat(a1,axis=1), macro[regione], y, ax=ax[1],
                                    cmap=cmap, norm=norm, aspect='auto',
                                    cbarlabel=units ) 
                ax[1].set_title( ( datetime.strptime( orafcst[1], '%d/%m/%Y %H:%M' ) - timedelta(days=1) ).strftime( '%d/%m/%Y' ) )
        
                # Titolo della figura    
                if subtype == 'average':
                    tipo = 'Media'
                elif subtype == 'max':
                    tipo = 'Massimo'
                elif subtype == 'percentile':
                    tipo = '%sesimo percentile'%percent
                fig.suptitle( "Corsa di COSMO-2I-EPS del {}\n" \
                              "{} delle probabilità sulle macroaree".format( inizio.strftime( '%d/%m/%Y %H:%M' ), tipo ),
                              y = 1.02 )    
                if subtype == 'percentile':    
                    fileout = "{}/{}{}_{}_{}_{}.png".format( fold_out, subtype, str(percent),
                                                             valore, j,
                                                             inizio.strftime( '%Y%m%d%H' ) )
                else:
                    # Creo il nome di output secondo la tassonomia di infomet.
                    # I campi "periodo di cumulazione" e "scadenza" vengono
                    # calcolati in automatico.  
                    fileout = "{}/MTG_FC_LENS_PR_0_TPPR_GRND_NULL_NULL_NULL_NULL_{}_{}_{}_{}_{}_scacchiera_{}.png".format( fold_out, inizio.strftime('%Y%m%d%H'), fine.strftime('%Y%m%d%H'), f"{int((x[-1]-x[-2])/3600):03d}", f"{int((x[-1]-x[0]+cum.iloc[0][0])/3600):03d}", subtype, regione)

                fig.savefig( fileout, bbox_inches='tight' )
                plt.close()

        # Elimino file csv        
        tobedeleted = glob.glob( "{}_{}*{}.csv".format( subtype, valore, regione ) )
        for f in tobedeleted:
            os.remove(f)

quit()
