#!/usr/bin/env python3
# encoding: utf-8
import os, sys, numpy as np, subprocess, glob
import pandas as pd

import matplotlib as mpl
import matplotlib.pyplot as plt
from matplotlib import colors
from mpl_toolkits.axes_grid1 import make_axes_locatable
from datetime import datetime,timedelta

def soglia(sf,sv):
    if sf>0:
        thresh=sv/(int('1'.ljust(abs(sf),'0'))*10)
    elif sf==0:
        thresh=sv
    else:
        thresh=sv*int('1'.ljust(abs(sf),'0'))*10
    return(thresh)

def estrai_campi_su_macroaree(fname,valore,sub_type,sf,sv,percent,aree):
    prodotto=os.path.basename(fname).split('.',1)[0]    
    if sf>0:
        thresh=sv/(int('1'.ljust(abs(sf),'0'))*10)
    elif sf==0:
        thresh=sv
    else:
        thresh=sv*int('1'.ljust(abs(sf),'0'))*10
    csvname="%s_%s_%s_soglia%s.csv"%(sub_type,valore,prodotto,str(thresh))
    
    grib_copy="grib_copy -w productDefinitionTemplateNumber=9,scaleFactorOfLowerLimit=%s,scaledValueOfLowerLimit=%s " \
        "%s campo.grib"%(sf,sv,fname)
    subprocess.call(grib_copy.split(),shell=False)

    vg6d_getpoint="vg6d_getpoint --coord-file=%s " \
        "--coord-format=shp --trans-type=polyinter --sub-type=%s " \
        "--percentile=%d --output-format=native campo.grib pre.v7d"%(aree,sub_type,percent)
    subprocess.call(vg6d_getpoint.split(),shell=False)
                
    v7d_trans="v7d_transform --input-format=native --output-format=csv --csv-header=0 " \
	"pre.v7d %s"%(csvname)
    subprocess.call(v7d_trans.split(),shell=False)

    # Elimino il file dati.v7d
    subprocess.call(["rm","pre.v7d"])
    subprocess.call(["rm","campo.grib"])
    return csvname                    

    
# Create a heatmap from a numpy array and two lists of labels.
def heatmap(data, row_labels, col_labels, gf_cbar=False, ax=None,
            cbar_kw={}, cbarlabel="", **kwargs):
    # -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -
    # Parameters:
    # data       = A 2D numpy array of shape (N, M).
    # gf_cbar    = create a plot with the correct colorbar to be used in the original plot
    # row_labels = A list or array of length N with the labels for the rows.
    # col_labels = A list or array of length M with the labels for the columns.
    # ax         = A `matplotlib.axes.Axes` instance to which the heatmap is plotted.
    #              If not provided, use current axes or create a new one.       Optional
    # cbar_kw    = A dictionary with arguments to `matplotlib.Figure.colorbar`. Optional
    # cbarlabel  = The label for the colorbar.                                  Optional
    # **kwargs   = All other arguments are forwarded to `imshow`.
    # -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -
    if not ax:
        ax = plt.gca()

    # Plot the heatmap or the "fake plot" from which the colorbar has to be taken
    if gf_cbar:
        im = ax.contourf(data, hatches=hatch_list, **kwargs)
    else:
        im = ax.imshow(data, **kwargs)

    # create an axes on the right side of ax. The width of cax will be 5%
    # of ax and the padding between cax and ax will be fixed at 0.05 inch.
    divider = make_axes_locatable(ax)
    cax = divider.append_axes("right", size="5%", pad=0.25)

    # Create colorbar
    cbar = ax.figure.colorbar(im, cax=cax, **cbar_kw)
    cbar.ax.set_title(cbarlabel,fontsize=14)
    cbar.ax.tick_params(labelsize=12)

    # We want to show all ticks...
    ax.set_xticks(np.arange(data.shape[1]))
    ax.set_yticks(np.arange(data.shape[0]))
    # ... and label them with the respective list entries.
    ax.set_xticklabels(col_labels)
    ax.set_yticklabels(row_labels)

    # Turn spines off and create white grid.
    for edge, spine in ax.spines.items():
        spine.set_visible(False)

    # Axes
    ax.set_xticks(np.arange(data.shape[1]+1)-.5, minor=True)
    ax.set_yticks(np.arange(data.shape[0]+1)-.5, minor=True)
    ax.grid(which="minor", color="w", linestyle='-', linewidth=2)
    ax.tick_params(which="minor", bottom=False, left=False)

    return im, cbar

#------------------------------------------------------------------------
# Creazione dei file contenenti i campi medi e massimi sulle macroaree
# dell'Emilia-Romagna da plottare in formato di scacchiera
#------------------------------------------------------------------------

if len(sys.argv) > 1:
    aree=sys.argv[1]
else:
    aree='/usr/local/share/libsim/macroaree_er.shp'
if len(sys.argv) > 2:
    path_in=sys.argv[2]
else:
    path_in='/autofs/scratch-rad/vpoli/FCST_PROB/fxtr/data'
if len(sys.argv) > 3:
    fold_out=sys.argv[3]
else:
    fold_out='tmp'

# Creo la directory "fold_out" se non esiste
if not os.path.exists("%s"%fold_out):
    os.makedirs("%s"%fold_out)
    

cumulate=['tpp01h','tpp03h','tpp24h']
valore='prob'
units='[%]'

# Definizione delle soglie usate per il calcolo della probabilità
sf=[[ 1, 0, 0,-1,-1],[-1,-1,-1,-1,-1],[-1,-1,-1,-2,-1]]
sv=[[ 5, 1, 5, 1, 3],[ 1, 2, 3, 5, 7],[ 3, 5, 7, 1,15]]

# Definisco i valori delle soglie
thresh=[[] for _ in range(len(sf))]
for i in range(len(sf)):
    for j in range(len(sf[i])):
        thresh[i].append(soglia(sf[i][j],sv[i][j]))

# Definisco la soglia per il calcolo del percentile
percent=90

# Calcolo dei campi di media e massimo sulle macroaree
for subtype in ['average','max','percentile']:
    for j in cumulate:
        # Lista dei file per scadenza 
        search="%s/%s*.grib"%(path_in,j)
        lista=glob.glob(search)

        if j=='tpp01h':
            filelist=sorted(lista)[3:27]
        elif j=='tpp03h':
            filelist=sorted(lista)[1:len(lista)]
        elif j=='tpp24h':
            filelist=sorted(lista)
        else:
            print("Questa cumulata non è definita")

        # Indice dei valori di soglia per la cumulata selezionata
        n=cumulate.index(j)

        # Estrazione campi sulle macroaree usando le soglie opportune
        for fname in filelist:
#            print(fname)
            for i in range(len(sf[n])):
                csvname=estrai_campi_su_macroaree(fname,valore,subtype,sf[n][i],sv[n][i],percent,aree) 


# Lettura dei csv prodotti per la generazione delle scacchiere                
    for j in cumulate:
        # Indice dei valori di soglia per la cumulata selezionata
        n=cumulate.index(j)

        val=[[] for _ in range(len(thresh[n]))]
        ind=0
        # Separo i file per soglia
        for i in thresh[n]:
            # Lista dei file per scadenza e soglia 
            search="%s_%s_%s_*soglia%s.csv"%(subtype,valore,j,str(i))
            lista=glob.glob(search)
            
            for f in lista:
                #print(ind," - Leggo file: ",f)
                df=pd.read_csv(f.strip(),delimiter=',',
                               names=['Date','Time range','P1','P2',
                                      'Longitude','Latitude','Level1',
                                      'L1','Level2','L2','Report',
                                      'B01192','B13011'],
                               skiprows=0)
                val[ind].append(df)
            ind=ind+1

        # Inizializzo alcune variabili necessarie
        # a generare l'immagine
        df  =[None]*len(thresh[n])
        dati=[None]*len(thresh[n])

        for k in range(len(thresh[n])):
            # Unisco fisicamente tutti i dati letti ordinandoli
            # per data
            df[k]=pd.concat(val[k]).sort_values(by=['Date'])

            # Raggruppo i dati per macroaree, creando una lista
            # contenente tutte le scadenze
            dati[k]=df[k].groupby('B01192')['B13011'].apply(list)
            # Estraggo la data del run, la scadenza e l'intervallo
            # di cumulata dal primo dataframe poichè i valori sono
            # tutti uguali per le diverse soglie
            run =df[0].groupby('B01192')['Date'].apply(list)
            lead=df[0].groupby('B01192')['P1'].apply(list)
            cum =df[0].groupby('B01192')['P2'].apply(list)

        # Creo un unico dataframe contenente TUTTI i dati
        # necessari a generare la scacchiera: ogni riga
        # corrisponde ad una macroarea, ogni colonna ad
        # una soglia.
        # La soglia più bassa verrà visualizzata in basso
        # per le cumulate orarie/triorarie. 
        # Per vedere le colonne: alldata[i]
        # Per vedere le righe: alldata.iloc[i]
        if j!='tpp24h':
            alldata=pd.DataFrame(zip(dati[4],dati[3],dati[2],dati[1],dati[0]))
        else:
            alldata=pd.DataFrame(zip(dati[0],dati[1],dati[2],dati[3],dati[4]))
        #print(alldata)

        # Definisco le macroaree 
        macro=['A','B','C','D','E','F','G','H']

        # Definisco le etichette date/time
        x=lead.iloc[0]
        orafcst=[]
        if j=='tpp24h':
            for i in range(len(run.iloc[0])):
                ora=datetime.strptime(run.iloc[0][i],'%Y-%m-%d %H:%M:%S').strftime('%H')
                orafcst.append(datetime.strptime(run.iloc[0][i],'%Y-%m-%d %H:%M:%S').strftime('%d/%m/%Y %H:%M'))
        else:
            for i in range(len(run.iloc[0])):
                ora=datetime.strptime(run.iloc[0][i],'%Y-%m-%d %H:%M:%S').strftime('%H')
                if ora=='00':
                    orafcst.append(datetime.strptime(run.iloc[0][i],'%Y-%m-%d %H:%M:%S').strftime('%d/%m\n%H'))
                else:
                    orafcst.append(datetime.strptime(run.iloc[0][i],'%Y-%m-%d %H:%M:%S').strftime('%H'))
            # Se le cumulate sono orarie/triorarie, aggiungo l'ora di inizio del fcst
            orazero=datetime.strptime(run.iloc[0][0],'%Y-%m-%d %H:%M:%S')-timedelta(seconds=cum.iloc[0][0])
            if orazero.strftime('%H')=='00':
                orafcst.insert(0,orazero.strftime('%d/%m\n%H'))
            else:
                orafcst.insert(0,orazero.strftime('%H'))        
        #print(orafcst)
        
        # Definisco date/time del run per il nome del file in output
        inizio=datetime.strptime(run.iloc[0][0],'%Y-%m-%d %H:%M:%S')-timedelta(seconds=lead.iloc[0][0])
        
        # Definisco le etichette di superamento soglia
        y=[]
        if j!='tpp24h':
            for i in reversed(thresh[n]):
                y.append('TP>'+str(i)+'mm')
        else:
            for i in thresh[n]:
                y.append('TP>'+str(i)+'mm')
        #print(y)

        
        bounds=[0,10,25,50,75,90,100]
        cmap=colors.ListedColormap(['#cccccc','#edf8fb',
                                    '#b3cde3','#8c96c6',
                                    '#8856a7','#810f7c'])
        norm=colors.BoundaryNorm(bounds,cmap.N)

        if j!='tpp24h':
            fig,ax=plt.subplots(len(macro),figsize=(14,20)) 
            # Scacchiera separata per macroarea
            for i in range(len(macro)):
                #            print(i,macro[i])
                a=pd.DataFrame(alldata.iloc[i].to_list(),index=alldata.iloc[i].index)
 
                im,cbar=heatmap(a,y,x,ax=ax[i],cmap=cmap,norm=norm,cbarlabel=units)
                ax[i].set_xticks(np.arange(-0.5,len(x)+0.5,1))
                ax[i].set_xticklabels(orafcst)
                ax[i].set_yticklabels(y)
                ax[i].grid(which="major",color="w",axis='x',linestyle='-',linewidth=2)

                ax[i].set_title('Macroarea '+macro[i])
                fig.tight_layout(pad=1.8)
        else:
            fig,ax=plt.subplots(figsize=(14,6),nrows=1,ncols=2)

            a0=[]
            a1=[]
            for i in range(len(thresh[n])):
                dum=pd.DataFrame(alldata[i].to_list(),index=alldata[i].index)
                a0.append(dum[0])
                a1.append(dum[1])
            
            im,cbar=heatmap(pd.concat(a0,axis=1),macro,y,ax=ax[0],
                            cmap=cmap,norm=norm,aspect='auto',
                            cbarlabel=units)
            ax[0].set_title(orafcst[0])
            
            im,cbar=heatmap(pd.concat(a1,axis=1),macro,y,ax=ax[1],
                            cmap=cmap,norm=norm,aspect='auto',
                            cbarlabel=units)
            ax[1].set_title(orafcst[1])

        # Titolo della figura    
        if subtype=='average':
            tipo='Media'
        elif subtype=='max':
            tipo='Massimo'
        elif subtype=='percentile':
            tipo='%sesimo percentile'%percent
        fig.suptitle("Corsa di COSMO-2I-EPS del %s\n" \
                     "%s delle probabilità sulle macroaree"
                     %(inizio.strftime('%d/%m/%Y %H:%M'),tipo),
                     y=1.02)    
        if subtype=='percentile':    
            fileout="%s/%s%s_%s_%s_%s.png"%(fold_out,subtype,str(percent),
                                          valore,j,inizio.strftime('%Y%m%d%H'))
        else:
            fileout="%s/%s_%s_%s_%s.png"%(fold_out,subtype,valore,j,
                                          inizio.strftime('%Y%m%d%H'))
        fig.savefig(fileout,bbox_inches='tight')
        plt.close()

    # Elimino file csv        
    tobedeleted=glob.glob("%s_%s*.csv"%(subtype,valore))
    for f in tobedeleted:
        os.remove(f)

quit()