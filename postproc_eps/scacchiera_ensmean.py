#!/usr/bin/env python3
# encoding: utf-8
import os, sys, numpy as np, subprocess, glob
import pandas as pd

import matplotlib as mpl
import matplotlib.pyplot as plt
from matplotlib import colors
from mpl_toolkits.axes_grid1 import make_axes_locatable
from datetime import datetime,timedelta


def estrai_campi_su_macroaree(fname,valore,sub_type,aree): #,sf,sv):
    prodotto=os.path.basename(fname).split('.',1)[0]    

    csvname="%s_%s_%s.csv"%(sub_type,valore,prodotto)
    
    grib_copy="grib_copy -w productDefinitionTemplateNumber=12,derivedForecast=0 " \
        "%s campo.grib"%(fname)
    subprocess.call(grib_copy.split(),shell=False)
    
    vg6d_getpoint="vg6d_getpoint --coord-file=%s " \
        "--coord-format=shp --trans-type=polyinter --sub-type=%s " \
        "--output-format=native campo.grib pre.v7d"%(aree,sub_type)
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
    cbar.ax.set_title(cbarlabel,fontsize=12)
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
valore='ensmean'
units='[mm]'

# Calcolo dei campi di media e massimo sulle macroaree
for subtype in ['average','max']:
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

        # Estrazione campi sulle macroaree usando le soglie opportune
        for fname in filelist:
            #print(fname)
            csvname=estrai_campi_su_macroaree(fname,valore,subtype,aree)

# Lettura dei csv prodotti per la generazione delle scacchiere                
for j in cumulate:
    # Preparo le figure
    n=0
    if j!='tpp24h':
        fig,ax=plt.subplots(2,figsize=(20,8))
        fontsize=12
    else:
        fig,ax=plt.subplots(figsize=(12,6),nrows=1,ncols=2)
        fontsize=10
        
    for subtype in ['average','max']:

        val =[]
        run =[]
        lead=[]
        cum =[]

        # Lista dei file da leggere
        search="%s_%s_%s_*.csv"%(subtype,valore,j)
        lista=glob.glob(search)
            
        for f in sorted(lista):
            #print("Leggo file: ",f)
            df=pd.read_csv(f.strip(),delimiter=',',
                           names=['Date','Time range','P1','P2',
                                  'Longitude','Latitude','Level1',
                                  'L1','Level2','L2','Report',
                                  'B01192','B13011'],
                           skiprows=0)
            val.append(df['B13011'])
            run.append(df['Date'])
            lead.append(df['P1'])
            cum.append(df['P2'])
            
        df=pd.concat(val,axis=1)
        data=pd.concat(run,axis=1)
        scad=pd.concat(lead,axis=1)
        icum=pd.concat(cum,axis=1)

        # Definisco le macroaree 
        macro=['A','B','C','D','E','F','G','H']

        # Trasformo date, scadenze e cumulate in liste
        dum=data.iloc[0].tolist()
        run=[s for s in dum]
        dum=scad.iloc[0].astype(int).tolist()
        x=[s for s in dum]
        dum=icum.iloc[0].astype(int).tolist()
        dt=[s for s in dum]

        orafcst=[]
#        if j=='tpp24h':
#            for i in range(len(run.iloc[0])):
#                ora=datetime.strptime(run.iloc[0][i],'%Y-%m-%d %H:%M:%S').strftime('%H')
#                orafcst.append(datetime.strptime(run.iloc[0][i],'%Y-%m-%d %H:%M:%S').strftime('%d/%m/%Y %H:%M'))
#        else:
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

        # Definisco date/time del run per il nome del file in output
        inizio=datetime.strptime(run[0],'%Y-%m-%d %H:%M:%S')-timedelta(seconds=x[0])
        
        bounds=[1,2,5,10,20,30,50,70,100,150,200,300,500]
        cmap=colors.ListedColormap(['#f0f0f0','#d0d1e6','#a6bddb',
                                    '#74a9cf','#2b8cbe','#045a8d',
                                    '#fecc5c','#fd8d3c','#e31a1c',
                                    '#dd3497','#ae017e','#7a0177'])
        norm=colors.BoundaryNorm(bounds,cmap.N)

        if j!='tpp24h':
            im,cbar=heatmap(df,macro,x,ax=ax[n],cmap=cmap,norm=norm,
                            cbarlabel=units,cbar_kw=dict(ticks=bounds))
        else:
            im,cbar=heatmap(df,macro,x,ax=ax[n],cmap=cmap,norm=norm,
                            cbarlabel=units,aspect='auto',
                            cbar_kw=dict(ticks=bounds))
        
        ax[n].set_xticks(np.arange(-0.5,len(x)+0.5,1))
        ax[n].set_xticklabels(orafcst,fontsize=fontsize)
        ax[n].grid(which="major",color="w",axis='x',linestyle='-',linewidth=2)
        if subtype=="average":
            ax[n].set_title("Precipitazione media della media dell'ensemble",
                            fontsize=14)
        elif subtype=="max":
            ax[n].set_title("Precipitazione massima della media dell'ensemble",
                            fontsize=14)
        fig.tight_layout(pad=1.0)
        n=n+1

    fig.suptitle("Corsa di COSMO-2I-EPS del %s\n" \
                 %(inizio.strftime('%d/%m/%Y %H:%M')),y=1.02)
    fileout="%s/%s_%s_%s.png"%(fold_out,valore,j,inizio.strftime('%Y%m%d%H'))
    fig.savefig(fileout,bbox_inches='tight')
    plt.close()

# Elimino i file csv
tobedeleted=glob.glob("*_%s*.csv"%(valore))
for f in tobedeleted:
    os.remove(f)

quit()
