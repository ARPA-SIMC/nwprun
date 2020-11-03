#!/usr/bin/env python3
# encoding: utf-8
import os, sys, numpy as np, subprocess, glob
import pandas as pd

import matplotlib as mpl
import matplotlib.pyplot as plt
from matplotlib import colors
from mpl_toolkits.axes_grid1 import make_axes_locatable
from datetime import datetime,timedelta
    
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
    path_in='/autofs/scratch-rad/vpoli/FCST_PROB/gpfs/meteo/lami/prod/cosmo_2I/fcens'
if len(sys.argv) > 3:
    fold_out=sys.argv[3]
else:
    fold_out='tmp'

# Creo la directory "fold_out" se non esiste
if not os.path.exists("%s"%fold_out):
    os.makedirs("%s"%fold_out)

# Estrazione di TP trioraria per ciascun membro e calcolo della media
# sulle macroaree 
sub_type='average'

thresh=[10,20,30,50,70]
units='[%]'
val=[]

for i in range(1,21): 
    dir_in="%s/cosmo.%s/data/lfff????0000"%(path_in,str(i))
#    for f in os.listdir(dir_in):
    for f in glob.glob(dir_in):
        # Estraggo i campi di TP
#        grib_copy="grib_copy -w shortName=tp %s tp_%s.grib"%(os.path.join(dir_in,f),f)
        grib_copy="grib_copy -w shortName=tp %s tp_%s.grib"%(f,os.path.basename(f))
        subprocess.call(grib_copy.split(),shell=False)
    # Unisco i file e calcolo le cumulate
    os.system("cat tp_*.grib > tp.grib")
    
    tobedeleted=glob.glob("tp_lfff*")
    for f in tobedeleted:
        os.remove(f)
    
    grib_cum="vg6d_transform --comp-stat-proc=1 --comp-step='00 03' --comp-full-steps " \
        "tp.grib tp3h_membro%s.grib"%str(i)
    os.system(grib_cum)
    subprocess.call(["rm","tp.grib"])
    
    vg6d_getpoint="vg6d_getpoint --coord-file=%s " \
        "--coord-format=shp --trans-type=polyinter --sub-type=%s " \
        "--output-format=native tp3h_membro%s.grib pre.v7d"%(aree,sub_type,str(i))
    subprocess.call(vg6d_getpoint.split(),shell=False)

    csvname="tp3h_membro%s.csv"%str(i)
    v7d_trans="v7d_transform --input-format=native --output-format=csv --csv-header=0 " \
	"pre.v7d %s"%csvname
    subprocess.call(v7d_trans.split(),shell=False)

    # Elimino il file dati non necessari
    subprocess.call(["rm","pre.v7d"])

    tobedeleted=glob.glob("tp3h_membro*.grib")
    for f in tobedeleted:
        os.remove(f)

    val.append(pd.read_csv(csvname,delimiter=',',
                          names=['Date','Time range','P1','P2',
                                 'Longitude','Latitude','Level1',
                                 'L1','Level2','L2','Report',
                                 'B01192','B13011'],
                          skiprows=0))


df=pd.concat(val).sort_values(by=['Date','B01192'])
pd.set_option('display.max_rows',df.shape[0]+1)

# Aggiungo colonne al dataframe contenenti i superamenti di soglia
y=[]     # Etichette superamento soglia
lista=[] # Intestazione colonne aggiuntive
for i in reversed(thresh):
    df["Soglia%s"%str(i)]=np.where((df["B13011"]>=i),1,0)
    lista.append("Soglia%s"%str(i))
    y.append('TP>'+str(i)+'mm')
    
#Calcolo la media sull'ens del superamento di soglia
dati=df.groupby(['Date','B01192'],as_index=False)[lista].agg(np.mean) 

# Definisco le etichette con le ore dei forecast
orafcst=[]
ore=dati['Date'].unique()
for i in ore:
    ora=datetime.strptime(i,'%Y-%m-%d %H:%M:%S').strftime('%H')
    if ora=='00':
        orafcst.append(datetime.strptime(i,'%Y-%m-%d %H:%M:%S').strftime('%d/%m\n%H'))
    else:
        orafcst.append(datetime.strptime(i,'%Y-%m-%d %H:%M:%S').strftime('%H'))
# Aggiungo l'ora di inizio del fcst
orazero=datetime.strptime(ore[0],'%Y-%m-%d %H:%M:%S')-timedelta(hours=3)
if orazero.strftime('%H')=='00':
    orafcst.insert(0,orazero.strftime('%d/%m\n%H'))
else:
    orafcst.insert(0,orazero.strftime('%H'))        
#print(orafcst)

# Definisco date/time del run per il nome del file in output
inizio=datetime.strptime(ore[0],'%Y-%m-%d %H:%M:%S')-timedelta(hours=3)

# Definisco le macroaree 
macro=['A','B','C','D','E','F','G','H']

fig,ax=plt.subplots(len(macro),figsize=(70,20)) 
bounds=[0,10,25,50,75,90,100]
cmap=colors.ListedColormap(['#cccccc','#fee08b',
                            '#d9ef8b','#67a9cf',
                            '#2166ac','#f768a1'])
norm=colors.BoundaryNorm(bounds,cmap.N)


# Scacchiera separata per macroarea
for i in range(len(macro)):
    # Seleziono la macroarea
    area=dati[dati['B01192']==(i+1)]
    #print(area)
    a=area[area.columns[-(len(thresh)):]]*100
    print(a[1:])
    
    im,cbar=heatmap(a[1:].T,y,ore[1:],ax=ax[i],cmap=cmap,norm=norm,cbarlabel=units)
    ax[i].set_xticks(np.arange(-0.5,len(ore[1:])+0.5,1))
    ax[i].set_xticklabels(orafcst[1:])
    ax[i].set_yticklabels(y)
    ax[i].grid(which="major",color="w",axis='x',linestyle='-',linewidth=2)
    
    ax[i].set_title('Macroarea '+macro[i])
    fig.tight_layout(pad=1.8)


# Titolo della figura    
if sub_type=='average':
    tipo='media'
fig.suptitle("Corsa di COSMO-2I-EPS del %s\n" \
             "Probabilità di superamento soglie della precipitazione %s sulle macroaree "
             %(inizio.strftime('%d/%m/%Y %H:%M'),tipo),
             y=1.02)    

fileout="%s/prob_%s_tot3hprec_%s.png"%(fold_out,sub_type,inizio.strftime('%Y%m%d%H'))
fig.savefig(fileout,bbox_inches='tight')
plt.close()

tobedeleted=glob.glob("tp3h_membro*.csv")
for f in tobedeleted:
    os.remove(f)
quit()
