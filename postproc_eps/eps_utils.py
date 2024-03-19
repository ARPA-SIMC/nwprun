# -*- coding: utf-8 -*-

import os, sys, numpy as np, subprocess, glob
import argparse
from math import *
from mpl_toolkits.axes_grid1 import make_axes_locatable

# Macroaree Regioni
macro={
    "abru":[ 'A', 'B', 'C', 'D1', 'D2', 'E' ],
    "basi":[ 'A1', 'A2', 'B', 'C', 'D', 'E1', 'E2' ],
    "cala":[ '1','2','3','4','5','6'],
    "camp":[ '1','2','3','4','5','6','7','8'],
    "emro":[ 'A','B','C','D','E','F','G','H'],
    "friu":[ 'A','B','C','D'],
    "lazi":[ 'A','B','C','D','E','F','G'],
    "ligu":[ 'A','B','C','D','E'],
    "lomb":[ '01','02','03','04','05','06','07','08','09','10','11','12','13','14'],
    "marc":[ '1','2','3','4','5','6'],
    "moli":[ 'A','B','C'],
    "piem":[ 'A','B','C','D','E','F','G','H','I','L','M'],
    "pugl":[ 'A','B','C','D','E','F','G','H','I'],
    "sard":[ 'A','B','C','D','E','F','G'],
    "sici":[ 'A','B','C','D','E','F','G','H','I'],
    "tosc":[ 'A1','A2','A3','A4','A5','A6','B','C','E1','E2','E3','F1','F2','I','L','M','O1','O2','O3','R1','R2','S1','S2','S3','T','V'],
    "tren":[ 'A','B'],
    "umbr":[ 'A','B','C','D','E','F'],
    "vdao":[ 'A','B','C','D'],
    "vene":[ 'A','B','C','D','E','F','G','H']
}

def get_args():
    parser = argparse.ArgumentParser()

    parser.add_argument( '-in', '--path_in', dest="path_in", required=False,
                         help="Directory di input dei dati",
                         default="/autofs/scratch-rad/vpoli/FCST_PROB/fxtr/data" )
    parser.add_argument( '-out', '--path_out', dest="fold_out", required=False,
                         help="Directory di output",
                         default="tmp")
    parser.add_argument( '-st', '-sub_type', nargs='+', dest="subtype", required=False,
                         default=['average'], help="Sub_type richiesto (average, max, percentile)" )
    parser.add_argument( '-tp', '-accumulation_time', nargs='+', dest="cumulate",
                         required=False, default=[ 'tpp01h', 'tpp03h' ],
                         help="Tempo di cumulazione richiesto (tpp01h, tpp03h)" )
    parser.add_argument( '-shp', '--shp_regione', dest="aree", required=False,
                         help="Regione per cui si vuole eseguire il post-processing",
                         default='/usr/local/share/libsim/macroaree_er.shp' )
    parser.add_argument( '-shp_it', '--shp_italia', dest="aree_it", required=False,
                         help="Shapefile per mappa locale",
                         default='/autofs/nfshomes/vpoli/FileShapes/shapefile_confini/gadm41_ITA_1.shp' )
    parser.add_argument( '-thr', '--thr_paint', dest="thr", required=False,
                         type=float, default=10.,
                         help="Soglia per cui vengono prodotte le immagini" )
    parser.add_argument( '-op', '--operations', dest="operations", required=False,
                         help="Elenco di operazioni da compiere, C=cumulazione, S=scacchiera",
                         default="CS" )
    
    args = parser.parse_args()
    return args

def get_args_maps():
    parser = argparse.ArgumentParser()

    parser.add_argument( '-in', '--file_in', dest="file_in", required=True,
                         help="File dati in input" )
    parser.add_argument( '-out', '--path_out', dest="fold_out", required=False,
                         help="Directory di output",
                         default="tmp")
    parser.add_argument( '-shp_it', '--shp_italia', dest="aree_it", required=False,
                         help="Shapefile per mappa locale",
                         default='/autofs/nfshomes/vpoli/FileShapes/shapefile_confini/gadm41_ITA_1.shp' )
    parser.add_argument( '-shp_er', '--shp_regione', dest="aree_er", required=False,
                         help="Shapefile per mappa locale",
                         default='/usr/local/share/libsim/macroaree_er.shp' )
    parser.add_argument( '-op', '--operations', dest="operations", required=False,
                         help="Elenco di operazioni da compiere, C=cumulazione, S=scacchiera",
                         default="CS" ) 
    
    args = parser.parse_args()
    return args

def soglia( cumulata ):

    # Valori di default dei coefficienti per il calcolo delle
    # soglie in funzione del tempo di cumulata
    if cumulata == 'tpp01h':
        sf = [ 1, 0, 0, -1, -1 ]
        sv = [ 5, 1, 5, 1, 3 ]
    elif cumulata == 'tpp03h':
        sf = [ -1, -1, -1, -1, -1 ]
        sv = [ 1, 2, 3, 5, 7 ]
    elif cumulata == 'tpp24h':
        sf = [ -1, -1, -1, -2, -1 ]
        sv = [ 3, 5, 7, 1, 15 ]
    else:
        sys.exit( "\nERRORE! Questa cumulata non è gestita\n" )

    thresh = []
    for i in range( len(sf) ):
        if sf[i] > 0:
            thresh.append( sv[i] / ( int('1'.ljust(abs(sf[i]),'0') ) * 10 ) )
        elif sf[i] == 0:
            thresh.append( sv[i] )
        else:
            thresh.append( sv[i] * int( '1'.ljust(abs(sf[i]),'0') ) * 10 ) 

    return( thresh, sf, sv )

def cumula_membri(nmemb, path_in, cumulate=None):
    for i in range(1, nmemb+1):
        dir_in = f"{path_in}/input.{i:03d}/lfff????0000"
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

        if cumulate[3:5] == '24':
            cum = '01 00'
        else:
            cum = '00 {}'.format( cumulate[3:5] )

        # Calcolo le cumulate secondo quanto richiesto
        grib_cum = "vg6d_transform --comp-stat-proc=1 --comp-step='{}' " \
            "--comp-full-steps tp.grib " \
            "{}_membro{}.grib".format( cum, cumulate, str(i) )
        os.system( grib_cum )
 
        subprocess.call( [ "rm", "tp.grib" ] )


def estrai_campi_su_macroaree( fname, valore, sub_type, aree, regione ):
    prodotto = os.path.basename(fname).split('.', 1)[0]
    # definisco nomi unici per usare diverse istanze in parallelo
    campo_un = "campo_{}.grib".format(regione)
    pre_un = "pre_{}.v7d".format(regione)

    csvname = "{}_{}_{}_{}.csv".format( sub_type, valore, prodotto, regione )
    
    grib_copy = "grib_copy -w productDefinitionTemplateNumber=12,derivedForecast=0 " \
        "{} {}".format( fname, campo_un )
    subprocess.call( grib_copy.split(), shell=False )
    
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

    vg6d_getpoint="vg6d_getpoint --coord-file={} " \
        "--coord-format={} --trans-type={} --sub-type={} " \
        "--output-format=native {} {}".format( aree, c_format, trans_type, sub_type, campo_un, pre_un )
    subprocess.call( vg6d_getpoint.split(), shell=False )
                
    v7d_trans="v7d_transform --input-format=native --output-format=csv --csv-header=0 " \
	"{} {}".format( pre_un, csvname )
    subprocess.call( v7d_trans.split(), shell=False )

    # Elimino il file dati.v7d
    subprocess.call( [ "rm", pre_un ] )
    subprocess.call( [ "rm", campo_un ] )
    return csvname                    

def estrai_prob_su_macroaree( fname, valore, subtype, aree, regione, thresh, sf, sv, percent ):
    prodotto = os.path.basename(fname).split('.',1)[0]
    # definisco nomi unici per usare diverse istanze in parallelo
    campo_un = "campo_{}.grib".format(regione)
    pre_un = "pre_{}.v7d".format(regione)

    for i in range(len(thresh)):        
        csvname = "{}_{}_{}_soglia{}_{}.csv".format( subtype, valore, prodotto, str(thresh[i]), regione )
        #print( i, str(thresh[i]), sf[i], sv[i] )
        grib_copy = "grib_copy -w productDefinitionTemplateNumber=9,scaleFactorOfLowerLimit={},scaledValueOfLowerLimit={} " \
            "{} {}".format(sf[i], sv[i], fname, campo_un)
        subprocess.call( grib_copy.split(), shell=False )
    
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
            "--percentile={:d} --output-format=native {} {}".format( aree,  \
                                                                     c_format, trans_type, subtype, percent, campo_un, pre_un )
        subprocess.call( vg6d_getpoint.split(), shell=False )
            
        v7d_trans = "v7d_transform --input-format=native --output-format=csv --csv-header=0 " \
	    "{} {}".format( pre_un, csvname )
        subprocess.call( v7d_trans.split(), shell=False )

    # Elimino il file dati.v7d
    subprocess.call( [ "rm", pre_un ] )
    subprocess.call( [ "rm", campo_un ] )
    
    return csvname                    

#-------------------------------------------------------------------------------
# Create a heatmap from a numpy array and two lists of labels.
def heatmap( data, row_labels, col_labels, gf_cbar=False, ax=None,
             cbar_kw={}, cbarlabel="", **kwargs ):
    
    # -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -
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
    # -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -
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

#-------------------------------------------------------------------------------
# FUNZIONI USATE DAI PROGRAMMI DI GRAFICA mappe_*.py

def get_var_grib(grib_data):
#-------------------------------------------------------------------------------
# Ricavo alcune variabili dal grib (tutte le keys sono in grb.key())
#-------------------------------------------------------------------------------
    medi = None
    perc = None
    thresh = None
    variabile = grib_data.shortName
    if variabile == 'unknown':
        if grib_data.parameterCategory == 17 and grib_data.parameterNumber == 192:
            variabile = 'lpi'
    data      = "%08d"%grib_data.dataDate
    ora       = "%04d"%grib_data.dataTime
    Ni        = int(grib_data['Ni'])
    Nj        = int(grib_data['Nj'])
    startstep = "%02d"%grib_data.startStep
    endstep   = "%02d"%grib_data.endStep
    if grib_data.has_key('indicatorOfUnitForTimeRange') and grib_data.has_key('lengthOfTimeRange'):
        itr = int(grib_data.indicatorOfUnitForTimeRange)
        ltr       = int(grib_data.lengthOfTimeRange)
        if itr == 0:
            cumstep = ltr/60
        else:
            cumstep=ltr
    else:
        cumstep = 0
    if grib_data.has_key('derivedForecast'): medi = grib_data.derivedForecast
    if grib_data.has_key('percentileValue'): perc = grib_data.percentileValue
    if grib_data.has_key('scaleFactorOfLowerLimit') and grib_data.has_key('scaledValueOfLowerLimit'):
        sf        = grib_data.scaleFactorOfLowerLimit
        sv        = grib_data.scaledValueOfLowerLimit
        if sf > 0: 
            thresh = sv/(int('1'.ljust(abs(sf),'0'))*10)
        elif sf == 0:
            thresh = sv
        else:
            thresh = sv*int('1'.ljust(abs(sf),'0'))*10
#    print("\nVariabile: ", variabile,   "\nData:      ", data,
#          "\nOra:       ", ora,         "\nScadenza: +", endstep,
#          "\nSoglia:    ", thresh,      "\nMedia:     ", medi,
#          "\nPercentile  ", perc, sep="")

    return variabile, data, ora, Ni, Nj, startstep, endstep, thresh, medi, perc, cumstep

def rotated_grid_transform (lon_arr, lat_arr, option, SP_coor):
#-------------------------------------------------------------------------------
# Trasformazione di coordinate da "regular latlon" a "rotated latlon"
# (option=1) e viceversa (option=0)
#-------------------------------------------------------------------------------
    # Conversione da gradi a radianti
    lon = (lon_arr*pi)/180
    lat = (lat_arr*pi)/180

    # Salvo coordinate del Southern Pole
    SP_lon, SP_lat = SP_coor[0], SP_coor[1];

    # Definisco l'angolo (in radianti) di rotazione attorno all'asse y (theta)
    # e z (phi)
    theta = (90+SP_lat)*pi/180
    phi   = (SP_lon)*pi/180

    # Converto da coordinate sferiche a cartesiane
    x = np.multiply(np.cos(lon), np.cos(lat)) 
    y = np.multiply(np.sin(lon), np.cos(lat))
    z = np.sin(lat);

    if option == 1: # Regular -> Rotated 
        cosphi_x = np.multiply(np.cos(phi), x)
        senphi_y = np.multiply(np.sin(phi), y)
        x_new =  np.multiply(cosphi_x, np.cos(theta)) + \
                 np.multiply(senphi_y, np.cos(theta)) + np.multiply(np.sin(theta), z)
        y_new = -np.multiply(np.sin(phi), x) + np.multiply(np.cos(phi), y)
        z_new = -np.multiply(cosphi_x, np.sin(theta)) - \
                 np.multiply(senphi_y, np.sin(theta)) + np.multiply(np.cos(theta), z)

    else:  # Rotated -> Regular
        phi = -phi;
        theta = -theta;

        costheta_x = np.multiply(np.cos(theta), x)
        sentheta_z = np.multiply(np.sin(theta), z)

        x_new =  np.multiply(costheta_x, np.cos(phi)) + np.multiply(np.sin(phi), y) + \
                 np.multiply(sentheta_z, np.cos(phi))
        y_new = -np.multiply(costheta_x, np.sin(phi)) + np.multiply(np.cos(phi), y) - \
                 np.multiply(sentheta_z, np.sin(phi))
        z_new = -np.multiply(np.sin(theta), x)        + np.multiply(np.cos(theta), z)

    # Ri-converto da coordinate cartesiane a sferiche e trasformo in radianti
    lon_new = (np.arctan2(y_new,x_new))*180/pi
    lat_new = (np.arcsin(z_new))*180/pi

    return lon_new,lat_new
