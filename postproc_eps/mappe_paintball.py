#!/usr/bin/env python3
# encoding: utf-8

import os, sys, numpy as np, subprocess, glob, pygrib
import pandas as pd

import cartopy, cartopy.crs as ccrs, cartopy.feature as cfeature
from cartopy.feature import ShapelyFeature
from cartopy.io.shapereader import Reader

import matplotlib, matplotlib.pyplot as plt
from matplotlib import colors
from matplotlib.colors import ListedColormap

from datetime import datetime,timedelta

from eps_utils import cumula_membri, get_args, get_var_grib, rotated_grid_transform 

# ------------------------------------------------------------------------------
# MAKE PAINTBALL PLOTS WITH PYTHON
# ------------------------------------------------------------------------------
# DESCRIZIONE
# This script makes paintball plots from COSMO-2I-EPS output

if __name__ == '__main__':

    args = get_args()
    
    path_in = args.path_in
        
    fold_out = args.fold_out # default = tmp
    # Creo la directory di output, "fold_out", se non esiste
    if not os.path.exists( "{}".format(fold_out) ):
        os.makedirs( "{}".format(fold_out) )

    cumulate = args.cumulate # default = ['tpp01h', 'tpp03h']
    if len(cumulate) > 1:
        print("I tempi di cumulazione di default sono tpp01h e tpp03h. \n"
              "Il programma gestisce solo un tempo di cumulazione alla volta, \n"
              "scelgo il primo di questi: ", cumulate)
    cumulate = cumulate[0]
    
    # Shapefiles per grafica
    shp_reg = args.aree
    shp_it  = args.aree_it
    shape_feature = ShapelyFeature(Reader(shp_it).geometries(),
                                   ccrs.PlateCarree(), edgecolor='black')
    
    threshold = args.thr
        
    #------------------------------------------------------
    # Estrazione e cumulazione dei campi di precipitazione 
    #------------------------------------------------------
    # Per sicurezza elimino il file contenente tutte le
    # precipitazioni, se esiste
    for filename in glob.glob( os.path.join( fold_out, "gribbone*.grib" ) ): 
        os.remove(filename) 

     if "C" in args.operations:
        #tobedeleted = glob.glob("tp3h_membro*.grib")
        for f in tobedeleted:
            os.remove(f)
        cumula_membri(nmemb, path_in, cumulate)
    """
    for i in range(1,21): 
        dir_in = "{}/cosmo.{}/data/lfff????0000".format( path_in, str(i) )
        #dir_in = "{}/2021100321.{}/lfff????0000".format( path_in, f"{i:03d}" )

        for f in glob.glob(dir_in):
            grib_copy = "grib_copy -w shortName=tp {} {}/tp_{}.grib".format(f, fold_out, os.path.basename(f))
            subprocess.call(grib_copy.split(), shell=False)

        # Unisco i file e calcolo le cumulate
        os.system("cat {}/tp_*.grib > {}/tp.grib".format(fold_out, fold_out))
        tobedeleted = glob.glob( "{}/tp_lfff*".format(fold_out) ) 
        for f in tobedeleted:
            os.remove(f)
       
        if cumulate[3:5] == '24':
            cum = '01 00'
        else:
            cum = '00 {}'.format( cumulate[3:5] )

        # Calcolo le cumulate secondo quanto richiesto
        grib_cum = "vg6d_transform --comp-stat-proc=1 --comp-step='{}' " \
            "--comp-full-steps {}/tp.grib " \
            "{}/{}_membro{}.grib".format( cum, fold_out, fold_out, cumulate, str(i) )
        os.system( grib_cum )
     """   

    # Estraggo le scadenze a seconda del tempo di cumulazione
    dump = "grib_get -p endStep {}/{}_membro1.grib".format( fold_out, cumulate )
    proc = subprocess.Popen(dump.split(), stdout=subprocess.PIPE)
    scad = proc.stdout.read().decode().splitlines()
                 
    #subprocess.call( [ "rm", "{}/tp.grib".format(fold_out) ] )         

    # Unisco i file e
    os.system( "cat {}/{}_membro*.grib > {}/gribbone_{}.grib".format( fold_out, cumulate, fold_out, cumulate ) )

    for iscad in scad:
        grib_copy = "grib_copy -w endStep={} {}/gribbone_{}.grib {}/{}_scad{}.grib".format( iscad, fold_out, cumulate, fold_out, cumulate, iscad)
        subprocess.call(grib_copy.split(), shell=False)
                  
    # Elimino i file intermedi di supporto
    tobedeleted = glob.glob( "{}/tp*_membro*.grib".format(fold_out) ) 
    for f in tobedeleted:
        os.remove(f)
        
    #------------------------------------------------------
    # Estrazione e cumulazione dei campi di precipitazione 
    #------------------------------------------------------
    for iscad in scad:
    
        # Open the plot
        fig = plt.figure(figsize=(8,8), dpi=150)
        ax = plt.axes(projection=ccrs.PlateCarree())
        ax.set_extent( [ 6., 20., 35., 48.], ccrs.PlateCarree() )
        font = {'size': 10}
        plt.rc('font', **font)
       
        colori = (["blue", "darkorange", "green", "gold", "red", 
                   "purple", "cyan", "lawngreen", "pink", "olive", 
                   "lightseagreen", "brown", "orchid", "lightskyblue", "yellow",
                   "coral", "darkviolet", "steelblue", "orange", "turquoise"])
        n = 0
          
        fname = "{}/{}_scad{}.grib".format( fold_out, cumulate, iscad )
        print("Lettura file:", fname)
        grbs   = pygrib.open(fname)
        
        for grb in grbs:
              
            variabile, data, ora, Ni, Nj, startstep, endstep, thresh, medi, perc, cumstep = get_var_grib(grb)
            # Save the selected data
            dati = grb.values

            # Al primo record calcolo il grigliato
            if n == 0:
                # -----------------------------------
                # CREAZIONE GRIGLIA E RITAGLIO
                # dal programma di Thomas
                # -----------------------------------
                # Se la griglia non è "rotated_ll" o "regular_ll",
                # non so gestirla quindi esco 
                if not (grb.gridType == "rotated_ll"  or  grb.gridType == "regular_ll"):
                    sys.exit("\nERRORE! La griglia %s non è gestita\n"%grb.gridType)
                        
                # Ricavo latitudine e longirudine del primo e dell'ultimo punto
                # della griglia e controllo la consistenza dei valori di
                # longitudine
                lat_first_point=float(grb['latitudeOfFirstGridPointInDegrees'])
                lon_first_point=float(grb['longitudeOfFirstGridPointInDegrees'])
                lat_last_point=float(grb['latitudeOfLastGridPointInDegrees'])
                lon_last_point=float(grb['longitudeOfLastGridPointInDegrees'])
                if lon_first_point>lon_last_point:
                    lon_first_point=lon_first_point-360
                    
                # Ricavo longitudini e latitudini
                lons=np.linspace(lon_first_point,lon_last_point,Ni)
                lats=np.linspace(lat_first_point,lat_last_point,Nj)
                grid_lon,grid_lat=np.meshgrid(lons,lats)

                # Se la griglia è "rotated_ll", la ruoto
                if grb.gridType=="rotated_ll":
                    SP=[float(grb['longitudeOfSouthernPoleInDegrees']),
                        float(grb['latitudeOfSouthernPoleInDegrees'])]
                    grid_lon,grid_lat=rotated_grid_transform(grid_lon,grid_lat,0,SP)

                # Controllo che la griglia sia a posto
                if ((np.shape(dati) != np.shape(grid_lon)) or \
                    (np.shape(dati) != np.shape(grid_lat))):
                    sys.exit("ERRORE! La griglia spaziale non coincide con quella della variabile!")

            # Apply a mask to compute the paintballs
            tp_paintballs = np.zeros( np.shape(dati) )
            tp_paintballs = np.where( dati >= threshold, 1, 0 )
            
            # Plot the paintballs
            #ax.scatter( grid_lon[tp_paintballs == 1], grid_lat[tp_paintballs == 1], tp_paintballs[tp_paintballs == 1], alpha=0.2 )
            pcm = ax.contourf( grid_lon, grid_lat, tp_paintballs,levels=[0.5,1], alpha=0.2, colors=colori[n] )
                    
            # Geografia
            ax.coastlines( resolution='10m' )
            ax.add_feature( cfeature.NaturalEarthFeature(category='cultural', 
                                                         name='admin_0_boundary_lines_land',
                                                         scale='10m'),
                            edgecolor='k', facecolor='none', zorder=1)
            ax.add_feature( cartopy.feature.LAKES.with_scale('10m'), edgecolor='k',
                            facecolor='none', zorder=1 )
            # Shapefile regioni
            ax.add_feature( shape_feature, edgecolor='black', facecolor="None", linewidth=0.8,
                            alpha=0.4, zorder=3 )
            
            n=n+1
            
        # Titolo
        var_tit = "Ensemble member paintballs precipitation > {} mm".format(threshold)
        sep = '\n'
        inizio   = datetime.strptime( data+ora, "%Y%m%d%H%M" )
        data_in  = datetime.strptime( data+ora, "%Y%m%d%H%M" ) + timedelta( hours=int(startstep) )
        data_fin = datetime.strptime( data+ora, "%Y%m%d%H%M" ) + timedelta( hours=int(endstep) )
        title="{} {}{} - {}".format( var_tit, sep,
                                     data_in.strftime("%Y/%m/%d %H:%M"),
                                     data_fin.strftime("%Y/%m/%d %H:%M") )
        ax.set_title( title, loc='left' )
            
        # Salvataggio output - ITALIA
        fileout = "{}/MAP_FC_LENS_PR_0_TPPC_GRND_NULL_NULL_NULL_NULL_{}_{}_{}_{}_paintball_italia.png".format( fold_out, inizio.strftime('%Y%m%d%H'), data_fin.strftime('%Y%m%d%H'), f"{int(cumstep):03d}", f"{int(endstep):03d}" )
        plt.savefig( fileout, bbox_inches='tight' )

        #---------------------------------------------------
        # Definizione e ciclo sui sottodomini
        #---------------------------------------------------
        zoom = {
            "nord" : [ 6., 15., 43., 48.5 ],
            "centro" : [ 7.5, 15., 38.5, 44.7 ],
            "sud" : [ 11.5, 19.5, 35., 42.5 ],
        }
                
        for key in zoom:
            ax.set_extent( zoom[key], ccrs.PlateCarree() )
                    
            # Salvataggio output - SOTTODOMINI
            fileout = "{}/MAP_FC_LENS_PR_0_TPPC_GRND_NULL_NULL_NULL_NULL_{}_{}_{}_{}_paintball_{}.png".format( fold_out, inizio.strftime('%Y%m%d%H'), data_fin.strftime('%Y%m%d%H'), f"{int(cumstep):03d}", f"{int(endstep):03d}", key )      
            plt.savefig( fileout, bbox_inches='tight' )

        #---------------------------------------------------
        # Subarea Emilia-Romagna
        #---------------------------------------------------            
        er = [ 9., 13., 43., 46. ]
        shape_er = ShapelyFeature(Reader(shp_reg).geometries(),
                                  ccrs.PlateCarree(), edgecolor='black')
        
        ax.set_extent( er, ccrs.PlateCarree() )
        ax.add_feature( shape_er, edgecolor='black', facecolor="None", linewidth=0.4,
                        alpha=0.5, zorder=2 )
                           
        # Nome file output        
        fileout = "{}/MAP_FC_LENS_PR_0_TPPC_GRND_NULL_NULL_NULL_NULL_{}_{}_{}_{}_paintball_emro.png".format( fold_out, inizio.strftime('%Y%m%d%H'), data_fin.strftime('%Y%m%d%H'), f"{int(cumstep):03d}", f"{int(endstep):03d}" )
        plt.savefig(fileout,bbox_inches='tight')
        
        plt.close()
    
    # Rimozione dei file grib nella directory di output utilizzati per la produzione delle immagini
    for fname in glob.glob( "{}/gribbone_{}.grib".format( fold_out, cumulate ) ): 
        os.remove(fname) 
    for fname in glob.glob( "{}/{}_scad*.grib".format( fold_out, cumulate ) ):
        os.remove(fname)
    

quit()
