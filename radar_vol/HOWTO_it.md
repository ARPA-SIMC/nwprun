## prove arkimet volumi radar ##

### Creazione dei dataset ###

Creo un dataset `radar_vol` con file di configurazione `config`:

```
filter = product:ODIMH5,PVOL,SCAN
name = radar_vol
replace = true
step = daily
type = iseg
format = odimh5
unique = reftime, area, product, origin
index = reftime, origin
description = Radar polar volumes
restrict = smr-er
delete age = 30
```

devo creare anche il dataset error con il seguente `config`:

```
name = error
step = daily
type = error
remote import = yes
delete age = 3
```

ora creo il file di configurazione comune da usare in fase di
importazione (arki-scan):

```
arki-mergeconf radar_vol/ error/ > config
```

### Importazione dei file ###

Avendo suffisso `hdf`, non riconosciuto da arkimet, devo prefissare
con `odim:`:

```
arki-scan --yaml --dispatch=./config `for f in *.hdf; do echo "odim:$f"; done`
```

### Estrazione dei dati ###

Faccio un summary preventivo per vedere quante "origin" diverse ho:

```
arki-query --summary --yaml '' radar_vol/|grep '^ *Origin:'|sort -u
```

estrazione singola:

```
arki-query --data -o tibecco.hdf \
 "reftime:=2020-03-05 14:50; origin:ODIMH5,16107;" radar_vol
```

estrazione multipla:

```
RADLIST="16082 16092 16101 16102 16103 16105 16106 16107 16112 16144 16199 16905 16907 16996 16997 16998 16999"
for r in $RADLIST; do
  arki-query --data -o radar_$r.hdf \
 "reftime:=2020-03-05 14:50; origin:ODIMH5,$r;" radar_vol
done
```

### Separazione dei dati estratti in diversi file ###

Se la stringa di query restituisce più file, questi saranno
concatenati fra loro in un nuico file e quindi poco utilizzabili.

In teoria per separare i diversi hdf5 che escono appiccicati in
un'unica estrazione si potrebbero usare gli strumenti di radarlib, ma
Pier Paolo ha provato e, su questi dati, sono falliti subito con
errori variopinti.

Alternativamente, solo per splittare per reftime, si può anche usare
`arki-xargs`, ad esempio:

```
RADLIST="16082 16092 16101 16102 16103 16105 16106 16107 16112 16144 16199 16905 16907 16996 16997 16998 16999"
for r in $RADLIST; do
  arki-query --inline
 "reftime:>2020-03-05 14:00,<=2020-03-05 15:00; origin:ODIMH5,$r;" radar_vol | \
 arki-xargs --time-interval=minute ./split.sh $r
done
```

(notare `--inline`, importante) dove `split.sh` è il seguente script:

```
#!/bin/sh
datestring=`date --date="$ARKI_XARGS_TIME_START" "+%Y%m%d%H%M"`
cat $2 > ${1}_${datestring}.hdf
```
