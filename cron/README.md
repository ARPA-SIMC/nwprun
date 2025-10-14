# cron directory #

Questa cartella contiene una serie di script progettati per girare in
modalità cron, senza necessariamente la supervisione di ecflow.

## script get_common ##

La maggior parte degli script effettua scaricamento dati e si appoggia
al framework comune get_common, costituito dal modulo di shell
`get_common_ng.sh`, oltre che agli script del pacchetto
nwpconf. Get_common fornisce una funzione principale `main_loop` che
esegue le seguenti operazioni:

 * carica una configurazione specifica `prod/$CONF_PREFIX$PROCNAME` nel
   ramo `nwprun/conf/`
 * evita l'esecuzione concorrente di diverse istanze dello script
 * ridireziona stdout e stderr su un file di log `$LOGDIR/`basename
   $0`
 * gestisce i segnali kill e hangup, in particolare:
   * nel caso di `kill <pid>` o ctrl-c lo script non esce immediatamente
     ma in un momento "di tranquillità" per evitare perdite di dati
   * nel caso di `kill -HUP <pid>` riavvia lo script ricaricando la
     configurazione, anche qui evitando perdite di dati
 * ripulisce, ricrea ed entra nella cartella `$WORKDIR` definita in
   `npwrun/conf/conf.sh` (normalmente termina con la configurazione
   specifica dello script `prod/$CONF_PREFIX$PROCNAME`)
 * ricarica dal ramo specifico di configurazione lo stato precedente
   dello script (tipicamente dal file
   `prod/$CONF_PREFIX$PROCNAME`/$PROCNAME.state` recupera la data
   dell'ultimo reference time scaricato)
 * incrementa la data di `$PROC_STEP` secondi (da definire nella
   configurazione specifica dello script `conf.sh`)
 * attende l'istante specificato in `conf.sh` rispetto a data e ora
   di riferimento (se `DAEMON=Y`), oppure esce se l'istante specificato
   non è ancora giunto
 * inizia il loop di attesa e scaricamento dei dati, la funzione `get_one`
   (v. sotto) viene ripetutamente richiamata finché essa non dichiara che
   tutti i dati sono stati scaricati o finché non è passato il tempo massimo
   consentito per lo scaricamento
 * aggiorna lo stato dello script nel ramo di configurazione indipendetemente
   dal successo o meno dello scaricamento
 * Passa all'istante successivo (+`$PROC_STEP`) e ricomincia il ciclo
   di lavoro.

Get_common fornisce inoltre una funzione `log` che invia in stdout
(ridirezionato su file log) una stringa di log corredata di data e ora
e numero identificativo del processo.

Le funzioni rilevanti chiamate da `main_loop` e che possono essere
definite nello script dell'utente sono:

`get_init`: viene chiamata prima di fare il sourcing della
configurazione; è il punto consigliato per definire le variabili
`$PROCNAME` e, opzionalmente, `$CONF_PREFIX` che definiscono il
percorso di configurazione; `$CONF_PREFIX` è tipicamente un prefisso,
terminante con `/` che riunisce diversi script con diverso
`$PROCNAME`.

`get_post`: viene chiamata subito dopo il sourcing della
configurazione, è opzionale.

`get_setup`: viene chiamata dopo la fase di "demonizzazione"
(ridirezionamento output in log, cambio di directory di lavoro,
definizione di data e ora di riferimento), nel caso di loop infinito
(`DAEMON=Y`) viene richiamata ad ogni iterazione sulle date.

`get_one`: viene chiamata una o più volte per scaricare i dati (è a
scelta dell'utente se scaricare tutto in un'unica chiamata o un
sottoinsieme di dati per chiamata), deve impostare `retval=0` per
indicate a `main_loop` che tutti i dati sono stati scaricati e quindi
si può passara all'istante di riferimento successivo, `retval=1` per
indicare che non tutti i dati sono stati scaricati, per cui
`main_loop` deve effettuare un ciclo di attesa e richiamare `get_one`
se il tempo totale di attesa non è scaduto`, `getval=2` per indicare
che il dato corrente non è disponibile o completo ma esiste un dato
successivo. La funzione dovrebbe gestire gli errori in sé stessa e
nelle sottofunzioni con le modalità indicate nell'esempio.

`get_cleanup`: viene eseguita al termine dello scaricamento di un
singolo istante, terminato con o senza successo.


### Anatomia di uno script ###

Uno script che usa il modulo get_common deve fare il source del
module, definire una serie di funzioni standard e al termine invocare
la funzione `main_loop`.


```
#!/bin/bash

# source common get_ procedures
. `dirname $0`/get_common_ng.sh

# define custom functions
get_init() {
    export PROCNAME=very_useful_download_script
}

get_setup() {
    putarki_configured_setup $PROCNAME "reftime=$DATE$TIME" "format=bufr" "signa
l=gts_bufr"
}

get_cleanup() {
    putarki_configured_end $PROCNAME
}

get_one() {

# qui c'è la maggior parte della logica dello script

}

# enter main loop
main_loop "$@"
```


## script multi_importer_common ##
