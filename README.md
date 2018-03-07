# Minimalizacia deterministickeho konecneho automatu
### autor: Bc. Adam Bezák xbezak01@stud.fit.vutbr.cz

Program sa spusta ako

**dka-2-mka [volby] [vstup]**

**volby**
* **-i** Overenie zda sa jedna o DKA a v pripade spravne zadaneho DKA sa ulozi a znovu vypise na standarny vystup v pozadovanom tvare.
* **-t** Minimalizacia vstupneho DKA a vypisanie na standartny vystup v pozadovanom tvare.

**vstup**
* V pripade, ze nie je specifikovany vstupny subor tak sa berie ako vstup standartni vstup stdin

Naimplementovane riesenie overuje spravnost zadanych argumentov prikazoveho riadku. 
* Nie je mozne nastavit viacej ako **jeden** vstupny subor.
* Je nutne specifikovat aspon jeden vstupny parameter.

Taktiez overuje spravnost zadaneho vstupneho DKA.
* Spravne zadany format..
* Zda sa pouzite stavy v pravidlach, koncovych stavoch a startovaci stav nachadzaju v mnozine vsetkych stavov.
* Zda automat prijma len abecedu zlozenu z malych pismen [a-z]

Algoritmus minimalizacie funguje presne podla algortimu rieseneho na cviceni k predmetu STI.
V prvom kroku sa rucne vytvoria dve ekvivalencne triedy zlozene z mnozin koncovych stavov a vsetkych ostatnych stavov.
Nainicializuju sa prvotne "bunky tabulky" - pre kazdy znak z abecedy a kazdy prechod sa vytvori jedna `CellTransition` pozostavajuca zo startovacieho stavu, daneho znaku a cisla ekvivalencnej triedy v 
ktorej lezi koncovy stav prechodu.
Po nainicializovani prvotnych ekvivalencnych tried sa zacne rekurzivne prechadzat kazda ekv. trieda a zistuje sa zda sa musi rozdelit. Rozdelenie spociva v tom, ze z danej ekv. triedy
sa odstrania tie stavy ktore sa presuvaju do novej ekv. triedy a nova ekv. trieda sa prida na koniec listu ekv. tried. Po rozdeleni je nutne znovu prepocitat hodnoty `CellTransition` (buniek tabulky).
Funkcia sa rekurzivne vola az pokedy uz nie je co rozdelovat.
