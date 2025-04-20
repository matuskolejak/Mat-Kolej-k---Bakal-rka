# Generátor úloh lineárnych transformácií (Bakalárska práca)

Tento repozitár obsahuje zdrojové kódy k bakalárskej práci "Generátor úloh lineárnych transformácií pre vzdelávacie účely" (FIIT STU, 2024/2025). Cieľom práce bolo vytvoriť interaktívne nástroje v prostredí Wolfram Mathematica, ktoré pomáhajú študentom pochopiť lineárne geometrické transformácie (posun, rotácia, škálovanie, symetria, skosenie) prepojením algebraického postupu (matice) a vizuálnej reprezentácie.

## Obsah repozitára

* **Hlavné balíky (`*.wl`)**: Súbory ako `TrojuholnikHardBalik.wl`, `StvorecHardBalik.wl`, `PriamkaHardBalik.wl`, `BodHardBalik.wl`, `DomcekHardBalik.wl` [source: STU_FIIT_Bachelor_Thesis_Matus_Kolejak_NEW (2).pdf: 154] obsahujú hlavnú logiku pre generovanie úloh pre daný geometrický útvar.
* **Adresár `Transforms/`**: Obsahuje samostatné moduly (`.wl`) pre jednotlivé transformácie (napr. `Posun.wl`, `Rotacia.wl` [source: STU_FIIT_Bachelor_Thesis_Matus_Kolejak_NEW (2).pdf: 155]), ktoré sú volané hlavnými balíkmi.
* **Ukážkový notebook (`testhard.nb`)**: Jednoduchý Mathematica notebook pre rýchle spustenie a otestovanie funkcie generátora (napr. pre trojuholník) [source: 6, 8, 21].
* **Používateľská príručka (`Spustenie_Balikov_Tutorial.pdf`)**: Stručný návod na inštaláciu, načítanie a použitie balíkov [source: 1].

## Požiadavky

* Nainštalovaná aplikácia Wolfram Mathematica (testované na verzii [uveď verziu, ak vieš, inak vynechaj]).

## Inštalácia a príprava

1.  Stiahnite alebo naklonujte obsah tohto repozitára.
2.  Umiestnite adresár obsahujúci balík (napr. `TrojuholnikHardBalik`) na miesto, odkiaľ ho budete v Mathematice používať [source: 4]. Adresár musí obsahovať hlavný `.wl` súbor (napr. `TrojuholnikHardBalik.wl`) a podadresár `Transforms` so všetkými transformačnými `.wl` súbormi [source: 5].

## Použitie

**DÔLEŽITÉ:** Pred prvým použitím alebo spustením `testhard.nb` je potrebné **manuálne načítať všetky `.wl` balíky** v Mathematice:

1.  Otvorte postupne **každý** `.wl` súbor z adresára `Transforms/` v Mathematice.
2.  V každom otvorenom súbore kliknite na tlačidlo **"Run Package"** (alebo ekvivalent vo vašej verzii) [source: 7, 12].
3.  Nakoniec otvorte aj hlavný `.wl` balík (napr. `TrojuholnikHardBalik.wl`) a takisto kliknite na **"Run Package"** [source: 8].

Po úspešnom načítaní všetkých balíkov môžete:

* Otvoriť `testhard.nb` a spustiť príkaz v ňom (napr. `TrojuholnikTrojitaTransformacia[]`) [source: 9, 18].
* Alebo priamo v inom Mathematica notebooku volať funkcie z balíka (napr. `TrojuholnikTrojitaTransformacia[]`). Funkcia následne zobrazí dialógové okná pre výber transformácií a vygeneruje vizualizáciu spolu s algebraickým postupom [source: 10, 20].

Podobné funkcie existujú aj pre ostatné tvary (napr. `StvorecTrojitaTransformacia[]` atď.).

## Bakalárska práca

Tento softvér bol vytvorený ako súčasť bakalárskej práce na Fakulte informatiky a informačných technológií STU v Bratislave.
* [Odkaz na text práce v CRZP alebo univerzitnom repozitári - ak existuje, vlož sem]

## Autor

* **Bc. Matúš Koleják**

## Licencia

Kód je poskytnutý bez špecifickej licencie pre účely náhľadu v rámci bakalárskej práce.
