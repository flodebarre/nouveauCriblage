# nouveauCriblage

## Avec les données ouvertes

Les données de criblage sont enfin disponibles sur [data.gouv.fr](https://www.data.gouv.fr/fr/datasets/donnees-de-laboratoires-pour-le-depistage-indicateurs-sur-les-mutations/).

Le code pour tracer les figures est [ici](https://github.com/flodebarre/nouveauCriblage/blob/main/scripts/mutations.Rmd).

La sortie HTML permettant de voir les figures est [là](https://htmlpreview.github.io/?https://raw.githubusercontent.com/flodebarre/nouveauCriblage/main/scripts/mutations.html). 

## Avant les données ouvertes

### Motivation

Les open data du nouveau criblage ne sont toujours pas disponibles. Selon Guillaume Rozier qui a [interpelé Olivier Véran](https://twitter.com/GuillaumeRozier/status/1409766713076428800?s=20) sur Twitter, les données devraient [arriver jeudi](https://twitter.com/GuillaumeRozier/status/1409806585078165504?s=20).
En attendant, des données sont publiées sur la [page variants](https://www.santepubliquefrance.fr/dossiers/coronavirus-covid-19/coronavirus-circulation-des-variants-du-sars-cov-2#block-270756) de Santé Publique France. La page est sauvegardée sur [WayBack Machine](http://web.archive.org/web/*/https://www.santepubliquefrance.fr/dossiers/coronavirus-covid-19/coronavirus-circulation-des-variants-du-sars-cov-2), et on peut donc retrouver les données des jours précédents.

Les voici donc regroupées dans un [fichier .csv](https://github.com/flodebarre/nouveauCriblage/blob/main/data/nouveauCriblage.csv), en attendant les open data

### Format des données

Column name | Content |
--- | --- |
location | Echelle géographique (si on a le courage d'entrer les données régionales)|
nbCriblage | Nombre de résultats de criblage saisis en nouvelle nomenclature |
propTest | Proportion des tests (TAG et PCR) positifs qui sont criblés |
nbXXX | Nombre de résultats positifs pour la mutation XXX |
pXXX | Pourcentage parmi les PCR criblées en nouvelle nomenclature où la mutation XXX est recherchée |
nbE484KandL452R	| Nombre de résultats où les deux mutations sont recherchées (NA) |
source | source of the data |

### Crédits

@rozieres pour l'idée d'utiliser WayBack Machine  
@touna14 pour les données 8-14 juin
