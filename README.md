# Customizable dashboard

Dit is een prototype voor een dashboard met customizable widgets.

## Instructies

Packages:

- ggplot2
- lgrdata
- shiny
- dplyr
- glue
- shinyjs
- shinyjqui

Usage:

1. Run app.
1. Selecteer variabelen, groep variabele, plot type, labels etc., en klik 'Add Plot'. Herhaal voor meer plots.
1. Widgets kunnen met *drag and drop* gerangschikt worden.
1. Om een bestaande plot te **editen**, klik op de plot (de controls voor die plot worden in het menu ge-update). Stel nieuwe controls in, en klik dan op het 'Edit' knopje (icoon) op het plotje (niet op Add Plot!). 
1. Om bestaande dashboard op te slaan, voer een naam in en klik 'Opslaan'.
1. Dashboard compleet wissen met 'Wissen' knopje.
1. Selecteer opgeslagen dashboard en klik 'Laden' om dashboard te laden.

## To do

- Dashboards worden niet opgeslagen, alleen voor 1 sessie.
- Betere UI met editen (knopje 'Update' in menu erboven, ipv. klikken op edit knop).
- Omzetten naar `shinydashboard`, met 1 pagina de widgets zonder edit knopjes etc., en 1 pagina 'Admin' waar widgets worden toegevoegd etc.
- Meer plot mogelijkheden.
- Selecteer eerst plottype, bijbehorende menuutjes worden dan toegevoegd (andere settings voor scatter plots en barplots, bijvoorbeeld).
- Gebruik van *global variables* om plot settings op te slaan, vervangen door database.
- Opslaan dashboard onthoudt niet de volgorde van de widgets als deze handmatig zijn verplaatst (hiervoor moet een JS functie geschreven worden die de locaties van de widgets bepaalt).




