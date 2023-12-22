&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
*/
&Scoped-define WINDOW-NAME w-livre
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS w-livre 
{include/i-prgvrs.i prm1203 1.00.00.000}

/* Chamada a include do gerenciador de licenáas. Necessario alterar os parametros */
/*                                                                                */
/* <programa>:  Informar qual o nome do programa.                                 */
/* <m¢dulo>:  Informar qual o m¢dulo a qual o programa pertence.                  */

&IF "{&EMSFND_VERSION}" >= "1.00" &THEN
    {include/i-license-manager.i prm1203 MFT}
&ENDIF

/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */
{prmp/prm1203.i}

  

{cdp/cd0666.i} /* tt-erro */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE w-livre
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER WINDOW

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME f-cad
&Scoped-define BROWSE-NAME br-ped-venda

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES tt-ped-item-prm

/* Definitions for BROWSE br-ped-venda                                  */
&Scoped-define FIELDS-IN-QUERY-br-ped-venda tt-ped-item-prm.sel tt-ped-item-prm.it-codigo /*Willian Santana 26/01/2023 - Inclus∆o item do cliente*/ tt-ped-item-prm.item-do-cli /*Fim - Willian Santana*/ tt-ped-item-prm.descricao tt-ped-item-prm.nr-sequencia tt-ped-item-prm.nome-abrev tt-ped-item-prm.nr-pedcli getDescSitItem(INPUT tt-ped-item-prm.cod-sit-item) // Gustavo Alfredo 11/08/2022 - Incluindo n£mero da ordem de prod tt-ped-item-prm.nr-ord-producao /*-Data Faturamento - Vitor Hugo-*/ tt-ped-item-prm.data-faturamento //-------- Fim alteraá∆o - Gustavo Alfredo --------------------------- tt-ped-item-prm.dt-implant tt-ped-item-prm.dt-entorig tt-ped-item-prm.dt-entrega tt-ped-item-prm.dt-planeja tt-ped-item-prm.fm-codigo tt-ped-item-prm.ge-codigo tt-ped-item-prm.fm-cod-com // Gustavo Alfredo 11/08/2022 - Incluindo valor unit†rio e valor total tt-ped-item-prm.vl-preuni tt-ped-item-prm.vl-liq-abe //-------- Fim alteraá∆o - Gustavo Alfredo --------------------------- tt-ped-item-prm.qt-a-atender tt-ped-item-prm.peso-bruto tt-ped-item-prm.compra-material // Vitor Hugo 15/12/2022 - Incluindo saldo item tt-ped-item-prm.saldo-item   
&Scoped-define ENABLED-FIELDS-IN-QUERY-br-ped-venda tt-ped-item-prm.compra-material   
&Scoped-define ENABLED-TABLES-IN-QUERY-br-ped-venda tt-ped-item-prm
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-br-ped-venda tt-ped-item-prm
&Scoped-define SELF-NAME br-ped-venda
&Scoped-define QUERY-STRING-br-ped-venda FOR EACH tt-ped-item-prm NO-LOCK BY tt-ped-item-prm.dt-implant                                                              BY tt-ped-item-prm.nr-pedido                                                              BY tt-ped-item-prm.nr-sequencia
&Scoped-define OPEN-QUERY-br-ped-venda OPEN QUERY br-ped-venda FOR EACH tt-ped-item-prm NO-LOCK BY tt-ped-item-prm.dt-implant                                                              BY tt-ped-item-prm.nr-pedido                                                              BY tt-ped-item-prm.nr-sequencia.
&Scoped-define TABLES-IN-QUERY-br-ped-venda tt-ped-item-prm
&Scoped-define FIRST-TABLE-IN-QUERY-br-ped-venda tt-ped-item-prm


/* Definitions for FRAME f-cad                                          */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS rt-button RECT-5 bt-filtro bt-consulta ~
fi-dt-planeja br-ped-venda bt-todos bt-nenhum bt-dividir-item 
&Scoped-Define DISPLAYED-OBJECTS fi-dt-planeja fi-tot-cilind fi-tot-valor 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fn-qtd-data w-livre 
FUNCTION fn-qtd-data RETURNS DECIMAL
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getDescSitItem w-livre 
FUNCTION getDescSitItem RETURNS CHARACTER(INPUT situacao AS INTEGER)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR w-livre AS WIDGET-HANDLE NO-UNDO.

/* Menu Definitions                                                     */
DEFINE SUB-MENU mi-programa 
       MENU-ITEM mi-consultas   LABEL "Co&nsultas"     ACCELERATOR "CTRL-L"
       MENU-ITEM mi-imprimir    LABEL "&Relat¢rios"    ACCELERATOR "CTRL-P"
       RULE
       MENU-ITEM mi-sair        LABEL "&Sair"          ACCELERATOR "CTRL-X".

DEFINE SUB-MENU m_Ajuda 
       MENU-ITEM mi-conteudo    LABEL "&Conteudo"     
       MENU-ITEM mi-sobre       LABEL "&Sobre..."     .

DEFINE MENU m-livre MENUBAR
       SUB-MENU  mi-programa    LABEL "&prm1203"      
       SUB-MENU  m_Ajuda        LABEL "&Ajuda"        .


/* Definitions of handles for SmartObjects                              */
DEFINE VARIABLE h_p-exihel AS HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON bt-atualizar 
     LABEL "Atualizar Planejamento" 
     SIZE 18.43 BY 1.13 TOOLTIP "Somente Grupo Usu†rios PCP tem permiss∆o de Atualizar Planejamento".

DEFINE BUTTON bt-chek-compra 
     IMAGE-UP FILE "image/toolbar/im-chck1.bmp":U
     LABEL "Atualizar Compra" 
     SIZE 5 BY 1.13 TOOLTIP "Atualizar".

DEFINE BUTTON bt-consulta 
     IMAGE-UP FILE "image/toolbar/im-ordrh.bmp":U
     LABEL "Consultar" 
     SIZE 5 BY 1.13 TOOLTIP "Atualizar".

DEFINE BUTTON bt-dividir-item 
     LABEL "Dividir Item" 
     SIZE 15 BY 1.13.

DEFINE BUTTON bt-filtro 
     IMAGE-UP FILE "image/toolbar/im-fil.bmp":U
     LABEL "Filtro" 
     SIZE 5 BY 1.13 TOOLTIP "Filtro".

DEFINE BUTTON bt-nenhum 
     LABEL "Nenhum" 
     SIZE 15 BY 1.13.

DEFINE BUTTON bt-todos 
     LABEL "Todos" 
     SIZE 15 BY 1.13.

DEFINE BUTTON bt-todos-comprar 
     LABEL "Comprar Todos" 
     SIZE 15 BY 1.13.

DEFINE VARIABLE fi-dt-planeja AS DATE FORMAT "99/99/9999":U INITIAL 01/01/1900 
     LABEL "Nova Data Entrega" 
     VIEW-AS FILL-IN 
     SIZE 10.29 BY .88 NO-UNDO.

DEFINE VARIABLE fi-tot-cilind AS INTEGER FORMAT ">>>,>>>,>>9":U INITIAL 0 
     LABEL "Total de Cilindros" 
     VIEW-AS FILL-IN 
     SIZE 13.72 BY .88 NO-UNDO.

DEFINE VARIABLE fi-tot-valor AS DECIMAL FORMAT "->>>,>>>,>>9.99":U INITIAL 0 
     LABEL "Valor Total" 
     VIEW-AS FILL-IN 
     SIZE 13.72 BY .88 NO-UNDO.

DEFINE RECTANGLE RECT-5
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 165.72 BY 2.

DEFINE RECTANGLE rt-button
     EDGE-PIXELS 2 GRAPHIC-EDGE    
     SIZE 165.72 BY 1.46
     BGCOLOR 7 .

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY br-ped-venda FOR 
      tt-ped-item-prm SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE br-ped-venda
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS br-ped-venda w-livre _FREEFORM
  QUERY br-ped-venda NO-LOCK DISPLAY
      tt-ped-item-prm.sel           COLUMN-LABEL "Sel"         WIDTH 03  VIEW-AS TOGGLE-BOX 
tt-ped-item-prm.it-codigo           COLUMN-LABEL "Item"        WIDTH 07

/*Willian Santana 26/01/2023 - Inclus∆o item do cliente*/
tt-ped-item-prm.item-do-cli         COLUMN-LABEL "Item do Cliente" WIDTH 12
/*Fim - Willian Santana*/
tt-ped-item-prm.descricao           COLUMN-LABEL "Descriá∆o"   WIDTH 60
tt-ped-item-prm.nr-sequencia        COLUMN-LABEL "Seq."        WIDTH 03
tt-ped-item-prm.nome-abrev          FORMAT "x(12)"             COLUMN-LABEL "Nome Abrev"       WIDTH 12.8
tt-ped-item-prm.nr-pedcli           FORMAT "x(12)"             COLUMN-LABEL "Pedido Cli."      WIDTH 09
getDescSitItem(INPUT tt-ped-item-prm.cod-sit-item) FORMAT "x(20)" COLUMN-LABEL "Situaá∆o Atend."  WIDTH 12
// Gustavo Alfredo 11/08/2022 - Incluindo n£mero da ordem de prod
tt-ped-item-prm.nr-ord-producao     FORMAT ">>>,>>>,>>>"       COLUMN-LABEL "Ordem Prod"

/*-Data Faturamento - Vitor Hugo-*/
tt-ped-item-prm.data-faturamento    FORMAT "99/99/9999"        COLUMN-LABEL "Data Fatur"       WIDTH 10
//-------- Fim alteraá∆o - Gustavo Alfredo ---------------------------
tt-ped-item-prm.dt-implant          FORMAT "99/99/9999"        COLUMN-LABEL "Data Implant"     WIDTH 10
tt-ped-item-prm.dt-entorig          FORMAT "99/99/9999"        COLUMN-LABEL "Dt Entreg Orig"   WIDTH 10
tt-ped-item-prm.dt-entrega          FORMAT "99/99/9999"        COLUMN-LABEL "Data Entrega"     WIDTH 10
tt-ped-item-prm.dt-planeja          FORMAT "99/99/9999"        COLUMN-LABEL "Data Planej."     WIDTH 10
tt-ped-item-prm.fm-codigo           COLUMN-LABEL "Fam°lia Material"  WIDTH 21
tt-ped-item-prm.ge-codigo           COLUMN-LABEL "Grupo Estoque"     WIDTH 21
tt-ped-item-prm.fm-cod-com          COLUMN-LABEL "Fam°lia Comercial" WIDTH 21
// Gustavo Alfredo 11/08/2022 - Incluindo valor unit†rio e valor total
tt-ped-item-prm.vl-preuni           FORMAT ">>>,>>>,>>9.99999" COLUMN-LABEL "Vl Unit†rio(Item)"
tt-ped-item-prm.vl-liq-abe          FORMAT ">>>>,>>>,>>9.99"   COLUMN-LABEL "Vl Total(Item)"
//-------- Fim alteraá∆o - Gustavo Alfredo ---------------------------
tt-ped-item-prm.qt-a-atender        COLUMN-LABEL "Qtde. a Atender"  WIDTH 11
tt-ped-item-prm.peso-bruto          COLUMN-LABEL "Peso Bruto"       WIDTH 09
tt-ped-item-prm.compra-material     COLUMN-LABEL "Oxi."             WIDTH 07 VIEW-AS TOGGLE-BOX
// Vitor Hugo 15/12/2022 - Incluindo saldo item
tt-ped-item-prm.saldo-item          COLUMN-LABEL "Saldo Item"            WIDTH 10  
ENABLE tt-ped-item-prm.compra-material
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ASSIGN NO-ROW-MARKERS SEPARATORS SIZE 165.72 BY 20.29
         FONT 1
         TITLE "Itens dos Pedidos de Venda" ROW-HEIGHT-CHARS .67 FIT-LAST-COLUMN.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME f-cad
     bt-filtro AT ROW 1.5 COL 2.72 WIDGET-ID 2
     bt-consulta AT ROW 1.5 COL 8.14 WIDGET-ID 10
     bt-chek-compra AT ROW 1.5 COL 13.57 WIDGET-ID 80
     fi-dt-planeja AT ROW 3 COL 102.29 COLON-ALIGNED WIDGET-ID 26
     fi-tot-cilind AT ROW 3 COL 127.14 COLON-ALIGNED WIDGET-ID 18
     fi-tot-valor AT ROW 3 COL 150.43 COLON-ALIGNED WIDGET-ID 86
     br-ped-venda AT ROW 4 COL 2.14 WIDGET-ID 200
     bt-todos AT ROW 24.67 COL 3.29 WIDGET-ID 4
     bt-nenhum AT ROW 24.67 COL 18.14 WIDGET-ID 6
     bt-todos-comprar AT ROW 24.67 COL 34.14 WIDGET-ID 84
     bt-dividir-item AT ROW 24.67 COL 83.57 WIDGET-ID 88
     bt-atualizar AT ROW 24.67 COL 147.29 HELP
          "Somente Grupo Usu†rios PCP tem permiss∆o de Atualizar" WIDGET-ID 14
     rt-button AT ROW 1.33 COL 2.14
     RECT-5 AT ROW 24.33 COL 2.14 WIDGET-ID 78
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 168.43 BY 25.71
         FONT 1 WIDGET-ID 100.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: w-livre
   Allow: Basic,Browse,DB-Fields,Smart,Window,Query
   Container Links: 
   Add Fields to: Neither
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW w-livre ASSIGN
         HIDDEN             = YES
         TITLE              = "Template Livre <Insira complemento>"
         HEIGHT             = 25.58
         WIDTH              = 168.72
         MAX-HEIGHT         = 40.5
         MAX-WIDTH          = 274.29
         VIRTUAL-HEIGHT     = 40.5
         VIRTUAL-WIDTH      = 274.29
         RESIZE             = yes
         SCROLL-BARS        = no
         STATUS-AREA        = yes
         BGCOLOR            = ?
         FGCOLOR            = ?
         THREE-D            = yes
         MESSAGE-AREA       = no
         SENSITIVE          = yes.
ELSE {&WINDOW-NAME} = CURRENT-WINDOW.

ASSIGN {&WINDOW-NAME}:MENUBAR    = MENU m-livre:HANDLE.
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB w-livre 
/* ************************* Included-Libraries *********************** */

{src/adm/method/containr.i}
{include/w-livre.i}
{utp/ut-glob.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW w-livre
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME f-cad
   FRAME-NAME L-To-R                                                    */
/* BROWSE-TAB br-ped-venda fi-tot-valor f-cad */
ASSIGN 
       br-ped-venda:ALLOW-COLUMN-SEARCHING IN FRAME f-cad = TRUE
       br-ped-venda:COLUMN-RESIZABLE IN FRAME f-cad       = TRUE.

/* SETTINGS FOR BUTTON bt-atualizar IN FRAME f-cad
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON bt-chek-compra IN FRAME f-cad
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON bt-todos-comprar IN FRAME f-cad
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-tot-cilind IN FRAME f-cad
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-tot-valor IN FRAME f-cad
   NO-ENABLE                                                            */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(w-livre)
THEN w-livre:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE br-ped-venda
/* Query rebuild information for BROWSE br-ped-venda
     _START_FREEFORM
    OPEN QUERY br-ped-venda FOR EACH tt-ped-item-prm NO-LOCK BY tt-ped-item-prm.dt-implant
                                                             BY tt-ped-item-prm.nr-pedido
                                                             BY tt-ped-item-prm.nr-sequencia.
     _END_FREEFORM
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _Query            is NOT OPENED
*/  /* BROWSE br-ped-venda */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME w-livre
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL w-livre w-livre
ON END-ERROR OF w-livre /* Template Livre <Insira complemento> */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL w-livre w-livre
ON WINDOW-CLOSE OF w-livre /* Template Livre <Insira complemento> */
DO:
  /* This ADM code must be left here in order for the SmartWindow
     and its descendents to terminate properly on exit. */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME br-ped-venda
&Scoped-define SELF-NAME br-ped-venda
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br-ped-venda w-livre
ON LEFT-MOUSE-CLICK OF br-ped-venda IN FRAME f-cad /* Itens dos Pedidos de Venda */
DO:
    DEF VAR h-rotulo AS HANDLE NO-UNDO. 
     
    /* handle da coluna selecionada */
    h-rotulo = br-ped-venda:CURRENT-COLUMN IN FRAME {&FRAME-NAME}.
     
    /* verifica se alguma columa foi selecionada*/
    IF VALID-HANDLE (h-rotulo) THEN 
        ASSIGN c-campo = h-rotulo:LABEL.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br-ped-venda w-livre
ON MOUSE-SELECT-DBLCLICK OF br-ped-venda IN FRAME f-cad /* Itens dos Pedidos de Venda */
DO:

    FIND CURRENT tt-ped-item-prm.
    IF AVAIL tt-ped-item-prm THEN DO:
        IF tt-ped-item-prm.sel THEN
            ASSIGN tt-ped-item-prm.sel = NO
                   tt-ped-item-prm.sel:CHECKED IN BROWSE br-ped-venda = NO
                   i-tot-cilind        = i-tot-cilind - INT(tt-ped-item-prm.qt-a-atender)
                   dec-tot-valor       = dec-tot-valor - tt-ped-item-prm.vl-tot-it.
        ELSE
            ASSIGN tt-ped-item-prm.sel = YES
                   tt-ped-item-prm.sel:CHECKED IN BROWSE br-ped-venda = YES
                   i-tot-cilind        = i-tot-cilind + INT(tt-ped-item-prm.qt-a-atender)
                   dec-tot-valor       = dec-tot-valor + tt-ped-item-prm.vl-tot-it.
    END.
    
    ASSIGN fi-tot-cilind:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(i-tot-cilind)
           fi-tot-valor:SCREEN-VALUE IN FRAME {&FRAME-NAME}  = STRING(dec-tot-valor).

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br-ped-venda w-livre
ON ROW-DISPLAY OF br-ped-venda IN FRAME f-cad /* Itens dos Pedidos de Venda */
DO:

/*     DEFINE VARIABLE i-cont AS INTEGER           NO-UNDO.       */
/*     DEFINE VARIABLE i-cor  AS INTEGER INITIAL 0 NO-UNDO.       */
/*                                                                */
/*     IF tt-ped-venda-prm.l-cross THEN                           */
/*         ASSIGN i-cor = 10.                                     */
/*                                                                */
/*     IF i-cor > 0 THEN DO i-cont = 1 TO EXTENT(h-colunas) BY 1: */
/*         //ASSIGN h-colunas[i-cont]:FGCOLOR = i-cor.            */
/*         ASSIGN h-colunas[i-cont]:BGCOLOR = i-cor.              */
/*     END.                                                       */

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br-ped-venda w-livre
ON START-SEARCH OF br-ped-venda IN FRAME f-cad /* Itens dos Pedidos de Venda */
DO:
    
    CASE c-campo:
        WHEN tt-ped-item-prm.it-codigo:LABEL IN BROWSE br-ped-venda THEN
            OPEN QUERY br-ped-venda FOR EACH tt-ped-item-prm NO-LOCK BY tt-ped-item-prm.it-codigo
                                                                     BY tt-ped-item-prm.nr-pedido         
                                                                     BY tt-ped-item-prm.nr-sequencia.
        WHEN tt-ped-item-prm.descricao:LABEL IN BROWSE br-ped-venda THEN
            OPEN QUERY br-ped-venda FOR EACH tt-ped-item-prm NO-LOCK BY tt-ped-item-prm.descricao
                                                                     BY tt-ped-item-prm.nr-pedido         
                                                                     BY tt-ped-item-prm.nr-sequencia.
        WHEN tt-ped-item-prm.nome-abrev:LABEL IN BROWSE br-ped-venda THEN
            OPEN QUERY br-ped-venda FOR EACH tt-ped-item-prm NO-LOCK BY tt-ped-item-prm.nome-abrev
                                                                     BY tt-ped-item-prm.nr-pedido         
                                                                     BY tt-ped-item-prm.nr-sequencia.
        WHEN tt-ped-item-prm.nr-pedcli:LABEL IN BROWSE br-ped-venda THEN
            OPEN QUERY br-ped-venda FOR EACH tt-ped-item-prm NO-LOCK BY tt-ped-item-prm.nr-pedcli
                                                                     BY tt-ped-item-prm.nr-pedido         
                                                                     BY tt-ped-item-prm.nr-sequencia.
        WHEN tt-ped-item-prm.dt-implant:LABEL IN BROWSE br-ped-venda THEN
            OPEN QUERY br-ped-venda FOR EACH tt-ped-item-prm NO-LOCK BY tt-ped-item-prm.dt-implant
                                                                     BY tt-ped-item-prm.nr-pedido         
                                                                     BY tt-ped-item-prm.nr-sequencia.
        WHEN tt-ped-item-prm.dt-entorig:LABEL IN BROWSE br-ped-venda THEN
            OPEN QUERY br-ped-venda FOR EACH tt-ped-item-prm NO-LOCK BY tt-ped-item-prm.dt-entorig
                                                                     BY tt-ped-item-prm.nr-pedido         
                                                                     BY tt-ped-item-prm.nr-sequencia.
        WHEN tt-ped-item-prm.dt-entrega:LABEL IN BROWSE br-ped-venda THEN
            OPEN QUERY br-ped-venda FOR EACH tt-ped-item-prm NO-LOCK BY tt-ped-item-prm.dt-entrega
                                                                     BY tt-ped-item-prm.nr-pedido         
                                                                     BY tt-ped-item-prm.nr-sequencia.
        WHEN tt-ped-item-prm.dt-planeja:LABEL IN BROWSE br-ped-venda THEN
            OPEN QUERY br-ped-venda FOR EACH tt-ped-item-prm NO-LOCK BY tt-ped-item-prm.dt-planeja
                                                                     BY tt-ped-item-prm.nr-pedido         
                                                                     BY tt-ped-item-prm.nr-sequencia.
        WHEN tt-ped-item-prm.fm-codigo:LABEL IN BROWSE br-ped-venda THEN
            OPEN QUERY br-ped-venda FOR EACH tt-ped-item-prm NO-LOCK BY tt-ped-item-prm.fm-codigo
                                                                     BY tt-ped-item-prm.nr-pedido         
                                                                     BY tt-ped-item-prm.nr-sequencia.
    END CASE.  

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-atualizar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-atualizar w-livre
ON CHOOSE OF bt-atualizar IN FRAME f-cad /* Atualizar Planejamento */
DO:

    EMPTY TEMP-TABLE tt-erro.
    ASSIGN i-seq-erro   = 0
           d-dt-planeja = INPUT FRAME {&FRAME-NAME} fi-dt-planeja:INPUT-VALUE.

    IF d-dt-planeja < TODAY THEN DO:
        /* WARNING: 27979 */
        RUN utp/ut-msgs.p (INPUT "Show",
                           INPUT 27979,
                           INPUT "Data Planejamento inv†lida!" + "~~" +
                                 "A Nova Data de Entrega informada Ç inv†lida, a data dever ser maior ou igual a data atual.").
        RETURN NO-APPLY.    
    END.

    IF NOT CAN-FIND(FIRST tt-ped-item-prm WHERE tt-ped-item-prm.sel) THEN DO:
        /* WARNING: 27979 */
        RUN utp/ut-msgs.p (INPUT "Show",
                           INPUT 27979,
                           INPUT "Nenhum Item dos Pedidos de Venda foi selecionado/marcado!" + "~~" +
                                 "Atualizaá∆o da Data de Entrega n∆o permitida, nenhum Item dos Pedidos de Venda foi selecionado.").
        RETURN NO-APPLY.
    END.

    

    FOR EACH calen-prod NO-LOCK
       WHERE calen-prod.data        = INPUT FRAME {&FRAME-NAME} fi-dt-planeja:INPUT-VALUE
         AND calen-prod.tipo-dia    <> 1 .
        RUN utp/ut-msgs.p (INPUT "Show",
                   INPUT 27979,
                   INPUT "Data n∆o Ç dia £til no calend†rio da produá∆o(CD0911)!" + "~~" +
                         "Atualizaá∆o n∆o permitida, altere a data de entrega.").
        RETURN NO-APPLY.    
    END.
        

    /* PERGUNTA: 27100 */
    RUN utp/ut-msgs.p (INPUT "show",
                       INPUT 27100,
                       INPUT "Confirma Atualizaá∆o da Data de Entrega dos Itens?" + "~~" +
                             "Confirmando Atualizaá∆o da Data de Entrega dos Itens.").
    IF RETURN-VALUE = "YES" THEN DO:
        
        FOR EACH tt-ped-item-prm WHERE tt-ped-item-prm.sel:  

            IF d-dt-planeja < tt-ped-item-prm.dt-implant THEN DO:
                CREATE tt-erro.
                ASSIGN tt-erro.cd-erro  = 17006
                       tt-erro.mensagem = "Item: " + tt-ped-item-prm.it-codig + " | Seq: " + STRING(tt-ped-item-prm.nr-sequencia) +
                                          " | Ped Cli: " + tt-ped-item-prm.nr-pedcli + " | Nome Abrev: " + tt-ped-item-prm.nome-abrev +
                                          " |  Data Entrega: " + STRING(d-dt-planeja,"99/99/9999") + 
                                          " menor que a Data Implant.: " + STRING(d-dt-planeja,"99/99/9999") + " do Pedido de Venda."
                       tt-erro.i-sequen = i-seq-erro
                       i-seq-erro       = i-seq-erro + 1.
                NEXT.
            END.

            FIND FIRST prm-ped-item-planej EXCLUSIVE-LOCK
                 WHERE prm-ped-item-planej.nr-pedcli    = tt-ped-item-prm.nr-pedcli 
                   AND prm-ped-item-planej.nome-abrev   = tt-ped-item-prm.nome-abrev
                   AND prm-ped-item-planej.nr-sequencia = tt-ped-item-prm.nr-sequencia
                   AND prm-ped-item-planej.it-codigo    = tt-ped-item-prm.it-codigo   
                   AND prm-ped-item-planej.cod-refer    = tt-ped-item-prm.cod-refer    NO-ERROR.
            IF NOT AVAIL prm-ped-item-planej THEN DO:
                CREATE prm-ped-item-planej.                                    
                ASSIGN prm-ped-item-planej.nr-pedcli       = tt-ped-item-prm.nr-pedcli   
                       prm-ped-item-planej.nome-abrev      = tt-ped-item-prm.nome-abrev  
                       prm-ped-item-planej.nr-sequencia    = tt-ped-item-prm.nr-sequencia
                       prm-ped-item-planej.it-codigo       = tt-ped-item-prm.it-codigo   
                       prm-ped-item-planej.cod-refer       = tt-ped-item-prm.cod-refer
                       prm-ped-item-planej.compra-material = tt-ped-item-prm.compra-material:CHECKED IN BROWSE br-ped-venda.
            END.
            ASSIGN prm-ped-item-planej.dt-planeja      = TODAY.

            FIND FIRST ped-item EXCLUSIVE-LOCK
                 WHERE ped-item.nr-pedcli    = tt-ped-item-prm.nr-pedcli 
                   AND ped-item.nome-abrev   = tt-ped-item-prm.nome-abrev
                   AND ped-item.nr-sequencia = tt-ped-item-prm.nr-sequencia
                   AND ped-item.it-codigo    = tt-ped-item-prm.it-codigo   
                   AND ped-item.cod-refer    = tt-ped-item-prm.cod-refer  NO-ERROR.
            IF AVAIL ped-item THEN DO:                              
                ASSIGN ped-item.dt-entrega = d-dt-planeja.
                FIND FIRST ped-ent OF ped-item EXCLUSIVE-LOCK NO-ERROR.
                IF AVAIL ped-ent THEN
                    ASSIGN ped-ent.dt-entrega = d-dt-planeja.
                RELEASE ped-ent.
            END.
            RELEASE ped-item.
           
        END.       

        RUN pi-tt-ped-venda-prm. 

    END.

    RUN pi-lista-log.
   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-chek-compra
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-chek-compra w-livre
ON CHOOSE OF bt-chek-compra IN FRAME f-cad /* Atualizar Compra */
DO:
   DEFINE VARIABLE h-acomp AS HANDLE NO-UNDO.

    IF NOT CAN-FIND(FIRST tt-ped-item-prm) THEN DO:
        /* WARNING: 27979 */
        RUN utp/ut-msgs.p (INPUT "Show",
                           INPUT 27979,
                           INPUT "N∆o existe nada para ser atualizado!" + "~~" +
                                 "Atualizaá∆o da compra n∆o permitida, tabela pode estar vazia.").
        RETURN NO-APPLY.
    END.

    
    RUN utp/ut-acomp.p PERSISTENT SET h-acomp.
    RUN pi-inicializar IN h-acomp (INPUT "Iniciando").

    FOR EACH tt-ped-item-prm:  

        RUN pi-acompanhar IN h-acomp (INPUT "Processando: " + tt-ped-item-prm.nr-pedcli).

        FIND FIRST prm-ped-item-planej EXCLUSIVE-LOCK
             WHERE prm-ped-item-planej.nr-pedcli    = tt-ped-item-prm.nr-pedcli 
               AND prm-ped-item-planej.nome-abrev   = tt-ped-item-prm.nome-abrev
               AND prm-ped-item-planej.nr-sequencia = tt-ped-item-prm.nr-sequencia
               AND prm-ped-item-planej.it-codigo    = tt-ped-item-prm.it-codigo   
               AND prm-ped-item-planej.cod-refer    = tt-ped-item-prm.cod-refer    NO-ERROR.
        IF NOT AVAIL prm-ped-item-planej THEN DO:
            CREATE prm-ped-item-planej.                                    
            ASSIGN prm-ped-item-planej.nr-pedcli       = tt-ped-item-prm.nr-pedcli   
                   prm-ped-item-planej.nome-abrev      = tt-ped-item-prm.nome-abrev  
                   prm-ped-item-planej.nr-sequencia    = tt-ped-item-prm.nr-sequencia
                   prm-ped-item-planej.it-codigo       = tt-ped-item-prm.it-codigo   
                   prm-ped-item-planej.cod-refer       = tt-ped-item-prm.cod-refer
                   prm-ped-item-planej.compra-material = tt-ped-item-prm.compra-material.
        END.
    
        ASSIGN prm-ped-item-planej.compra-material = tt-ped-item-prm.compra-material.

    END. 

    RUN utp/ut-msgs.p (INPUT "SHOW":U,
                       INPUT 15825,
                       INPUT "Campo de compra atualizado com sucesso!").

    IF VALID-HANDLE(h-acomp) THEN DO:
        DELETE OBJECT h-acomp.
        ASSIGN h-acomp = ?.
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-consulta
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-consulta w-livre
ON CHOOSE OF bt-consulta IN FRAME f-cad /* Consultar */
DO:

    RUN pi-tt-ped-venda-prm. 

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-dividir-item
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-dividir-item w-livre
ON CHOOSE OF bt-dividir-item IN FRAME f-cad /* Dividir Item */
DO:
    DEF VAR i-tot-sel               AS INT INITIAL 0 NO-UNDO.
    DEF VAR rw-tt-ped-item-prm      AS RAW NO-UNDO.
    DEF BUFFER b-tt-ped-item-prm    FOR tt-ped-item-prm.
    
    FOR EACH b-tt-ped-item-prm NO-LOCK WHERE b-tt-ped-item-prm.sel = TRUE:
            ASSIGN i-tot-sel = i-tot-sel + 1.
    END.

    IF i-tot-sel > 1 THEN DO:
        /* WARNING: 27979 */
        RUN utp/ut-msgs.p (INPUT "Show",
                           INPUT 27979,
                           INPUT "Mais de um item selecionado!" + "~~" +
                                 "Divis∆o de Item n∆o permitida, Ç permitido a divis∆o de apenas um item por vez.").
        RETURN NO-APPLY.
    END.
    ELSE DO:
        IF i-tot-sel = 0 THEN DO:
            RUN utp/ut-msgs.p (INPUT "Show",
                               INPUT 27979,
                               INPUT "Nenhum Item dos Pedidos de Venda foi selecionado/marcado!" + "~~" +
                                     "Divis∆o de Item n∆o permitida, nenhum Item dos Pedidos de Venda foi selecionado.").
            RETURN NO-APPLY.
        END.
    END.

/*     IF NOT CAN-FIND(FIRST b-tt-ped-item-prm WHERE b-tt-ped-item-prm.saldo-item > 0) THEN DO:            */
/*         /* WARNING: 27979 */                                                                            */
/*         RUN utp/ut-msgs.p (INPUT "Show",                                                                */
/*                            INPUT 27979,                                                                 */
/*                            INPUT "Item selecionado n∆o possui saldo!" + "~~" +                          */
/*                                  "Divis∆o de Item n∆o permitida, item n∆o possui saldo para divis∆o."). */
/*         RETURN NO-APPLY.                                                                                */
/*     END.                                                                                                */
    
    FIND FIRST tt-ped-item-prm NO-LOCK WHERE tt-ped-item-prm.sel = TRUE NO-ERROR.
   
    RAW-TRANSFER tt-ped-item-prm TO rw-tt-ped-item-prm.

    RUN prmp\prm1203b.w(INPUT rw-tt-ped-item-prm,
                        OUTPUT p-ok).

    IF p-ok = TRUE THEN 
        RUN pi-tt-ped-venda-prm.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-filtro
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-filtro w-livre
ON CHOOSE OF bt-filtro IN FRAME f-cad /* Filtro */
DO:
    RUN prmp\prm1203a.w(INPUT-OUTPUT c-cod-estabel-ini  ,   
                        INPUT-OUTPUT i-cod-emitente-ini ,
                        INPUT-OUTPUT i-cod-emitente-fim ,
                        INPUT-OUTPUT c-nome-abrev-ini   ,
                        INPUT-OUTPUT c-nome-abrev-fim   ,
                        INPUT-OUTPUT i-nr-pedido-ini    ,   
                        INPUT-OUTPUT i-nr-pedido-fim    ,   
                        INPUT-OUTPUT c-nr-pedcli-ini    ,
                        INPUT-OUTPUT c-nr-pedcli-fim    ,
                        INPUT-OUTPUT da-dt-implant-ini  , 
                        INPUT-OUTPUT da-dt-implant-fim  , 
                        INPUT-OUTPUT c-familia-ini      ,
                        INPUT-OUTPUT c-familia-fim      ,
                        INPUT-OUTPUT c-it-codigo-ini    ,
                        INPUT-OUTPUT c-it-codigo-fim    ,
                        INPUT-OUTPUT da-dt-entrega-ini  , 
                        INPUT-OUTPUT da-dt-entrega-fim  , 
                        INPUT-OUTPUT c-it-cli-ini       ,
                        INPUT-OUTPUT c-it-cli-fim       ,
                        INPUT-OUTPUT i-ge-codigo-ini    , 
                        INPUT-OUTPUT i-ge-codigo-fim    , 
                        INPUT-OUTPUT c-fm-cod-com-ini   ,
                        INPUT-OUTPUT c-fm-cod-com-fim   ,
                        INPUT-OUTPUT tg-itens-faturados ,
                        INPUT-OUTPUT p-fi-ini-dt-fatur  ,
                        INPUT-OUTPUT p-fi-fim-dt-fatur  ,
                        OUTPUT p-ok).
    IF p-ok = TRUE THEN 
        RUN pi-tt-ped-venda-prm.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-nenhum
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-nenhum w-livre
ON CHOOSE OF bt-nenhum IN FRAME f-cad /* Nenhum */
DO:
    IF CAN-FIND(FIRST tt-ped-item-prm) THEN DO:
        FOR EACH tt-ped-item-prm:
            ASSIGN tt-ped-item-prm.sel = FALSE.
        END.
        br-ped-venda:REFRESH().
    END.
    ASSIGN i-tot-cilind = 0.
    ASSIGN fi-tot-cilind:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(i-tot-cilind).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-todos
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-todos w-livre
ON CHOOSE OF bt-todos IN FRAME f-cad /* Todos */
DO:
    ASSIGN i-tot-cilind = 0.
    IF CAN-FIND(FIRST tt-ped-item-prm) THEN DO:
        FOR EACH tt-ped-item-prm:
            ASSIGN tt-ped-item-prm.sel = TRUE
                   i-tot-cilind        = i-tot-cilind + INT(tt-ped-item-prm.qt-a-atender).
        END.
        br-ped-venda:REFRESH().
    END.
    ASSIGN fi-tot-cilind:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(i-tot-cilind).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-todos-comprar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-todos-comprar w-livre
ON CHOOSE OF bt-todos-comprar IN FRAME f-cad /* Comprar Todos */
DO:
    
    IF CAN-FIND(FIRST tt-ped-item-prm) THEN DO:
        FOR EACH tt-ped-item-prm:
            ASSIGN tt-ped-item-prm.compra-material = TRUE.
        END.
        br-ped-venda:REFRESH().
    END.
    
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-dt-planeja
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-dt-planeja w-livre
ON LEAVE OF fi-dt-planeja IN FRAME f-cad /* Nova Data Entrega */
DO:
    FIND FIRST usuar_grp_usuar NO-LOCK
         WHERE usuar_grp_usuar.cod_grp_usuar = "PML"
           AND usuar_grp_usuar.cod_usuario   = c-seg-usuario NO-ERROR.
    IF AVAIL usuar_grp_usuar THEN DO:    
        ASSIGN tt-ped-item-prm.compra-material:READ-ONLY IN BROWSE br-ped-venda = FALSE
               bt-chek-compra:SENSITIVE = YES.
    
        bt-chek-compra:LOAD-IMAGE("image\toolbar\im-chck1.bmp").
        bt-todos-comprar:SENSITIVE = TRUE.
    
    END.
    ELSE DO:
        ASSIGN tt-ped-item-prm.compra-material:READ-ONLY IN BROWSE br-ped-venda = TRUE.
        bt-chek-compra:load-image("image\toolbar\ii-chck2.bmp").
    END.

    FOR EACH calen-prod NO-LOCK
       WHERE calen-prod.data        = INPUT FRAME {&FRAME-NAME} fi-dt-planeja:INPUT-VALUE         
         AND calen-prod.tipo-dia    <> 1 .
        RUN utp/ut-msgs.p (INPUT "Show",
                   INPUT 27979,
                   INPUT "Data n∆o Ç dia £til no calend†rio da produá∆o(CD0911)!" + "~~" +
                         "Atualizaá∆o n∆o permitida, altere a data de entrega.").
        RETURN NO-APPLY.    
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME mi-consultas
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mi-consultas w-livre
ON CHOOSE OF MENU-ITEM mi-consultas /* Consultas */
DO:
  RUN pi-consulta IN h_p-exihel.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME mi-conteudo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mi-conteudo w-livre
ON CHOOSE OF MENU-ITEM mi-conteudo /* Conteudo */
OR HELP OF FRAME {&FRAME-NAME}
DO:
  RUN pi-ajuda IN h_p-exihel.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME mi-imprimir
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mi-imprimir w-livre
ON CHOOSE OF MENU-ITEM mi-imprimir /* Relat¢rios */
DO:
  RUN pi-imprimir IN h_p-exihel.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME mi-programa
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mi-programa w-livre
ON MENU-DROP OF MENU mi-programa /* prm1203 */
DO:
  run pi-disable-menu.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME mi-sair
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mi-sair w-livre
ON CHOOSE OF MENU-ITEM mi-sair /* Sair */
DO:
  RUN pi-sair IN h_p-exihel.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME mi-sobre
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mi-sobre w-livre
ON CHOOSE OF MENU-ITEM mi-sobre /* Sobre... */
DO:
  {include/sobre.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK w-livre 


/* ***************************  Main Block  *************************** */

/* Include custom  Main Block code for SmartWindows. */
{src/adm/template/windowmn.i}


FIND FIRST usuar_grp_usuar NO-LOCK
     WHERE usuar_grp_usuar.cod_grp_usuar = "PML"
       AND usuar_grp_usuar.cod_usuario   = c-seg-usuario NO-ERROR.
IF AVAIL usuar_grp_usuar THEN DO:    
    ASSIGN tt-ped-item-prm.compra-material:READ-ONLY IN BROWSE br-ped-venda = FALSE
           bt-chek-compra:SENSITIVE = YES.

    bt-chek-compra:LOAD-IMAGE("image\toolbar\im-chck1.bmp").
    bt-todos-comprar:SENSITIVE = TRUE.

END.
ELSE DO:
    ASSIGN tt-ped-item-prm.compra-material:READ-ONLY IN BROWSE br-ped-venda = TRUE.
    bt-chek-compra:load-image("image\toolbar\ii-chck2.bmp").
END.



ON VALUE-CHANGED OF tt-ped-item-prm.compra-material IN BROWSE br-ped-venda DO:

    IF NOT AVAIL tt-ped-item-prm THEN NEXT.
    
    ASSIGN tt-ped-item-prm.compra-material = SELF:INPUT-VALUE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-create-objects w-livre  _ADM-CREATE-OBJECTS
PROCEDURE adm-create-objects :
/*------------------------------------------------------------------------------
  Purpose:     Create handles for all SmartObjects used in this procedure.
               After SmartObjects are initialized, then SmartLinks are added.
  Parameters:  <none>
------------------------------------------------------------------------------*/
  DEFINE VARIABLE adm-current-page  AS INTEGER NO-UNDO.

  RUN get-attribute IN THIS-PROCEDURE ('Current-Page':U).
  ASSIGN adm-current-page = INTEGER(RETURN-VALUE).

  CASE adm-current-page: 

    WHEN 0 THEN DO:
       RUN init-object IN THIS-PROCEDURE (
             INPUT  'panel/p-exihel.w':U ,
             INPUT  FRAME f-cad:HANDLE ,
             INPUT  'Edge-Pixels = 2,
                     SmartPanelType = NAV-ICON,
                     Right-to-Left = First-On-Left':U ,
             OUTPUT h_p-exihel ).
       RUN set-position IN h_p-exihel ( 1.46 , 150.86 ) NO-ERROR.
       /* Size in UIB:  ( 1.25 , 16.00 ) */

       /* Links to SmartPanel h_p-exihel. */
       RUN add-link IN adm-broker-hdl ( h_p-exihel , 'State':U , THIS-PROCEDURE ).

       /* Adjust the tab order of the smart objects. */
       RUN adjust-tab-order IN adm-broker-hdl ( h_p-exihel ,
             bt-filtro:HANDLE IN FRAME f-cad , 'BEFORE':U ).
    END. /* Page 0 */

  END CASE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-row-available w-livre  _ADM-ROW-AVAILABLE
PROCEDURE adm-row-available :
/*------------------------------------------------------------------------------
  Purpose:     Dispatched to this procedure when the Record-
               Source has a new row available.  This procedure
               tries to get the new row (or foriegn keys) from
               the Record-Source and process it.
  Parameters:  <none>
------------------------------------------------------------------------------*/

  /* Define variables needed by this internal procedure.             */
  {src/adm/template/row-head.i}

  /* Process the newly available records (i.e. display fields,
     open queries, and/or pass records on to any RECORD-TARGETS).    */
  {src/adm/template/row-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI w-livre  _DEFAULT-DISABLE
PROCEDURE disable_UI :
/*------------------------------------------------------------------------------
  Purpose:     DISABLE the User Interface
  Parameters:  <none>
  Notes:       Here we clean-up the user-interface by deleting
               dynamic widgets we have created and/or hide 
               frames.  This procedure is usually called when
               we are ready to "clean-up" after running.
------------------------------------------------------------------------------*/
  /* Delete the WINDOW we created */
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(w-livre)
  THEN DELETE WIDGET w-livre.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI w-livre  _DEFAULT-ENABLE
PROCEDURE enable_UI :
/*------------------------------------------------------------------------------
  Purpose:     ENABLE the User Interface
  Parameters:  <none>
  Notes:       Here we display/view/enable the widgets in the
               user-interface.  In addition, OPEN all queries
               associated with each FRAME and BROWSE.
               These statements here are based on the "Other 
               Settings" section of the widget Property Sheets.
------------------------------------------------------------------------------*/
  DISPLAY fi-dt-planeja fi-tot-cilind fi-tot-valor 
      WITH FRAME f-cad IN WINDOW w-livre.
  ENABLE rt-button RECT-5 bt-filtro bt-consulta fi-dt-planeja br-ped-venda 
         bt-todos bt-nenhum bt-dividir-item 
      WITH FRAME f-cad IN WINDOW w-livre.
  {&OPEN-BROWSERS-IN-QUERY-f-cad}
  VIEW w-livre.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-destroy w-livre 
PROCEDURE local-destroy :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'destroy':U ) .
  {include/i-logfin.i}

  /* Code placed here will execute AFTER standard behavior.    */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-exit w-livre 
PROCEDURE local-exit :
/* -----------------------------------------------------------
  Purpose:  Starts an "exit" by APPLYing CLOSE event, which starts "destroy".
  Parameters:  <none>
  Notes:    If activated, should APPLY CLOSE, *not* dispatch adm-exit.   
-------------------------------------------------------------*/
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  
  RETURN.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-initialize w-livre 
PROCEDURE local-initialize :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE VARIABLE i-cont AS INTEGER  NO-UNDO.

    /* Code placed here will execute PRIOR to standard behavior. */
    run pi-before-initialize.

    {include/win-size.i}

    {utp/ut9000.i "prm1203" "1.00.00.000"}

    /* Dispatch standard ADM method.                             */
    RUN dispatch IN THIS-PROCEDURE ( INPUT 'initialize':U ) .

    /* Code placed here will execute AFTER standard behavior.    */
    run pi-after-initialize.
    
    ASSIGN EXTENT(h-colunas) = br-ped-venda:NUM-COLUMNS.
    DO i-cont = 1 TO EXTENT(h-colunas) BY 1:
        h-colunas[i-cont] = br-ped-venda:GET-BROWSE-COLUMN(i-cont).
    END.

    ASSIGN fi-dt-planeja:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(TODAY,"99/99/9999").

    FIND FIRST usuar_grp_usuar NO-LOCK
         WHERE usuar_grp_usuar.cod_grp_usuar = "PCP"
           AND usuar_grp_usuar.cod_usuario   = c-seg-usuario NO-ERROR.
    IF AVAIL usuar_grp_usuar THEN
        ENABLE bt-atualizar WITH FRAME {&FRAME-NAME}.

    FIND FIRST usuar_grp_usuar NO-LOCK
         WHERE usuar_grp_usuar.cod_grp_usuar = "PML"
           AND usuar_grp_usuar.cod_usuario   = c-seg-usuario NO-ERROR.
    IF AVAIL usuar_grp_usuar THEN
        ENABLE bt-todos-comprar WITH FRAME {&FRAME-NAME}.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-lista-log w-livre 
PROCEDURE pi-lista-log :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE VARIABLE c-arquivo AS CHAR NO-UNDO.

    IF CAN-FIND(FIRST tt-erro) THEN DO:
        ASSIGN c-arquivo = SESSION:TEMP-DIR + '\Log-Item-Ped-Planeja_' + STRING(TODAY,'99999999') + '_' + STRING(TIME) + '.txt'.
        OUTPUT TO VALUE(c-arquivo) NO-CONVERT.

        PUT FILL("-",130) FORMAT "x(130)" TODAY FORMAT "99/99/9999" "--" STRING(TIME,"hh:mm:ss") SKIP. 
        PUT "                  LOG DO PLANEJAMENTO DOS ITENS DOS PEDIDOS DE VENDA" FORMAT "X(100)" SKIP.
        PUT FILL("-",150) FORMAT "x(150)" SKIP.
        PUT "SEQ.      C‡DIGO    DESCRIÄ«O LOG" FORMAT "X(100)" SKIP.
        PUT FILL("-",150) FORMAT "x(150)" SKIP.
        FOR EACH tt-erro:
            PUT UNFORMATTED
                 STRING(tt-erro.i-sequen)  AT 01
                 STRING(tt-erro.cd-erro)   AT 10
                 tt-erro.mensagem          AT 20 SKIP.
        END.
        PUT FILL("-",150) FORMAT "x(150)" SKIP.

        OUTPUT CLOSE.
        DOS SILENT START notepad.exe VALUE(c-arquivo).    
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-tt-ped-venda-prm w-livre 
PROCEDURE pi-tt-ped-venda-prm :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE VARIABLE i-linha-br AS INTEGER NO-UNDO.
    
    IF NOT CAN-FIND(FIRST estabelec WHERE estabelec.cod-estabel = c-cod-estabel-ini) THEN DO:
        /* ERRO: 17006*/
        RUN utp/ut-msgs.p (INPUT "Show",
                           INPUT 17006,
                           INPUT "Estabelecimento informado no filtro n∆o foi encontrado!" + "~~" +
                                 "Estabelecimento informado no filtro n∆o foi encontrado.").
       RETURN "ADM-ERROR":U.
    END.

    EMPTY TEMP-TABLE tt-ped-item-prm.    

    RUN utp\ut-acomp.p PERSISTENT SET h-acomp.
    RUN pi-inicializar IN h-acomp(INPUT "Consultando Pedidos...").

    FOR EACH ped-venda WHERE ped-venda.cod-estabel  >= c-cod-estabel-ini
                       AND   ped-venda.cod-estabel  <= c-cod-estabel-ini
                       AND   ped-venda.cod-emitente >= i-cod-emitente-ini
                       AND   ped-venda.cod-emitente <= i-cod-emitente-fim 
                       AND   ped-venda.nome-abrev   >= c-nome-abrev-ini
                       AND   ped-venda.nome-abrev   <= c-nome-abrev-fim
                       AND   ped-venda.nr-pedido    >= i-nr-pedido-ini 
                       AND   ped-venda.nr-pedido    <= i-nr-pedido-fim 
                       AND   ped-venda.nr-pedcli    >= c-nr-pedcli-ini 
                       AND   ped-venda.nr-pedcli    <= c-nr-pedcli-fim 
                       AND   ped-venda.dt-implant   >= da-dt-implant-ini 
                       AND   ped-venda.dt-implant   <= da-dt-implant-fim 
/*                        AND   ped-venda.dt-entrega   >= da-dt-entrega-ini */
/*                        AND   ped-venda.dt-entrega   <= da-dt-entrega-fim */
                       //AND   ped-venda.cod-sit-ped  < 3
                       AND   ped-venda.completo,
         EACH ped-item NO-LOCK
        WHERE ped-item.nr-pedcli     = ped-venda.nr-pedcli 
          AND ped-item.nome-abrev    = ped-venda.nome-abrev
          AND ped-item.it-codigo    >= c-it-codigo-ini  
          AND ped-item.it-codigo    <= c-it-codigo-fim  
          AND ped-item.dt-entrega   >= da-dt-entrega-ini
          AND ped-item.dt-entrega   <= da-dt-entrega-fim
          //AND ped-item.cod-sit-item < 3
        ,
        FIRST ITEM NO-LOCK
        WHERE ITEM.it-codigo   = ped-item.it-codigo
          AND ITEM.fm-codigo  >= c-familia-ini 
          AND ITEM.fm-codigo  <= c-familia-fim
          AND ITEM.ge-codigo  >= i-ge-codigo-ini 
          AND ITEM.ge-codigo  <= i-ge-codigo-fim
          AND ITEM.fm-cod-com >= c-fm-cod-com-ini 
          AND ITEM.fm-cod-com <= c-fm-cod-com-fim,
        EACH item-cli NO-LOCK
             WHERE item-cli.nome-abrev = ped-item.nome-abrev
               AND item-cli.it-codigo = ped-item.it-codigo
               AND item-cli.item-do-cli >= c-it-cli-ini 
               AND item-cli.item-do-cli <= c-it-cli-fim .

        RUN pi-acompanhar IN h-acomp(INPUT "Pedido Venda: " + STRING(ped-venda.nr-pedido) + " Item: " + ped-item.it-codigo).

        IF tg-itens-faturados = FALSE AND (ped-venda.cod-sit-ped  >= 3 OR ped-item.cod-sit-item >= 3) THEN NEXT.
        
        FIND FIRST tt-ped-item-prm NO-LOCK
             WHERE tt-ped-item-prm.nr-pedcli    = ped-item.nr-pedcli 
               AND tt-ped-item-prm.nome-abrev   = ped-item.nome-abrev
               AND tt-ped-item-prm.nr-sequencia = ped-item.nr-sequencia
               AND tt-ped-item-prm.it-codigo    = ped-item.it-codigo   
               AND tt-ped-item-prm.cod-refer    = ped-item.cod-refer    NO-ERROR.
        IF NOT AVAIL tt-ped-item-prm THEN DO:     

            CREATE tt-ped-item-prm.
            BUFFER-COPY ped-item TO tt-ped-item-prm.
    
            ASSIGN d-saldo = 0.
            IF ped-venda.cod-estabel = "102" THEN DO:
                FOR EACH saldo-estoq NO-LOCK
                   WHERE saldo-estoq.cod-estabel = ped-venda.cod-estabel
                     AND saldo-estoq.cod-refer   = ped-item.cod-refer
                     AND saldo-estoq.it-codigo   = ITEM.it-codigo
                     AND saldo-estoq.cod-depos   = 'LEX':
                    ASSIGN d-saldo = d-saldo + fn-qtd-data().
                END.
            END.
            ELSE DO:
                FOR EACH saldo-estoq NO-LOCK
                   WHERE saldo-estoq.cod-estabel = ped-venda.cod-estabel
                     AND saldo-estoq.cod-refer   = ped-item.cod-refer
                     AND saldo-estoq.it-codigo   = ITEM.it-codigo
                     AND saldo-estoq.cod-depos   = 'EXP':
                    ASSIGN d-saldo = d-saldo + fn-qtd-data().
                END.
            END.

            ASSIGN tt-ped-item-prm.sel          = NO
                   tt-ped-item-prm.cod-emitente = ped-venda.cod-emitente
                   tt-ped-item-prm.nr-pedido    = ped-venda.nr-pedido
                   tt-ped-item-prm.dt-implant   = ped-venda.dt-implant
                   tt-ped-item-prm.descricao    = item.desc-item
                   tt-ped-item-prm.cod-un       = item.un
                   tt-ped-item-prm.saldo-item   = d-saldo.
    
            // Gustavo Alfredo - 11/08/2022 - incluindo numero da ordem relacionado a ped-item
            FIND FIRST ord-prod NO-LOCK USE-INDEX cliente-ped
                 WHERE ord-prod.nome-abrev   = ped-item.nome-abrev
                   AND ord-prod.nr-pedido    = ped-item.nr-pedcli
                   AND ord-prod.nr-sequencia = ped-item.nr-sequencia NO-ERROR.
            IF AVAILABLE ord-prod THEN DO:
                ASSIGN tt-ped-item-prm.nr-ord-producao = ord-prod.nr-ord-produ.
            END.
    
            FIND FIRST familia NO-LOCK
                 WHERE familia.fm-codigo = item.fm-codigo NO-ERROR.
            IF AVAIL familia THEN
                ASSIGN tt-ped-item-prm.fm-codigo = familia.fm-codigo + " - " + familia.descricao.

            FIND FIRST grup-estoque NO-LOCK
                 WHERE grup-estoque.ge-codigo = item.ge-codigo NO-ERROR.
            IF AVAIL grup-estoque THEN
                ASSIGN tt-ped-item-prm.ge-codigo = STRING(grup-estoque.ge-codigo) + " - " + grup-estoque.descricao.

            FIND FIRST fam-comerc NO-LOCK
                 WHERE fam-comerc.fm-cod-com = item.fm-cod-com NO-ERROR.
            IF AVAIL fam-comerc THEN
                ASSIGN tt-ped-item-prm.fm-cod-com = fam-comerc.fm-cod-com  + " - " + fam-comerc.descricao.

            ASSIGN tt-ped-item-prm.qt-a-atender = tt-ped-item-prm.qt-pedida - tt-ped-item-prm.qt-atendida
                   tt-ped-item-prm.peso-bruto   = tt-ped-item-prm.qt-a-atender * item.peso-bruto.           
    
            ASSIGN d-dt-planeja = 01/01/0001.
            FIND FIRST prm-ped-item-planej NO-LOCK
                 WHERE prm-ped-item-planej.nr-pedcli    = ped-item.nr-pedcli 
                   AND prm-ped-item-planej.nome-abrev   = ped-item.nome-abrev
                   AND prm-ped-item-planej.nr-sequencia = ped-item.nr-sequencia
                   AND prm-ped-item-planej.it-codigo    = ped-item.it-codigo   
                   AND prm-ped-item-planej.cod-refer    = ped-item.cod-refer    NO-ERROR.
            IF AVAIL prm-ped-item-planej THEN DO:
                ASSIGN d-dt-planeja = prm-ped-item-planej.dt-planeja
                       tt-ped-item-prm.compra-material = prm-ped-item-planej.compra-material.
            END.
    
            ASSIGN tt-ped-item-prm.dt-planeja = d-dt-planeja.
            

            /*-- Inclus∆o da data de faturamento --*/
            FIND LAST it-nota-fisc NO-LOCK
                 WHERE it-nota-fisc.nome-ab-cli  = ped-item.nome-abrev   
                   AND it-nota-fisc.nr-pedcli    = ped-item.nr-pedcli  
                   AND it-nota-fisc.nr-seq-ped   = ped-item.nr-sequencia
                   AND it-nota-fisc.it-codigo    = ped-item.it-codigo   
                  // AND it-nota-fisc.cod-refer    = ped-item.cod-refer   
                  // AND it-nota-fisc.nr-entrega   = ped-item.
                  // AND it-nota-fisc.dt-emis-nota = ped-item.
                NO-ERROR.
            IF AVAIL it-nota-fisc THEN 
                ASSIGN tt-ped-item-prm.data-faturamento = it-nota-fisc.dt-emis-nota.

            /*-- Fim - Inclus∆o da data de faturamento --*/
    
            /*Willian Santana 26/01/2023 - Inclus∆o item do cliente*/
            /*FIND FIRST item-cli NO-LOCK
                 WHERE item-cli.nome-abrev = ped-item.nome-abrev
                   AND item-cli.it-codigo = ped-item.it-codigo NO-ERROR.*/
            IF AVAIL item-cli THEN DO:
                ASSIGN tt-ped-item-prm.item-do-cli = item-cli.item-do-cli.
            END.
            /*Fim - Willian Santana*/
    
            //ASSIGN tt-ped-item-prm.dt-planeja = INPUT FRAME {&FRAME-NAME} fi-dt-planeja:INPUT-VALUE .

        END.

    END.
    
    FOR EACH tt-ped-item-prm EXCLUSIVE-LOCK.
        IF tg-itens-faturados = YES AND tt-ped-item-prm.data-faturamento = ? THEN DO:
            DELETE tt-ped-item-prm.
            NEXT.
        END.

        IF tt-ped-item-prm.data-faturamento < p-fi-ini-dt-fatur 
        OR tt-ped-item-prm.data-faturamento > p-fi-fim-dt-fatur THEN DO:
            DELETE tt-ped-item-prm.
            NEXT.
        END.
    END.


    RUN pi-finalizar IN h-acomp.

    IF (br-ped-venda:NUM-SELECTED-ROWS IN FRAME {&FRAME-NAME} = 1 AND AVAIL tt-ped-item-prm ) THEN DO:
        ASSIGN i-linha-br = CURRENT-RESULT-ROW("br-ped-venda").
       // IF (tt-ped-item-prm.sel = "*") THEN
       //     ASSIGN tt-ped-item-prm.sel = "".
       // ELSE
       //     ASSIGN tt-ped-item-prm.sel = "*".
    END.

    {&OPEN-QUERY-br-ped-venda}

    ASSIGN i-tot-cilind = 0.
    ASSIGN dec-tot-valor = 0.
    ASSIGN fi-tot-cilind:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(i-tot-cilind).
    ASSIGN fi-tot-valor:SCREEN-VALUE IN FRAME {&FRAME-NAME}  = STRING(dec-tot-valor).

    IF AVAIL tt-ped-item-prm THEN DO:
        REPOSITION br-ped-venda TO ROW i-linha-br.
        br-ped-venda:REFRESH() IN FRAME {&FRAME-NAME}.
        DISP tt-ped-item-prm.sel WITH BROWSE br-ped-venda.
    END.
        
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE send-records w-livre  _ADM-SEND-RECORDS
PROCEDURE send-records :
/*------------------------------------------------------------------------------
  Purpose:     Send record ROWID's for all tables used by
               this file.
  Parameters:  see template/snd-head.i
------------------------------------------------------------------------------*/

  /* Define variables needed by this internal procedure.               */
  {src/adm/template/snd-head.i}

  /* For each requested table, put it's ROWID in the output list.      */
  {src/adm/template/snd-list.i "tt-ped-item-prm"}

  /* Deal with any unexpected table requests before closing.           */
  {src/adm/template/snd-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE state-changed w-livre 
PROCEDURE state-changed :
/*:T -----------------------------------------------------------
  Purpose:     Manuseia trocas de estado dos SmartObjects
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
  DEFINE INPUT PARAMETER p-issuer-hdl AS HANDLE NO-UNDO.
  DEFINE INPUT PARAMETER p-state AS CHARACTER NO-UNDO.

  run pi-trata-state (p-issuer-hdl, p-state).
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fn-qtd-data w-livre 
FUNCTION fn-qtd-data RETURNS DECIMAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
    DEFINE VARIABLE d-tot-disponivel  AS DECIMAL      NO-UNDO.
    DEFINE VARIABLE d-saldo-calculado AS DECIMAL      NO-UNDO.
    DEFINE VARIABLE dt-saldo          AS DATE         NO-UNDO.

    assign d-tot-disponivel = 0
           dt-saldo = TODAY.
    assign d-tot-disponivel  = d-tot-disponivel + (saldo-estoq.qtidade-atu - saldo-estoq.qt-alocada -
                                                   saldo-estoq.qt-aloc-prod - saldo-estoq.qt-aloc-ped)
           d-saldo-calculado = saldo-estoq.qtidade-atu.

    IF  item.tipo-con-est = 1 THEN
        open query qr-movto
        for each movto-estoq use-index item-data where
                             movto-estoq.it-codigo   = saldo-estoq.it-codigo    and
                             movto-estoq.cod-refer   = saldo-estoq.cod-refer    and
                             movto-estoq.cod-estabel = saldo-estoq.cod-estabel  and
                             movto-estoq.cod-depos   = saldo-estoq.cod-depos    and
                             movto-estoq.lote        = saldo-estoq.lote         and
                             movto-estoq.cod-localiz = saldo-estoq.cod-localiz  and
                             movto-estoq.esp-docto   <> 37                      and
                             movto-estoq.dt-trans     > TODAY no-lock.
    ELSE
        open query qr-movto
        for each movto-estoq use-index item-estab where
                             movto-estoq.it-codigo   = saldo-estoq.it-codigo    and
                             movto-estoq.cod-refer   = saldo-estoq.cod-refer    and
                             movto-estoq.cod-estabel = saldo-estoq.cod-estabel  and
                             movto-estoq.cod-depos   = saldo-estoq.cod-depos    and
                             movto-estoq.lote        = saldo-estoq.lote         and
                             movto-estoq.cod-localiz = saldo-estoq.cod-localiz  and
                             movto-estoq.esp-docto   <> 37                      and
                             movto-estoq.dt-trans     > TODAY no-lock.

    get first qr-movto.
    do while avail (movto-estoq):
        if movto-estoq.tipo-trans = 1 then
            assign d-saldo-calculado = d-saldo-calculado - movto-estoq.quantidade.
        else
            assign d-saldo-calculado = d-saldo-calculado + movto-estoq.quantidade.
        get next qr-movto.
    end. /* while */

    RETURN d-tot-disponivel /*d-saldo-calculado*/ .   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getDescSitItem w-livre 
FUNCTION getDescSitItem RETURNS CHARACTER(INPUT situacao AS INTEGER) :

    RETURN {diinc/i03di149.i 04 tt-ped-item-prm.cod-sit-item}.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

