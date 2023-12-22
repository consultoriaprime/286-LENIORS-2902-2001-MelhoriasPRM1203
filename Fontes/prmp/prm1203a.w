&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME w-window
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS w-window 
{include/i-prgvrs.i PRM1203A 1.00.00.000}

/* Chamada a include do gerenciador de licenáas. Necessario alterar os parametros */
/*                                                                                */
/* <programa>:  Informar qual o nome do programa.                                 */
/* <m¢dulo>:  Informar qual o m¢dulo a qual o programa pertence.                  */

&IF "{&EMSFND_VERSION}" >= "1.00" &THEN
    {include/i-license-manager.i PRM1203A MFT}
&ENDIF

/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */
DEFINE INPUT-OUTPUT PARAMETER p-cod-estabel-ini    AS CHARACTER    NO-UNDO.
DEFINE INPUT-OUTPUT PARAMETER p-cod-emitente-ini   AS INTEGER      NO-UNDO.
DEFINE INPUT-OUTPUT PARAMETER p-cod-emitente-fim   AS INTEGER      NO-UNDO.
DEFINE INPUT-OUTPUT PARAMETER p-nome-abrev-ini     AS CHARACTER    NO-UNDO.
DEFINE INPUT-OUTPUT PARAMETER p-nome-abrev-fim     AS CHARACTER    NO-UNDO.
DEFINE INPUT-OUTPUT PARAMETER p-nr-pedido-ini      AS INTEGER      NO-UNDO.
DEFINE INPUT-OUTPUT PARAMETER p-nr-pedido-fim      AS INTEGER      NO-UNDO.
DEFINE INPUT-OUTPUT PARAMETER p-nr-pedcli-ini      AS CHARACTER    NO-UNDO.
DEFINE INPUT-OUTPUT PARAMETER p-nr-pedcli-fim      AS CHARACTER    NO-UNDO.
DEFINE INPUT-OUTPUT PARAMETER p-dt-implant-ini     AS DATE         NO-UNDO.
DEFINE INPUT-OUTPUT PARAMETER p-dt-implant-fim     AS DATE         NO-UNDO.
DEFINE INPUT-OUTPUT PARAMETER p-familia-ini        AS CHARACTER    NO-UNDO.
DEFINE INPUT-OUTPUT PARAMETER p-familia-fim        AS CHARACTER    NO-UNDO.
DEFINE INPUT-OUTPUT PARAMETER p-it-codigo-ini      AS CHARACTER    NO-UNDO.
DEFINE INPUT-OUTPUT PARAMETER p-it-codigo-fim      AS CHARACTER    NO-UNDO.
DEFINE INPUT-OUTPUT PARAMETER p-dt-entrega-ini     AS DATE         NO-UNDO.
DEFINE INPUT-OUTPUT PARAMETER p-dt-entrega-fim     AS DATE         NO-UNDO.
DEFINE INPUT-OUTPUT PARAMETER p-it-cli-ini         AS CHARACTER    NO-UNDO.
DEFINE INPUT-OUTPUT PARAMETER p-it-cli-fim         AS CHARACTER    NO-UNDO.
DEFINE INPUT-OUTPUT PARAMETER p-ge-codigo-ini      AS INTEGER      NO-UNDO.
DEFINE INPUT-OUTPUT PARAMETER p-ge-codigo-fim      AS INTEGER      NO-UNDO.
DEFINE INPUT-OUTPUT PARAMETER p-fm-cod-com-ini     AS CHARACTER    NO-UNDO.
DEFINE INPUT-OUTPUT PARAMETER p-fm-cod-com-fim     AS CHARACTER    NO-UNDO.
DEFINE INPUT-OUTPUT PARAMETER p-tg-itens-faturados AS LOG          NO-UNDO.
DEFINE INPUT-OUTPUT PARAMETER p-fi-ini-dt-fatur    AS DATE         NO-UNDO.
DEFINE INPUT-OUTPUT PARAMETER p-fi-fim-dt-fatur    AS DATE         NO-UNDO.

DEFINE OUTPUT PARAMETER       p-ok                 AS LOGICAL      NO-UNDO.

/* Local Variable Definitions ---                                       */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE JanelaDetalhe
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER WINDOW

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME F-Main

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-1 IMAGE-1 IMAGE-2 IMAGE-11 IMAGE-12 ~
IMAGE-13 IMAGE-14 IMAGE-15 IMAGE-16 IMAGE-17 IMAGE-18 IMAGE-19 IMAGE-20 ~
RECT-5 IMAGE-29 IMAGE-30 IMAGE-31 IMAGE-32 RECT-6 IMAGE-33 IMAGE-34 ~
IMAGE-35 IMAGE-36 IMAGE-37 IMAGE-38 RECT-7 IMAGE-39 IMAGE-40 ~
fi-cod-estabel-ini fi-ini-cod-emitente fi-fim-cod-emitente ~
fi-ini-nome-abrev fi-fim-nome-abrev fi-ini-nr-pedido fi-fim-nr-pedido ~
fi-ini-nr-pedcli fi-fim-nr-pedcli fi-ini-dt-implant fi-fim-dt-implant ~
fi-ini-it-codigo fi-fim-it-codigo fi-ini-familia fi-fim-familia ~
fi-ini-ge-codigo fi-fim-ge-codigo fi-ini-cod-fm-com fi-fim-cod-fm-com ~
fi-ini-dt-entrega fi-fim-dt-entrega fi-ini-it-do-cli fi-fim-it-do-cli ~
fi-ini-dt-fatur fi-fim-dt-fatur tg-itens-faturados bt-ok bt-cancelar 
&Scoped-Define DISPLAYED-OBJECTS fi-cod-estabel-ini fi-ini-cod-emitente ~
fi-fim-cod-emitente fi-ini-nome-abrev fi-fim-nome-abrev fi-ini-nr-pedido ~
fi-fim-nr-pedido fi-ini-nr-pedcli fi-fim-nr-pedcli fi-ini-dt-implant ~
fi-fim-dt-implant fi-ini-it-codigo fi-fim-it-codigo fi-ini-familia ~
fi-fim-familia fi-ini-ge-codigo fi-fim-ge-codigo fi-ini-cod-fm-com ~
fi-fim-cod-fm-com fi-ini-dt-entrega fi-fim-dt-entrega fi-ini-it-do-cli ~
fi-fim-it-do-cli fi-ini-dt-fatur fi-fim-dt-fatur tg-itens-faturados 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR w-window AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON bt-cancelar AUTO-END-KEY 
     LABEL "Cancelar" 
     SIZE 10 BY 1.

DEFINE BUTTON bt-ok AUTO-GO 
     LABEL "OK" 
     SIZE 10 BY 1.

DEFINE VARIABLE fi-cod-estabel-ini AS CHARACTER FORMAT "X(5)":U 
     LABEL "Estabelecimento" 
     VIEW-AS FILL-IN 
     SIZE 6.86 BY .88 TOOLTIP "Estabelecimento do pedido de venda" NO-UNDO.

DEFINE VARIABLE fi-fim-cod-emitente AS INTEGER FORMAT ">>>>>>>>9":U INITIAL 999999999 
     VIEW-AS FILL-IN 
     SIZE 13.72 BY .88 NO-UNDO.

DEFINE VARIABLE fi-fim-cod-fm-com AS CHARACTER FORMAT "X(8)":U INITIAL "ZZZZZZZZ" 
     VIEW-AS FILL-IN 
     SIZE 13.72 BY .88 NO-UNDO.

DEFINE VARIABLE fi-fim-dt-entrega AS DATE FORMAT "99/99/9999":U INITIAL 12/31/9999 
     VIEW-AS FILL-IN 
     SIZE 13.72 BY .88 NO-UNDO.

DEFINE VARIABLE fi-fim-dt-fatur AS DATE FORMAT "99/99/9999":U INITIAL 12/31/9999 
     VIEW-AS FILL-IN 
     SIZE 13.72 BY .88 NO-UNDO.

DEFINE VARIABLE fi-fim-dt-implant AS DATE FORMAT "99/99/9999":U INITIAL 12/31/9999 
     VIEW-AS FILL-IN 
     SIZE 13.72 BY .88 NO-UNDO.

DEFINE VARIABLE fi-fim-familia AS CHARACTER FORMAT "X(8)":U INITIAL "ZZZZZZZZ" 
     VIEW-AS FILL-IN 
     SIZE 13.72 BY .88 NO-UNDO.

DEFINE VARIABLE fi-fim-ge-codigo AS INTEGER FORMAT ">9":U INITIAL 99 
     VIEW-AS FILL-IN 
     SIZE 13.72 BY .88 NO-UNDO.

DEFINE VARIABLE fi-fim-it-codigo AS CHARACTER FORMAT "X(16)":U INITIAL "ZZZZZZZZZZZZZZZZ" 
     VIEW-AS FILL-IN 
     SIZE 17.14 BY .88 NO-UNDO.

DEFINE VARIABLE fi-fim-it-do-cli AS CHARACTER FORMAT "X(30)":U INITIAL "ZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZ" 
     VIEW-AS FILL-IN 
     SIZE 23.29 BY .88 NO-UNDO.

DEFINE VARIABLE fi-fim-nome-abrev AS CHARACTER FORMAT "X(12)":U INITIAL "ZZZZZZZZZZZZ" 
     VIEW-AS FILL-IN 
     SIZE 13.72 BY .88 NO-UNDO.

DEFINE VARIABLE fi-fim-nr-pedcli AS CHARACTER FORMAT "X(12)":U INITIAL "ZZZZZZZZZZZZ" 
     VIEW-AS FILL-IN 
     SIZE 13.72 BY .88 NO-UNDO.

DEFINE VARIABLE fi-fim-nr-pedido AS INTEGER FORMAT ">>>,>>>,>>9":U INITIAL 999999999 
     VIEW-AS FILL-IN 
     SIZE 13.72 BY .88 NO-UNDO.

DEFINE VARIABLE fi-ini-cod-emitente AS INTEGER FORMAT ">>>>>>>>9":U INITIAL 0 
     LABEL "Cliente" 
     VIEW-AS FILL-IN 
     SIZE 14.86 BY .88 NO-UNDO.

DEFINE VARIABLE fi-ini-cod-fm-com AS CHARACTER FORMAT "X(8)":U 
     LABEL "Familia Comercial" 
     VIEW-AS FILL-IN 
     SIZE 14.86 BY .88 NO-UNDO.

DEFINE VARIABLE fi-ini-dt-entrega AS DATE FORMAT "99/99/9999":U INITIAL 01/01/1900 
     LABEL "Data Entrega Item" 
     VIEW-AS FILL-IN 
     SIZE 14.86 BY .88 NO-UNDO.

DEFINE VARIABLE fi-ini-dt-fatur AS DATE FORMAT "99/99/9999":U INITIAL 01/01/1900 
     LABEL "Data Faturamento" 
     VIEW-AS FILL-IN 
     SIZE 14.86 BY .88 NO-UNDO.

DEFINE VARIABLE fi-ini-dt-implant AS DATE FORMAT "99/99/9999":U INITIAL 01/01/1900 
     LABEL "Data Implantaá∆o" 
     VIEW-AS FILL-IN 
     SIZE 14.86 BY .88 NO-UNDO.

DEFINE VARIABLE fi-ini-familia AS CHARACTER FORMAT "X(8)":U 
     LABEL "Familia Material" 
     VIEW-AS FILL-IN 
     SIZE 14.86 BY .88 NO-UNDO.

DEFINE VARIABLE fi-ini-ge-codigo AS INTEGER FORMAT ">9":U INITIAL 0 
     LABEL "Grupo Estoque" 
     VIEW-AS FILL-IN 
     SIZE 14.86 BY .88 NO-UNDO.

DEFINE VARIABLE fi-ini-it-codigo AS CHARACTER FORMAT "X(16)":U 
     LABEL "Item" 
     VIEW-AS FILL-IN 
     SIZE 17.14 BY .88 NO-UNDO.

DEFINE VARIABLE fi-ini-it-do-cli AS CHARACTER FORMAT "X(30)":U 
     LABEL "Item Cliente" 
     VIEW-AS FILL-IN 
     SIZE 23.29 BY .88 NO-UNDO.

DEFINE VARIABLE fi-ini-nome-abrev AS CHARACTER FORMAT "X(12)":U 
     LABEL "Nome Abrev. Cli." 
     VIEW-AS FILL-IN 
     SIZE 14.86 BY .88 NO-UNDO.

DEFINE VARIABLE fi-ini-nr-pedcli AS CHARACTER FORMAT "X(12)":U 
     LABEL "Pedido Cliente" 
     VIEW-AS FILL-IN 
     SIZE 14.86 BY .88 NO-UNDO.

DEFINE VARIABLE fi-ini-nr-pedido AS INTEGER FORMAT ">>>,>>>,>>9":U INITIAL 0 
     LABEL "Pedido Interno" 
     VIEW-AS FILL-IN 
     SIZE 14.86 BY .88 NO-UNDO.

DEFINE VARIABLE fi-nome AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 33.14 BY .88 NO-UNDO.

DEFINE IMAGE IMAGE-1
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-11
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-12
     FILENAME "image\im-las":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-13
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-14
     FILENAME "image\im-las":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-15
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-16
     FILENAME "image\im-las":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-17
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-18
     FILENAME "image\im-las":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-19
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-2
     FILENAME "image\im-las":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-20
     FILENAME "image\im-las":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-29
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-30
     FILENAME "image\im-las":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-31
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-32
     FILENAME "image\im-las":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-33
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-34
     FILENAME "image\im-las":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-35
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-36
     FILENAME "image\im-las":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-37
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-38
     FILENAME "image\im-las":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-39
     FILENAME "image\im-las":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-40
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE    
     SIZE 74.29 BY 1.38
     BGCOLOR 7 .

DEFINE RECTANGLE RECT-5
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 74.29 BY 7.

DEFINE RECTANGLE RECT-6
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 74.29 BY 6.96.

DEFINE RECTANGLE RECT-7
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 74.29 BY 1.79.

DEFINE VARIABLE tg-itens-faturados AS LOGICAL INITIAL no 
     LABEL "Mostrar Itens Faturados" 
     VIEW-AS TOGGLE-BOX
     SIZE 21.72 BY .83 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     fi-cod-estabel-ini AT ROW 2.33 COL 18.43 COLON-ALIGNED HELP
          "Estabelecimento do pedido de venda" WIDGET-ID 118
     fi-nome AT ROW 2.33 COL 25.29 COLON-ALIGNED NO-LABEL WIDGET-ID 116
     fi-ini-cod-emitente AT ROW 3.33 COL 18.43 COLON-ALIGNED WIDGET-ID 4
     fi-fim-cod-emitente AT ROW 3.33 COL 44.72 COLON-ALIGNED NO-LABEL WIDGET-ID 10
     fi-ini-nome-abrev AT ROW 4.33 COL 18.43 COLON-ALIGNED WIDGET-ID 12
     fi-fim-nome-abrev AT ROW 4.33 COL 44.72 COLON-ALIGNED NO-LABEL WIDGET-ID 60
     fi-ini-nr-pedido AT ROW 5.33 COL 18.43 COLON-ALIGNED WIDGET-ID 18
     fi-fim-nr-pedido AT ROW 5.33 COL 44.72 COLON-ALIGNED NO-LABEL WIDGET-ID 24
     fi-ini-nr-pedcli AT ROW 6.33 COL 18.43 COLON-ALIGNED WIDGET-ID 62
     fi-fim-nr-pedcli AT ROW 6.33 COL 44.72 COLON-ALIGNED NO-LABEL WIDGET-ID 68
     fi-ini-dt-implant AT ROW 7.33 COL 18.43 COLON-ALIGNED WIDGET-ID 26
     fi-fim-dt-implant AT ROW 7.33 COL 44.72 COLON-ALIGNED NO-LABEL WIDGET-ID 32
     fi-ini-it-codigo AT ROW 10 COL 16.14 COLON-ALIGNED WIDGET-ID 122
     fi-fim-it-codigo AT ROW 10 COL 44.86 COLON-ALIGNED NO-LABEL WIDGET-ID 120
     fi-ini-familia AT ROW 11 COL 18.43 COLON-ALIGNED WIDGET-ID 130
     fi-fim-familia AT ROW 11 COL 44.86 COLON-ALIGNED NO-LABEL WIDGET-ID 128
     fi-ini-ge-codigo AT ROW 12 COL 18.43 COLON-ALIGNED WIDGET-ID 152
     fi-fim-ge-codigo AT ROW 12 COL 44.86 COLON-ALIGNED NO-LABEL WIDGET-ID 150
     fi-ini-cod-fm-com AT ROW 13 COL 18.43 COLON-ALIGNED WIDGET-ID 160
     fi-fim-cod-fm-com AT ROW 13 COL 44.86 COLON-ALIGNED NO-LABEL WIDGET-ID 158
     fi-ini-dt-entrega AT ROW 14 COL 18.43 COLON-ALIGNED WIDGET-ID 34
     fi-fim-dt-entrega AT ROW 14 COL 44.86 COLON-ALIGNED NO-LABEL WIDGET-ID 40
     fi-ini-it-do-cli AT ROW 15 COL 10 COLON-ALIGNED WIDGET-ID 146
     fi-fim-it-do-cli AT ROW 15 COL 44.86 COLON-ALIGNED NO-LABEL WIDGET-ID 148
     fi-ini-dt-fatur AT ROW 17.58 COL 37.43 COLON-ALIGNED WIDGET-ID 174
     fi-fim-dt-fatur AT ROW 17.58 COL 59.57 COLON-ALIGNED NO-LABEL WIDGET-ID 172
     tg-itens-faturados AT ROW 17.63 COL 4.72 WIDGET-ID 166
     bt-ok AT ROW 19.25 COL 3.14
     bt-cancelar AT ROW 19.25 COL 14.14
     "Itens Faturados" VIEW-AS TEXT
          SIZE 17.14 BY .67 AT ROW 16.67 COL 4.43 WIDGET-ID 170
     "Item do Pedido de Venda" VIEW-AS TEXT
          SIZE 17.14 BY .67 AT ROW 9 COL 4.43 WIDGET-ID 138
     "Pedido de Venda" VIEW-AS TEXT
          SIZE 12.57 BY .67 AT ROW 1.33 COL 4.43 WIDGET-ID 80
     RECT-1 AT ROW 19.04 COL 2.14
     IMAGE-1 AT ROW 3.33 COL 35.86 WIDGET-ID 6
     IMAGE-2 AT ROW 3.33 COL 43.29 WIDGET-ID 8
     IMAGE-11 AT ROW 4.33 COL 35.86 WIDGET-ID 14
     IMAGE-12 AT ROW 4.33 COL 43.29 WIDGET-ID 16
     IMAGE-13 AT ROW 5.33 COL 35.86 WIDGET-ID 20
     IMAGE-14 AT ROW 5.33 COL 43.29 WIDGET-ID 22
     IMAGE-15 AT ROW 7.33 COL 35.86 WIDGET-ID 28
     IMAGE-16 AT ROW 7.33 COL 43.29 WIDGET-ID 30
     IMAGE-17 AT ROW 14 COL 35.86 WIDGET-ID 36
     IMAGE-18 AT ROW 14 COL 43.43 WIDGET-ID 38
     IMAGE-19 AT ROW 6.33 COL 35.86 WIDGET-ID 64
     IMAGE-20 AT ROW 6.33 COL 43.29 WIDGET-ID 66
     RECT-5 AT ROW 1.67 COL 2.14 WIDGET-ID 78
     IMAGE-29 AT ROW 10 COL 35.86 WIDGET-ID 124
     IMAGE-30 AT ROW 10 COL 43.43 WIDGET-ID 126
     IMAGE-31 AT ROW 11 COL 35.86 WIDGET-ID 132
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 76.57 BY 19.63
         FONT 1 WIDGET-ID 100.

/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME F-Main
     IMAGE-32 AT ROW 11 COL 43.43 WIDGET-ID 134
     RECT-6 AT ROW 9.38 COL 2.14 WIDGET-ID 136
     IMAGE-33 AT ROW 15 COL 35.86 WIDGET-ID 142
     IMAGE-34 AT ROW 15 COL 43.43 WIDGET-ID 144
     IMAGE-35 AT ROW 12 COL 35.86 WIDGET-ID 154
     IMAGE-36 AT ROW 12 COL 43.29 WIDGET-ID 156
     IMAGE-37 AT ROW 13 COL 35.86 WIDGET-ID 162
     IMAGE-38 AT ROW 13 COL 43.43 WIDGET-ID 164
     RECT-7 AT ROW 17 COL 2.14 WIDGET-ID 168
     IMAGE-39 AT ROW 17.58 COL 58.43 WIDGET-ID 176
     IMAGE-40 AT ROW 17.58 COL 54.72 WIDGET-ID 178
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 76.57 BY 19.63
         FONT 1 WIDGET-ID 100.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: JanelaDetalhe
   Allow: Basic,Browse,DB-Fields,Smart,Window,Query
   Container Links: 
   Add Fields to: Neither
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW w-window ASSIGN
         HIDDEN             = YES
         TITLE              = "<insert Custom SmartWindow title>"
         HEIGHT             = 19.71
         WIDTH              = 76.43
         MAX-HEIGHT         = 27.33
         MAX-WIDTH          = 114.29
         VIRTUAL-HEIGHT     = 27.33
         VIRTUAL-WIDTH      = 114.29
         RESIZE             = no
         SCROLL-BARS        = no
         STATUS-AREA        = yes
         BGCOLOR            = ?
         FGCOLOR            = ?
         THREE-D            = yes
         MESSAGE-AREA       = no
         SENSITIVE          = yes.
ELSE {&WINDOW-NAME} = CURRENT-WINDOW.
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB w-window 
/* ************************* Included-Libraries *********************** */

{src/adm/method/containr.i}
{include/w-window.i}
{utp/ut-glob.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW w-window
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME F-Main
   FRAME-NAME                                                           */
/* SETTINGS FOR FILL-IN fi-nome IN FRAME F-Main
   NO-DISPLAY NO-ENABLE                                                 */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(w-window)
THEN w-window:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME w-window
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL w-window w-window
ON END-ERROR OF w-window /* <insert Custom SmartWindow title> */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL w-window w-window
ON WINDOW-CLOSE OF w-window /* <insert Custom SmartWindow title> */
DO:
  /* This ADM code must be left here in order for the SmartWindow
     and its descendents to terminate properly on exit. */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-cancelar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-cancelar w-window
ON CHOOSE OF bt-cancelar IN FRAME F-Main /* Cancelar */
DO:
    ASSIGN p-ok = NO.

    apply "close":U to this-procedure.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-ok
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-ok w-window
ON CHOOSE OF bt-ok IN FRAME F-Main /* OK */
DO:

    IF NOT CAN-FIND(FIRST estabelec WHERE estabelec.cod-estabel = fi-cod-estabel-ini:SCREEN-VALUE IN FRAME {&FRAME-NAME}) THEN DO:
        /* ERRO: 17006*/
        RUN utp/ut-msgs.p (INPUT "Show",
                           INPUT 17006,
                           INPUT "Estabelecimento n∆o encontrado!" + "~~" +
                                 "Estabelecimento informado n∆o foi encontrado.").
        RETURN NO-APPLY.
    END.

    ASSIGN p-cod-estabel-ini    = INPUT FRAME f-main fi-cod-estabel-ini:SCREEN-VALUE
           p-cod-emitente-ini   = INPUT FRAME f-main fi-ini-cod-emitente:INPUT-VALUE    
           p-cod-emitente-fim   = INPUT FRAME f-main fi-fim-cod-emitente:INPUT-VALUE    
           p-nome-abrev-ini     = INPUT FRAME f-main fi-ini-nome-abrev:INPUT-VALUE    
           p-nome-abrev-fim     = INPUT FRAME f-main fi-fim-nome-abrev:INPUT-VALUE    
           p-nr-pedido-ini      = INPUT FRAME f-main fi-ini-nr-pedido:INPUT-VALUE       
           p-nr-pedido-fim      = INPUT FRAME f-main fi-fim-nr-pedido:INPUT-VALUE       
           p-nr-pedcli-ini      = INPUT FRAME f-main fi-ini-nr-pedcli :INPUT-VALUE       
           p-nr-pedcli-fim      = INPUT FRAME f-main fi-fim-nr-pedcli :INPUT-VALUE
           p-dt-implant-ini     = INPUT FRAME f-main fi-ini-dt-implant:INPUT-VALUE      
           p-dt-implant-fim     = INPUT FRAME f-main fi-fim-dt-implant:INPUT-VALUE      
           p-familia-ini        = INPUT FRAME f-main fi-ini-familia:INPUT-VALUE       
           p-familia-fim        = INPUT FRAME f-main fi-fim-familia:INPUT-VALUE
           p-it-codigo-ini      = INPUT FRAME f-main fi-ini-it-codigo:INPUT-VALUE      
           p-it-codigo-fim      = INPUT FRAME f-main fi-fim-it-codigo:INPUT-VALUE      
           p-dt-entrega-ini     = INPUT FRAME f-main fi-ini-dt-entrega:INPUT-VALUE      
           p-dt-entrega-fim     = INPUT FRAME f-main fi-fim-dt-entrega:INPUT-VALUE
           p-it-cli-ini         = INPUT FRAME f-main fi-ini-it-do-cli:INPUT-VALUE
           p-it-cli-fim         = INPUT FRAME f-main fi-fim-it-do-cli:INPUT-VALUE
           p-ge-codigo-ini      = INPUT FRAME f-main fi-ini-ge-codigo:INPUT-VALUE       
           p-ge-codigo-fim      = INPUT FRAME f-main fi-fim-ge-codigo:INPUT-VALUE  
           p-fm-cod-com-ini     = INPUT FRAME f-main fi-ini-cod-fm-com:INPUT-VALUE
           p-fm-cod-com-fim     = INPUT FRAME f-main fi-fim-cod-fm-com:INPUT-VALUE
           p-tg-itens-faturados = INPUT FRAME f-main tg-itens-faturados:INPUT-VALUE
           p-fi-ini-dt-fatur    = INPUT FRAME f-main fi-ini-dt-fatur:INPUT-VALUE
           p-fi-fim-dt-fatur    = INPUT FRAME f-main fi-fim-dt-fatur:INPUT-VALUE.

    ASSIGN p-ok = YES.

    APPLY "close":U TO THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-cod-estabel-ini
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-cod-estabel-ini w-window
ON F5 OF fi-cod-estabel-ini IN FRAME F-Main /* Estabelecimento */
DO:
    {include/zoomvar.i &prog-zoom=adzoom/z01ad107.r
                           &campo=fi-cod-estabel-ini
                           &campo2=fi-nome
                           &campozoom=cod-estabel
                           &campozoom2=nome
                           &FRAME={&FRAME-NAME}}

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-cod-estabel-ini w-window
ON LEAVE OF fi-cod-estabel-ini IN FRAME F-Main /* Estabelecimento */
DO:
    ASSIGN fi-nome:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "".
    FIND FIRST estabelec NO-LOCK                                                         
         WHERE estabelec.cod-estabel = fi-cod-estabel-ini:SCREEN-VALUE IN FRAME {&FRAME-NAME} NO-ERROR.
    IF AVAIL estabelec THEN
        ASSIGN fi-nome:SCREEN-VALUE IN FRAME {&FRAME-NAME} = estabelec.nome.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-cod-estabel-ini w-window
ON MOUSE-SELECT-DBLCLICK OF fi-cod-estabel-ini IN FRAME F-Main /* Estabelecimento */
DO:
    APPLY "F5" TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK w-window 


/* ***************************  Main Block  *************************** */

/* Include custom  Main Block code for SmartWindows. */
{src/adm/template/windowmn.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-create-objects w-window  _ADM-CREATE-OBJECTS
PROCEDURE adm-create-objects :
/*------------------------------------------------------------------------------
  Purpose:     Create handles for all SmartObjects used in this procedure.
               After SmartObjects are initialized, then SmartLinks are added.
  Parameters:  <none>
------------------------------------------------------------------------------*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-row-available w-window  _ADM-ROW-AVAILABLE
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI w-window  _DEFAULT-DISABLE
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
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(w-window)
  THEN DELETE WIDGET w-window.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI w-window  _DEFAULT-ENABLE
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
  DISPLAY fi-cod-estabel-ini fi-ini-cod-emitente fi-fim-cod-emitente 
          fi-ini-nome-abrev fi-fim-nome-abrev fi-ini-nr-pedido fi-fim-nr-pedido 
          fi-ini-nr-pedcli fi-fim-nr-pedcli fi-ini-dt-implant fi-fim-dt-implant 
          fi-ini-it-codigo fi-fim-it-codigo fi-ini-familia fi-fim-familia 
          fi-ini-ge-codigo fi-fim-ge-codigo fi-ini-cod-fm-com fi-fim-cod-fm-com 
          fi-ini-dt-entrega fi-fim-dt-entrega fi-ini-it-do-cli fi-fim-it-do-cli 
          fi-ini-dt-fatur fi-fim-dt-fatur tg-itens-faturados 
      WITH FRAME F-Main IN WINDOW w-window.
  ENABLE RECT-1 IMAGE-1 IMAGE-2 IMAGE-11 IMAGE-12 IMAGE-13 IMAGE-14 IMAGE-15 
         IMAGE-16 IMAGE-17 IMAGE-18 IMAGE-19 IMAGE-20 RECT-5 IMAGE-29 IMAGE-30 
         IMAGE-31 IMAGE-32 RECT-6 IMAGE-33 IMAGE-34 IMAGE-35 IMAGE-36 IMAGE-37 
         IMAGE-38 RECT-7 IMAGE-39 IMAGE-40 fi-cod-estabel-ini 
         fi-ini-cod-emitente fi-fim-cod-emitente fi-ini-nome-abrev 
         fi-fim-nome-abrev fi-ini-nr-pedido fi-fim-nr-pedido fi-ini-nr-pedcli 
         fi-fim-nr-pedcli fi-ini-dt-implant fi-fim-dt-implant fi-ini-it-codigo 
         fi-fim-it-codigo fi-ini-familia fi-fim-familia fi-ini-ge-codigo 
         fi-fim-ge-codigo fi-ini-cod-fm-com fi-fim-cod-fm-com fi-ini-dt-entrega 
         fi-fim-dt-entrega fi-ini-it-do-cli fi-fim-it-do-cli fi-ini-dt-fatur 
         fi-fim-dt-fatur tg-itens-faturados bt-ok bt-cancelar 
      WITH FRAME F-Main IN WINDOW w-window.
  {&OPEN-BROWSERS-IN-QUERY-F-Main}
  VIEW w-window.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-destroy w-window 
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-exit w-window 
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-initialize w-window 
PROCEDURE local-initialize :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

    /* Code placed here will execute PRIOR to standard behavior. */
    {include/win-size.i}
  
    {utp/ut9000.i "PRM1203A" "1.00.00.000"}

    /* Dispatch standard ADM method.                             */
    RUN dispatch IN THIS-PROCEDURE ( INPUT 'initialize':U ) .

    IF fi-cod-estabel-ini:LOAD-MOUSE-POINTER("image/lupa.cur") THEN.

    /* Code placed here will execute AFTER standard behavior.    */
    ASSIGN INPUT FRAME f-main fi-cod-estabel-ini:SCREEN-VALUE    = p-cod-estabel-ini 
           INPUT FRAME f-main fi-ini-cod-emitente:SCREEN-VALUE   = STRING(p-cod-emitente-ini)
           INPUT FRAME f-main fi-fim-cod-emitente:SCREEN-VALUE   = STRING(p-cod-emitente-fim)
           INPUT FRAME f-main fi-ini-nome-abrev:SCREEN-VALUE     = p-nome-abrev-ini
           INPUT FRAME f-main fi-fim-nome-abrev:SCREEN-VALUE     = p-nome-abrev-fim
           INPUT FRAME f-main fi-ini-nr-pedido:SCREEN-VALUE      = STRING(p-nr-pedido-ini)
           INPUT FRAME f-main fi-fim-nr-pedido:SCREEN-VALUE      = STRING(p-nr-pedido-fim)
           INPUT FRAME f-main fi-ini-nr-pedcli:SCREEN-VALUE      = STRING(p-nr-pedcli-ini)
           INPUT FRAME f-main fi-fim-nr-pedcli:SCREEN-VALUE      = STRING(p-nr-pedcli-fim)
           INPUT FRAME f-main fi-ini-dt-implant:SCREEN-VALUE     = STRING(p-dt-implant-ini)
           INPUT FRAME f-main fi-fim-dt-implant:SCREEN-VALUE     = STRING(p-dt-implant-fim)
           INPUT FRAME f-main fi-ini-familia:SCREEN-VALUE        =  p-familia-ini  
           INPUT FRAME f-main fi-fim-familia:SCREEN-VALUE        =  p-familia-fim  
           INPUT FRAME f-main fi-ini-it-codigo:SCREEN-VALUE      =  p-it-codigo-ini
           INPUT FRAME f-main fi-fim-it-codigo:SCREEN-VALUE      =  p-it-codigo-fim
           INPUT FRAME f-main fi-ini-dt-entrega:SCREEN-VALUE     = STRING(p-dt-entrega-ini)
           INPUT FRAME f-main fi-fim-dt-entrega:SCREEN-VALUE     = STRING(p-dt-entrega-fim)
           INPUT FRAME f-main tg-itens-faturados:SCREEN-VALUE    = STRING(p-tg-itens-faturados)
           INPUT FRAME f-main fi-ini-dt-fatur:SCREEN-VALUE       = STRING(p-fi-ini-dt-fatur)
           INPUT FRAME f-main fi-fim-dt-fatur:SCREEN-VALUE       = STRING(p-fi-fim-dt-fatur).

   APPLY "LEAVE" TO fi-cod-estabel-ini IN FRAME {&FRAME-NAME}.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE send-records w-window  _ADM-SEND-RECORDS
PROCEDURE send-records :
/*------------------------------------------------------------------------------
  Purpose:     Send record ROWID's for all tables used by
               this file.
  Parameters:  see template/snd-head.i
------------------------------------------------------------------------------*/

  /* SEND-RECORDS does nothing because there are no External
     Tables specified for this JanelaDetalhe, and there are no
     tables specified in any contained Browse, Query, or Frame. */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE state-changed w-window 
PROCEDURE state-changed :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
  DEFINE INPUT PARAMETER p-issuer-hdl AS HANDLE NO-UNDO.
  DEFINE INPUT PARAMETER p-state AS CHARACTER NO-UNDO.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

