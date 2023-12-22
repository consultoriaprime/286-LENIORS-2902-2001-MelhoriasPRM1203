&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME w-window
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS w-window 
{include/i-prgvrs.i PRM1203B 1.00.00.000}

/* Chamada a include do gerenciador de licenáas. Necessario alterar os parametros */
/*                                                                                */
/* <programa>:  Informar qual o nome do programa.                                 */
/* <m¢dulo>:  Informar qual o m¢dulo a qual o programa pertence.                  */

&IF "{&EMSFND_VERSION}" >= "1.00" &THEN
    {include/i-license-manager.i PRM1203B MFT}
&ENDIF

/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */
{prmp/prm1203.i}

//DEFINE INPUT PARAMETER TABLE FOR tt-ped-item-prm.
DEFINE INPUT     PARAMETER rw-tt-ped-item-prm AS RAW       NO-UNDO.
DEFINE OUTPUT    PARAMETER p-ok-divide        AS LOGICAL   NO-UNDO.
DEFINE VARIABLE            c-erros            AS CHARACTER NO-UNDO.


/* Parameters Definitions ---                                           */

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
&Scoped-Define ENABLED-OBJECTS RECT-1 RECT-5 RECT-6 fi-nome-abrev fi-pedcli ~
fi-seq-item fi-item fi-item-descricao fi-qtd-atender fi-qtd-nova-seq ~
fi-qtd-seq-atual fi-dt-entrega bt-ok bt-cancelar 
&Scoped-Define DISPLAYED-OBJECTS fi-nome-abrev fi-pedcli fi-seq-item ~
fi-item fi-item-descricao fi-qtd-atender fi-qtd-nova-seq fi-qtd-seq-atual ~
fi-dt-entrega 

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

DEFINE VARIABLE fi-dt-entrega AS DATE FORMAT "99/99/9999":U INITIAL 01/01/1900 
     LABEL "Data Entrega" 
     VIEW-AS FILL-IN 
     SIZE 14.86 BY .88 NO-UNDO.

DEFINE VARIABLE fi-item AS CHARACTER FORMAT "X(256)":U 
     LABEL "Item" 
     VIEW-AS FILL-IN 
     SIZE 14.86 BY .88 NO-UNDO.

DEFINE VARIABLE fi-item-descricao AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 45 BY .88 NO-UNDO.

DEFINE VARIABLE fi-nome-abrev AS CHARACTER FORMAT "X(12)":U 
     LABEL "Nome Abrev." 
     VIEW-AS FILL-IN 
     SIZE 14.86 BY .88 NO-UNDO.

DEFINE VARIABLE fi-pedcli AS CHARACTER FORMAT "X(12)":U 
     LABEL "Pedido Cliente" 
     VIEW-AS FILL-IN 
     SIZE 14.86 BY .88 NO-UNDO.

DEFINE VARIABLE fi-qtd-atender AS DECIMAL FORMAT ">>,>>>,>>9.99":U INITIAL 0 
     LABEL "Qtdade Atender" 
     VIEW-AS FILL-IN 
     SIZE 14.86 BY .88 NO-UNDO.

DEFINE VARIABLE fi-qtd-nova-seq AS DECIMAL FORMAT ">>,>>>,>>9.99":U INITIAL 0 
     LABEL "Qtdade Nova Seq" 
     VIEW-AS FILL-IN 
     SIZE 14.86 BY .88 NO-UNDO.

DEFINE VARIABLE fi-qtd-seq-atual AS DECIMAL FORMAT "->>,>>>,>>9.99":U INITIAL 0 
     LABEL "Qtdade Seq Atual" 
     VIEW-AS FILL-IN 
     SIZE 14.86 BY .88 NO-UNDO.

DEFINE VARIABLE fi-seq-item AS INTEGER FORMAT ">>>>>>>>9":U INITIAL 0 
     LABEL "Seq Item" 
     VIEW-AS FILL-IN 
     SIZE 14.86 BY .88 NO-UNDO.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE    
     SIZE 74.29 BY 1.38
     BGCOLOR 7 .

DEFINE RECTANGLE RECT-5
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 74.29 BY 5.63.

DEFINE RECTANGLE RECT-6
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 74.29 BY 2.79.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     fi-nome-abrev AT ROW 2.17 COL 13.43 COLON-ALIGNED WIDGET-ID 12 NO-TAB-STOP 
     fi-pedcli AT ROW 3.17 COL 13.43 COLON-ALIGNED WIDGET-ID 62 NO-TAB-STOP 
     fi-seq-item AT ROW 4.17 COL 13.43 COLON-ALIGNED WIDGET-ID 4 NO-TAB-STOP 
     fi-item AT ROW 5.17 COL 13.43 COLON-ALIGNED WIDGET-ID 150 NO-TAB-STOP 
     fi-item-descricao AT ROW 5.17 COL 28.29 COLON-ALIGNED NO-LABEL WIDGET-ID 152 NO-TAB-STOP 
     fi-qtd-atender AT ROW 6.17 COL 13.43 COLON-ALIGNED WIDGET-ID 154 NO-TAB-STOP 
     fi-qtd-nova-seq AT ROW 8.17 COL 13.43 COLON-ALIGNED WIDGET-ID 156
     fi-qtd-seq-atual AT ROW 8.17 COL 58.43 COLON-ALIGNED WIDGET-ID 158 NO-TAB-STOP 
     fi-dt-entrega AT ROW 9.17 COL 13.43 COLON-ALIGNED WIDGET-ID 34
     bt-ok AT ROW 10.92 COL 3.14
     bt-cancelar AT ROW 10.92 COL 14.14
     RECT-1 AT ROW 10.71 COL 2.14
     RECT-5 AT ROW 1.92 COL 2.14 WIDGET-ID 78
     RECT-6 AT ROW 7.54 COL 2.14 WIDGET-ID 136
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 76.29 BY 11.38
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
         HEIGHT             = 11.29
         WIDTH              = 76.29
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
ASSIGN 
       fi-item:READ-ONLY IN FRAME F-Main        = TRUE.

ASSIGN 
       fi-item-descricao:READ-ONLY IN FRAME F-Main        = TRUE.

ASSIGN 
       fi-nome-abrev:READ-ONLY IN FRAME F-Main        = TRUE.

ASSIGN 
       fi-pedcli:READ-ONLY IN FRAME F-Main        = TRUE.

ASSIGN 
       fi-qtd-atender:READ-ONLY IN FRAME F-Main        = TRUE.

ASSIGN 
       fi-qtd-seq-atual:READ-ONLY IN FRAME F-Main        = TRUE.

ASSIGN 
       fi-seq-item:READ-ONLY IN FRAME F-Main        = TRUE.

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

   IF DEC(fi-qtd-nova-seq:SCREEN-VALUE IN FRAME {&FRAME-NAME}) <= 0 
       OR DEC(fi-qtd-nova-seq:SCREEN-VALUE IN FRAME {&FRAME-NAME}) >= tt-ped-item-prm.qt-a-atender THEN DO:
       /* ERRO: 17006*/
       RUN utp/ut-msgs.p (INPUT "Show",
                          INPUT 17006,
                          INPUT "Quantidade n∆o permitida!" + "~~" +
                                "Nova quantidade deve ser maior que 0 e menor que a quantidade a atender atual.").
       RETURN NO-APPLY.
   END.

   RUN pi-divide-item.
    
   IF p-ok-divide = YES THEN DO:
       RUN utp/ut-msgs.p (INPUT "SHOW":U,
                          INPUT 15825,
                          INPUT "Item dividido com sucesso!" + "~~" + 
                                "Nova sequància de item criada.").
       APPLY "close":U TO THIS-PROCEDURE.
   END.
 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-qtd-nova-seq
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-qtd-nova-seq w-window
ON ENTRY OF fi-qtd-nova-seq IN FRAME F-Main /* Qtdade Nova Seq */
DO:
   IF CAN-FIND(FIRST tt-ped-item-prm) THEN DO:
        ASSIGN fi-nome-abrev:SCREEN-VALUE IN FRAME {&FRAME-NAME}      = STRING(tt-ped-item-prm.nome-abrev)
               fi-pedcli:SCREEN-VALUE IN FRAME {&FRAME-NAME}          = STRING(tt-ped-item-prm.nr-pedcli)
               fi-seq-item:SCREEN-VALUE IN FRAME {&FRAME-NAME}        = STRING(tt-ped-item-prm.nr-sequencia)
               fi-item:SCREEN-VALUE IN FRAME {&FRAME-NAME}            = STRING(tt-ped-item-prm.it-codigo)
               fi-item-descricao:SCREEN-VALUE IN FRAME {&FRAME-NAME}  = STRING(tt-ped-item-prm.descricao)
               fi-qtd-atender:SCREEN-VALUE IN FRAME {&FRAME-NAME}     = STRING(tt-ped-item-prm.qt-a-atender)
               //fi-qtd-seq-atual:SCREEN-VALUE IN FRAME {&FRAME-NAME}   = STRING(tt-ped-item-prm.saldo-item)
               fi-dt-entrega:SCREEN-VALUE IN FRAME {&FRAME-NAME}      = STRING(TODAY)
            .
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-qtd-nova-seq w-window
ON LEAVE OF fi-qtd-nova-seq IN FRAME F-Main /* Qtdade Nova Seq */
DO:
  DEF VAR d-val-atualizado AS DEC INITIAL 0 NO-UNDO.

  ASSIGN d-val-atualizado = DEC(fi-qtd-atender:SCREEN-VALUE IN FRAME {&FRAME-NAME}) - DEC(fi-qtd-nova-seq:SCREEN-VALUE IN FRAME {&FRAME-NAME}).

  ASSIGN fi-qtd-seq-atual:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(d-val-atualizado).
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
  DISPLAY fi-nome-abrev fi-pedcli fi-seq-item fi-item fi-item-descricao 
          fi-qtd-atender fi-qtd-nova-seq fi-qtd-seq-atual fi-dt-entrega 
      WITH FRAME F-Main IN WINDOW w-window.
  ENABLE RECT-1 RECT-5 RECT-6 fi-nome-abrev fi-pedcli fi-seq-item fi-item 
         fi-item-descricao fi-qtd-atender fi-qtd-nova-seq fi-qtd-seq-atual 
         fi-dt-entrega bt-ok bt-cancelar 
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
  
    {utp/ut9000.i "PRM1203B" "1.00.00.000"}

    /* Dispatch standard ADM method.                             */
    RUN dispatch IN THIS-PROCEDURE ( INPUT 'initialize':U ) .

    CREATE tt-ped-item-prm.
    RAW-TRANSFER rw-tt-ped-item-prm TO tt-ped-item-prm.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-divide-item w-window 
PROCEDURE pi-divide-item :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEF VAR i-nova-qtd-item-original AS DEC INITIAL 0 NO-UNDO.
    DEF VAR i-nova-seq-item-novo     AS INT INITIAL 0 NO-UNDO.
    DEF VAR d-dt-entrega-novo-item   AS DATE          NO-UNDO.
    DEF BUFFER b-ped-item            FOR ped-item.
    DEF BUFFER b-ped-item-2          FOR ped-item.

    /*Gerar pr¢ximo n£mero de sequencia de acordo com a sequencia do pedido*/
    FOR EACH b-ped-item NO-LOCK 
       WHERE b-ped-item.nome-abrev    = tt-ped-item-prm.nome-abrev          
         AND b-ped-item.nr-pedcli     = tt-ped-item-prm.nr-pedcli           
         //AND b-ped-item.nr-sequencia  = tt-ped-item-prm.nr-sequencia        
         //AND b-ped-item.it-codigo     = tt-ped-item-prm.it-codigo           
         //AND b-ped-item.cod-refer     = tt-ped-item-prm.cod-refer 
        BY b-ped-item.nr-sequencia. 
        
        IF b-ped-item.nr-sequencia < 10 THEN DO:
            ASSIGN i-nova-seq-item-novo = 1.
        END.
        ELSE DO:
            ASSIGN i-nova-seq-item-novo = 10.
        END.
        LEAVE.
    END.
    /*Pegar £ltima sequencia do item do pedido*/
    FOR EACH b-ped-item-2 NO-LOCK 
       WHERE b-ped-item-2.nome-abrev    = tt-ped-item-prm.nome-abrev          
         AND b-ped-item-2.nr-pedcli     = tt-ped-item-prm.nr-pedcli           
         //AND b-ped-item-2.nr-sequencia  = tt-ped-item-prm.nr-sequencia        
         //AND b-ped-item-2.it-codigo     = tt-ped-item-prm.it-codigo           
         //AND b-ped-item-2.cod-refer     = tt-ped-item-prm.cod-refer 
        BY b-ped-item-2.nr-sequencia DESC.

        ASSIGN i-nova-seq-item-novo = i-nova-seq-item-novo + b-ped-item-2.nr-sequencia.
        LEAVE.
    END.
    

    /*Atualiza o saldo */
    FIND FIRST ped-item NO-LOCK 
         WHERE ped-item.nome-abrev    = tt-ped-item-prm.nome-abrev  
           AND ped-item.nr-pedcli     = tt-ped-item-prm.nr-pedcli   
           AND ped-item.nr-sequencia  = tt-ped-item-prm.nr-sequencia
           AND ped-item.it-codigo     = tt-ped-item-prm.it-codigo   
           AND ped-item.cod-refer     = tt-ped-item-prm.cod-refer NO-ERROR.

    ASSIGN i-nova-qtd-item-original = DEC(fi-qtd-nova-seq:SCREEN-VALUE IN FRAME {&FRAME-NAME})
           d-dt-entrega-novo-item   = DATE(fi-dt-entrega:SCREEN-VALUE IN FRAME {&FRAME-NAME}). 
    
    RUN prmapi\prmapi1203.p (INPUT  ped-item.nome-abrev,
                             INPUT  ped-item.nr-pedcli,
                             INPUT  ped-item.nr-sequencia,
                             INPUT  ped-item.it-codigo,
                             INPUT  ped-item.cod-refer,
                             //INPUT  6, /* Itens com data inferior */
                             INPUT i-nova-qtd-item-original,
                             INPUT d-dt-entrega-novo-item,
                             INPUT i-nova-seq-item-novo,
                             OUTPUT c-erros).
    /* *************************************************** */
    IF c-erros <> '' THEN DO:
        RUN utp/ut-msgs.p (INPUT 'show':u,
                           INPUT 17006,
                           INPUT 'Erro ao dividir item.~~'
                               + 'Erros encontrados: ' + c-erros).
    END.
    ELSE DO:
        ASSIGN p-ok-divide = YES.
    END.

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

