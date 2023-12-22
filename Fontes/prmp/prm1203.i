/*************************************************************************************************************************************************************************
** Copyright PRIME Consultoria (2015)                                                                                                                                   **
** Todos os Direitos Reservados.                                                                                                                                        **
**                                                                                                                                                                      **
** Este fonte Ç de propriedade exclusiva da PRIME Consultoria, sua reproduá∆oo parcial ou total por qualquer meio, s¢ poder† ser feita mediante autorizaá∆o expressa    **
**                                                                                                                                                                      **
**************************************************************************************************************************************************************************
** Programa .....: prm1203.i                                                                                                                                            **
** Data .........: Julho de 2022                                                                                                                                        **
** Autor ........: Prime Consultoria - Claudio R. Dornelas                                                                                                              **
** Objetivo .....: Incluide das Vari†veis do prm1203                                                                                                                    **
** Revis‰es **************************************************************************************************************************************************************
** Autor                    Ver.    Data        Cliente      Solicitante     Descriá∆o                                                                                  **
** Claudio Renato Dornelas  00.001  14/07/2022  BUSA         Caique          1)  Desenvolvimento inicial do programa                                                    **
**                                                                                                                                                                      **
*************************************************************************************************************************************************************************/
                                                                                
DEFINE VARIABLE c-cod-estabel-ini     AS CHARACTER   INITIAL "102"                              NO-UNDO.
DEFINE VARIABLE i-cod-emitente-ini    AS INTEGER     INITIAL 0                                  NO-UNDO.
DEFINE VARIABLE i-cod-emitente-fim    AS INTEGER     INITIAL 999999999                          NO-UNDO.
DEFINE VARIABLE c-nome-abrev-ini      AS CHARACTER   INITIAL ""                                 NO-UNDO.
DEFINE VARIABLE c-nome-abrev-fim      AS CHARACTER   INITIAL "ZZZZZZZZZZZZ"                     NO-UNDO.
DEFINE VARIABLE i-nr-pedido-ini       AS INTEGER     INITIAL 0                                  NO-UNDO.
DEFINE VARIABLE i-nr-pedido-fim       AS INTEGER     INITIAL 999999999                          NO-UNDO.
DEFINE VARIABLE c-nr-pedcli-ini       AS CHARACTER   INITIAL ""                                 NO-UNDO.
DEFINE VARIABLE c-nr-pedcli-fim       AS CHARACTER   INITIAL "ZZZZZZZZZZZZ"                     NO-UNDO.
DEFINE VARIABLE da-dt-implant-ini     AS DATE        INITIAL 01/01/2020                         NO-UNDO.
DEFINE VARIABLE da-dt-implant-fim     AS DATE        INITIAL 12/31/2999                         NO-UNDO.
DEFINE VARIABLE c-familia-ini         AS CHARACTER   INITIAL ""                                 NO-UNDO.
DEFINE VARIABLE c-familia-fim         AS CHARACTER   INITIAL "ZZZZZZZZ"                         NO-UNDO.
DEFINE VARIABLE c-it-codigo-ini       AS CHARACTER   INITIAL ""                                 NO-UNDO.
DEFINE VARIABLE c-it-codigo-fim       AS CHARACTER   INITIAL "ZZZZZZZZZZZZZZZZZ"                NO-UNDO.
DEFINE VARIABLE da-dt-entrega-ini     AS DATE        INITIAL 01/01/2020                         NO-UNDO.
DEFINE VARIABLE da-dt-entrega-fim     AS DATE        INITIAL 12/31/2999                         NO-UNDO.
DEFINE VARIABLE d-saldo               AS DECIMAL     		                                    NO-UNDO.
DEFINE VARIABLE c-it-cli-ini          AS CHARACTER   INITIAL ""                                 NO-UNDO.
DEFINE VARIABLE c-it-cli-fim          AS CHARACTER   INITIAL "ZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZ"   NO-UNDO.
DEFINE VARIABLE i-ge-codigo-ini       AS INTEGER     INITIAL 0                                  NO-UNDO.
DEFINE VARIABLE i-ge-codigo-fim       AS INTEGER     INITIAL 99                                 NO-UNDO.    
DEFINE VARIABLE c-fm-cod-com-ini      AS CHARACTER   INITIAL ""                                 NO-UNDO.
DEFINE VARIABLE c-fm-cod-com-fim      AS CHARACTER   INITIAL "ZZZZZZZZ"                         NO-UNDO. 
DEFINE VARIABLE tg-itens-faturados    AS LOG         INITIAL NO                                 NO-UNDO.
DEFINE VARIABLE p-fi-ini-dt-fatur     AS DATE        INITIAL 01/01/2020                         NO-UNDO.
DEFINE VARIABLE p-fi-fim-dt-fatur     AS DATE        INITIAL 12/31/2999                         NO-UNDO.
 
DEFINE VARIABLE h-acomp               AS HANDLE                              NO-UNDO.
DEFINE VARIABLE i-cont                AS INTEGER     INITIAL 0               NO-UNDO.
DEFINE VARIABLE h-colunas             AS HANDLE EXTENT                       NO-UNDO.
DEFINE VARIABLE p-OK                  AS LOG                                 NO-UNDO.
DEFINE VARIABLE c-campo               AS CHAR                                NO-UNDO.
DEFINE VARIABLE i-tot-cilind          AS INT                                 NO-UNDO.
DEFINE VARIABLE dec-tot-valor         AS DECIMAL                             NO-UNDO.
DEFINE VARIABLE d-dt-planeja          AS DATE                                NO-UNDO.
DEFINE VARIABLE i-seq-erro            AS INT                                 NO-UNDO.
DEFINE NEW GLOBAL SHARED VAR c-seg-usuario AS CHARACTER FORMAT "x(12)"       NO-UNDO.

DEFINE TEMP-TABLE tt-ped-item-prm NO-UNDO LIKE ped-item
    FIELD sel               AS LOGICAL
    FIELD cod-emitente      AS INTEGER
    FIELD nr-pedido         AS INTEGER
    FIELD dt-implant        AS DATE
    FIELD descricao         AS CHAR FORMAT "X(100)"
    FIELD politica          AS INTEGER
    FIELD qt-a-atender      AS DEC
    FIELD fm-codigo         AS CHAR FORMAT "X(40)"
    FIELD ge-codigo         AS CHAR FORMAT "X(40)"
    FIELD fm-cod-com        AS CHAR FORMAT "X(40)"
    FIELD dt-planeja        AS DATE
    FIELD compra-material   AS LOGICAL
    FIELD nr-ord-producao   AS INTEGER
    FIELD saldo-item        AS DEC
    FIELD item-do-cli       AS CHARACTER FORMAT "x(30)" /*Willian Santana 26/01/2023 - Inclus∆o item do cliente*/
    FIELD data-faturamento  AS DATE
    FIELD peso-bruto        AS DEC    
    .
