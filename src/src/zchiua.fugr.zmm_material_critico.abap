FUNCTION ZMM_MATERIAL_CRITICO.
*"----------------------------------------------------------------------
*"*"Interfase local
*"  IMPORTING
*"     VALUE(MATNR) LIKE  CSAP_MBOM-MATNR
*"     VALUE(WERKS) LIKE  CSAP_MBOM-WERKS OPTIONAL
*"  TABLES
*"      T_PROH STRUCTURE  ZEST_PRIFRA_COLLECT_MP OPTIONAL
*"  EXCEPTIONS
*"      ERROR
*"----------------------------------------------------------------------

  DATA: TI_STPO  TYPE TABLE OF STPOX WITH HEADER LINE,
        T_MATCAT TYPE TABLE OF CSCMAT WITH HEADER LINE.

  DATA: TI_CLASS   TYPE TABLE OF SCLASS WITH HEADER LINE,
        TI_OBJDATA TYPE TABLE OF CLOBJDAT WITH HEADER LINE.

  DATA: V_VERPR  TYPE P DECIMALS 4, "LIKE MBEW-VERPR,  " Precio unitario
        V_PRECIO TYPE P DECIMALS 4,  " Precio total por material
        V_PRECIF TYPE P DECIMALS 4,  " Precio final del material original
        V_LINES  TYPE I,
        V_MATNR  LIKE AUSP-OBJEK,
        V_MENGE  LIKE STKO-BMENG,
        V_MNGKO  LIKE STKO-BMENG.

  DATA: V_FLOAT    TYPE F,
        V_FCHAR(8) TYPE C,
        R_TOPMAT   LIKE CSTMAT.

V_MENGE = 1000.

  SELECT SINGLE * FROM MARA WHERE MATNR = MATNR
                                  AND ( MSTAE = '11'
                                   OR MSTAE = '12'
                                   OR MSTAE = '13' ).
      IF SY-SUBRC = 0.
        CASE MARA-MSTAE.
          WHEN '11'.
            MOVE 'CRITICO'  TO T_PROH-VAL_IFRA.
          WHEN '12'.
            MOVE 'RIESGO'  TO T_PROH-VAL_IFRA.
          WHEN '13'.
            MOVE 'CONTROLADO'  TO T_PROH-VAL_IFRA.
          WHEN OTHERS.
        ENDCASE.
        MOVE-CORRESPONDING TI_STPO TO T_PROH.
        MOVE MATNR TO T_PROH-MATNR.


        COLLECT T_PROH.
        CLEAR T_PROH.
      ENDIF.

* CS_BOM_EXPL_MAT_V2 Estas es la función que hace toda la explosión sin buscar mas
  CALL FUNCTION 'CS_BOM_EXPL_MAT_V2'
    EXPORTING
      AUMNG                 = 0
      CUOBJ                 = '000000000000000'
      CAPID                 = ''
      DATUV                 = SY-DATUM
      EHNDL                 = '1'
      EMENG                 = V_MENGE
      MKTLS                 = 'X'
      MEHRS                 = 'X'
      MTNRV                 = MATNR
      STLAL                 = '1'
      STLAN                 = '1'
      STPST                 = 0
      SVWVO                 = 'X'
      WERKS                 = WERKS
      VRSVO                 = 'X'
    IMPORTING
      TOPMAT                = R_TOPMAT
    TABLES
      STB                   = TI_STPO
      MATCAT                = T_MATCAT
    EXCEPTIONS
      ALT_NOT_FOUND         = 1
      CALL_INVALID          = 2
      MATERIAL_NOT_FOUND    = 3
      MISSING_AUTHORIZATION = 4
      NO_BOM_FOUND          = 5
      NO_PLANT_DATA         = 6
      NO_SUITABLE_BOM_FOUND = 7
      CONVERSION_ERROR      = 8.


  IF SY-SUBRC = 0.
    LOOP AT TI_STPO.   " Recorre la lista para buscar materiales críticos
      SELECT SINGLE * FROM MARA WHERE MATNR = TI_STPO-IDNRK
                                  AND ( MSTAE = '11'
                                   OR MSTAE = '12'
                                   OR MSTAE = '13' ).
      IF SY-SUBRC = 0.
        CASE MARA-MSTAE.
          WHEN '11'.
            MOVE 'CRITICO'  TO T_PROH-VAL_IFRA.
          WHEN '12'.
            MOVE 'RIESGO'  TO T_PROH-VAL_IFRA.
          WHEN '13'.
            MOVE 'CONTROLADO'  TO T_PROH-VAL_IFRA.
          WHEN OTHERS.
        ENDCASE.
        MOVE-CORRESPONDING TI_STPO TO T_PROH.
        MOVE TI_STPO-IDNRK TO T_PROH-MATNR.


        COLLECT T_PROH.
        CLEAR T_PROH.
      ENDIF.
    ENDLOOP.
  ELSEIF SY-SUBRC <> 3.   " No tiene lista de material
    SELECT SINGLE * FROM MARA WHERE MATNR = MATNR
                                AND MSTAE = 'ZC'.
      IF SY-SUBRC = 0.
        MOVE MATNR      TO T_PROH-MATNR.
        MOVE 'CRITICO'  TO T_PROH-VAL_IFRA.

        COLLECT T_PROH.
        CLEAR T_PROH.
      ENDIF.
  ELSEIF SY-SUBRC = 3.

  ENDIF.

ENDFUNCTION.
