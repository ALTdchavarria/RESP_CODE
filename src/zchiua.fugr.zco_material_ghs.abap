FUNCTION ZCO_MATERIAL_GHS.
*"----------------------------------------------------------------------
*"*"Interfase local
*"  IMPORTING
*"     VALUE(MATNR) LIKE  CSAP_MBOM-MATNR
*"     VALUE(WERKS) LIKE  CSAP_MBOM-WERKS
*"     VALUE(MENGE) LIKE  STKO-BMENG DEFAULT 1000
*"     VALUE(IFRA) TYPE  CADKZ DEFAULT ' '
*"  EXPORTING
*"     VALUE(VERPR) TYPE  CHAR15
*"     VALUE(PRECIO) TYPE  CHAR15
*"     VALUE(TOPMTNR) LIKE  CSTMAT STRUCTURE  CSTMAT
*"     VALUE(E_MENGE) TYPE  CHAR10
*"  TABLES
*"      STPO STRUCTURE  ZEST_PRGHS OPTIONAL
*"      T_IFRA STRUCTURE  ZEST_PRIFRA_COLLECT_MP OPTIONAL
*"      F_IFRA STRUCTURE  ZEST_PRIFRA_COLLECT_FAM OPTIONAL
*"      T_NLIS STRUCTURE  ZEST_PRIFRA_COLLECT_MP OPTIONAL
*"      T_PROH STRUCTURE  ZEST_PRIFRA_COLLECT_MP OPTIONAL
*"      STPO_GHS STRUCTURE  ZEST_PRGHS OPTIONAL
*"  EXCEPTIONS
*"      ERROR
*"----------------------------------------------------------------------

*TABLES: ZFAM_IFRA.

  DATA: TI_STPO  TYPE TABLE OF STPOX WITH HEADER LINE,
        T_LISTA  TYPE TABLE OF STPOX WITH HEADER LINE,
        TI_STPOT TYPE TABLE OF ZEST_PRGHS WITH HEADER LINE,
        TI_STPOTF TYPE TABLE OF ZEST_PRGHS WITH HEADER LINE,
        TI_STPOC TYPE TABLE OF STPO_API02 WITH HEADER LINE,
        TI_STKO  TYPE TABLE OF STKO_API02 WITH HEADER LINE,
        TI_STKOT TYPE TABLE OF STKO_API02 WITH HEADER LINE,
        TI_STKOC TYPE TABLE OF STKO_API02 WITH HEADER LINE,
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

  VERPR = 0.
  V_VERPR = 0.
  V_PRECIF = 0.
  V_MENGE = 0.
  V_FLOAT = 0.
* CS_BOM_EXPL_MAT_V2 Estas es la función que hace toda la explosión sin buscar mas
  CALL FUNCTION 'CS_BOM_EXPL_MAT_V2'
    EXPORTING
      AUMNG                 = 0
      CUOBJ                 = '000000000000000'
      CAPID                 = ''
      DATUV                 = SY-DATUM
      EHNDL                 = '1'
      EMENG                 = MENGE
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
    " Revisa si es Material con BOM de familias (Dummy)
    IF R_TOPMAT-STLST = '03'.
      SELECT SINGLE * FROM MBEW
                   WHERE MATNR = MATNR.

      IF SY-SUBRC = 0.
        IF MBEW-VPRSV = 'V'.
          V_VERPR = MBEW-VERPR / MBEW-PEINH.
        ELSE.
          V_VERPR = MBEW-STPRS / MBEW-PEINH.
        ENDIF.
*      V_VERPR = MBEW-VERPR / MBEW-PEINH.
        V_VERPR = V_VERPR / 100.      " Este costo parece que hay que dividirlo entre 100, espero la confirmación de Tere

        V_PRECIO = V_VERPR * MENGE.
        V_PRECIF = V_PRECIO.
      ENDIF.
    ENDIF.
    LOOP AT TI_STPO.    " WHERE SBDKZ IS INITIAL.
      V_MNGKO = 0.
      SELECT SINGLE * FROM MBEW
                   WHERE MATNR = TI_STPO-IDNRK.

      IF SY-SUBRC = 0.
        IF MBEW-VPRSV = 'V'.
          V_VERPR = MBEW-VERPR / MBEW-PEINH.
        ELSE.
          V_VERPR = MBEW-STPRS / MBEW-PEINH.
        ENDIF.
        V_VERPR = V_VERPR / 100.      " Este costo parece que hay que dividirlo entre 100, espero la confirmación de Tere

*****   Revisar si el material tiene BOM
        READ TABLE T_MATCAT WITH KEY MATNR = TI_STPO-IDNRK.

        IF SY-SUBRC = 0.
          CALL FUNCTION 'CS_BOM_EXPL_MAT_V2'
            EXPORTING
              AUMNG                 = 0
              CUOBJ                 = '000000000000000'
              CAPID                 = ''
              DATUV                 = SY-DATUM
              EHNDL                 = '1'
              EMENG                 = MENGE
              MKTLS                 = 'X'
              MEHRS                 = 'X'
              MTNRV                 = TI_STPO-IDNRK
              STLAL                 = '1'
              STLAN                 = '1'
              STPST                 = 0
              SVWVO                 = 'X'
              WERKS                 = WERKS
              VRSVO                 = 'X'
            IMPORTING
              TOPMAT                = R_TOPMAT
            TABLES
              STB                   = T_LISTA
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

          IF R_TOPMAT-STLST = '03'.
            MOVE 0 TO V_MNGKO.
          ELSE.
            V_MENGE = V_MENGE + TI_STPO-MNGKO.
            MOVE TI_STPO-MNGLG TO V_MNGKO.
          ENDIF.
        ELSE.
          V_MENGE = V_MENGE + TI_STPO-MNGKO.
          MOVE TI_STPO-MNGLG TO V_MNGKO.
        ENDIF.



        V_PRECIO = V_VERPR * TI_STPO-MNGKO.


        IF TI_STPO-SANKA = ''.     " El material tiene indicador N de no almacen entonces el precio es cero
          V_PRECIO = 0.
        ENDIF.

        MOVE V_PRECIO TO TI_STPO-VERPR.
        MOVE 1 TO TI_STPO-PREIH.

        MOVE: TI_STPO-IDNRK TO TI_STPOT-MATNR,
              TI_STPO-OJTXP TO TI_STPOT-MAKTX,
              V_VERPR       TO TI_STPOT-VERPR,
              TI_STPO-PREIH TO TI_STPOT-PEINH,
              V_PRECIO      TO TI_STPOT-WRBTR,
              TI_STPO-MNGLG TO TI_STPOT-MNGLG,
              V_MNGKO       TO TI_STPOT-MNGKO,
              TI_STPO-TTIDX TO TI_STPOT-TTIDX,
              TI_STPO-OJTXP TO TI_STPOT-OJTXP,
              TI_STPO-OJTXB TO TI_STPOT-OJTXB.

        READ TABLE T_MATCAT WITH KEY MATNR = TI_STPO-IDNRK.
        IF TI_STPO-TTIDX = 1 AND SY-SUBRC = 0 AND TI_STPO-SANKA = 'X'
           AND R_TOPMAT-STLST = '01'.   "TI_STPO-SBDKZ IS NOT INITIAL.
          MOVE: 0           TO TI_STPOT-WRBTR,
                0           TO TI_STPOT-MNGKO,
                0           TO V_PRECIO.
        ELSEIF TI_STPO-TTIDX > 1 AND TI_STPO-SANKA = 'X'
           AND R_TOPMAT-STLST = '01' AND TI_STPO-SBDKZ IS NOT INITIAL..
          MOVE: 0           TO TI_STPOT-WRBTR,
                0           TO TI_STPOT-MNGKO,
                0           TO V_PRECIO.
        ENDIF.

        V_PRECIF = V_PRECIF + V_PRECIO.
        "MODIFY TI_STPO.
        APPEND TI_STPOT.
        CLEAR  TI_STPOT.
      ELSE.    "SANKA = ''
        V_MENGE = V_MENGE + TI_STPO-MNGKO.
        MOVE 1 TO TI_STPO-PREIH.

        MOVE: TI_STPO-IDNRK TO TI_STPOT-MATNR,
              TI_STPO-OJTXP TO TI_STPOT-MAKTX,
              0             TO TI_STPOT-VERPR,
              TI_STPO-PREIH TO TI_STPOT-PEINH,
              0             TO TI_STPOT-WRBTR,
              TI_STPO-MNGLG TO TI_STPOT-MNGLG,
              TI_STPO-MNGLG TO TI_STPOT-MNGKO,
              TI_STPO-TTIDX TO TI_STPOT-TTIDX,
              TI_STPO-OJTXP TO TI_STPOT-OJTXP,
              TI_STPO-OJTXB TO TI_STPOT-OJTXB.

        IF TI_STPO-SBDKZ IS NOT INITIAL.
          MOVE: 0           TO TI_STPOT-WRBTR,
                0           TO TI_STPOT-MNGKO,
                0           TO V_PRECIO.
        ENDIF.

        APPEND TI_STPOT.
        CLEAR  TI_STPOT.
      ENDIF.
    ENDLOOP.
    IF V_MENGE < 1.
      V_MENGE = 1.
    ENDIF.
    V_VERPR = V_PRECIF / V_MENGE.
    E_MENGE = V_MENGE.
  ELSE.    " El material no tiene lista y se puede buscar el costo por gramo
    SELECT SINGLE * FROM MBEW
                   WHERE MATNR = MATNR.

    IF SY-SUBRC = 0.
      IF MBEW-VPRSV = 'V'.
        V_VERPR = MBEW-VERPR / MBEW-PEINH.
      ELSE.
        V_VERPR = MBEW-STPRS / MBEW-PEINH.
      ENDIF.
*      V_VERPR = MBEW-VERPR / MBEW-PEINH.
      V_VERPR = V_VERPR / 100.      " Este costo parece que hay que dividirlo entre 100

      V_PRECIO = V_VERPR * MENGE.
      V_PRECIF = V_PRECIO.
    ENDIF.
  ENDIF.

* Sumatoria de cantidades
  LOOP AT TI_STPOT.   " WHERE WRBTR > 0.
    V_VERPR = TI_STPOT-WRBTR.
    IF V_VERPR > 0.
      MOVE-CORRESPONDING TI_STPOT TO TI_STPOTF.
      CLEAR: TI_STPOTF-OJTXB,
             TI_STPOTF-VAL_IFRA.

      COLLECT TI_STPOTF.
    ENDIF.

    CLEAR V_VERPR.
  ENDLOOP.

  REFRESH TI_STPOT.

  TI_STPOT[] = TI_STPOTF[].

  IF IFRA = 'X'.    " Aqui comienza lo de GHS aunque usa las tablas de IFRA
    LOOP AT TI_STPOT.
      MOVE TI_STPOT-MATNR TO V_MATNR.
      CLEAR TI_OBJDATA. REFRESH TI_OBJDATA.
      CALL FUNCTION 'CLAF_CLASSIFICATION_OF_OBJECTS'
        EXPORTING
          CLASS              = 'GHS'
          CLASSTYPE          = 'Z10'
          OBJECT             = V_MATNR
          OBJECTTABLE        = 'MARA'
        TABLES
          T_CLASS            = TI_CLASS
          T_OBJECTDATA       = TI_OBJDATA
        EXCEPTIONS
          NO_CLASSIFICATION  = 1
          NO_CLASSTYPES      = 2
          INVALID_CLASS_TYPE = 3.

      IF SY-SUBRC = 0.
        TI_STPOT-VAL_IFRA = TI_STPOT-MNGKO * 100 / MENGE.  " Porcentaje de uso den la formula
        LOOP AT TI_OBJDATA WHERE ATNAM = 'TOX_ORAL'.   " Toxicidad oral
*          MOVE TI_OBJDATA-AUSP1 TO TI_STPOT-ORAL.
          MOVE TI_OBJDATA-ATFLV TO TI_STPOT-ORAL.

*          TI_STPOT-VAL_IFRA = TI_STPOT-MNGKO * 100 / MENGE.  " Porcentaje de uso den la formula
        ENDLOOP.

        LOOP AT TI_OBJDATA WHERE ATNAM = 'TOX_DERMAL'.   " Toxicidad dermal
          MOVE TI_OBJDATA-ATFLV TO TI_STPOT-DERMAL.    " Hay que crear el campo en otra estructura para dermal

*          TI_STPOT-VAL_IFRA = TI_STPOT-MNGKO * 100 / MENGE.  " Porcentaje de uso den la formula, hay que crear otro campo % dermal
        ENDLOOP.

        LOOP AT TI_OBJDATA WHERE ATNAM = 'TOX_ACUATICA'.   " Toxicidad dermal
          MOVE TI_OBJDATA-ATFLV TO TI_STPOT-ACUATICA.    " Hay que crear el campo en otra estructura para acatica

*          TI_STPOT-VAL_IFRA = TI_STPOT-MNGKO * 100 / MENGE.  " Porcentaje de uso den la formula, hay que crear otro campo % acuatica
        ENDLOOP.

        MODIFY TI_STPOT.

      ELSE.    " No tieme datos GHS por lo que debe agregarse sumando 0

      ENDIF.
    ENDLOOP.

    STPO[] = TI_STPOT[].

    DELETE TI_STPOT WHERE ( ORAL IS INITIAL AND DERMAL IS INITIAL AND ACUATICA IS INITIAL ) .
    SORT TI_STPOT BY MNGLG DESCENDING.

    STPO_GHS[] = TI_STPOT[].
  ENDIF.

ENDFUNCTION.
