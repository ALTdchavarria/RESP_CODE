FUNCTION ZCO_MATERIAL_MATPRIM.
*"----------------------------------------------------------------------
*"*"Interfase local
*"  IMPORTING
*"     VALUE(MATNR) LIKE  CSAP_MBOM-MATNR
*"     VALUE(WERKS) LIKE  CSAP_MBOM-WERKS OPTIONAL
*"     VALUE(MENGE) LIKE  STKO-BMENG DEFAULT 1000
*"     VALUE(IFRA) TYPE  CADKZ DEFAULT ' '
*"  EXPORTING
*"     VALUE(VERPR) TYPE  CHAR15
*"     VALUE(PRECIO) TYPE  CHAR15
*"     VALUE(TOPMTNR) LIKE  CSTMAT STRUCTURE  CSTMAT
*"     VALUE(E_MENGE) TYPE  CHAR10
*"  TABLES
*"      STPO STRUCTURE  ZEST_PRIFRA OPTIONAL
*"      T_IFRA STRUCTURE  ZEST_PRIFRA_COLLECT_MP OPTIONAL
*"      F_IFRA STRUCTURE  ZEST_PRIFRA_COLLECT_FAM OPTIONAL
*"      T_NLIS STRUCTURE  ZEST_PRIFRA_COLLECT_MP OPTIONAL
*"      T_PROH STRUCTURE  ZEST_PRIFRA_COLLECT_MP OPTIONAL
*"  EXCEPTIONS
*"      ERROR
*"----------------------------------------------------------------------
  TABLES: ZFAM_IFRA.

  DATA: TI_STPO  TYPE TABLE OF STPOX WITH HEADER LINE,
        T_LISTA  TYPE TABLE OF STPOX WITH HEADER LINE,
        TI_STPOT TYPE TABLE OF ZEST_PRIFRA WITH HEADER LINE,
        TI_STPOTF TYPE TABLE OF ZEST_PRIFRA WITH HEADER LINE,
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
      V_VERPR = V_VERPR / 100.      " Este costo parece que hay que dividirlo entre 100, espero la confirmación de Tere

      V_PRECIO = V_VERPR * MENGE.
      V_PRECIF = V_PRECIO.
    ENDIF.
  ENDIF.

  IF IFRA = 'X'.    " Aqui comienza lo de IFRA
    LOOP AT TI_STPOT.
      MOVE TI_STPOT-MATNR TO V_MATNR.
      CLEAR TI_OBJDATA. REFRESH TI_OBJDATA.
      CALL FUNCTION 'CLAF_CLASSIFICATION_OF_OBJECTS'
        EXPORTING
          CLASS              = 'IFRA'
          CLASSTYPE          = 'Z01'
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
        LOOP AT TI_OBJDATA WHERE ATNAM = 'FAM_IFRA'.   " 'LISTADO' AND AUSP1 = 'Listado'.
          MOVE TI_OBJDATA-AUSP1 TO TI_STPOT-FAM_IFRA.

          TI_STPOT-VAL_IFRA = TI_STPOT-MNGKO * 100 / MENGE.  " Porcentaje de uso den la formula

        ENDLOOP.

        LOOP AT TI_OBJDATA WHERE ATNAM = 'RESTRICCION'.
          CASE TI_OBJDATA-AUSP1.
            WHEN 'LIBRE'.
              MOVE 'L' TO TI_STPOT-ZSTATIFRA.
            WHEN 'POR RESTRICCION'.
              MOVE 'R' TO TI_STPOT-ZSTATIFRA.
            WHEN 'POR ESPECIFICACION'.
              MOVE 'E' TO TI_STPOT-ZSTATIFRA.
            WHEN 'PROHIBIDO'.
              MOVE 'P' TO TI_STPOT-ZSTATIFRA.
            WHEN 'POR RESTRICCION/ESPECIFICACION'.
              MOVE 'X' TO TI_STPOT-ZSTATIFRA.
            WHEN OTHERS.
              MOVE 'L' TO TI_STPOT-ZSTATIFRA.
          ENDCASE.
          " MOVE TI_OBJDATA-AUSP1 TO TI_STPOT-ZSTATIFRA.

          MODIFY TI_STPOT.

        ENDLOOP.

        MODIFY TI_STPOT.
        MOVE-CORRESPONDING TI_STPOT TO TI_STPOTF.

        APPEND TI_STPOTF.
        CLEAR  TI_STPOTF.

        LOOP AT TI_OBJDATA WHERE ATNAM = 'LISTADO' AND ( AUSP1 = 'NO LISTADO' OR AUSP1 = '?' ).
          MOVE-CORRESPONDING TI_STPOT TO T_NLIS.
          MOVE ''                     TO T_NLIS-VAL_IFRA.

          APPEND T_NLIS.
        ENDLOOP.
******** Agregar la selección del prohibido
        LOOP AT TI_OBJDATA WHERE ATNAM = 'RESTRICCION' AND AUSP1 = 'PROHIBIDO'.
          MOVE-CORRESPONDING TI_STPOT TO T_PROH.
          MOVE 'PROHIBIDO'  TO T_PROH-VAL_IFRA.

          APPEND T_PROH.
        ENDLOOP.
      ELSE.
***** El material no tiene las caracteristicas de IFRA cargadas
        IF TI_STPOT-MNGKO > 0.
          MOVE-CORRESPONDING TI_STPOT TO T_NLIS.
          MOVE 'SIN DATOS IFRA'     TO T_NLIS-VAL_IFRA.

          APPEND T_NLIS.
        ENDIF.
      ENDIF.
    ENDLOOP.
*  ENDIF.

* Esto es para eliminar las diluciones de el NLIS
    LOOP AT T_NLIS.
      SELECT SINGLE * FROM MAST WHERE MATNR = T_NLIS-MATNR AND WERKS = WERKS.
      IF SY-SUBRC = 0.
        DELETE T_NLIS.
      ENDIF.
    ENDLOOP.

* Sumarizar las cantidades para IFRA
    LOOP AT TI_STPOT.
      LOOP AT T_NLIS WHERE MATNR = TI_STPOT-MATNR.
        EXIT.
      ENDLOOP.
      IF SY-SUBRC <> 0.
        MOVE-CORRESPONDING TI_STPOT TO T_IFRA.
        MOVE-CORRESPONDING TI_STPOT TO F_IFRA.

        IF T_IFRA-MNGKO <> 0 AND F_IFRA-VAL_IFRA IS NOT INITIAL.
          CLEAR: T_IFRA-VAL_IFRA.  " , F_IFRA-VAL_IFRA.
          COLLECT: T_IFRA.   ", F_IFRA.
        ENDIF.
      ENDIF.

      CLEAR:   T_IFRA, F_IFRA.
    ENDLOOP.

    LOOP AT TI_STPOTF.
*      LOOP AT T_NLIS WHERE MATNR = TI_STPOTF-MATNR.
*        EXIT.
*      ENDLOOP.
*      IF SY-SUBRC <> 0.
      MOVE-CORRESPONDING TI_STPOTF TO T_IFRA.
      MOVE-CORRESPONDING TI_STPOTF TO F_IFRA.

      IF T_IFRA-MNGKO <> 0 AND F_IFRA-VAL_IFRA IS NOT INITIAL.
        CLEAR: T_IFRA-VAL_IFRA, F_IFRA-VAL_IFRA.
        COLLECT: F_IFRA.
      ENDIF.
*      ENDIF.

      CLEAR:   T_IFRA, F_IFRA.
    ENDLOOP.

    LOOP AT F_IFRA.
      V_FLOAT = 0.
      F_IFRA-VAL_IFRA = F_IFRA-MNGKO * 100 / MENGE.  " Porcentaje de uso en la formula

      SELECT SINGLE * FROM ZFAM_IFRA WHERE ZNOM_FAM = F_IFRA-FAM_IFRA.

      IF SY-SUBRC = 0 AND ( F_IFRA-VAL_IFRA IS NOT INITIAL ).
        MOVE ZFAM_IFRA-ZSTATIFRA TO F_IFRA-ZSTATIFRA.
        IF ( ZFAM_IFRA-CLASS1 IS NOT INITIAL ).
          V_FLOAT = ZFAM_IFRA-CLASS1 / F_IFRA-VAL_IFRA * 100.

          IF V_FLOAT >= 100.
            F_IFRA-CLASS1 = 100.
          ELSE.
            CALL FUNCTION 'MURC_ROUND_FLOAT_TO_PACKED'
              EXPORTING
                IF_FLOAT              = V_FLOAT
*              IF_SIGNIFICANT_PLACES = 4
              IMPORTING
                EF_PACKED             = F_IFRA-CLASS1
              EXCEPTIONS
                OVERFLOW              = 1.
          ENDIF.
        ENDIF.

        IF ( ZFAM_IFRA-CLASS2 IS NOT INITIAL ).
          V_FLOAT = ZFAM_IFRA-CLASS2 / F_IFRA-VAL_IFRA * 100.

          IF V_FLOAT >= 100.
            F_IFRA-CLASS2 = 100.
          ELSE.
            CALL FUNCTION 'MURC_ROUND_FLOAT_TO_PACKED'
              EXPORTING
                IF_FLOAT              = V_FLOAT
*              IF_SIGNIFICANT_PLACES = 4
              IMPORTING
                EF_PACKED             = F_IFRA-CLASS2
              EXCEPTIONS
                OVERFLOW              = 1.
          ENDIF.
        ENDIF.

*        IF ( ZFAM_IFRA-CLASS_3 IS NOT INITIAL ).
*          V_FLOAT = ZFAM_IFRA-CLASS_3 / F_IFRA-VAL_IFRA * 100.
*
*          IF V_FLOAT >= 100.
*            F_IFRA-CLASS_3 = 100.
*          ELSE.
*            CALL FUNCTION 'MURC_ROUND_FLOAT_TO_PACKED'
*              EXPORTING
*                IF_FLOAT              = V_FLOAT
**              IF_SIGNIFICANT_PLACES = 4
*              IMPORTING
*                EF_PACKED             = F_IFRA-CLASS_3
*              EXCEPTIONS
*                OVERFLOW              = 1.
*          ENDIF.
*        ENDIF.

        IF ( ZFAM_IFRA-CLASS_3A IS NOT INITIAL ).
          V_FLOAT = ZFAM_IFRA-CLASS_3A / F_IFRA-VAL_IFRA * 100.

          IF V_FLOAT >= 100.
            F_IFRA-CLASS_3A = 100.
          ELSE.
            CALL FUNCTION 'MURC_ROUND_FLOAT_TO_PACKED'
              EXPORTING
                IF_FLOAT              = V_FLOAT
*              IF_SIGNIFICANT_PLACES = 4
              IMPORTING
                EF_PACKED             = F_IFRA-CLASS_3A
              EXCEPTIONS
                OVERFLOW              = 1.
          ENDIF.
        ENDIF.

        IF ( ZFAM_IFRA-CLASS_3B IS NOT INITIAL ).
          V_FLOAT = ZFAM_IFRA-CLASS_3B / F_IFRA-VAL_IFRA * 100.

          IF V_FLOAT >= 100.
            F_IFRA-CLASS_3B = 100.
          ELSE.
            CALL FUNCTION 'MURC_ROUND_FLOAT_TO_PACKED'
              EXPORTING
                IF_FLOAT              = V_FLOAT
*              IF_SIGNIFICANT_PLACES = 4
              IMPORTING
                EF_PACKED             = F_IFRA-CLASS_3B
              EXCEPTIONS
                OVERFLOW              = 1.
          ENDIF.
        ENDIF.

        IF ( ZFAM_IFRA-CLASS_3C IS NOT INITIAL ).
          V_FLOAT = ZFAM_IFRA-CLASS_3C / F_IFRA-VAL_IFRA * 100.

          IF V_FLOAT >= 100.
            F_IFRA-CLASS_3C = 100.
          ELSE.
            CALL FUNCTION 'MURC_ROUND_FLOAT_TO_PACKED'
              EXPORTING
                IF_FLOAT              = V_FLOAT
*              IF_SIGNIFICANT_PLACES = 4
              IMPORTING
                EF_PACKED             = F_IFRA-CLASS_3C
              EXCEPTIONS
                OVERFLOW              = 1.
          ENDIF.
        ENDIF.

        IF ( ZFAM_IFRA-CLASS_3D IS NOT INITIAL ).
          V_FLOAT = ZFAM_IFRA-CLASS_3D / F_IFRA-VAL_IFRA * 100.

          IF V_FLOAT >= 100.
            F_IFRA-CLASS_3D = 100.
          ELSE.
            CALL FUNCTION 'MURC_ROUND_FLOAT_TO_PACKED'
              EXPORTING
                IF_FLOAT              = V_FLOAT
*              IF_SIGNIFICANT_PLACES = 4
              IMPORTING
                EF_PACKED             = F_IFRA-CLASS_3D
              EXCEPTIONS
                OVERFLOW              = 1.
          ENDIF.
        ENDIF.

*        IF ( ZFAM_IFRA-CLASS_4 IS NOT INITIAL ).
*          V_FLOAT = ZFAM_IFRA-CLASS_4 / F_IFRA-VAL_IFRA * 100.
*
*          IF V_FLOAT >= 100.
*            F_IFRA-CLASS_4 = 100.
*          ELSE.
*            CALL FUNCTION 'MURC_ROUND_FLOAT_TO_PACKED'
*              EXPORTING
*                IF_FLOAT              = V_FLOAT
**              IF_SIGNIFICANT_PLACES = 4
*              IMPORTING
*                EF_PACKED             = F_IFRA-CLASS_4
*              EXCEPTIONS
*                OVERFLOW              = 1.
*          ENDIF.
*        ENDIF.

        IF ( ZFAM_IFRA-CLASS_4A IS NOT INITIAL ).
          V_FLOAT = ZFAM_IFRA-CLASS_4A / F_IFRA-VAL_IFRA * 100.

          IF V_FLOAT >= 100.
            F_IFRA-CLASS_4A = 100.
          ELSE.
            CALL FUNCTION 'MURC_ROUND_FLOAT_TO_PACKED'
              EXPORTING
                IF_FLOAT              = V_FLOAT
*              IF_SIGNIFICANT_PLACES = 4
              IMPORTING
                EF_PACKED             = F_IFRA-CLASS_4A
              EXCEPTIONS
                OVERFLOW              = 1.
          ENDIF.
        ENDIF.

        IF ( ZFAM_IFRA-CLASS_4B IS NOT INITIAL ).
          V_FLOAT = ZFAM_IFRA-CLASS_4B / F_IFRA-VAL_IFRA * 100.

          IF V_FLOAT >= 100.
            F_IFRA-CLASS_4B = 100.
          ELSE.
            CALL FUNCTION 'MURC_ROUND_FLOAT_TO_PACKED'
              EXPORTING
                IF_FLOAT              = V_FLOAT
*              IF_SIGNIFICANT_PLACES = 4
              IMPORTING
                EF_PACKED             = F_IFRA-CLASS_4B
              EXCEPTIONS
                OVERFLOW              = 1.
          ENDIF.
        ENDIF.

        IF ( ZFAM_IFRA-CLASS_4C IS NOT INITIAL ).
          V_FLOAT = ZFAM_IFRA-CLASS_4C / F_IFRA-VAL_IFRA * 100.

          IF V_FLOAT >= 100.
            F_IFRA-CLASS_4C = 100.
          ELSE.
            CALL FUNCTION 'MURC_ROUND_FLOAT_TO_PACKED'
              EXPORTING
                IF_FLOAT              = V_FLOAT
*              IF_SIGNIFICANT_PLACES = 4
              IMPORTING
                EF_PACKED             = F_IFRA-CLASS_4C
              EXCEPTIONS
                OVERFLOW              = 1.
          ENDIF.
        ENDIF.

        IF ( ZFAM_IFRA-CLASS_4D IS NOT INITIAL ).
          V_FLOAT = ZFAM_IFRA-CLASS_4D / F_IFRA-VAL_IFRA * 100.

          IF V_FLOAT >= 100.
            F_IFRA-CLASS_4D = 100.
          ELSE.
            CALL FUNCTION 'MURC_ROUND_FLOAT_TO_PACKED'
              EXPORTING
                IF_FLOAT              = V_FLOAT
*              IF_SIGNIFICANT_PLACES = 4
              IMPORTING
                EF_PACKED             = F_IFRA-CLASS_4D
              EXCEPTIONS
                OVERFLOW              = 1.
          ENDIF.
        ENDIF.

        IF ( ZFAM_IFRA-CLASS_5 IS NOT INITIAL ).
          V_FLOAT = ZFAM_IFRA-CLASS_5 / F_IFRA-VAL_IFRA * 100.

          IF V_FLOAT >= 100.
            F_IFRA-CLASS_5 = 100.
          ELSE.
            CALL FUNCTION 'MURC_ROUND_FLOAT_TO_PACKED'
              EXPORTING
                IF_FLOAT              = V_FLOAT
*              IF_SIGNIFICANT_PLACES = 4
              IMPORTING
                EF_PACKED             = F_IFRA-CLASS_5
              EXCEPTIONS
                OVERFLOW              = 1.
          ENDIF.
        ENDIF.

        IF ( ZFAM_IFRA-CLASS_6 IS NOT INITIAL ).
          V_FLOAT = ZFAM_IFRA-CLASS_6 / F_IFRA-VAL_IFRA * 100.

          IF V_FLOAT >= 100.
            F_IFRA-CLASS_6 = 100.
          ELSE.
            CALL FUNCTION 'MURC_ROUND_FLOAT_TO_PACKED'
              EXPORTING
                IF_FLOAT              = V_FLOAT
*              IF_SIGNIFICANT_PLACES = 4
              IMPORTING
                EF_PACKED             = F_IFRA-CLASS_6
              EXCEPTIONS
                OVERFLOW              = 1.
          ENDIF.
        ENDIF.

        IF ( ZFAM_IFRA-CLASS_7A IS NOT INITIAL ).
          V_FLOAT = ZFAM_IFRA-CLASS_7A / F_IFRA-VAL_IFRA * 100.

          IF V_FLOAT >= 100.
            F_IFRA-CLASS_7A = 100.
          ELSE.
            CALL FUNCTION 'MURC_ROUND_FLOAT_TO_PACKED'
              EXPORTING
                IF_FLOAT              = V_FLOAT
*              IF_SIGNIFICANT_PLACES = 4
              IMPORTING
                EF_PACKED             = F_IFRA-CLASS_7A
              EXCEPTIONS
                OVERFLOW              = 1.
          ENDIF.
        ENDIF.

        IF ( ZFAM_IFRA-CLASS_7B IS NOT INITIAL ).
          V_FLOAT = ZFAM_IFRA-CLASS_7B / F_IFRA-VAL_IFRA * 100.

          IF V_FLOAT >= 100.
            F_IFRA-CLASS_7B = 100.
          ELSE.
            CALL FUNCTION 'MURC_ROUND_FLOAT_TO_PACKED'
              EXPORTING
                IF_FLOAT              = V_FLOAT
*              IF_SIGNIFICANT_PLACES = 4
              IMPORTING
                EF_PACKED             = F_IFRA-CLASS_7B
              EXCEPTIONS
                OVERFLOW              = 1.
          ENDIF.
        ENDIF.

        IF ( ZFAM_IFRA-CLASS_8A IS NOT INITIAL ).
          V_FLOAT = ZFAM_IFRA-CLASS_8A / F_IFRA-VAL_IFRA * 100.

          IF V_FLOAT >= 100.
            F_IFRA-CLASS_8A = 100.
          ELSE.
            CALL FUNCTION 'MURC_ROUND_FLOAT_TO_PACKED'
              EXPORTING
                IF_FLOAT              = V_FLOAT
*              IF_SIGNIFICANT_PLACES = 4
              IMPORTING
                EF_PACKED             = F_IFRA-CLASS_8A
              EXCEPTIONS
                OVERFLOW              = 1.
          ENDIF.
        ENDIF.

        IF ( ZFAM_IFRA-CLASS_8B IS NOT INITIAL ).
          V_FLOAT = ZFAM_IFRA-CLASS_8B / F_IFRA-VAL_IFRA * 100.

          IF V_FLOAT >= 100.
            F_IFRA-CLASS_8B = 100.
          ELSE.
            CALL FUNCTION 'MURC_ROUND_FLOAT_TO_PACKED'
              EXPORTING
                IF_FLOAT              = V_FLOAT
*              IF_SIGNIFICANT_PLACES = 4
              IMPORTING
                EF_PACKED             = F_IFRA-CLASS_8B
              EXCEPTIONS
                OVERFLOW              = 1.
          ENDIF.
        ENDIF.

        IF ( ZFAM_IFRA-CLASS_9A IS NOT INITIAL ).
          V_FLOAT = ZFAM_IFRA-CLASS_9A / F_IFRA-VAL_IFRA * 100.

          IF V_FLOAT >= 100.
            F_IFRA-CLASS_9A = 100.
          ELSE.
            CALL FUNCTION 'MURC_ROUND_FLOAT_TO_PACKED'
              EXPORTING
                IF_FLOAT              = V_FLOAT
*              IF_SIGNIFICANT_PLACES = 4
              IMPORTING
                EF_PACKED             = F_IFRA-CLASS_9A
              EXCEPTIONS
                OVERFLOW              = 1.
          ENDIF.
        ENDIF.

        IF ( ZFAM_IFRA-CLASS_9B IS NOT INITIAL ).
          V_FLOAT = ZFAM_IFRA-CLASS_9B / F_IFRA-VAL_IFRA * 100.

          IF V_FLOAT >= 100.
            F_IFRA-CLASS_9B = 100.
          ELSE.
            CALL FUNCTION 'MURC_ROUND_FLOAT_TO_PACKED'
              EXPORTING
                IF_FLOAT              = V_FLOAT
*              IF_SIGNIFICANT_PLACES = 4
              IMPORTING
                EF_PACKED             = F_IFRA-CLASS_9B
              EXCEPTIONS
                OVERFLOW              = 1.
          ENDIF.
        ENDIF.

        IF ( ZFAM_IFRA-CLASS_9C IS NOT INITIAL ).
          V_FLOAT = ZFAM_IFRA-CLASS_9C / F_IFRA-VAL_IFRA * 100.

          IF V_FLOAT >= 100.
            F_IFRA-CLASS_9C = 100.
          ELSE.
            CALL FUNCTION 'MURC_ROUND_FLOAT_TO_PACKED'
              EXPORTING
                IF_FLOAT              = V_FLOAT
*              IF_SIGNIFICANT_PLACES = 4
              IMPORTING
                EF_PACKED             = F_IFRA-CLASS_9C
              EXCEPTIONS
                OVERFLOW              = 1.
          ENDIF.
        ENDIF.

        IF ( ZFAM_IFRA-CLASS_10A IS NOT INITIAL ).
          V_FLOAT = ZFAM_IFRA-CLASS_10A / F_IFRA-VAL_IFRA * 100.

          IF V_FLOAT >= 100.
            F_IFRA-CLASS_10A = 100.
          ELSE.
            CALL FUNCTION 'MURC_ROUND_FLOAT_TO_PACKED'
              EXPORTING
                IF_FLOAT              = V_FLOAT
*              IF_SIGNIFICANT_PLACES = 4
              IMPORTING
                EF_PACKED             = F_IFRA-CLASS_10A
              EXCEPTIONS
                OVERFLOW              = 1.
          ENDIF.
        ENDIF.

        IF ( ZFAM_IFRA-CLASS_10B IS NOT INITIAL ).
          V_FLOAT = ZFAM_IFRA-CLASS_10B / F_IFRA-VAL_IFRA * 100.

          IF V_FLOAT >= 100.
            F_IFRA-CLASS_10B = 100.
          ELSE.
            CALL FUNCTION 'MURC_ROUND_FLOAT_TO_PACKED'
              EXPORTING
                IF_FLOAT              = V_FLOAT
*              IF_SIGNIFICANT_PLACES = 4
              IMPORTING
                EF_PACKED             = F_IFRA-CLASS_10B
              EXCEPTIONS
                OVERFLOW              = 1.
          ENDIF.
        ENDIF.

        IF ( ZFAM_IFRA-CLASS_11 IS NOT INITIAL ).
          V_FLOAT = ZFAM_IFRA-CLASS_11 / F_IFRA-VAL_IFRA * 100.

          IF V_FLOAT >= 100.
            F_IFRA-CLASS_11 = 100.
          ELSE.
            CALL FUNCTION 'MURC_ROUND_FLOAT_TO_PACKED'
              EXPORTING
                IF_FLOAT              = V_FLOAT
*              IF_SIGNIFICANT_PLACES = 4
              IMPORTING
                EF_PACKED             = F_IFRA-CLASS_11
              EXCEPTIONS
                OVERFLOW              = 1.
          ENDIF.
        ENDIF.

*        IF ( ZFAM_IFRA-CLASS_11B IS NOT INITIAL ).
*          V_FLOAT = ZFAM_IFRA-CLASS_11B / F_IFRA-VAL_IFRA * 100.
*
*          IF V_FLOAT >= 100.
*            F_IFRA-CLASS_11B = 100.
*          ELSE.
*            CALL FUNCTION 'MURC_ROUND_FLOAT_TO_PACKED'
*              EXPORTING
*                IF_FLOAT              = V_FLOAT
**              IF_SIGNIFICANT_PLACES = 4
*              IMPORTING
*                EF_PACKED             = F_IFRA-CLASS_11B
*              EXCEPTIONS
*                OVERFLOW              = 1.
*          ENDIF.
*        ENDIF.

      ENDIF.

      MODIFY F_IFRA.
    ENDLOOP.
  ENDIF.

  DELETE F_IFRA WHERE FAM_IFRA IS INITIAL.

  VERPR = V_VERPR.   " Regresa el costo por gramo (unidad)
  PRECIO = V_PRECIF. " Regresa el Costo por la cantidad de producto

  STPO[] = TI_STPOT[].
*  T_STKO[] = TI_STKO[].

ENDFUNCTION.
