
CLASS zcl_generate_fac_data DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
  INTERFACES if_oo_adt_classrun.
*    INTERFACES zif_oo_adt_fac_data.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.

CLASS zcl_generate_fac_data IMPLEMENTATION.

  METHOD if_oo_adt_classrun~main.
   DATA itab TYPE TABLE OF zdata_vbrk.
   types ty_data TYPE zdata_vbrk.
    FIELD-SYMBOLS: <ls_vbrk> TYPE vbrk.
*    TYPES:
*      BEGIN OF ty_data,
*        client type MANDT,
*        vbeln TYPE VBELN_VF,
*
*      END   OF ty_data.
*   fill internal travel table (itab)
    SELECT * FROM vbrk INTO TABLE @DATA(zvbrk). " UP TO 10 ROWS.
    LOOP AT zvbrk ASSIGNING <ls_vbrk>.
      APPEND VALUE ty_data(    client = <ls_vbrk>-mandt
                               vbeln = <ls_vbrk>-vbeln
*                               created_at = sy-datum
                               created_by = sy-uname
*                               last_changed_at = sy-datum
                               last_changed_by = sy-uname
*                               created_at = cl_abap_context_info=>get_system_date
*                               created_by = cl_abap_context_info=>get_user_alias
*                               last_changed_at = cl_abap_context_info=>get_system_date
*                               last_changed_by = cl_abap_context_info=>get_user_alias
                               ) TO itab.
    ENDLOOP.
*   delete existing entries in the database table
    DELETE FROM zdata_vbrk.
*   insert the new table entries
    INSERT zdata_vbrk FROM TABLE @itab.
*   output the result as a console message
    out->write( |{ sy-dbcnt } Facturas ingresadas!| ).
  ENDMETHOD.

ENDCLASS.
