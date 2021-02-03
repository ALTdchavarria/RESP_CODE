interface ZIF_OO_ADT_FAC_CLASSRUN_OUT
  public .

  METHODS write
    IMPORTING
      !data         TYPE any
      !name         TYPE string OPTIONAL
    RETURNING
      VALUE(output) TYPE REF TO zif_oo_adt_fac_classrun_out .
  METHODS get
    IMPORTING
      !data         TYPE any OPTIONAL
      !name         TYPE string OPTIONAL PREFERRED PARAMETER data
    RETURNING
      VALUE(output) TYPE string .

endinterface.
