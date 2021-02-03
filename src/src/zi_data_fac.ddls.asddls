@AbapCatalog.sqlViewName: 'ZIDATAFAC'
@AbapCatalog.compiler.compareFilter: true
@EndUserText.label: 'Datos de factura'
@AccessControl.authorizationCheck: #CHECK
@AbapCatalog.preserveKey: true
define root view ZI_DATA_FAC
  as select from zdata_vbrk
{
  key vbeln           as Vbeln,
      /*-- Admin data --*/
      @Semantics.user.createdBy: true
      created_by      as CreatedBy,
      @Semantics.systemDateTime.createdAt: true
      created_at      as CreatedAt,
      @Semantics.user.lastChangedBy: true
      last_changed_by as LastChangedBy,
      @Semantics.systemDateTime.lastChangedAt: true
      last_changed_at as LastChangedAt
      //                                                   created_by as CreatedBy,
      //                                                   created_at as CreatedAt,
      //                                                   last_changed_by as LastChangedBy,
      //                                                   last_changed_at as LastChangedAt
}
