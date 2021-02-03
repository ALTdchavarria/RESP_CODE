@EndUserText.label: 'Datos de factura'
@AccessControl.authorizationCheck: #NOT_REQUIRED

@UI: {
 headerInfo: { typeName: 'Factura', typeNamePlural: 'Facturas', title: { type: #STANDARD, value: 'Vbeln' } } }

@Search.searchable: true

define root view entity ZC_DATA_FAC
  as projection on ZI_DATA_FAC
{

      @UI: {
         lineItem:       [ { position: 10, importance: #HIGH } ],
         identification: [ { position: 10, label: 'Factura [1,...,99999999]' } ] }
      @Search.defaultSearchElement: true
  key Vbeln         as Vbeln,
      
      @UI: {
         lineItem:       [ { position: 20, importance: #MEDIUM } ],
         identification: [ { position: 20, label: 'Creado por [1,...,99999999]' } ] }
      @Search.defaultSearchElement: false
      CreatedBy     as CreatedBy,
      @UI: {
         lineItem:       [ { position: 30, importance: #MEDIUM } ],
         identification: [ { position: 30, label: 'Creado el  [1,...,99999999]' } ] }
      @Search.defaultSearchElement: false
      CreatedAt     as CreatedAt,
      @UI: {
         lineItem:       [ { position: 40, importance: #MEDIUM } ],
         identification: [ { position: 40, label: 'Ultimo cambio [1,...,99999999]' } ] }
      @Search.defaultSearchElement: false
      LastChangedBy as LastChangedBy,
      @UI: {
         lineItem:       [ { position: 50, importance: #MEDIUM } ],
         identification: [ { position: 50, label: 'Cambio por[1,...,99999999]' } ] }
      @Search.defaultSearchElement: false
      LastChangedAt as LastChangedAt

}
