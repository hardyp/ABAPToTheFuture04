@AbapCatalog.sqlViewName: 'Z4V_SO_HEAD'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Monster Sales Order Header'
@ObjectModel.compositionRoot: true
@ObjectModel.modelCategory: #BUSINESS_OBJECT
@ObjectModel.writeEnabled: true
@ObjectModel.writeActivePersistence: 'Z4T_ORDER_HEADER'
@ObjectModel.createEnabled: true
@ObjectModel.deleteEnabled: true
@ObjectModel.updateEnabled: true
@ObjectModel.transactionalProcessingEnabled: true
@OData.publish: true
define view Z4I_MONSTER_SO_HEADER
  as select from z4t_order_header as order_header
  association [1..*] to Z4I_MONSTER_SO_ITEMS as _order_items on order_header.db_key = _order_items.db_key
{
  key mandt,
  key db_key,
      @ObjectModel.association.type: #TO_COMPOSITION_CHILD
      _order_items,
      order_number,
      customer,
      delivery_address,
      createdat,
      lastchangedat,
      createdby,
      lastchangedby
}
