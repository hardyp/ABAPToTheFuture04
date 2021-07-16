@AbapCatalog.sqlViewName: 'Z4V_MONS_DELS'
@AbapCatalog.compiler.compareFilter: true
@AccessControl.authorizationCheck: #CHECK
@EndUserText.label: 'Monster Delivery CDS View'
@ObjectModel.modelCategory: #BUSINESS_OBJECT
@ObjectModel.alternativeKey: [{element: ['DELIVERY_NUMBER']}]
@ObjectModel.semanticKey: ['DELIVERY_NUMBER']
@ObjectModel.compositionRoot: true
@ObjectModel.writeEnabled: true
@ObjectModel.writeActivePersistence: 'Z4T_DELIVERIES'
@ObjectModel.createEnabled: true
@ObjectModel.deleteEnabled: true
@ObjectModel.updateEnabled: true
@ObjectModel.transactionalProcessingEnabled: true
@ObjectModel.draftEnabled: true
@ObjectModel.writeDraftPersistence: 'Z4T_DRAFT_DELS'

define view Z4CDS_MONSTER_DELIVERIES
  as select from z4t_deliveries as delivery
{
  key delivery.db_key,
      delivery.delivery_number,
      delivery.order_number,
      delivery.order_item,
      delivery.monster_number,
      delivery.monster_name,
      delivery.castle_number,
      delivery.village_number,
      delivery.village_address,
      delivery.task_description,
      delivery.due_date,
      delivery.due_time,
      delivery.actual_date,
      delivery.actual_time,
      delivery.current_status
}
