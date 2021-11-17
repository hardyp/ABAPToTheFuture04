@AbapCatalog.sqlViewName: 'ZV_SO_ITEMS'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Monster Sales Order Items'
@ObjectModel.modelCategory: #BUSINESS_OBJECT
@ObjectModel.writeEnabled: true
@ObjectModel.writeActivePersistence: 'Z4T_ORDER_ITEMS'
@ObjectModel.createEnabled: true
@ObjectModel.deleteEnabled: true
@ObjectModel.updateEnabled: true
define view Z4I_MONSTER_SO_ITEMS
  as select from z4t_order_items as order_items
  association [1..1] to Z4I_MONSTER_SO_HEADER as _order_header on order_items.db_key = _order_header.db_key
{
      @ObjectModel.association.type: [#TO_COMPOSITION_ROOT,
                                      #TO_COMPOSITION_PARENT]
      _order_header,
  key mandt,
  key db_key,
      parent_key,
      order_number,
      order_item,
      task_description,
      castle_number,
      fd_target_date,
      fd_target_time,
      reservation_number,
      foul_deed_status,
      missng_monster_status,
      max_sanity_desired,
      usage_desired,
      evilness_desired,
      scariness_desired,
      rages_per_day_desired,
      brain_size_desired,
      color_desired,
      early_age_strength_desired,
      eas_days_desired,
      growth_percentage_desired,
      model_desired,
      osoup_percentage_desired,
      createdat,
      lastchangedat,
      createdby,
      lastchangedby
}
