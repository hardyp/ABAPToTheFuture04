@AbapCatalog.sqlViewName: 'Z4V_MAM'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #CHECK
@EndUserText.label: 'Monster Atrocity Monitor'
/*+[hideWarning] { "IDS" : [ "KEY_CHECK" ]  } */
define view z4C_MONSTER_ATROCITY_MONITOR
  as select from z4t_deliveries
{
  delivery_number  as DeliveryNumber,
  order_number     as OrderNumber,
  order_item       as OrderItem,
  monster_number   as MonsterNumber,
  monster_name     as MonsterName,
  castle_number    as CastleNumber,
  village_number   as VillageNumber,
  village_address  as VillageAddress,
  task_description as TaskDescription,
  due_date         as DueDate,
  due_time         as DueTime,
  actual_date      as ActualDate,
  actual_time      as ActualTime,
  actual_no_scared as ActualNoScared,
  current_status   as CurrentStatus
}
