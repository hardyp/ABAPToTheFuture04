@AbapCatalog.sqlViewName: 'Z4V_MADL'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #CHECK
@EndUserText.label: 'Monster Atrocity Due List'
define view Z4C_MONSTER_ATROCITY_DUE_LIST
  as select from    z4t_order_header as header
    left outer join z4t_order_items  as items on header.order_number = items.order_number
{
  header.order_number              as OrderNumber,
  items.order_item                 as OrderItem,
  header.customer                  as Customer,
  header.delivery_address          as DeliveryAddress,
  items.task_description           as TaskDescription,
  items.castle_number              as CastleNumber,
  items.fd_target_date             as FdTargetDate,
  items.fd_target_time             as FdTargetTime,
  items.reservation_number         as ReservationNumber,
  items.foul_deed_status           as FoulDeedStatus,
  items.missng_monster_status      as MissngMonsterStatus,
  items.max_sanity_desired         as MaxSanityDesired,
  items.usage_desired              as UsageDesired,
  items.evilness_desired           as EvilnessDesired,
  items.scariness_desired          as ScarinessDesired,
  items.rages_per_day_desired      as RagesPerDayDesired,
  items.brain_size_desired         as BrainSizeDesired,
  items.color_desired              as ColorDesired,
  items.early_age_strength_desired as EarlyAgeStrengthDesired,
  items.eas_days_desired           as EasDaysDesired,
  items.growth_percentage_desired  as GrowthPercentageDesired,
  items.model_desired              as ModelDesired,
  items.osoup_percentage_desired   as OsoupPercentageDesired

}
