@AbapCatalog.sqlViewName: 'ZV_RESERVATIONS'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #CHECK
@EndUserText.label: 'Reservation Business Object'
define view Z4I_RESERVATIONS
  as select from z4t_reservations
{
  key mandt                                as Mandt,
  key db_key                               as DbKey,
      reservation_number                   as ReservationNumber,
      monster_number                       as MonsterNumber,
      @ObjectModel.virtualElement: true
      @ObjectModel.virtualElementCalculatedBy: 'ZCL_4_MONSTER_BUNNY_JUPITER'
      cast( '' as boolean preserving type) as JupiterOk,
      createdat                            as Createdat,
      lastchangedat                        as Lastchangedat,
      localastchangedat                    as Localastchangedat,
      createdby                            as Createdby,
      lastchangedby                        as Lastchangedby
}
