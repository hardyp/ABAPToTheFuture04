@AbapCatalog.sqlViewName: 'ZV4_MONSTERS'
@AbapCatalog.compiler.compareFilter: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Monster CDS View'
// Listing 06.28 : Complete DDL Source for Generating CDS View
define view Zcds_4_Monsters_Join
  as select from z4t_monster_head as monster_header
    inner join   z4t_monster_itms as monster_items on monster_header.monster_number = monster_items.monster_number
{
  key monster_header.monster_number          as monster_number,
      monster_header.name                    as monster_name,
      SUBSTRING( monster_header.name, 1, 1 ) as first_initial,
      case monster_header.evilness
      when 'BANK' then 'REALLY SCARY'
      when 'VERY' then
        case monster_header.strength
          when 100 then 'SCARY'
          else 'NOT SO SCARY REALLY'
        end
      when 'EVIL' then 'SLIGHTLY SCARY'
      else 'NOT REALLY SCARY AT ALL'
      end                                    as scariness_description
}
where
      monster_header.sanity_percentage < 10
  and monster_header.color             = 'GREN'
