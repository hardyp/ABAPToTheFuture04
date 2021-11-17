interface ZIF_4_MONSTER_SIMULATOR
  public .


  types:
    BEGIN OF M_TYP_MONSTERS_UNDER_BED,
    bed_name      TYPE string,
    monster_type  TYPE string,
    total_of_type TYPE int4,
  END OF   M_TYP_MONSTERS_UNDER_BED .
  types:
    m_tt_monsters_under_bed TYPE STANDARD TABLE OF M_TYP_MONSTERS_UNDER_BED WITH EMPTY KEY .

  methods CALCULATE_SCARINESS
    importing
      !IS_BOM_INPUT_DATA type Z4S_MONSTER_INPUT_DATA
    returning
      value(RD_SCARINESS) type Z4DE_MONSTER_SCARINESS .
  methods SIMULATE_MONSTER_BOM
    importing
      !IS_BOM_INPUT_DATA type Z4S_MONSTER_INPUT_DATA
    returning
      value(RT_BOM_DATA) type Z4TT_MONSTER_ITEMS .
  methods DERIVE_ALL_HEADS_UNDER_BED
    importing
      !ID_BED_NAME type STRING
    returning
      value(RESULT) type INT4 .
  methods OPEN_MONSTERS_EYES
    raising
      ZCX_4_STATIC_ELECTRICITY .
  methods GET_COMPONENT_SPLIT
    importing
      !ID_STRENGTH type Z4DE_MONSTER_STRENGTH
      !ID_BRAIN_SIZE type Z4DE_MONSTER_BRAIN_SIZE
      !ID_SANITY type Z4DE_MONSTER_SANITY
    exporting
      !ID_SSATN type Z4DE_COMPONENT_TYPE_PERCENTAGE
      !ID_SSPDT type Z4DE_COMPONENT_TYPE_PERCENTAGE .
endinterface.
