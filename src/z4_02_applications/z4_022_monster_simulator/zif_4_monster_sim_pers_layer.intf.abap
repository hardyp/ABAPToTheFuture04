interface ZIF_4_MONSTER_SIM_PERS_LAYER
  public .


  methods DERIVE_HEADS_PER_MONSTER_TYPE default ignore
    importing
      !ID_MONSTER_TYPE type STRING
    returning
      value(RESULT) type INT4 .
  methods DERIVE_MONSTERS_UNDER_BED default ignore
    importing
      !ID_BED_NAME type STRING
    returning
      value(RT_MONSTERS_UNDER_BED) type ZIF_4_MONSTER_SIMULATOR=>M_TT_MONSTERS_UNDER_BED .
endinterface.
