interface ZIF_4_MONSTER_MODEL
  public .


  types:
    mtt_monster_headers TYPE HASHED TABLE OF z4sc_monster_header
                        WITH UNIQUE KEY monster_number .
  types:
    mtt_monster_items   TYPE SORTED TABLE OF z4sc_monster_items
                        WITH UNIQUE KEY monster_number monster_item .

  methods ARE_VALUES_DERIVATION_RELEVANT
    importing
      !IS_HEADER_VALUES type Z4SC_MONSTER_HEADER_EX
    returning
      value(RF_RELEVANT) type ABAP_BOOL .
  methods ACTION_HOWL_AT_THE_MOON
    importing
      !IS_HOWL_REQUEST type Z4SA_HOWL_AT_THE_MOON .
  methods DERIVE_HEADER_FIELDS
    changing
      !CS_MONSTER_HEADER type Z4SC_MONSTER_HEADER_EX .
  methods DERIVE_ITEM_FIELDS
    changing
      !CS_MONSTER_ITEMS type ZSTR_MONSTER_ITEMS .
  methods VALIDATE_ACTION_HOWL
    importing
      !IS_HEADER_VALUES type Z4SC_MONSTER_HEADER_EX .
  methods IS_DERIVATION_RELEVANT
    importing
      !IT_CHANGED_FIELDS type BAL_T_FLD
    returning
      value(RF_RELEVANT) type ABAP_BOOL .
  methods VALIDATE_MONSTER_HEADER
    importing
      !IS_HEADER_VALUES type Z4SC_MONSTER_HEADER_EX .
endinterface.
