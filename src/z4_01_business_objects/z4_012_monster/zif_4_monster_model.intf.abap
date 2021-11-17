interface ZIF_4_MONSTER_MODEL
  public .


  types:
    mtt_monster_headers TYPE HASHED TABLE OF z4sc_monster_header
                        WITH UNIQUE KEY monster_number .
  types:
    mtt_monster_items   TYPE SORTED TABLE OF z4sc_monster_items
                        WITH UNIQUE KEY monster_number monster_item .

  methods ACTION_HOWL_AT_THE_MOON
    importing
      !IS_HOWL_REQUEST type Z4SA_HOWL_AT_THE_MOON .
  methods ARE_VALUES_DERIVATION_RELEVANT
    importing
      !IS_HEADER_VALUES type Z4SC_MONSTER_HEADER_EX
    returning
      value(RF_RELEVANT) type ABAP_BOOL .
  methods DERIVE_HEADER_FIELDS
    changing
      !CS_MONSTER_HEADER type Z4SC_MONSTER_HEADER_EX .
  methods DERIVE_ITEM_FIELDS
    changing
      !CS_MONSTER_ITEMS type Z4SC_MONSTER_ITEMS_EX .
  methods IS_DERIVATION_RELEVANT
    importing
      !IT_CHANGED_FIELDS type BAL_T_FLD
    returning
      value(RF_RELEVANT) type ABAP_BOOL .
  methods IS_SCARY
    returning
      value(RF_YES_IT_IS) type ABAP_BOOL .
  methods VALIDATE_ACTION_HOWL
    importing
      !IS_HEADER_VALUES type Z4SC_MONSTER_HEADER_EX .
  methods VALIDATE_MONSTER_HEADER
    importing
      !IS_HEADER_VALUES type Z4SC_MONSTER_HEADER_EX .
  methods WANTS_TO_BLOW_UP_WORLD
    returning
      value(RF_YES_IT_DOES) type ABAP_BOOL .
  methods INVITE_TO_PARTY
    importing
      !ID_MONSTER_NAME type Z4DE_MONSTER_NAME
      !ID_PARTY_NAME type STRING .
endinterface.
