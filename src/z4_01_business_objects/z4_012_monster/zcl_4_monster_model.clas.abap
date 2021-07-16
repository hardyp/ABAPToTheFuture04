class ZCL_4_MONSTER_MODEL definition
  public
  create public .

public section.

  interfaces ZIF_4_MONSTER_MODEL .

  aliases ACTION_HOWL_AT_THE_MOON
    for ZIF_4_MONSTER_MODEL~ACTION_HOWL_AT_THE_MOON .
  aliases ARE_VALUES_DERIVATION_RELEVANT
    for ZIF_4_MONSTER_MODEL~ARE_VALUES_DERIVATION_RELEVANT .
  aliases DERIVE_HEADER_FIELDS
    for ZIF_4_MONSTER_MODEL~DERIVE_HEADER_FIELDS .
  aliases DERIVE_ITEM_FIELDS
    for ZIF_4_MONSTER_MODEL~DERIVE_ITEM_FIELDS .
  aliases IS_DERIVATION_RELEVANT
    for ZIF_4_MONSTER_MODEL~IS_DERIVATION_RELEVANT .
  aliases VALIDATE_ACTION_HOWL
    for ZIF_4_MONSTER_MODEL~VALIDATE_ACTION_HOWL .
  aliases VALIDATE_MONSTER_HEADER
    for ZIF_4_MONSTER_MODEL~VALIDATE_MONSTER_HEADER .
  aliases MTT_MONSTER_HEADERS
    for ZIF_4_MONSTER_MODEL~MTT_MONSTER_HEADERS .
  aliases MTT_MONSTER_ITEMS
    for ZIF_4_MONSTER_MODEL~MTT_MONSTER_ITEMS .

  class-methods ARM_ALL_MONSTERS .
  class-methods CREATE_MONSTER_RECORD
    importing
      !IS_MONSTER_RECORD type Z4SC_MONSTER_RECORD .
  class-methods DERIVE_MONSTER_RECORD
    importing
      !ID_MONSTER_NUMBER type Z4DE_MONSTER_NUMBER
    returning
      value(RS_MONSTER_RECORD) type Z4SC_MONSTER_RECORD .
  class-methods DERIVE_MONSTER_RECORD_4_UPDATE
    importing
      !ID_MONSTER_NUMBER type Z4DE_MONSTER_NUMBER
    returning
      value(RS_MONSTER_RECORD) type Z4SC_MONSTER_RECORD
    raising
      ZCX_MONSTER_EXCEPTIONS .
  class-methods UPDATE_MONSTER_RECORD
    importing
      !IS_MONSTER_RECORD type Z4SC_MONSTER_RECORD .
  class-methods DELETE_MONSTER_RECORD
    importing
      !ID_MONSTER_NUMBER type Z4DE_MONSTER_NUMBER
    raising
      ZCX_MONSTER_EXCEPTIONS .
  class-methods GET_INSTANCE
    importing
      !ID_MONSTER_NUMBER type Z4DE_MONSTER_NUMBER
    returning
      value(RO_MONSTER_MODEL) type ref to ZIF_4_MONSTER_MODEL .
  class-methods RETRIEVE_HEADERS_BY_ATTRIBUTE
    importing
      !IT_SELECTIONS type ZTT_BC_COSELTAB
    returning
      value(RT_MONSTER_HEADERS) type Z4TT_MONSTER_HEADER_EX .
  methods CONSTRUCTOR .
  class-methods GET_AHEAD_GET_A_HAT
    importing
      !ID_MONSTER_NUMBER type ZDE_MONSTER_NUMBER
    exporting
      !ED_NUMBER_OF_HEADS type SY-TABIX
      !ED_NUMBER_OF_HATS type SY-TABIX .
  methods INVITE_TO_PARTY
    importing
      !ID_MONSTER_NAME type ZDE_MONSTER_NAME
      !ID_PARTY_NAME type STRING .
  methods IS_MAD
    importing
      !ID_MONSTER_NUMBER type ZDE_MONSTER_NUMBER
    returning
      value(RF_YES_IT_IS) type ABAP_BOOL .
  methods IS_SCARY
    returning
      value(RF_YES_IT_IS) type ABAP_BOOL .
  methods WANTS_TO_BLOW_UP_WORLD
    returning
      value(RF_YES_IT_DOES) type ABAP_BOOL .
protected section.
private section.

  types:
    BEGIN OF m_typ_monster_model,
           monster_number TYPE z4de_monster_number,
           monster_model  TYPE REF TO zif_4_monster_model,
         END OF m_typ_monster_model .
  types:
    mtt_monster_models TYPE HASHED TABLE OF m_typ_monster_model
        WITH UNIQUE KEY monster_number .

  class-data MO_PL type ref to ZIF_4_MONSTERMODEL_PERS_LAYER .
  class-data MO_RULES type ref to ZCL_MONSTER_RULES .
  class-data MT_MONSTER_MODELS type MTT_MONSTER_MODELS .
ENDCLASS.



CLASS ZCL_4_MONSTER_MODEL IMPLEMENTATION.


  METHOD ARM_ALL_MONSTERS.
*--------------------------------------------------------------------*
* Listing 03.06 : Creating Short-Lived Variables
*--------------------------------------------------------------------*
    SELECT *
      FROM ztmonster_header
      INTO TABLE @DATA(all_monsters).

    DATA(iterator) = NEW lcl_weapon_iterator( ).

    DO lines( all_monsters[] ) TIMES.
      DATA(arming_description) = CONV string(
        LET weapon_name  = iterator->get_next_weapon( )
            monster_name = all_monsters[ sy-index ]-name
            date_string  =
           |{ sy-datum+6(2) } / { sy-datum+4(2) } / { sy-datum(4) }|
        IN |Monster { monster_name } was issued a { weapon_name } on { date_string }| ).
      MESSAGE arming_description TYPE 'I'.
    ENDDO.

  ENDMETHOD.


METHOD CONSTRUCTOR.

* Get the PL and Rules ojecst via a factory

  DATA(lo_factory) = zcl_4_monster_factory=>get_instance( ).

  mo_pl = lo_factory->get_monster_bo_pl( ).

ENDMETHOD.


METHOD create_monster_record.

  mo_pl->create_monster_record( is_monster_record  ).

ENDMETHOD."Create Monster Record


  METHOD DELETE_MONSTER_RECORD.
* Monsters do not like being deleted
  ENDMETHOD.


METHOD DERIVE_MONSTER_RECORD.

  rs_monster_record = mo_pl->derive_monster_record( id_monster_number ).

ENDMETHOD.                    "Retrieve Monster Record


METHOD derive_monster_record_4_update.

  rs_monster_record = mo_pl->derive_monster_record_4_update( id_monster_number ).

ENDMETHOD.                    "Retrieve Monster Record


  method GET_AHEAD_GET_A_HAT.
* Used as ABAP example
  endmethod.


METHOD GET_INSTANCE.
* Local Variables
  DATA: monster_model_info TYPE m_typ_monster_model.

* Firstly, check buffer, the vampire slayer
  READ TABLE mt_monster_models INTO monster_model_info
  WITH TABLE KEY monster_number = id_monster_number.

  IF sy-subrc = 0.
    ro_monster_model = monster_model_info-monster_model.
    RETURN.
  ENDIF.

* If not in buffer, create new model, and add to buffer
  CREATE OBJECT ro_monster_model TYPE zcl_4_monster_model.

  monster_model_info-monster_number = id_monster_number.
  monster_model_info-monster_model  = ro_monster_model.
  INSERT monster_model_info INTO TABLE mt_monster_models.

ENDMETHOD.


  METHOD INVITE_TO_PARTY.
* Another ABAP Example
  ENDMETHOD.


  METHOD IS_MAD.

    rf_yes_it_is = abap_true.

  ENDMETHOD.


  METHOD IS_SCARY.
* Used for ABAP Example
  ENDMETHOD.


METHOD retrieve_headers_by_attribute.

  rt_monster_headers = mo_pl->derive_headers_by_attribute( it_selections ).

ENDMETHOD.


METHOD update_monster_record.

  mo_pl->update_monster_record( is_monster_record ).

ENDMETHOD."Update Monster Record


  METHOD WANTS_TO_BLOW_UP_WORLD.

    "Monsters are not environmentally friendly
    rf_yes_it_does = abap_true.

  ENDMETHOD.


METHOD ZIF_4_MONSTER_MODEL~ACTION_HOWL_AT_THE_MOON.
*--------------------------------------------------------------------*
* Listing 08.16 - Coding the Howl at the Moon Method in the Model Class
*--------------------------------------------------------------------*
  IF is_howl_request-no_of_howls = 0.
    RAISE EXCEPTION TYPE zcx_4_monster_exceptions.
  ENDIF.

  DO is_howl_request-no_of_howls TIMES.
    MESSAGE 'Ooooooooooooooooooooooooo'(001) TYPE 'I'.
  ENDDO.

ENDMETHOD."Howl at the Moon


METHOD ZIF_4_MONSTER_MODEL~ARE_VALUES_DERIVATION_RELEVANT.

  IF is_header_values-no_of_heads > 0.
    rf_relevant = abap_true.
  ELSE.
    rf_relevant = abap_false.
  ENDIF.

ENDMETHOD.


METHOD ZIF_4_MONSTER_MODEL~DERIVE_HEADER_FIELDS.
*--------------------------------------------------------------------*
* Listing 07.09 - Filling Derived Header Fields
*--------------------------------------------------------------------*

  cs_monster_header-hat_size_description =
  COND #(
  WHEN cs_monster_header-hat_size > 10 THEN 'REALLY BIG HAT'
  WHEN cs_monster_header-hat_size > 5  THEN 'BIG HAT'
  ELSE 'NORMAL HAT' ).

  cs_monster_header-sanity_description =
  COND #(
  WHEN cs_monster_header-sanity_percentage > 75 THEN 'VERY SANE'
  WHEN cs_monster_header-sanity_percentage > 50 THEN 'SANE'
  WHEN cs_monster_header-sanity_percentage > 25 THEN 'SLIGHTLY MAD'
  WHEN cs_monster_header-sanity_percentage > 12 THEN 'VERY MAD'
  WHEN cs_monster_header-sanity_percentage > 1  THEN 'BONKERS'
  ELSE 'RENAMES SAP PRODUCTS' ).

ENDMETHOD."Derive Header Fields


METHOD ZIF_4_MONSTER_MODEL~DERIVE_ITEM_FIELDS.

  CASE cs_monster_items-part_category.
    WHEN 'HD'.
      cs_monster_items-part_description = 'Head'.
    WHEN 'LG'.
      cs_monster_items-part_description = 'Leg'.
    WHEN 'AR'.
      cs_monster_items-part_description = 'Arm'.
    WHEN 'TA'.
      cs_monster_items-part_description = 'Tail'.
    WHEN 'WI'.
      cs_monster_items-part_description = 'Wing'.
    WHEN 'TN'.
      cs_monster_items-part_description = 'Tentacle'.
    WHEN OTHERS.
      RETURN.
  ENDCASE.

ENDMETHOD.


METHOD ZIF_4_MONSTER_MODEL~IS_DERIVATION_RELEVANT.

  LOOP AT it_changed_fields INTO DATA(changed_field).
    IF changed_field = 'SANITY' OR
       changed_field = 'HAT_SIZE'.
      rf_relevant = abap_true.
      RETURN.
    ENDIF.
  ENDLOOP.

ENDMETHOD."Is Derivation Relevant


METHOD ZIF_4_MONSTER_MODEL~VALIDATE_ACTION_HOWL.
*--------------------------------------------------------------------*
* Listing 08.18 - Validating the Howl Action in the Monster Model Class
*--------------------------------------------------------------------*
  IF is_header_values-no_of_heads EQ 0.
    RAISE EXCEPTION TYPE zcx_4_monster_exceptions_mc
      EXPORTING
        textid = zcx_4_monster_exceptions_mc=>no_head_howling_problem.
  ENDIF.

ENDMETHOD."Validate Howl Action


METHOD ZIF_4_MONSTER_MODEL~VALIDATE_MONSTER_HEADER.
*--------------------------------------------------------------------*
* Listing 07.14 - Validation Method in Main Monster Model
*--------------------------------------------------------------------*
  IF is_header_values-hat_size    GT 0 AND
     is_header_values-no_of_heads EQ 0.
    RAISE EXCEPTION TYPE zcx_4_monster_exceptions_mc
      EXPORTING
        textid = zcx_4_monster_exceptions_mc=>head_hat_disparity.
  ENDIF.

ENDMETHOD."Validate Monster Header
ENDCLASS.
