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
  aliases INVITE_TO_PARTY
    for ZIF_4_MONSTER_MODEL~INVITE_TO_PARTY .
  aliases IS_DERIVATION_RELEVANT
    for ZIF_4_MONSTER_MODEL~IS_DERIVATION_RELEVANT .
  aliases IS_SCARY
    for ZIF_4_MONSTER_MODEL~IS_SCARY .
  aliases VALIDATE_ACTION_HOWL
    for ZIF_4_MONSTER_MODEL~VALIDATE_ACTION_HOWL .
  aliases VALIDATE_MONSTER_HEADER
    for ZIF_4_MONSTER_MODEL~VALIDATE_MONSTER_HEADER .
  aliases WANTS_TO_BLOW_UP_WORLD
    for ZIF_4_MONSTER_MODEL~WANTS_TO_BLOW_UP_WORLD .
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
      ZCX_4_MONSTER_EXCEPTIONS_LOP .
  class-methods UPDATE_MONSTER_RECORD
    importing
      !IS_MONSTER_RECORD type Z4SC_MONSTER_RECORD .
  class-methods DELETE_MONSTER_RECORD
    importing
      !ID_MONSTER_NUMBER type Z4DE_MONSTER_NUMBER .
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
      !ID_MONSTER_NUMBER type Z4DE_MONSTER_NUMBER
    exporting
      !ED_NUMBER_OF_HEADS type SY-TABIX
      !ED_NUMBER_OF_HATS type SY-TABIX .
  class-methods IS_MAD
    importing
      !ID_MONSTER_NUMBER type ZDE_MONSTER_NUMBER
    returning
      value(RF_YES_IT_IS) type ABAP_BOOL .
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
  class-data MT_MONSTER_MODELS type MTT_MONSTER_MODELS .
ENDCLASS.



CLASS ZCL_4_MONSTER_MODEL IMPLEMENTATION.


  METHOD arm_all_monsters.
*--------------------------------------------------------------------*
* Listing 03.06 : Creating Short-Lived Variables
*--------------------------------------------------------------------*
    SELECT *
      FROM z4t_monster_head
      INTO TABLE @DATA(all_monsters).

    DATA(iterator) = NEW lcl_weapon_iterator( ).

    DO lines( all_monsters[] ) TIMES.
      DATA(arming_description) = CONV string(
        LET weapon_name  = iterator->get_next_weapon( )
            monster_name = all_monsters[ sy-index ]-name
            date_string  =
           |{ sy-datum+6(2) } / { sy-datum+4(2) } / { sy-datum(4) }|
        IN |{ 'Monster'(008) } { monster_name } { 'was issued a'(009) } { weapon_name } on { date_string }| ).
      MESSAGE arming_description TYPE 'I'.
    ENDDO.

  ENDMETHOD.


METHOD CONSTRUCTOR.

* Get the PL and Rules ojecst via a factory

  DATA(lo_factory) = zcl_4_monster_factory=>get_instance( ).

  mo_pl = lo_factory->get_monster_bo_pl( ).

ENDMETHOD.


METHOD create_monster_record.

  mo_pl->create_monster_record( is_monster_record ).

ENDMETHOD."Create Monster Record


  METHOD delete_monster_record ##NEEDED.
* Monsters do not like being deleted
  ENDMETHOD.


METHOD DERIVE_MONSTER_RECORD.

  rs_monster_record = mo_pl->derive_monster_record( id_monster_number ).

ENDMETHOD.                    "Retrieve Monster Record


METHOD derive_monster_record_4_update.

  rs_monster_record = mo_pl->derive_monster_record_4_update( id_monster_number ).

ENDMETHOD.                    "Retrieve Monster Record


  METHOD get_ahead_get_a_hat ##NEEDED.
* Used as ABAP example
  ENDMETHOD.


METHOD get_instance.

  "Firstly, check buffer, the vampire slayer
  READ TABLE mt_monster_models INTO DATA(monster_model_info)
  WITH TABLE KEY monster_number = id_monster_number.

  IF sy-subrc = 0.
    ro_monster_model = monster_model_info-monster_model.
    RETURN.
  ENDIF.

  "If not in buffer, create new model, and add to buffer
  ro_monster_model = NEW zcl_4_monster_model( ).

  INSERT VALUE #(
  monster_number = id_monster_number
  monster_model  = ro_monster_model )
  INTO TABLE mt_monster_models.

ENDMETHOD.


  METHOD is_mad.

    rf_yes_it_is = abap_true.

  ENDMETHOD.


METHOD retrieve_headers_by_attribute.

  rt_monster_headers = mo_pl->derive_headers_by_attribute( it_selections ).

ENDMETHOD.


METHOD update_monster_record.

  mo_pl->update_monster_record( is_monster_record ).

ENDMETHOD."Update Monster Record


METHOD zif_4_monster_model~action_howl_at_the_moon.
*------------------------------------------------------------------------*
* Listing 08.15: - Coding the Howl at the Moon Method in the Model Class
*------------------------------------------------------------------------*
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


METHOD zif_4_monster_model~derive_header_fields.
*--------------------------------------------------------------------*
* Listing 07.09: - Filling Derived Header Fields
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


METHOD zif_4_monster_model~derive_item_fields.

  CASE cs_monster_items-part_category.
    WHEN 'HD'.
      cs_monster_items-part_description = 'Head'(002).
    WHEN 'LG'.
      cs_monster_items-part_description = 'Leg'(003).
    WHEN 'AR'.
      cs_monster_items-part_description = 'Arm'(004).
    WHEN 'TA'.
      cs_monster_items-part_description = 'Tail'(005).
    WHEN 'WI'.
      cs_monster_items-part_description = 'Wing'(006).
    WHEN 'TN'.
      cs_monster_items-part_description = 'Tentacle'(007).
    WHEN OTHERS.
      RETURN.
  ENDCASE.

ENDMETHOD.


  METHOD ZIF_4_MONSTER_MODEL~INVITE_TO_PARTY ##NEEDED.
* Another ABAP Example
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


  METHOD ZIF_4_MONSTER_MODEL~IS_SCARY ##NEEDED.
* Used for ABAP Example
  ENDMETHOD.


METHOD zif_4_monster_model~validate_action_howl.
*--------------------------------------------------------------------*
* This listing appears to have vanished in the fourth editon
*--------------------------------------------------------------------*
  IF is_header_values-no_of_heads EQ 0.
    RAISE EXCEPTION TYPE zcx_4_monster_exceptions_mc
      EXPORTING
        textid = zcx_4_monster_exceptions_mc=>no_head_howling_problem.
  ENDIF.

ENDMETHOD."Validate Howl Action


METHOD zif_4_monster_model~validate_monster_header.
*--------------------------------------------------------------------*
* Listing 07.13: - Validation Method in Main Monster Model
*--------------------------------------------------------------------*
  IF is_header_values-hat_size    GT 0 AND
     is_header_values-no_of_heads EQ 0.
    RAISE EXCEPTION TYPE zcx_4_monster_exceptions_mc
      EXPORTING
        textid = zcx_4_monster_exceptions_mc=>head_hat_disparity.
  ENDIF.

* The Stamp Collecting Monster reads "Improving the Quality of ABAP Code" ISBN 978-1-4842-6710-3
* He really knows how to have a good time!
ENDMETHOD."Validate Monster Header


  METHOD ZIF_4_MONSTER_MODEL~WANTS_TO_BLOW_UP_WORLD.

    "Monsters are not environmentally friendly
    rf_yes_it_does = abap_true.

  ENDMETHOD.
ENDCLASS.
