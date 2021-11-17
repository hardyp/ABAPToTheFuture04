class ZCL_4_EXCEPTIONAL_MONSTER definition
  public
  create public .

public section.

  data NO_OF_HEADS type SY-TABIX read-only .
  data NO_OF_EYES type SY-TABIX read-only .
  data EYES_ARE_OPEN type ABAP_BOOL read-only .
  data SCARINESS type STRING read-only .
  data BOLTS_IN_NECK type SY-TABIX read-only .
  data FLUFFINESS type INT4 read-only .
  data COLOR type STRING read-only .

  methods HOWL_AT_MOON .
  methods TERRORIZE_VILLAGE .
  methods SELL_MORTGAGES .
  methods LOCK .
  methods UNLOCK .
  methods REMOVE_CURRENT_HEAD .
  methods ADD_NEW_HEAD .
  methods REATTACH_OLD_HEAD .
  methods CONSTRUCTOR
    importing
      !NAME type Z4DE_MONSTER_NAME optional .
  methods HEAD_SWAP_OPERATION .
  methods LEG_SWAP_OPERATION .
  methods ARM_SWAP_OPERATION .
  methods ATTACK_VILLAGERS .
  methods OPEN_MONSTERS_EYES .
  methods GET_MONSTERS
    importing
      !IT_MONSTER_TYPES type ANY TABLE
    returning
      value(RT_MONSTERS) type Z4TT_MONSTER_HEADER .
  methods GET_AHEAD_GET_A_HAT
    importing
      !ID_MONSTER_NUMBER type Z4DE_MONSTER_NUMBER
    exporting
      !ED_NUMBER_OF_HEADS type I
      !ED_NUMBER_OF_HATS type I .
  methods INVITE_TO_PARTY
    importing
      !ID_NAME type CHAR20 .
  methods LOG
    importing
      !ID_VALUE type ref to DATA .
  class-methods FACTORY
    importing
      !ID_MONSTER_NUMBER type Z4DE_MONSTER_NUMBER
    returning
      value(RO_MONSTER) type ref to ZCL_4_EXCEPTIONAL_MONSTER .
  methods IS_IT_MAD
    importing
      !ID_MONSTER_NUMBER type Z4DE_MONSTER_NUMBER
    returning
      value(RF_IT_IS_MAD) type ABAP_BOOL .
  PROTECTED SECTION.
  PRIVATE SECTION.

    DATA md_name TYPE z4de_monster_name .
ENDCLASS.



CLASS ZCL_4_EXCEPTIONAL_MONSTER IMPLEMENTATION.


  METHOD add_new_head.

    MESSAGE 'Add New Head to Monster'(005) TYPE 'I'.

    ADD 1 TO no_of_heads.

  ENDMETHOD.


  METHOD arm_swap_operation.

    MESSAGE 'Swapping Monsters Arms'(011) TYPE 'I'.

  ENDMETHOD.


  METHOD attack_villagers.

    MESSAGE 'Monster is Attacking the Villagers!'(012) TYPE 'I'.

  ENDMETHOD.


  METHOD constructor.

    no_of_heads = 1.

    md_name = name.

  ENDMETHOD.


  METHOD factory.
  ENDMETHOD.


  METHOD get_ahead_get_a_hat.
  ENDMETHOD.


  METHOD get_monsters.
  ENDMETHOD.


  METHOD head_swap_operation.

    DATA(lo_candle) = NEW zcl_4_candle( ).

    TRY.
        MESSAGE 'Swapping Monsters Head'(007) TYPE 'I'.
        TRY.
            remove_current_head( ).
          CATCH zcx_4_power_failure.
            lo_candle->light( ).
        ENDTRY.
        MESSAGE 'Oh No! Here Comes the Villagers!'(008) TYPE 'I'.
        RAISE EXCEPTION TYPE zcx_4_stormed_by_villagers.
      CLEANUP.
        MESSAGE 'CLEANUP block being excuted with HEAD_SWAP_OPERATION'(009) TYPE 'I'.
        reattach_old_head( ).
    ENDTRY.

  ENDMETHOD.


  METHOD howl_at_moon.
  ENDMETHOD.


  METHOD invite_to_party.
  ENDMETHOD.


  METHOD is_it_mad.
    rf_it_is_mad = abap_true.
  ENDMETHOD.


  METHOD leg_swap_operation.

    MESSAGE 'Swapping Monsters Legs'(010) TYPE 'I'.

  ENDMETHOD.


  METHOD lock.

    MESSAGE 'Monster Locked' TYPE 'I'.

  ENDMETHOD.


  METHOD log.
  ENDMETHOD.


  METHOD open_monsters_eyes.
*--------------------------------------------------------------------*
* Listing 04.13: - Design by Contract in ABAP
*--------------------------------------------------------------------*
    "Preconditions
    zcl_dbc=>require( that             = 'The Monster has at least one eye'(013)
                      which_is_true_if = xsdbool( no_of_eyes GE 1 ) ).

    "Code to Open the Monsters Eyes
    eyes_are_open = abap_true.

    "Postconditions
    zcl_dbc=>ensure( that             = 'The Monsters Eyes are Open'(014)
                     which_is_true_if = xsdbool( eyes_are_open = abap_true ) ).

  ENDMETHOD.


  METHOD reattach_old_head.

    MESSAGE 'Re-Attaching Old Head to Monster'(006) TYPE 'I'.

    ADD 1 TO no_of_heads.

  ENDMETHOD.


  METHOD remove_current_head.

    MESSAGE 'Removing Monsters Head'(001) TYPE 'I'.

    IF no_of_heads = 0.
      MESSAGE 'Monster has no head!'(002) TYPE 'I'.
      RETURN.
    ENDIF.

    SUBTRACT 1 FROM no_of_heads.

    IF sy-uzeit > '200000'.
      "Once it goes dark, the villagers storm the castle
      "waving flaming brands
      MESSAGE 'Here come the Villagers!'(003) TYPE 'I'.
      RAISE EXCEPTION TYPE zcx_4_stormed_by_villagers.
    ELSEIF sy-uzeit > '120000'.
      "In the afternoon, the power goes out
      MESSAGE 'Power has gone out!'(004) TYPE 'I'.
      RAISE EXCEPTION TYPE zcx_4_power_failure.
    ENDIF.

  ENDMETHOD.


  METHOD sell_mortgages.

    MESSAGE 'Monster is about to sell a Mortgage' TYPE 'I'.
    "Even the Baron can't go through with this, evil as he is
    "So he presses the CANCEL button
    MESSAGE 'Baron has pressed the CANCEL button' TYPE 'I'.
    RAISE EXCEPTION TYPE zcx_4_user_cancelled.

  ENDMETHOD.


  METHOD terrorize_village.
  ENDMETHOD.


  METHOD unlock.

    MESSAGE 'Monster Unlocked' TYPE 'I'.

  ENDMETHOD.
ENDCLASS.
