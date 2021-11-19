*&---------------------------------------------------------------------*
*& Report  Z4_EXCEPTION_EXAMPLES
*&
*&---------------------------------------------------------------------*
* Ideally every example here will be moved to one of the business
* applications
*--------------------------------------------------------------------*
* This program will not do anything when run, it is intended to be read and will
* concentrate on the various ways you can use exception classes to
* handle unexpected situations
* Each method will have a reference to the listing in the book where the
* code is explained in detail
*&---------------------------------------------------------------------*
REPORT z4_exception_examples.

CLASS lcl_golf_scores DEFINITION INHERITING FROM zcl_function_module_wrapper FINAL.
  PUBLIC SECTION.
    METHODS golf_handicap_of_monster IMPORTING id_monster_number TYPE z4de_monster_number
                                     RETURNING VALUE(result)     TYPE i.
ENDCLASS."LCL_GOLF_SCORES Defintion

*--------------------------------------------------------------------*
* Listing 04.06: Wrapping a Function Module in a Method
*--------------------------------------------------------------------*
CLASS lcl_golf_scores IMPLEMENTATION.

  METHOD golf_handicap_of_monster.

    remove_existing_messages( ).

    CALL FUNCTION 'ZMONSTER_GOLF_SCORES'
      EXPORTING
        id_monster_number             = id_monster_number
      IMPORTING
        ed_golf_handicap              = result
      EXCEPTIONS
        monster_only_one_inch_tall    = 1
        monster_has_no_silly_trousers = 2
        OTHERS                        = 3.

    IF sy-subrc <> 0.
      TRY.
          throw_exception_on_error_from( 'ZMONSTER_GOLF_SCORES' ).
        CATCH zcx_function_module_error INTO DATA(function_module_error).
          "Need to raise the excetion here in the calling code in order to get the call stack correct
          RAISE EXCEPTION function_module_error.
      ENDTRY.
    ENDIF.

  ENDMETHOD.

ENDCLASS."LCL_GLF_SCORES
*--------------------------------------------------------------------*
* Listing 04.15:  Pink and Fluffy Is Not Scary!
*--------------------------------------------------------------------*
CLASS lcl_monster_constraint DEFINITION FINAL.

  PUBLIC SECTION.
    INTERFACES if_constraint.

ENDCLASS. "Monster Constraint Definition

CLASS lcl_monster_constraint IMPLEMENTATION.

  METHOD if_constraint~is_valid.
*-------------------------------------------------------*
* IMPORTING data_object TYPE data
* RETURNING result      TYPE abap_bool
*-------------------------------------------------------*
* Local Variables
    DATA: monster TYPE REF TO zcl_4_exceptional_monster.

    monster ?= data_object.

    result = abap_false.

    IF monster->scariness NS 'SCARY'.
      RETURN.
    ENDIF.

    IF monster->bolts_in_neck NE 2.
      RETURN.
    ENDIF.

    IF monster->fluffiness GT 0.
      RETURN.
    ENDIF.

    IF monster->color EQ 'PINK'.
      RETURN.
    ENDIF.

    result = abap_true.

  ENDMETHOD.                      "IF_CONSTRAINT~is_valid

  METHOD if_constraint~get_description.
*-------------------------------------------------------*
* RETURNING result TYPE string_table
*-------------------------------------------------------*
* Local Variables
    DATA: message TYPE string.

    message = 'Monster is no longer a monster!'(001).

    APPEND message TO result.

  ENDMETHOD. "IF_CONSTRAINT~get_description

ENDCLASS. "Monster Constraint Implementation
*----------------------------------------------------------------------*
*       CLASS lcl_monstrous_exceptions DEFINITION
*----------------------------------------------------------------------*
CLASS lcl_monstrous_exceptions DEFINITION FINAL.
  PUBLIC SECTION.
    CLASS-METHODS: fun_with_static_electricity RAISING zcx_4_static_electricity,
      user_command     IMPORTING id_ucomm      TYPE sy-ucomm
                                 id_chosen_row TYPE i,
      try_catch_block    RAISING zcx_4_monster_exceptions_lop,
      classic_conversion,
      wrapping_a_function IMPORTING id_monster_number       TYPE z4de_monster_number
                          RETURNING VALUE(rd_golf_handicap) TYPE i,
      head_swap_operation_wrong,
      head_swap_operation_right,
      replace_everything,
      retry,
      resumable,
      open_monsters_eyes,
      raise_short_dump.

  PRIVATE SECTION.
    CLASS-DATA:
      mo_monster         TYPE REF TO zcl_4_exceptional_monster,
      mo_candle          TYPE REF TO zcl_4_candle,
      mf_data_is_rubbish TYPE abap_bool.

    CLASS-METHODS:
      get_chosen_monster IMPORTING id_chosen_row     TYPE i
                         RETURNING VALUE(ro_monster) TYPE REF TO zcl_4_exceptional_monster ##needed,
      do_something RAISING zcx_4_monster_exceptions_lop,
      handle_this_exception IMPORTING io_exception TYPE REF TO zcx_4_monster_exceptions,
      handle_that_exception IMPORTING io_exception TYPE REF TO zcx_4_monster_exceptions_mc,
      cleanup_main_method,
      do_not_know RAISING RESUMABLE(zcx_4_static_electricity).

ENDCLASS.                    "lcl_monstrous_exceptions DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_monstrous_exceptions IMPLEMENTATION
*----------------------------------------------------------------------*
CLASS lcl_monstrous_exceptions IMPLEMENTATION.
*--------------------------------------------------------------------*
* Listing 04.01: - Handling an Exception Locally or Propogating it
*                  up the call stack
*--------------------------------------------------------------------*
  METHOD fun_with_static_electricity."RAISING zcx_4_static_electricity

    MESSAGE 'Executing Method FUN_WITH_STATIC_ELECTRICITY'(002) TYPE 'I'.

    IF sy-uzeit > '120000'.
      "In the afternoons we do not want to handle the error
      "locally, so we propogate it up the stack
      MESSAGE 'Exception raised in method FWSE'(003) TYPE 'I'.
      RAISE EXCEPTION TYPE zcx_4_static_electricity.
    ENDIF.

    TRY.
        "In the mornings we want to handle this exception within this
        "method
        MESSAGE 'Exception raised in method FWSE'(003) TYPE 'I'.
        RAISE EXCEPTION TYPE zcx_4_static_electricity.
      CATCH zcx_4_static_electricity.
        MESSAGE 'Static Exception caught inside of method FWSE'(004) TYPE 'I'.
    ENDTRY.
*-------------------------------------------------------------------------------------*
* One Haunted Castle has a large black dog that leaps at visitors before disappearing
*-------------------------------------------------------------------------------------*
  ENDMETHOD.

*--------------------------------------------------------------------*
* Listing 04.02: - Using a NO_CHECK exception during user command
*                  processing
*--------------------------------------------------------------------*
  METHOD user_command.
*--------------------------------------------------------------------*
* IMPORTING id_ucomm      TYPE sy-ucomm
*           id_chosen_row TYPE i.
*--------------------------------------------------------------------*

    MESSAGE 'Executing Method USER_COMMAND'(005) TYPE 'I'.

    DATA(lo_monster) = get_chosen_monster( id_chosen_row ).

    IF lo_monster IS NOT BOUND.
      "Frankenstein Unbound
      RETURN.
    ENDIF.

    DATA(ld_message) = |{ 'Monster is Responding to'(033) } { id_ucomm } { 'user command'(034) }|.
    MESSAGE ld_message TYPE 'I'.

    TRY.
        lo_monster->lock( ).

        CASE id_ucomm.
          WHEN 'HOWL'.
            lo_monster->howl_at_moon( ).
          WHEN 'TERROR'.
            lo_monster->terrorize_village( ).
          WHEN 'SUBPRIME'.
            "Most Monstrous activity possible
            lo_monster->sell_mortgages( )."etc...
          WHEN OTHERS.
            MESSAGE 'Function is not available'(006) TYPE 'I'.
            RAISE EXCEPTION TYPE zcx_4_user_cancelled.
        ENDCASE.
      CATCH zcx_4_user_cancelled."NO_CHECK
        MESSAGE 'User Cancelled Exception has been caught'(007) TYPE 'I'.
        lo_monster->unlock( ).
        RETURN.
    ENDTRY.

* Do common tasks that you need to do after a successful user
* command like update the monsters bank balance based on what
* they just did

    lo_monster->unlock( ).

  ENDMETHOD.                    "user_command

*--------------------------------------------------------------------*
* Listing 04.03: - TRY / CATCH / CLEANUP
*--------------------------------------------------------------------*
  METHOD try_catch_block.
*--------------------------------------------------------------------*
* A demonstration of the separtion of concerns, which each method
* looking after one task only i.e. a method does ONE THING only
*--------------------------------------------------------------------*
    MESSAGE 'Executing Method TRY_CATCH_BLOCK'(008) TYPE 'I'.

    TRY.
        "ONE THING is the executing the actual business logic - the task at hand
        do_something( ).
      CATCH zcx_4_monster_exceptions INTO DATA(monster_exception).
        "ONE THING is handling one sort of error
        handle_this_exception( monster_exception ).
      CATCH zcx_4_monster_exceptions_mc INTO DATA(monster_exception_mc).
        "ONE THING is handling another sort of error
        handle_that_exception( monster_exception_mc ).
      CLEANUP.
        "ONE THING is restoring system state to the way it was at the start of
        "method TRY_CATCH_BLOCK in the event another sort of
        "exception is called, one that cannot be handled locally
        "and so has to be propogated from this method up the call stack
        cleanup_main_method( ).
    ENDTRY.

  ENDMETHOD.                    "try_catch_block

  METHOD classic_conversion.
*--------------------------------------------------------------------*
* Listing 04.05: Converting Classical Exception to Class-Based
*--------------------------------------------------------------------*
    DATA: current_monster_number TYPE z4de_monster_number,
          monster_handicap       TYPE i ##NEEDED,
          current_www_count      TYPE i VALUE 3.

    CALL FUNCTION 'ZMONSTER_GOLF_SCORES'
      EXPORTING
        id_monster_number             = current_monster_number
      IMPORTING
        ed_golf_handicap              = monster_handicap
      EXCEPTIONS
        monster_only_one_inch_tall    = 1
        monster_has_no_silly_trousers = 2
        error_message                 = 3
        OTHERS                        = 4.

    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_4_monster_exceptions_mc
        MESSAGE ID sy-msgid
        TYPE sy-msgty
        NUMBER sy-msgno
        WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
        EXPORTING
          wibbly_wobbly_woos = current_www_count.
    ENDIF.

  ENDMETHOD."Classic Conversion

  METHOD wrapping_a_function.
*--------------------------------------------------------------------*
* IMPORTING id_monster_number       TYPE z4de_monster_number
* RETURNING VALUE(rd_golf_handicap) TYPE i.
*--------------------------------------------------------------------*
    DATA(golf_scores) = NEW lcl_golf_scores( ).

    rd_golf_handicap = golf_scores->golf_handicap_of_monster( id_monster_number ).

  ENDMETHOD.

  METHOD head_swap_operation_wrong.
*--------------------------------------------------------------------*
* Listing 04.07: - Head Swap Operation
* In this example the CLEANUP block does not get called, leading to
* disaster
*--------------------------------------------------------------------*
    mo_monster = NEW #( ).
    mo_candle  = NEW #( ).

    MESSAGE 'Executing Method HEAD_SWAP_OPERATION_WRONG'(009) TYPE 'I'.

    TRY.
        mo_monster->remove_current_head( ).
      CATCH zcx_4_power_failure.
        mo_candle->light( ).
      CLEANUP.
        "You would expect that the CLEANUP block would be executed
        "after the ZCX_POWER_FAILURE exception has been processed.
        "How wrong you would be.
        MESSAGE 'Cleanup Block being Executed'(010) TYPE 'I'.
        mo_monster->reattach_old_head( ).
    ENDTRY.

    IF mo_monster->no_of_heads = 0.
      MESSAGE 'The Monster has No Head!'(011) TYPE 'I'.
    ENDIF.

  ENDMETHOD.                    "head_swap_operation_wrong

  METHOD head_swap_operation_right.
*-------------------------------------------------------------------------*
* Listing 04.08: - Making 100% sure a method cleans up after an exception
*-------------------------------------------------------------------------*
    mo_monster = NEW #( ).
    mo_candle  = NEW #( ).

    MESSAGE 'Executing Method HEAD_SWAP_OPERATION_RIGHT'(012) TYPE 'I'.

    TRY.
        mo_monster->remove_current_head( ).
      CATCH zcx_4_power_failure.
        mo_candle->light( ).
      CLEANUP.
        "In case an exception is raised which is not caught
        "inside this routine e.g. the Castle is Stormed by the Villagers
        MESSAGE 'Cleanup Block being Executed'(010) TYPE 'I'.
        mo_monster->reattach_old_head( ).
    ENDTRY.

    "In case the exception is raised within this routine e.g. Candle Power Failure
    "and so the new head has not been attached
    IF mo_monster->no_of_heads = 0.
      MESSAGE 'The Monster has No Head! Better do something about that'(013) TYPE 'I'.
      mo_monster->reattach_old_head( ).
    ENDIF.

  ENDMETHOD.                    "head_swap_operation_right

  METHOD replace_everything.
*--------------------------------------------------------------------*
* Listing 04.09: - Catching an Exception in a Different Routine
*--------------------------------------------------------------------*
    mo_monster = NEW #( ).

    MESSAGE 'Executing Method REPLACE_EVERYTHING'(014) TYPE 'I'.

    TRY.
        mo_monster->head_swap_operation( ).
        mo_monster->leg_swap_operation( ).
        mo_monster->arm_swap_operation( ).
      CATCH zcx_4_stormed_by_villagers.
        MESSAGE 'Exception caught within method REPLACE_EVERYTHING'(015) TYPE 'I'.
        mo_monster->attack_villagers( ).
    ENDTRY.

* Each of the five Suspect Monsters has a different BOOK they like to read
  ENDMETHOD.                    "replace_everything

*--------------------------------------------------------------------*
* Listing 04.10: - RETRY after Handling an Exception
*--------------------------------------------------------------------*
  METHOD retry.
* Local Variables
    DATA: has_repair_been_attempted TYPE abap_bool.

    mo_monster = NEW #( ).
    mo_candle  = NEW #( ).

    MESSAGE 'Executing Method RETRY'(016) TYPE 'I'.

    TRY.
        TRY.
            mo_monster->remove_current_head( ).
          CATCH zcx_4_power_failure.
            mo_candle->light( ).
          CLEANUP.
            "You would expect that the CLEANUP block would be executed
            "after the ZCX_POWER_FAILURE exception has been processed.
            "How wrong you would be.
            MESSAGE 'Cleanup Block being Executed'(010) TYPE 'I'.
            mo_monster->reattach_old_head( ).
        ENDTRY."Small TRY Block

        IF mo_monster->no_of_heads = 0.

          MESSAGE 'The Monster has No Head!'(011) TYPE 'I'.
          MESSAGE 'Raise an Exception!'(017)      TYPE 'I'.
          RAISE EXCEPTION NEW zcx_4_monster_exceptions_lop(
              textid = zcx_4_monster_exceptions_lop=>head_error
              heads  = mo_monster->no_of_heads ).

        ENDIF.

      CATCH zcx_4_monster_exceptions_lop.
        IF has_repair_been_attempted = abap_false.
          "Set parameters such that the user can call the MONSTER MONITOR
          "and add the extra head themselves via a link from the message
          "long text
          "007 = Please re-attach the Monsters Head!
          MESSAGE i007(z4monsters).
          "Hopefully the user has fixed the problem, so let us try
          "again, from the top of the Big TRY Block
          has_repair_been_attempted = abap_true.
          MESSAGE 'RETRY' TYPE 'I'.
          "If at first you don't succeed...
          RETRY.
        ELSE.
          MESSAGE 'Monster STILL has no heads, I am giving up!'(018) TYPE 'I'.
        ENDIF.
    ENDTRY."Big Try Block

  ENDMETHOD.                    "retry

*--------------------------------------------------------------------*
* Part of Listing 04.11: - Raising a Resumable Exception
*--------------------------------------------------------------------*
  METHOD resumable.

    MESSAGE 'Executing Method RESUMABLE'(019) TYPE 'I'.

    "Start off by faking system state such that we have a problem,
    "namely that the data is rubbish
    mf_data_is_rubbish = abap_true.

    TRY.
        "Then call a method which does not know how to deal with
        "the unexpected situation of the data being rubbish
        do_not_know( ).
      CATCH BEFORE UNWIND zcx_4_static_electricity.
        "Repair the data using special knowledge that only this
        "level of the call stack knows
        IF mf_data_is_rubbish = abap_true.
          "Data is now repaired so it is no longer rubbish
          mf_data_is_rubbish = abap_false."Now everything is fine!
        ENDIF.
        RESUME.
    ENDTRY.

  ENDMETHOD.                    "resumable

  METHOD open_monsters_eyes.

    DATA(sleepy_monster) = NEW zcl_4_exceptional_monster( ).

    sleepy_monster->open_monsters_eyes( ).

*--------------------------------------------------------------------*
* Listing 04.16: Calling Class Invariant at End of Each Method Call
*--------------------------------------------------------------------*
    DATA(monster_constraint) = NEW lcl_monster_constraint( ).
    zcl_dbc=>ensure(
    that             = 'The Monster is still a Monster'(020)
    which_is_true_if = monster_constraint->if_constraint~is_valid( sleepy_monster ) ).

  ENDMETHOD.

  METHOD raise_short_dump ##NEEDED.
*--------------------------------------------------------------------*
* Listing 04.17: - Passing Exception Classes to Short Dumps
* Only works from 7.53 upwards
*--------------------------------------------------------------------*
*    TRY.
*     open_monsters_eyes( ).
*    CATCH zcx_violated_precondition INTO lo_precondition.
*      "A bug was detected at the start of a subroutine - the caller of the subroutine is at fault
*      lo_precondition->mo_error_log->show_error_log( ).
*      RAISE SHORTDUMP lo_precondition.
*    CATCH zcx_violated_postcondition INTO lo_postcondition.
*      "A bug was detected at the end of a subroutine - the subroutine is at fault
*      lo_postcondition->mo_error_log->show_error_log( ).
*      RAISE SHORTDUMP lo_postcondition.
*  ENDTRY.

  ENDMETHOD.

  METHOD get_chosen_monster.
*--------------------------------------------------------------------*
* IMPORTING id_chosen_row TYPE i
* RETURNING value(ro_monster) TYPE REF TO zcl_monster.
*--------------------------------------------------------------------*
    " You would read an internal table being displayed on a SALV grid
    " and then return the monster object instance related to that row
    " For this example we just create a generic monster
    ro_monster = NEW #( ).
  ENDMETHOD.                    "get_chosen_monster

  METHOD do_something.
*--------------------------------------------------------------------*
* Listing 04.04: - Exporting Vital Information when raising an
* exception
*--------------------------------------------------------------------*
    DATA: wibbly_wobbly_woos TYPE i,
          scariness          TYPE string,
          heads              TYPE int4,
          arms               TYPE int4,
          legs               TYPE int4,
          bolts              TYPE int4.

    wibbly_wobbly_woos = 94.
    scariness          = 'REALLY_SCARY'.
    heads              = 2.
    arms               = 8.
    legs               = 3.
    bolts              = 2.

    MESSAGE 'Performing generic TRY / CATCH / CLEANUP method business logic'(021) TYPE 'I'.

    IF sy-uzeit GT '120000'.
      "In the afternoon we raise one type of exception
      MESSAGE 'Raising an exception due to excessive number of wibbly wobbly woos'(022) TYPE 'I'.

      RAISE EXCEPTION NEW zcx_4_monster_exceptions(
          textid             = zcx_4_monster_exceptions=>wibbly_wobbly_woo_problem
          wibbly_wobbly_woos = wibbly_wobbly_woos ).

    ELSEIF sy-uzeit LT '120000'.
      "In the morning we raise another type of exception
      MESSAGE 'Raising an exception due to unusual number of monster components'(023) TYPE 'I'.

      RAISE EXCEPTION NEW zcx_4_monster_exceptions_mc(
          textid    = zcx_4_monster_exceptions_mc=>component_problem
          scariness = scariness
          heads     = heads
          arms      = arms
          legs      = legs ).

    ELSE.
      "At noon we raise a different sort of exception yet again
      MESSAGE 'Raising an exception with five whole parameters!'(024) TYPE 'I'.

      RAISE EXCEPTION NEW zcx_4_monster_exceptions_lop(
          scariness = scariness
          heads     = heads
          arms      = arms
          legs      = legs
          bolts     = bolts ).

    ENDIF.

  ENDMETHOD.                    "do_something

  METHOD handle_this_exception.
*--------------------------------------------------------------------*
* IMPORTING io_exception TYPE REF TO zcx_4_monster_exceptions
*--------------------------------------------------------------------*

    DATA(ld_error_message)
     = |{ 'There are'(035) } { io_exception->wibbly_wobbly_woos } { 'wibbly wobbly woos which is terrible!'(036) }|.

    MESSAGE ld_error_message TYPE 'I'.

  ENDMETHOD.                    "handle_this_exception

  METHOD handle_that_exception ##NEEDED.
*--------------------------------------------------------------------*
* IMPORTING io_exception TYPE REF TO zcx_monster_exception_mc
*--------------------------------------------------------------------*
    DATA(ld_error_message) = io_exception->get_text( ).

    MESSAGE ld_error_message TYPE 'I'.

  ENDMETHOD.                    "handle_that_exception

  METHOD cleanup_main_method.

    MESSAGE 'Cleaning up TRY_CATCH_CLEANUP method prior to exception propogation'(025) TYPE 'I'.

  ENDMETHOD.                    "cleanup_main_method

*--------------------------------------------------------------------*
* Part of Listing 04.11: - Raising a Resumable Exception
*--------------------------------------------------------------------*
  METHOD do_not_know.
*--------------------------------------------------------------------*
* This routine does not know how to repair incorrect data
*--------------------------------------------------------------------*

    TRY.
        MESSAGE 'Starting Method DO_NOT_KNOW'(026) TYPE 'I'.

        IF mf_data_is_rubbish = abap_true.
          MESSAGE 'The data is rubbish - raise resumable exception'(027) TYPE 'I'.
          RAISE RESUMABLE EXCEPTION TYPE zcx_4_static_electricity.
        ELSE.
          MESSAGE 'The data is fine'(028) TYPE 'I'.
        ENDIF.

        MESSAGE 'The Data has now been repaired - Completing Method DO_NOT_KNOW'(029) TYPE 'I'.
      CATCH zcx_4_monster_exceptions ##NO_HANDLER.
        "Do Nothing
      CLEANUP.
        "This never gets called due to BEFORE_UNWIND in calling method
        MESSAGE 'Cleanup'(030) TYPE 'I'.
    ENDTRY.

  ENDMETHOD.                    "do_not_know

ENDCLASS.                    "lcl_monstrous_exceptions IMPLEMENTATION

*--------------------------------------------------------------------*
* Start-of-Selection
*--------------------------------------------------------------------*
START-OF-SELECTION.
  PERFORM main.

FORM main RAISING zcx_4_static_electricity.
  LEAVE PROGRAM ##STMNT_EXIT."Not supposed to actually run this
*--------------------------------------------------------------------*
* Listing 04.01 : CX_STATIC_CHECK
*--------------------------------------------------------------------*
  "Testing an exception class derived from CX_STATIC_CHECK
  TRY.
      lcl_monstrous_exceptions=>fun_with_static_electricity( ).
    CATCH zcx_4_static_electricity.
      MESSAGE 'Static Exception caught outside of method FUN_WITH_STATIC_ELECTRICITY'(031) TYPE 'I'.
  ENDTRY.

*--------------------------------------------------------------------*
* Listing 04.02 : CX_NO_CHECK
*--------------------------------------------------------------------*
  "Testing an exception class derived from CX_NO_CHECK
  lcl_monstrous_exceptions=>user_command( id_ucomm      = 'SUBPRIME'
                                          id_chosen_row = 1 ).

*--------------------------------------------------------------------*
* Listing 04.03 : TRY / CATCH / CLEANUP
* Listing 04.04 : Exporting Vital Information While Raising Exception
*--------------------------------------------------------------------*
  "Generic example of TRY / CATCH / CLEANUP
  TRY.
      lcl_monstrous_exceptions=>try_catch_block( ).
    CATCH zcx_4_monster_exceptions_lop INTO DATA(exception_lop).
      DATA(error_message) = exception_lop->get_text( ).
      MESSAGE error_message TYPE 'I'.
  ENDTRY.

*--------------------------------------------------------------------*
* Listing 04.05 : Converting Classical Exception to Class-Based
*--------------------------------------------------------------------*
  lcl_monstrous_exceptions=>classic_conversion( ).

*--------------------------------------------------------------------*
* Listing 04.06 : Wrapping a Function Module in a Method
*--------------------------------------------------------------------*
  lcl_monstrous_exceptions=>wrapping_a_function( id_monster_number = '1' ).

*--------------------------------------------------------------------*
* Listing 04.07 : Head Swap Operations
*--------------------------------------------------------------------*
  "Wrong usage of CLEANUP
  lcl_monstrous_exceptions=>head_swap_operation_wrong( ).

*--------------------------------------------------------------------*
* Listing 04.08 : Making 100% Sure Method Cleans Up after Exception
*--------------------------------------------------------------------*
  "Correct usage of CLEANUP
  TRY.
      lcl_monstrous_exceptions=>head_swap_operation_right( ).
    CATCH zcx_4_stormed_by_villagers.
      MESSAGE 'Castle Storming Exception has been Caught outside of main method'(032) TYPE 'I'.
  ENDTRY.

*--------------------------------------------------------------------*
* Listing 04.09 : Catching Exception in Different Routine
*--------------------------------------------------------------------*
* Another example of CLEANUP done correctly
  lcl_monstrous_exceptions=>replace_everything( ).

*--------------------------------------------------------------------*
* Listing 04.10 : RETRY after Handling Exception
*--------------------------------------------------------------------*
  "Example of RETRY
  lcl_monstrous_exceptions=>retry( ).

*--------------------------------------------------------------------*
* Listing 04.11 :  Raising Resumable Exception
*--------------------------------------------------------------------*
  "Example of RESUME
  lcl_monstrous_exceptions=>resumable( ).

*--------------------------------------------------------------------*
* Listing 04.13 : Design by Contract in ABAP
* Listing 04.16 : Class Invariants
*--------------------------------------------------------------------*
  lcl_monstrous_exceptions=>open_monsters_eyes( ).

*--------------------------------------------------------------------*
* Listing 04.17 : Passsing Exception Classes to Short Dumps
* Only works from 7.53 upwards
*--------------------------------------------------------------------*
  lcl_monstrous_exceptions=>raise_short_dump( ).

ENDFORM.
