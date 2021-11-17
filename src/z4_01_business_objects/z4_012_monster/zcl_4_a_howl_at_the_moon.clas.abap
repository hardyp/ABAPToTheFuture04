class ZCL_4_A_HOWL_AT_THE_MOON definition
  public
  inheriting from /BOBF/CL_LIB_A_SUPERCLASS
  final
  create public .

public section.

  methods /BOBF/IF_FRW_ACTION~EXECUTE
    redefinition .
protected section.
private section.
ENDCLASS.



CLASS ZCL_4_A_HOWL_AT_THE_MOON IMPLEMENTATION.


  METHOD /bobf/if_frw_action~execute.
*--------------------------------------------------------------------*
* Listing 07.16: - Executing Action
*--------------------------------------------------------------------*
    "Local variables
    DATA: monster_header_records TYPE z4tt_monster_header.

    "Clear exporting parameters
    CLEAR: eo_message,
           et_failed_key,
           ev_static_action_failed,
           et_data.

    "Get the current header values
    io_read->retrieve(
    EXPORTING iv_node = zif_4_monster_c=>sc_node-monster_header
              it_key  = it_key
    IMPORTING et_data = monster_header_records ).

    DATA(monster_header_record) = monster_header_records[ 1 ].

    IF monster_header_record-monster_number IS INITIAL.
      RETURN.
    ENDIF.

    TRY.
        DATA(monster_model) =
        zcl_4_monster_model=>get_instance( monster_header_record-monster_number ).
      CATCH zcx_4_monster_exceptions.
        RETURN.
    ENDTRY.

    "Transform generic import structure into concrete structure
    DATA: howl_request_record TYPE z4sa_howl_at_the_moon.

    FIELD-SYMBOLS: <howl_request_record> TYPE any.

    ASSIGN is_parameters->* TO <howl_request_record>.
    howl_request_record = <howl_request_record>.

    "Off we go!
    TRY.
        monster_model->action_howl_at_the_moon( howl_request_record ).

        "Error handling time. The Monster class sends a framework-
        "agnostic error in the form of an exception class. We will
        "adapt this to a BOPF-specific exception
      CATCH zcx_4_monster_exceptions_mc INTO DATA(monster_exception).

        DATA(monster_key) = it_key[ 1 ]."Only one line

        "This key (node) failed at the job of performing the action
        "Shame upon it
        INSERT monster_key INTO TABLE et_failed_key.

        "Send the error message in the format BOPF desires
        DATA(origin_location_information) = VALUE /bobf/s_frw_location(
        node_key = is_ctx-node_key
        key      = monster_key-key )."I heard you the first time

        DATA(message_in_a_bottle) = NEW /bobf/cm_frw_core(
        textid             = monster_exception->if_t100_message~t100key
        severity           = /bobf/cm_frw=>co_severity_error
        symptom            = /bobf/if_frw_message_symptoms=>co_bo_inconsistency
        lifetime           = /bobf/if_frw_c=>sc_lifetime_set_by_bopf
        ms_origin_location = origin_location_information ).

        zcl_4_bc_bopf_pl_helper=>put_message_in_bottle(
           EXPORTING i_hope_that_someone_gets_my = message_in_a_bottle
           CHANGING  co_bottle                   = eo_message ).

    ENDTRY.

  ENDMETHOD."Execute action of ZCL_4_A_HOWL_AT_THE_MOON
ENDCLASS.
