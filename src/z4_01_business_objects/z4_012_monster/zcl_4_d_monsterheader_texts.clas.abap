class ZCL_4_D_MONSTERHEADER_TEXTS definition
  public
  inheriting from /BOBF/CL_LIB_D_SUPERCLASS
  create public .

public section.

  methods /BOBF/IF_FRW_DETERMINATION~CHECK_DELTA
    redefinition .
  methods /BOBF/IF_FRW_DETERMINATION~EXECUTE
    redefinition .
  methods /BOBF/IF_FRW_DETERMINATION~CHECK
    redefinition .
protected section.
private section.
ENDCLASS.



CLASS ZCL_4_D_MONSTERHEADER_TEXTS IMPLEMENTATION.


  METHOD /bobf/if_frw_determination~check.
*--------------------------------------------------------------------*
* Listing 07.11 : Coding CHECK Method for Determination
*--------------------------------------------------------------------*
TRY.
  "Set specific table type for generic return table
  DATA: bopf_monster_header_records TYPE z4tt_monster_header.

  io_read->retrieve(
  EXPORTING iv_node = zif_4_monster_c=>sc_node-monster_header
                  it_key  = ct_key
  IMPORTING et_data = bopf_monster_header_records ).

  READ TABLE bopf_monster_header_records INTO
  DATA(bopf_monster_header_record) INDEX 1.

  IF sy-subrc NE 0.
    RETURN.
  ENDIF.

DATA(monster_model) = zcl_4_monster_model=>get_instance(
bopf_monster_header_record-monster_number ).

"Adapt BOPF-specific structure to generic monster structure
DATA(external_monster_header_record) =
CORRESPONDING z4sc_monster_header_ex( bopf_monster_header_record ).

DATA(is_determination_needed) =
monster_model->are_values_derivation_relevant( external_monster_header_record ).

IF is_determination_needed = abap_false.
   DELETE ct_key WHERE key = bopf_monster_header_record-key.
ENDIF.

CATCH zcx_monster_exceptions.
  RETURN.
ENDTRY.

ENDMETHOD."CHECK of ZCL_4_D_MONSTERHEADER_TEXTS


  METHOD /bobf/if_frw_determination~check_delta.
*--------------------------------------------------------------------*
* Listing 07.10 - Checking Changed Fields
*-----------------------------------------------------------*
* We’re looking to see if any field in the monster header
* record have changed, which may require a derived field being
* updated as a result e.g., hat size changes, hat size description
* to be updated
*------------------------------------------------------------*

    "(1) See what changes there are
    "CT_KEY will contain any monsters with changes
    io_read->compare(
    EXPORTING iv_node_key        = zif_4_monster_c=>sc_node-monster_header
              it_key             = ct_key
              iv_fill_attributes = abap_true
    IMPORTING eo_change          = DATA(change_object) ).

    change_object->get_changes( IMPORTING et_change = DATA(changed_header_records) ).

    LOOP AT ct_key ASSIGNING FIELD-SYMBOL(<monster_key>).

      READ TABLE changed_header_records ASSIGNING FIELD-SYMBOL(<changed_header_record>)
      WITH KEY key1 COMPONENTS
       node_key = zif_4_monster_c=>sc_node-monster_header
       key      = <monster_key>-key.

      IF sy-subrc NE 0.
        "Impossible Situation
        CONTINUE.
      ENDIF.

      "Check Delta is not relevant for creation, only changes
      IF <changed_header_record>-change_mode NE /bobf/if_frw_c=>sc_modify_update.
        CONTINUE.
      ENDIF.

      "(2) Outsource business logic to model class
      DATA: current_monster TYPE z4de_monster_number.

      current_monster = <monster_key>-key.

      TRY.
          DATA(monster_model) = zcl_4_monster_model=>get_instance(
           current_monster ).
        CATCH zcx_monster_exceptions.
          CONTINUE.
      ENDTRY.

      "Adapt BOPF-specific structure to generic change structure
      DATA: for_the_changed_fields TYPE bal_t_fld,
            changed_field          LIKE LINE OF for_the_changed_fields.

      CLEAR for_the_changed_fields.

      LOOP AT <changed_header_record>-attributes
        ASSIGNING FIELD-SYMBOL(<field_from_attribute>).
        changed_field = <field_from_attribute>.
        INSERT changed_field INTO TABLE for_the_changed_fields.
      ENDLOOP.

      DATA(is_determination_needed) =
      monster_model->is_derivation_relevant( for_the_changed_fields ).

      "(3) If no need for determination, delete monster from table
      "of nodes to be checked
      IF is_determination_needed = abap_false.
        DELETE ct_key WHERE key = <monster_key>-key.
      ENDIF.

    ENDLOOP."Keys

  ENDMETHOD."Check Delta of ZCL_4_D_FILL_MONSTERHEADER_TEXTS


  METHOD /BOBF/IF_FRW_DETERMINATION~EXECUTE.
*--------------------------------------------------------------------*
* Listing 07.12 : Executing Determination
*--------------------------------------------------------------------*
* Local variables
DATA: bopf_monster_header_records    TYPE z4tt_monster_header,
      external_monster_header_record TYPE z4sc_monster_header_ex.

* Clear exporting parameters
CLEAR: eo_message,
       et_failed_key.

TRY.
    "Get persistent (database) values
    io_read->retrieve(
      EXPORTING iv_node = zif_4_monster_c=>sc_node-monster_header
                it_key  = it_key
      IMPORTING et_data = bopf_monster_header_records ).

LOOP AT bopf_monster_header_records INTO DATA(bopf_monster_header_record).

CLEAR external_monster_header_record.
external_monster_header_record = CORRESPONDING #( bopf_monster_header_record ).

* Use the model to derive the transient values
DATA(monster_model) =
zcl_4_monster_model=>get_instance( external_monster_header_record-monster_number ).

monster_model->derive_header_fields(
CHANGING cs_monster_header = external_monster_header_record ).

MOVE-CORRESPONDING external_monster_header_record TO bopf_monster_header_record.

* The combined structure is now full, pass it back to the BOPF
DATA: header_record_reference TYPE REF TO data.

header_record_reference = REF #( bopf_monster_header_record ).

io_modify->update(
  iv_node = is_ctx-node_key
  iv_key  = bopf_monster_header_record-key
  is_data = header_record_reference ).

ENDLOOP."Monsters being Queried

CATCH zcx_4_monster_exceptions_mc INTO DATA(monster_exception).
"This is a generic monster exception
"and now we’ll adapt it to a BOPF exception
READ TABLE it_key INTO DATA(monster_key) INDEX 1."Only one line

IF sy-subrc NE 0.
  "Impossible Situation
  RETURN.
ENDIF.

"This key (node) has failed in its mission. Shame upon it
INSERT monster_key INTO TABLE et_failed_key.

"Now send an error message in the format BOPF desires
DATA(origin_location_information) =
VALUE /bobf/s_frw_location(  "Location Location Location
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

ENDMETHOD."EXECUTE of ZCL_4_D_MONSTERHEADER_TEXTS
ENDCLASS.
