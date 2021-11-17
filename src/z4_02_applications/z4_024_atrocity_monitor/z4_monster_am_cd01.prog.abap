*&---------------------------------------------------------------------*
*&  Include           Z_MONSTER_AM_CD01
*&---------------------------------------------------------------------*
************************************************************************
* Class Definitions
************************************************************************

*----------------------------------------------------------------------*
*       CLASS lcl_persistency_layer DEFINITION
*----------------------------------------------------------------------*
CLASS lcl_persistency_layer DEFINITION ##CLASS_FINAL.

  PUBLIC SECTION.
    METHODS:
      get_data RETURNING VALUE(rt_output_data) TYPE g_tt_output_data.

ENDCLASS.                    "lcl_persistency_layer DEFINITION
*----------------------------------------------------------------------*
*       CLASS lcl_model DEFINITION
*----------------------------------------------------------------------*
CLASS lcl_model DEFINITION INHERITING FROM zcl_bc_model ##CLASS_FINAL.

  PUBLIC SECTION.
    DATA: mo_persistency_layer TYPE REF TO lcl_persistency_layer,
          mt_output_data       TYPE g_tt_output_data.

    METHODS: constructor IMPORTING io_access_class TYPE REF TO lcl_persistency_layer OPTIONAL,
      data_retrieval,
      prepare_data_for_ouput,
      fill_user_commands    REDEFINITION,
      fill_editable_fields  REDEFINITION,
      fill_hidden_fields    REDEFINITION,
      fill_technical_fields REDEFINITION,
      fill_hotspot_fields   REDEFINITION,
      fill_subtotal_fields  REDEFINITION,
      fill_field_texts      REDEFINITION,
      fill_checkbox_fields  REDEFINITION,
      user_command          REDEFINITION,
      allocate_monster IMPORTING is_output_data TYPE g_typ_alv_output_data.

ENDCLASS.                    "lcl_model DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_view DEFINITION
*----------------------------------------------------------------------*
* We want a subclass whereby we can add extra features specific to
* this application only
*----------------------------------------------------------------------*
CLASS lcl_view DEFINITION INHERITING FROM zcl_bc_view_salv_table ##CLASS_FINAL.

  PUBLIC SECTION.
    METHODS: make_column_editable REDEFINITION.

ENDCLASS.                    "lcl_view DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_controller DEFINITION
*----------------------------------------------------------------------*
CLASS lcl_controller DEFINITION ##CLASS_FINAL.

  PUBLIC SECTION.
    INTERFACES zif_bc_controller.

    ALIASES: on_user_command FOR zif_bc_controller~on_user_command.

    DATA: mo_model TYPE REF TO lcl_model,
          mo_view  TYPE REF TO zif_bc_alv_report_view.

    METHODS: constructor     IMPORTING io_model TYPE REF TO lcl_model
                                       io_view  TYPE REF TO zif_bc_alv_report_view,
      on_data_changed FOR EVENT data_changed OF lcl_model.

  PRIVATE SECTION.
    METHODS: make_column_editable IMPORTING id_column_name TYPE dd03l-fieldname
                                  CHANGING  ct_fcat        TYPE lvc_t_fcat ##RELAX.

ENDCLASS.                    "lcl_controller DEFINITION
*----------------------------------------------------------------------*
*       CLASS lcl_application DEFINITION
*----------------------------------------------------------------------*
CLASS lcl_application DEFINITION ##CLASS_FINAL.

  PUBLIC SECTION.
    CLASS-DATA: mo_model      TYPE REF TO lcl_model,
                mo_controller TYPE REF TO lcl_controller,
                mo_view       TYPE REF TO zif_bc_alv_report_view.

    CLASS-METHODS: main,
      re_read_database,
      refresh.

ENDCLASS.                    "lcl_application DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_selections DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_selections DEFINITION ##CLASS_FINAL.
  PUBLIC SECTION.
    DATA: s_date         TYPE RANGE OF z4c_monster_atrocity_monitor-duedate,
          p_cstl         TYPE z4c_monster_atrocity_monitor-castlenumber,
          p_vari         TYPE disvariant-variant,
          date_selection TYPE z4c_monster_atrocity_monitor-duedate READ-ONLY.

    METHODS: constructor IMPORTING
                           is_date LIKE s_date
                           ip_cstl LIKE p_cstl
                           ip_vari LIKE p_vari.

ENDCLASS.                    "lcl_selections DEFINITION

*--------------------------------------------------------------------*
* Local Class to Receive Push Channel Messages
*--------------------------------------------------------------------*
* Listing 14.04: - ABAP Messaging Channels Receiver Class Definition
*--------------------------------------------------------------------*
CLASS lcl_amc_receiver DEFINITION
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES: if_amc_message_receiver_pcp.

    CLASS-DATA md_message          TYPE string.
    CLASS-DATA mt_pcp_fields       TYPE pcp_fields.
    CLASS-DATA mf_message_received TYPE abap_bool ##NEEDED.

ENDCLASS.
