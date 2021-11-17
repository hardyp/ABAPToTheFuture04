*&---------------------------------------------------------------------*
*&  Include           Z4_MONSTER_ADL_CD01
*&---------------------------------------------------------------------*
* Local Class Definitions
*----------------------------------------------------------------------*

*----------------------------------------------------------------------*
*       CLASS lcl_persistency_layer DEFINITION
*----------------------------------------------------------------------*
CLASS lcl_persistency_layer DEFINITION ##CLASS_FINAL.

  PUBLIC SECTION.
    METHODS: get_data RETURNING VALUE(rt_output_data) TYPE g_tt_output_data.

ENDCLASS.                    "lcl_persistency_layer DEFINITION
*----------------------------------------------------------------------*
*       CLASS lcl_model DEFINITION
*----------------------------------------------------------------------*
CLASS lcl_model DEFINITION INHERITING FROM zcl_bc_model ##CLASS_FINAL.

  PUBLIC SECTION.
    DATA: mo_persistency_layer TYPE REF TO lcl_persistency_layer,
          mt_output_data       TYPE g_tt_output_data.

    METHODS:
      constructor IMPORTING io_access_class TYPE REF TO lcl_persistency_layer OPTIONAL,
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
CLASS lcl_view DEFINITION INHERITING FROM zcl_bc_view_salv_table ##CLASS_FINAL ##NEEDED.

  PUBLIC SECTION.
    METHODS:
      make_column_editable REDEFINITION,
      ida_demo  ##CALLED,
      ida_demo2 ##CALLED.

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

    METHODS: constructor IMPORTING io_model TYPE REF TO lcl_model
                                   io_view  TYPE REF TO zif_bc_alv_report_view,
      send_email,
      adjust_spreadsheet CHANGING co_excel TYPE REF TO zcl_excel
                         RAISING  zcx_excel,
      on_data_changed FOR EVENT data_changed OF lcl_model.

  PRIVATE SECTION.
    METHODS: make_column_editable IMPORTING id_column_name TYPE dd03l-fieldname
                                  CHANGING  ct_fcat        TYPE lvc_t_fcat,
             change_edit_mode,
             download_spreadseet.

ENDCLASS.                    "lcl_controller DEFINITION
*----------------------------------------------------------------------*
*       CLASS lcl_application DEFINITION
*----------------------------------------------------------------------*
CLASS lcl_application DEFINITION ##CLASS_FINAL.

  PUBLIC SECTION.
    CLASS-DATA: mo_model      TYPE REF TO lcl_model,
                mo_controller TYPE REF TO lcl_controller,
                mo_view       TYPE REF TO zif_bc_alv_report_view.

    CLASS-METHODS: main.

ENDCLASS.                    "lcl_application DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_selections DEFINITION
*----------------------------------------------------------------------*
CLASS lcl_selections DEFINITION ##CLASS_FINAL.
  PUBLIC SECTION.
    DATA: s_date  TYPE RANGE OF z4c_monster_atrocity_monitor-duedate,
          s_cstl  TYPE RANGE OF z4c_monster_atrocity_monitor-castlenumber,
          s_numbr TYPE RANGE OF z4de_monster_number,
          s_name  TYPE RANGE OF z4de_monster_name,
          p_model TYPE z4de_monster_model,
          p_vari  TYPE disvariant-variant,
          p_edit  TYPE abap_bool,
          p_send  TYPE abap_bool,
          p_email TYPE ad_smtpadr,
          p_macro TYPE abap_bool.

    METHODS: constructor IMPORTING
                            is_date  LIKE s_date
                            is_cstl  LIKE s_cstl
                            ip_vari  LIKE p_vari
                            ip_edit  LIKE p_edit
                            ip_send  LIKE p_send
                            ip_email LIKE p_email.

ENDCLASS.                    "lcl_selections DEFINITION
*--------------------------------------------------------------------*
* Listing 10.42: - IDA Class to add Calculated Fields - Definition
*--------------------------------------------------------------------*
CLASS lcl_ida_calculated_m_fields DEFINITION FINAL."M for MONSTER!
  PUBLIC SECTION.
    INTERFACES if_salv_ida_calc_field_handler.

    METHODS: constructor.

  PRIVATE SECTION.
    DATA: mt_calc_field_names TYPE if_salv_ida_types=>yts_field_name,
          mo_monster_model    TYPE REF TO zcl_4_monster_model.

ENDCLASS.
