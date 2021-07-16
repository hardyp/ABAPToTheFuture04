*&---------------------------------------------------------------------*
*&  Include           Z_MONSTER_ADL_IO1
*&---------------------------------------------------------------------*
* Local Class Implementations
*--------------------------------------------------------------------*
CLASS lcl_application IMPLEMENTATION.
*--------------------------------------------------------------------*
* Listing 10.01 : Generic Template for Calling ALV Reports
*--------------------------------------------------------------------*
  METHOD main."Of Monster Atrocity Due List
* Local Variables
    DATA: ld_report_name TYPE string,
          ld_repid       TYPE sy-repid.

    ld_report_name = |{ sy-tcode } : { sy-title }|.
    "It is bad news to pass system variables as parameters
    ld_repid = sy-repid.

    mo_model = NEW #( ).

    "Hard Coded here - would use configuration or similar in
    "real life
    DATA(context_data_list) = VALUE wdr_simple_name_value_list( (
    name  = 'UI_TECHNOLOGY'
    value = 'CL_SALV_TABLE' ) ).

    zcl_ocp_factory=>return_object_given(
      EXPORTING it_context_data = context_data_list
      CHANGING  co_object       = mo_view ).

    mo_controller = NEW #( io_model = mo_model
                           io_view  = mo_view ).

    mo_model->data_retrieval( ).
    mo_model->prepare_data_for_ouput( ).

    IF sy-batch IS INITIAL.
*--------------------------------------------------------------------*
* Listing 10.31 - Calling a SALV report whilst creating a container
*                 automatically
*--------------------------------------------------------------------*
* Program flow is as follows:-
* ZCL_BC_VIEW_SALV_TABLE->CREATE_CONTAINER_PREPARE_DATA
* Function ZSALV_CSQT_CREATE_CONTAINER
* ZSALV_CSQT_CREATE_CONTAINER->FILL_CONTAINER_CONTENT
* ZCL_BC_VIEW_SALV_TABLE->PREPARE_DISPLAY_DATA
* --> INITIALISE (Generic)
* --> Application Specific Changes (Generic)
* --> Display (Generic)
      mo_view->create_container_prep_display(
      EXPORTING
        id_report_name        = ld_repid                                                                                                                                             " Calling program
        if_start_in_edit_mode = go_selections->p_edit
        id_edit_control_field = mo_model->md_edit_control_field
        it_editable_fields    = mo_model->mt_editable_fields
        it_technicals         = mo_model->mt_technicals
        it_hidden             = mo_model->mt_hidden
        it_hotspots           = mo_model->mt_hotspots
        it_checkboxes         = mo_model->mt_checkboxes
        it_subtotal_fields    = mo_model->mt_subtotal_fields
        it_field_texts        = mo_model->mt_field_texts
        it_sort_criteria      = mo_model->mt_sort_criteria
        it_user_commands      = mo_model->mt_user_commands
      CHANGING
        ct_data_table         = mo_model->mt_output_data ).

    ELSE.
* If this is running in the background there is no way
* in the world we want/need a container, as there is no
* chance for the user to press any user command buttons or
* edit the data, as there is no user, and no screen for the
* container to live on for that matter
      mo_view->prepare_display_data(
        EXPORTING
          id_report_name        = ld_repid
          it_technicals         = mo_model->mt_technicals
          it_hidden             = mo_model->mt_hidden
          it_hotspots           = mo_model->mt_hotspots
          it_checkboxes         = mo_model->mt_checkboxes
          it_subtotal_fields    = mo_model->mt_subtotal_fields
          it_field_texts        = mo_model->mt_field_texts
          it_sort_criteria      = mo_model->mt_sort_criteria
          it_user_commands      = mo_model->mt_user_commands
        CHANGING
          ct_data_table         = mo_model->mt_output_data ).
    ENDIF."Are we running in the background?

    IF go_selections->p_email IS NOT INITIAL.
      mo_controller->send_email( ).
    ENDIF.

  ENDMETHOD.                                               "main

ENDCLASS.                    "lcl_application IMPLEMENTATION

*----------------------------------------------------------------------*
*       CLASS lcl_selections IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_selections IMPLEMENTATION.

  METHOD constructor.

    s_date  = is_date.
    s_cstl  = is_cstl.
    p_vari  = ip_vari.
    p_edit  = ip_edit.
    p_send  = ip_send.
    p_email = ip_email.

  ENDMETHOD.                    "constructor

ENDCLASS."Local Selections

*----------------------------------------------------------------------*
*       CLASS lcl_persistency_layer IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_persistency_layer IMPLEMENTATION.

  METHOD constructor.

  ENDMETHOD.                    "constructor

  METHOD get_data.
*--------------------------------------------------------------------*
* EXPORTING et_output_data TYPE g_tt_output_data.
*--------------------------------------------------------------------*

    SELECT * ##too_many_itab_fields "in the world
      FROM ztmonster_adl
      INTO CORRESPONDING FIELDS OF TABLE et_output_data.

  ENDMETHOD.                                               "get_data

ENDCLASS.                    "lcl_persistency_layer IMPLEMENTATION

*----------------------------------------------------------------------*
*       CLASS lcl_model IMPLEMENTATION
*----------------------------------------------------------------------*
* When creating the model for real we do not fill the import parameter
* and thus the data is read for real
* When creating the model within a unit test, we pass in a reference to
* the fake database access class
*----------------------------------------------------------------------*
CLASS lcl_model IMPLEMENTATION.

  METHOD constructor.

    super->constructor( ).

    IF io_access_class IS SUPPLIED.
      mo_persistency_layer = io_access_class.
    ELSE.
      CREATE OBJECT mo_persistency_layer.
    ENDIF.

    fill_user_commands( ).

    fill_technical_fields( ).

    fill_hidden_fields( ).

    fill_hotspot_fields( ).

    fill_subtotal_fields( ).

    fill_field_texts( ).

    fill_editable_fields( ).

    fill_checkbox_fields( ).

    set_edit_control_field( ).

  ENDMETHOD.                                               "constructor

  METHOD data_retrieval.

    mo_persistency_layer->get_data( IMPORTING et_output_data = mt_output_data ).

  ENDMETHOD.                                               "data_retrieval

**********************************************************************
* METHOD prepare_data_for_output
**********************************************************************
* Get text names of objects, mapping, etc etc
*----------------------------------------------------------------------*
  METHOD prepare_data_for_ouput.

  ENDMETHOD.                                               "prepare_data_for_ouput

  METHOD fill_user_commands.
*--------------------------------------------------------------------*
* Listing 10.27 - Method in the Model Class to Define User Commands
*--------------------------------------------------------------------*
    CLEAR mt_user_commands.

    INSERT VALUE #(
    function  = 'ZALLOCATE'
    icon      = icon_deceased_patient
    butn_type = 0                        "Normal Button
    text      = 'Allocate Monster' )
    INTO TABLE mt_user_commands.

  ENDMETHOD.                                               "fill_user_commands

  METHOD fill_editable_fields.
    APPEND 'TASK_DESCRIPTION' TO mt_editable_fields."Can be edited if job not yet in progress
  ENDMETHOD.                    "fill_editable_fields

  METHOD fill_hidden_fields.
    "No Hidden Fields
  ENDMETHOD.                    "fill_hidden_fields

  METHOD fill_technical_fields.
    APPEND 'MANDT' TO mt_technicals.
  ENDMETHOD.                    "fill_technical_fields

  METHOD fill_hotspot_fields.
    "No Hotspots
  ENDMETHOD.                    "fill_hotspot_fields

  METHOD fill_subtotal_fields.
    "No Subtotals
  ENDMETHOD.                    "fill_subtotal_fields

  METHOD fill_field_texts.
* No Need to Rename Anything
  ENDMETHOD.                    "fill_field_texts

  METHOD fill_checkbox_fields.
    APPEND 'CHECK' TO mt_checkboxes.
  ENDMETHOD.                    "fill_checkbox_fields

  METHOD user_command.

    FIELD-SYMBOLS: <ls_output> LIKE LINE OF mt_output_data.

    CASE id_user_command.
      WHEN '&IC1'.
        READ TABLE mt_output_data ASSIGNING <ls_output> INDEX id_row.
        CHECK sy-subrc = 0.
        CASE id_column.
          WHEN 'CHECK'.
            IF <ls_output>-check = abap_false.
              <ls_output>-check = abap_true.
            ELSE.
              <ls_output>-check = abap_false.
            ENDIF.
            RAISE EVENT data_changed.
          WHEN OTHERS.
            RETURN.
        ENDCASE."What column was selected for drill down?

      WHEN 'ZALLOCATE'.
        LOOP AT mt_output_data ASSIGNING <ls_output> WHERE check = abap_true.
          allocate_monster( <ls_output> ).
        ENDLOOP.

      WHEN OTHERS.
        RETURN.
    ENDCASE."What user command was chosen?

  ENDMETHOD."User Command / Model

  METHOD allocate_monster.
*--------------------------------------------------------------------*
* IMPORTING is_output_data TYPE g_typ_alv_output_data
*--------------------------------------------------------------------*
* Local Variables
    DATA: lt_options        TYPE STANDARD TABLE OF spopli,
          ls_options        LIKE LINE OF lt_options,
          ls_titles         TYPE spop5,
          ld_answer         TYPE char01,
          ld_default_choice TYPE sy-lilli,
          ld_actual_choice  TYPE sy-tabix.

    "This next bit should be in the VIEW - thus this user command should be in the CONTROLLER
    ls_options-varoption = 'Bolts-Through-Neck'.
    APPEND ls_options TO lt_options.

    ls_options-varoption = 'Creeping Terror'.
    APPEND ls_options TO lt_options.

    ls_options-varoption = 'Creature from the Black Lagoon'.
    APPEND ls_options TO lt_options.

    ls_options-varoption = 'Killer Klown'.
    APPEND ls_options TO lt_options.

    ls_options-varoption = 'Thing with Two Heads'.
    APPEND ls_options TO lt_options.

    ld_default_choice = 1.

    ls_titles-titel     = 'Choose Monster'.
    ls_titles-textline1 = 'Which Monster shall do This Deed, This Deed so Vile?'.

    CALL FUNCTION 'POPUP_TO_DECIDE_LIST'
      EXPORTING
        cursorline         = ld_default_choice
        textline1          = ls_titles-textline1
        titel              = ls_titles-titel
      IMPORTING
        answer             = ld_answer
      TABLES
        t_spopli           = lt_options[]
      EXCEPTIONS
        not_enough_answers = 1
        too_much_answers   = 2
        too_much_marks     = 3
        OTHERS             = 4.

    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    CASE ld_answer.
      WHEN 'A'.
        RETURN.
      WHEN '1' OR '2' OR '3' OR '4' OR '5'.
        ld_actual_choice = ld_answer.
      WHEN OTHERS.
        RETURN.
    ENDCASE.

    DATA: ls_monitor TYPE ztmonster_am.

    MOVE-CORRESPONDING is_output_data TO ls_monitor.

    READ TABLE lt_options INTO ls_options INDEX ld_actual_choice.

    ls_monitor-monster_name   = ls_options-varoption.
    ls_monitor-current_status = 'A'."Atrocity Ready to be Committed
    ls_monitor-delivery_number = sy-datum+2(2) &&
                                 sy-datum+4(2) &&
                                 sy-datum+6(2) &&
                                 sy-uzeit(2)   &&
                                 sy-uzeit+2(2).

    "Create the Delivery (should be in persistency layer)
    MODIFY ztmonster_am FROM ls_monitor.

    IF sy-subrc <> 0.
      ROLLBACK WORK.
      RETURN.
    ENDIF.

    "Now update the order (should be in persistency layer)
    UPDATE ztmonster_adl SET   order_status = 'C' "Foul Deed has been Requested (needs constant)
                         WHERE order_number = is_output_data-order_number.

    IF sy-subrc <> 0.
      ROLLBACK WORK.
      RETURN.
    ELSE.
      COMMIT WORK.
      MESSAGE 'Horrible, Horrible, Deed has Been Scheduled' TYPE 'I'.
    ENDIF.

    "This next bit should be in it's own method of a dedicated local class
    "Now, let us tell the push channel that the monster has been scheduled
    "to do the atrocity
    "Determine message type of the AMC channel
    TRY.
        DATA(amc_dt_manager) =
        cl_amc_dt_manager=>create( i_application_id = 'ZAMC_4_MONSTERS'
                                   i_channel_id     = '/monsters' ).
        DATA(amc_message_type) = amc_dt_manager->get_message_type( ).
      CATCH cx_amc_dt_error INTO DATA(amc_dt_error).
        MESSAGE amc_dt_error->get_text( ) TYPE 'E'.
    ENDTRY.

    IF amc_message_type NE 'PCP'.
      RETURN.
    ENDIF.

    "Fill Payload
    TRY.
        "Create Bottle to Send
        "Set extension ID to be the Castle Number. Only monster
        "monitors subscribed to that particular castle will then
        "be notified of the new atrocity to be performed. Monitors
        "for other castles do not care
        DATA(message_bottle) = CAST if_amc_message_producer_pcp(
        cl_amc_channel_manager=>create_message_producer(
          i_application_id       = 'ZAMC_4_MONSTERS'
          i_channel_id           = '/monsters'
          i_channel_extension_id = CONV #( ls_monitor-castle_number ) ) ).
        "Create Message for Bottle
        DATA(pcp_message) = cl_ac_message_type_pcp=>create( ).
        pcp_message->set_text( 'A New Atrocity needs to be Committed' ).
        pcp_message->set_field( i_name  = 'Delivery Number'
                                i_value = CONV #( ls_monitor-delivery_number ) ).
        "Put message in bottle, and throw bottle into the sea
        message_bottle->send( pcp_message ).
      CATCH cx_ac_message_type_pcp_error INTO DATA(pcp_error).
        MESSAGE pcp_error->get_text( ) TYPE 'E'.
      CATCH cx_amc_error INTO DATA(amc_error).
        MESSAGE amc_error->get_text( ) TYPE 'E'.
    ENDTRY.

  ENDMETHOD.                    "allocate_monster

ENDCLASS.                    "lcl_model IMPLEMENTATION

*----------------------------------------------------------------------*
*       CLASS lcl_view IMPLEMENTATION
*----------------------------------------------------------------------*
* During the INITIALISATION method this method is called so that
* every row in the output table will be
* changed such that nominated columns have been made editable.
* Now we want to extend this logic to restrict the ability to change
* the task description. If a monster has always been assigned to the task,
* the nature of the task can no longer be changed.
*----------------------------------------------------------------------*
CLASS lcl_view IMPLEMENTATION.

  METHOD make_column_editable.
*--------------------------------------------------------------------*
* ID_EDIT_CONTROL_FIELD Importing Type  LVC_FNAME
* IT_EDITABLE_FIELDS    Importing Type  LVC_T_FNAM
* CT_DATA_TABLE         Changing  Type  ANY TABLE
*--------------------------------------------------------------------*
* Local Variables
    DATA : ls_celltab     TYPE lvc_s_styl,
           lt_celltab     TYPE lvc_t_styl,
           ld_index       TYPE sy-tabix,
           ldo_table_line TYPE REF TO data.

    FIELD-SYMBOLS: <ls_data_table> TYPE any,
                   <ls_celltab>    TYPE lvc_s_styl,
                   <lt_celltab>    TYPE lvc_t_styl,
                   <ld_status>     TYPE zde_monster_order_status.

    BREAK hardyp.

    super->make_column_editable(
      EXPORTING id_edit_control_field = id_edit_control_field
                it_editable_fields    = it_editable_fields
      CHANGING  ct_data_table         = ct_data_table ).

*--------------------------------------------------------------------*
* Now, when the status is "in progress" gray out the task
* description fields
*--------------------------------------------------------------------*
* Dynamically create work area for looping through the table
* that was passed in
*--------------------------------------------------------------------*
    CREATE DATA ldo_table_line LIKE LINE OF ct_data_table.

    ASSIGN ldo_table_line->*  TO <ls_data_table>.

    LOOP AT ct_data_table ASSIGNING <ls_data_table>.
* Determine the Order Status
      ASSIGN COMPONENT 'ORDER_STATUS' OF STRUCTURE <ls_data_table> TO <ld_status>.
      CHECK sy-subrc = 0.
* Based upon this, alter the CELLTAB nested table, to make the
* cell read only if need be
      CHECK <ld_status> = 'C'."Foul Deed has been Requested
* Orders in this status cannot have the task description changed
      ASSIGN COMPONENT 'CELLTAB' OF STRUCTURE <ls_data_table> TO <lt_celltab>.
      CHECK sy-subrc = 0.

      READ TABLE <lt_celltab> ASSIGNING <ls_celltab> WITH KEY fieldname = 'TASK_DESCRIPTION'.

      IF sy-subrc <> 0.
        ld_index             = sy-tabix.
        ls_celltab-fieldname = 'TASK_DESCRIPTION'.
        INSERT ls_celltab INTO <lt_celltab> INDEX ld_index.
        READ TABLE <lt_celltab> ASSIGNING <ls_celltab> WITH KEY fieldname = 'TASK_DESCRIPTION'.
      ENDIF.

      <ls_celltab>-style = cl_gui_alv_grid=>mc_style_disabled."Read Only

    ENDLOOP."Data Table

  ENDMETHOD.                    "application_specific_changes

ENDCLASS.                    "lcl_view IMPLEMENTATION

*----------------------------------------------------------------------*
*       CLASS lcl_controller IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_controller IMPLEMENTATION.

  METHOD constructor.

    super->constructor( ).

    mo_model = io_model.
    mo_view  = io_view.

    "Make the controller react to the views events
    SET HANDLER on_user_command FOR mo_view.
    "If the model changes some data, then it needs to
    "tell the controller, so it can tell the view
    "to refresh the data
    SET HANDLER on_data_changed FOR mo_model.

  ENDMETHOD.                    "constructor

  METHOD send_email.
* Local Variables
    DATA: ld_program_name TYPE sy-repid,
          ld_short_title  TYPE zexcel_sheet_title,
          ld_long_title   TYPE char80,
          ld_tcode        TYPE sy-tcode,
          lo_excel        TYPE REF TO zcl_excel.

* Preconditions
    CHECK go_selections->p_send EQ abap_true.

    ld_short_title  = sy-title.
    ld_long_title   = sy-title.
    ld_program_name = sy-repid.                            "bad news to pass this directly
    ld_tcode        = 'ZMADL'.

* Emailer Class does not yet exist in PHX
*    "Step 1 - convert internal table into an EXCEL object
*    zcl_excel_emailer=>convert_salv_to_excel(
*      EXPORTING id_title = ld_short_title
*                io_salv  = mo_view->mo_alv_grid
*                it_table = mo_model->mt_output_data[]
*      CHANGING  co_excel = lo_excel ).
*
    "Step 2 - make any changes to the spreadsheet object that relate to
    "this report
    TRY.
        adjust_spreadsheet( CHANGING co_excel = lo_excel ).
      CATCH zcx_excel.
    ENDTRY.

*    "Step 3 - send the amended EXCEL object as an email
*    zcl_excel_emailer=>send_excel_object(
*        io_excel        = lo_excel
*        id_tcode        = ld_tcode
*        id_report_title = ld_long_title
*        id_program_name = ld_program_name
*        id_email        = go_selections->p_email ).

  ENDMETHOD.

  METHOD adjust_spreadsheet.
*--------------------------------------------------------------------*
* Listing 11.06: Data Declarations
*--------------------------------------------------------------------*
    DATA: column_number TYPE zexcel_cell_column,
          row_number    TYPE zexcel_cell_row.
*--------------------------------------------------------------------*
* Listing 11.07: Looping Through All the Cells in a Spreadsheet
*--------------------------------------------------------------------*
    DATA(active_worksheet)  = co_excel->get_active_worksheet( ).
    DATA(number_of_rows)    = active_worksheet->get_highest_row( ).
    DATA(number_of_columns) = active_worksheet->get_highest_column( ).

    DO number_of_columns TIMES.
      column_number = column_number + 1.
      DATA(column_as_letter) =
      zcl_excel_common=>convert_column2alpha( column_number ).
      row_number = 0.
      DO number_of_rows TIMES.
        row_number = row_number + 1.
*--------------------------------------------------------------------*
* Listing 11.08: Finding the Style of a Spreadsheet Cell
*--------------------------------------------------------------------*
        active_worksheet->get_cell(
          EXPORTING ip_column = column_as_letter
                    ip_row    = row_number
          IMPORTING ep_guid   = DATA(cell_formatting_guid) ).

        TRY.
            DATA(style_information) =
            active_worksheet->excel->get_style_to_guid( cell_formatting_guid ).
          CATCH zcx_excel.
            CLEAR style_information.
        ENDTRY.
*--------------------------------------------------------------------*
* Listing 11.09: :Setting a Cell to be Formatted Appropriately
*--------------------------------------------------------------------*
        IF style_information-complete_style-number_format-format_code =
          '#,##0.00'.
          "This is a currency amount, use the accounting conventions
          "which are to have negative numbers as red in brackets, and show
          "zero values as dashes, so as to focus the eye on the real numbers
          style_information-complete_style-number_format-format_code =
          '$#,##0.00;[Red]($#,##0.00);-'.
          active_worksheet->change_cell_style(
          ip_column                    = column_as_letter
          ip_row                       = row_number
          ip_number_format_format_code =
          style_information-complete_style-number_format-format_code ).
        ENDIF."Currency Amount
      ENDDO."Rows
    ENDDO."Columns
*--------------------------------------------------------------------*
* Listing 11.10: Programmatically Changing the Print Orientation to Landscape
*--------------------------------------------------------------------*
    "Page printing settings
    "Margins are to be set to the values for "narrow". I just copy
    "the values in the "narrow" option on the print preview
    active_worksheet->sheet_setup->set_page_margins(
    ip_top    = '1.91'
    ip_bottom = '1.91'
    ip_left   = '0.64'
    ip_right  = '0.64'
    ip_header = '0.76'
    ip_footer = '0.76'
    ip_unit   = 'cm' ).
    active_worksheet->sheet_setup->black_and_white = abap_true.

    "Requirement is to fit all columns on one sheet
    active_worksheet->sheet_setup->fit_to_page = abap_true."You should turn this on to activate fit_to_height and fit_to_width
    active_worksheet->sheet_setup->fit_to_width = 1.  " used only if ip_fit_to_page = TRUE
    active_worksheet->sheet_setup->orientation = zcl_excel_sheet_setup=>c_orientation_landscape.
    active_worksheet->sheet_setup->page_order = zcl_excel_sheet_setup=>c_ord_downthenover.
    active_worksheet->sheet_setup->paper_size = zcl_excel_sheet_setup=>c_papersize_a4.
    active_worksheet->sheet_setup->scale = 80.  " used only if ip_fit_to_page = FALSE
    active_worksheet->sheet_setup->horizontal_centered = abap_true.
*--------------------------------------------------------------------*
* Listing 11.11: Coding Header and Footer Print Settings
*--------------------------------------------------------------------*
    "Put Tab Name in Header Centre
    DATA(header_information) = VALUE zexcel_s_worksheet_head_foot(
    center_value     = active_worksheet->get_title( )
    center_font-size = 8
    center_font-name = zcl_excel_style_font=>c_name_arial ).

    DATA(footer_information) = VALUE zexcel_s_worksheet_head_foot(
    "Put last save date on footer left
    left_value = |{ sy-datum DATE = USER }|
    left_font  = header_information-center_font
    "Put Spreadsheet path and name in Footer Centre
    center_value = '&Z&F'               "Path / Filename
    center_font = header_information-center_font
    "Put page X of Y on Footer Right
    right_value = 'page &P of &N' ##no_text  "page x of y
    right_font  = header_information-center_font ).

    active_worksheet->sheet_setup->set_header_footer(
    ip_odd_header = header_information
    ip_odd_footer = footer_information ).
*--------------------------------------------------------------------*
* Listing 11.12: Making the Header Row Repeat on Every Printed Sheet
*--------------------------------------------------------------------*
    active_worksheet->zif_excel_sheet_printsettings~set_print_repeat_rows(
    iv_rows_from = 1
    iv_rows_to   = 1 ).
*--------------------------------------------------------------------*
* Listing 11.14: Creating Colors to Use in a Spreadsheet
*--------------------------------------------------------------------*
    DATA(green_style)              = co_excel->add_new_style( ).
    green_style->fill->filltype    = zcl_excel_style_fill=>c_fill_solid.
    green_style->fill->bgcolor-rgb = zcl_excel_style_color=>c_green.
    DATA(green_guid)               = green_style->get_guid( ).

    DATA(red_style)              = co_excel->add_new_style( ).
    red_style->fill->filltype    = zcl_excel_style_fill=>c_fill_solid.
    red_style->fill->bgcolor-rgb = 'FFFF99FF'."Soft Red (Pink Really)
    DATA(red_guid)               = red_style->get_guid( ).

    DATA(yellow_style)              = co_excel->add_new_style( ).
    yellow_style->fill->filltype    = zcl_excel_style_fill=>c_fill_solid.
    yellow_style->fill->bgcolor-rgb = 'FFFF9900'.    "Orange - see ZDEMO_EXCEL21
    DATA(yellow_guid)               = yellow_style->get_guid( ).
*--------------------------------------------------------------------*
* Listing 11.15: Creating Conditional Formatting
*--------------------------------------------------------------------*
    DATA: first_data_row TYPE sy-tabix,
          last_data_row  TYPE sy-tabix.

    first_data_row = 2."i.e. first row after header
    last_data_row  = lines( mo_model->mt_output_data[] ) + 1.

    "High Strength Monster - Green for 'Good'
    DATA(style_conditional) = active_worksheet->add_new_style_cond( ).
    style_conditional->rule = zcl_excel_style_conditional=>c_rule_cellis.
    style_conditional->mode_cellis = VALUE #(
    formula    = '"REALLY STRONG"'
    operator   = zcl_excel_style_conditional=>c_operator_equal
    cell_style = green_guid ).
    style_conditional->priority      = 1.
    style_conditional->set_range( ip_start_column = strength_column_constant
                ip_start_row    = first_data_row
                ip_stop_column  = strength_column_constant
                ip_stop_row     = last_data_row ).

    "Low Strength Monster - Red for 'Bad'
    style_conditional = active_worksheet->add_new_style_cond( ).
    style_conditional->rule = zcl_excel_style_conditional=>c_rule_cellis.
    style_conditional->mode_cellis = VALUE #(
    formula    = '"LOW"'
    operator   = zcl_excel_style_conditional=>c_operator_equal
    cell_style = red_guid ).
    style_conditional->priority      = 2.
    style_conditional->set_range( ip_start_column = strength_column_constant
                ip_start_row    = first_data_row
                ip_stop_column  = strength_column_constant
                ip_stop_row     = last_data_row ).

    "Medium Strength Monster - Yellow for 'nothing special'
    style_conditional = active_worksheet->add_new_style_cond( ).
    style_conditional->rule = zcl_excel_style_conditional=>c_rule_cellis.
    style_conditional->mode_cellis = VALUE #(
    formula    = '"MEDIUM"'
    operator   = zcl_excel_style_conditional=>c_operator_equal
    cell_style = yellow_guid ).
    style_conditional->priority      = 3.
    style_conditional->set_range( ip_start_column = strength_column_constant
                ip_start_row    = first_data_row
                ip_stop_column  = strength_column_constant
                ip_stop_row     = last_data_row ).
*--------------------------------------------------------------------*
* Listing 11.16: Conditional Formatting: Testing the Start of a String
*--------------------------------------------------------------------*
    style_conditional = active_worksheet->add_new_style_cond( ).
    style_conditional->rule = zcl_excel_style_conditional=>c_operator_beginswith.
    style_conditional->mode_cellis = VALUE #(
    formula    = '1'
    operator   = zcl_excel_style_conditional=>c_operator_beginswith
    cell_style = red_guid ).
*--------------------------------------------------------------------*
* Listing 11.20: Conditional Formatting with Traffic Lights
*--------------------------------------------------------------------*
    DATA: conditional_icon_settings TYPE zexcel_conditional_iconset,
          number_as_string          TYPE string.

    "Green if below 7 days
    conditional_icon_settings-cfvo1_type  =
    zcl_excel_style_conditional=>c_cfvo_type_number.
    conditional_icon_settings-cfvo1_value = '-9999'.
    "Red if above 14 days
    conditional_icon_settings-cfvo2_type =
    zcl_excel_style_conditional=>c_cfvo_type_number.
    number_as_string    = 14.
    number_as_string    = '-' && number_as_string.
    CONDENSE number_as_string.
    conditional_icon_settings-cfvo2_value =
    number_as_string.
    "Yellow otherwise
    conditional_icon_settings-cfvo3_type =
    zcl_excel_style_conditional=>c_cfvo_type_number.
    number_as_string    = 7.
    number_as_string    = '-' && number_as_string.
    CONDENSE number_as_string.
    conditional_icon_settings-cfvo3_value =
    number_as_string.
    "Show the value as well as the ICON
    conditional_icon_settings-showvalue =
    zcl_excel_style_conditional=>c_showvalue_true.
    "We create a "style" to which we will add
    "to the settings we just defined
    style_conditional = active_worksheet->add_new_style_cond( ).
    "We are going to show ICONS
    style_conditional->rule     =
    zcl_excel_style_conditional=>c_rule_iconset.
    style_conditional->priority = 1.
    "The ICONS are going to look like Traffic Lights
    conditional_icon_settings-iconset =
    zcl_excel_style_conditional=>c_iconset_3trafficlights.
    style_conditional->mode_iconset   = conditional_icon_settings.
    style_conditional->set_range(
      ip_start_column = age_column_constant
      ip_start_row    = first_data_row
      ip_stop_column  = age_column_constant
      ip_stop_row     = last_data_row ).
*--------------------------------------------------------------------*
* Listing 11.21: Changing the Formatting to Make Negatives Look Positive
*--------------------------------------------------------------------*
    "We made all the monster ages negative earlier
    "Now we have to show them as positive on the spreadsheet
    DATA: current_sheet_row TYPE zexcel_cell_row.

    current_sheet_row = 1.

    WHILE current_sheet_row LE last_data_row.
      ADD 1 TO current_sheet_row.
      style_information-complete_style-number_format-format_code =
      '#,##0;#,##0'."i.e. do not show minus sign
      active_worksheet->change_cell_style(
        ip_column                    = age_column_constant
        ip_row                       = current_sheet_row
        ip_number_format_format_code =
        style_information-complete_style-number_format-format_code ).
    ENDWHILE.
*--------------------------------------------------------------------*
* Listing 11.23: Setting Up the Pie Chart Data Worksheet
*--------------------------------------------------------------------*
    active_worksheet = co_excel->add_new_worksheet( ).

    active_worksheet->set_title( 'Pie Chart Values' ).

    "In real life you would loop over an internal table to
    "populate the data values
    "Pie Chart - Monster Types
    active_worksheet->set_cell(
    ip_column = 'A' ip_row = 1 ip_value = 'Blue Monsters' ).
    active_worksheet->set_cell(
    ip_column = 'A' ip_row = 2 ip_value = 'Red Monsters' ).
    active_worksheet->set_cell(
    ip_column = 'A' ip_row = 3 ip_value = 'Green Monsters' ).
    active_worksheet->set_cell(
    ip_column = 'A' ip_row = 4 ip_value = 'Sky Blue Pink Monsters' ).

    "Pie Chart - Number of each Monster Type
    active_worksheet->set_cell( ip_column = 'B' ip_row = 1 ip_value = 5 ).
    active_worksheet->set_cell( ip_column = 'B' ip_row = 2 ip_value = 10 ).
    active_worksheet->set_cell( ip_column = 'B' ip_row = 3 ip_value = 15 ).
    active_worksheet->set_cell( ip_column = 'B' ip_row = 4 ip_value = 20 ).
*--------------------------------------------------------------------*
* Listing 11.24: Setting Up the Pie Chart Worksheet
*--------------------------------------------------------------------*
    "Add the worksheet with the actual pie chart on it
    active_worksheet = co_excel->add_new_worksheet( ).

    active_worksheet->set_title( 'Monster Pie Chart' ).

    DATA(monster_pie_chart) = NEW zcl_excel_graph_pie( ).

    "Tell the Pie Chart where it gets it's data from
    monster_pie_chart->create_serie(
    ip_order        = 0
    "The sheet the data comes from
    ip_sheet        = 'Pie Chart Values'
    "Range where the labels live
    ip_lbl_from_col = 'A'
    ip_lbl_from_row = '1'
    ip_lbl_to_col   = 'A'
    ip_lbl_to_row   = '4'
    "Range where the data values live
    ip_ref_from_col = 'B'
    ip_ref_from_row = '1'
    ip_ref_to_col   = 'B'
    ip_ref_to_row   = '4'
    ip_sername      = 'Monsters by Color' ).

    monster_pie_chart->set_style( zcl_excel_graph=>c_style_15 ).

    "Let us show the category names next to the pie chart
    monster_pie_chart->set_show_cat_name( zcl_excel_graph_pie=>c_show_true ).

    DATA(excel_drawing) = active_worksheet->excel->add_new_drawing(
      ip_type    = zcl_excel_drawing=>type_chart
      ip_title   = 'Monster Pie Chart' ).

    excel_drawing->graph      = monster_pie_chart.
    excel_drawing->graph_type = zcl_excel_drawing=>c_graph_pie.

    DATA: drawing_location_from_co_ords TYPE zexcel_drawing_location,
          drawing_location_to_co_ords   TYPE zexcel_drawing_location.

    drawing_location_to_co_ords-row = 20.
    drawing_location_to_co_ords-col = 10.
    excel_drawing->set_position2(
        ip_from = drawing_location_from_co_ords
        ip_to   = drawing_location_to_co_ords ).

    excel_drawing->set_media(
        ip_media_type = zcl_excel_drawing=>c_media_type_xml ).

    active_worksheet->add_drawing( excel_drawing ).

    "The value sheet for the pie chart is hidden by default
    co_excel->set_active_sheet_index( 2 ).
    active_worksheet = co_excel->get_active_worksheet( ).
    active_worksheet->zif_excel_sheet_properties~hidden =
    zif_excel_sheet_properties=>c_hidden.

    "We want the user to start on the first worksheet
    co_excel->set_active_sheet_index( 1 ).
*--------------------------------------------------------------------*
* Listing 11.32: Inserting Hyperlinks in a Spreadsheet
*--------------------------------------------------------------------*
*    DATA: hyperlink                TYPE REF TO zcl_excel_hyperlink,
*      hyperlink_url            TYPE string,
*      input_parameters         TYPE string.
*
*active_worksheet = co_excel->get_active_worksheet( ).

* Now we loop through the spreadsheet, adding hyperlinks so the user can drill
* down into the original document in SAP
*current_sheet_row = first_data_row.

*LOOP AT mo_model->mt_output_data INTO DATA(monster_output_table_row).
* Drill down into the monster master record
*IF monster_output_table_row-monster_number IS NOT INITIAL.
*  input_parameters = 'S_NUMBR-LOW=' &&
*                   monster_output_table_row-monster_number && ';'.
*  hyperlink_url = build_hyperlink_url(
*    id_transaction = 'ZMONSTER'
*    id_parameters  = input_parameters
*    id_ok_code     = '=ONLI' ).
*hyperlink = zcl_excel_hyperlink=>create_external_link(
*  iv_url = hyperlink_url ).
*active_worksheet->set_cell(
*  ip_column    = monster_column_constant
*  ip_row       = current_sheet_row
*  ip_value     = monster_output_table_row-monster_number
*  ip_hyperlink = hyperlink ).
*active_worksheet->change_cell_style(
*  ip_column         = monster_column_constant
*  ip_row            = current_sheet_row
*  ip_font_color_rgb = zcl_excel_style_color=>c_blue
*  ip_font_underline = abap_true ).
*ENDIF."Do we have a Monster Number?
*current_sheet_row = current_sheet_row + 1.
*ENDLOOP."Monster Table

  ENDMETHOD.

  METHOD on_user_command.
*--------------------------------------------------------------------*
* Listing 10.36 - User Command to Make a SALV Grid Editable
*--------------------------------------------------------------------*
* FOR EVENT added_function OF cl_salv_events
* IMPORTING ed_user_command
*           ed_row
*           ed_column.
*--------------------------------------------------------------------*
* Local Variables
    DATA: underlying_alv_grid TYPE REF TO cl_gui_alv_grid,
          layout_info         TYPE lvc_s_layo,
          field_catalog_table TYPE lvc_t_fcat.

    CASE ed_user_command.
      WHEN 'ZEDIT'. "A command to change the edit mode

        underlying_alv_grid = mo_view->get_alv_grid_object( ).

        IF underlying_alv_grid IS NOT BOUND.
          RETURN.
        ENDIF.

        underlying_alv_grid->get_frontend_fieldcatalog(
          IMPORTING
            et_fieldcatalog = field_catalog_table ).

        make_column_editable( :
        EXPORTING id_column_name = 'MONSTER_HATS'
        CHANGING ct_fcat = field_catalog_table ),
        EXPORTING id_column_name = 'MONSTER_HEADS'
        CHANGING ct_fcat = field_catalog_table  ).

        underlying_alv_grid->set_frontend_fieldcatalog( field_catalog_table ).
        layout_info-stylefname = 'CELLTAB'.
        underlying_alv_grid->set_frontend_layout( layout_info ).
        underlying_alv_grid->refresh_table_display( ).

        cl_gui_cfw=>flush( ).

    ENDCASE.

    mo_model->user_command(
      EXPORTING
        id_user_command = ed_user_command                                              " Function code that PAI triggered
        id_column       = ed_column                                                    " Selected Column
        id_row          = ed_row ).                                                    " Selected Row

    mo_view->refresh_display( ).

  ENDMETHOD."User Command / Controller

  METHOD on_data_changed.

    mo_view->refresh_display( ).

  ENDMETHOD.                                               "on_data_changed

  METHOD make_column_editable.
*--------------------------------------------------------------------*
* Listng 10.34 - MAKE_COLUMN_EDITABLE Method
*--------------------------------------------------------------------*
* IMPORTING id_column_name TYPE dd03l-fieldname
* CHANGING  ct_fcat        TYPE lvc_t_fcat.
*--------------------------------------------------------------------*
* Local Variables
    DATA :ls_celltab TYPE lvc_s_styl,
          ld_index   TYPE sy-tabix.

    FIELD-SYMBOLS: <ls_output>  LIKE LINE OF mo_model->mt_output_data,
                   <ls_celltab> TYPE lvc_s_styl.

    LOOP AT mo_model->mt_output_data ASSIGNING <ls_output>.

      READ TABLE <ls_output>-celltab ASSIGNING <ls_celltab> WITH KEY fieldname = id_column_name.

      IF sy-subrc <> 0.
        ld_index             = sy-tabix.
        ls_celltab-fieldname = id_column_name.
        INSERT ls_celltab INTO <ls_output>-celltab INDEX ld_index.
        READ TABLE <ls_output>-celltab ASSIGNING <ls_celltab> WITH KEY fieldname = id_column_name.
      ENDIF.

      IF <ls_celltab>-style EQ cl_gui_alv_grid=>mc_style_enabled.
        <ls_celltab>-style = cl_gui_alv_grid=>mc_style_disabled.
      ELSE.
        <ls_celltab>-style = cl_gui_alv_grid=>mc_style_enabled.
      ENDIF.

    ENDLOOP.

    FIELD-SYMBOLS: <ls_fcat> LIKE LINE OF ct_fcat.

    LOOP AT ct_fcat ASSIGNING <ls_fcat> WHERE fieldname = id_column_name.
      <ls_fcat>-edit = abap_true.
    ENDLOOP.

  ENDMETHOD."make  column editable

ENDCLASS.                    "lcl_controller IMPLEMENTATION
