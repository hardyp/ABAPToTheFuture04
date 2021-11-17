*&---------------------------------------------------------------------*
*&  Include           Z_MONSTER_ADL_IO1
*&---------------------------------------------------------------------*
* Local Class Implementations
*--------------------------------------------------------------------*
CLASS lcl_application IMPLEMENTATION.

  METHOD main.
*--------------------------------------------------------------------*
* Listing 14.06: - Subscribing to Messages
*--------------------------------------------------------------------*
*--------------------------------------------------------------------*
    "Set up Push Channels
    TRY.
        "We are going to subscribe to messages relating to this castle only
        gd_chext_id = go_selections->p_cstl.

        go_receiver = NEW #( ).
        DATA(dynamic_class) =
        `\PROGRAM=CL_AMC_CHANNEL_MANAGER========CP` &&
        `\CLASS=LCL_SAPGUI_CHANNEL_MANAGER`.

        CALL METHOD (dynamic_class)=>create_message_consumer
          EXPORTING
            i_application_id       = gd_appl_id
            i_channel_id           = gd_ch_id
            i_channel_extension_id = gd_chext_id
          RECEIVING
            r_consumer             = go_consumer.

        go_consumer->start_message_delivery( go_receiver ).

        gf_message_received = abap_false.

      CATCH cx_amc_error INTO DATA(amc_error).
        MESSAGE amc_error TYPE 'E'.
    ENDTRY.

    mo_model      = NEW #( ).
    mo_view       = NEW lcl_view( ).
    mo_controller = NEW #( io_model = mo_model
                           io_view  = mo_view ).

    mo_model->data_retrieval( ).
    mo_model->prepare_data_for_ouput( ).

    "It is bad news to pass system variables as parameters
    DATA(ld_repid) = sy-repid.

    IF sy-batch IS INITIAL.
*--------------------------------------------------------------------*
* Listing 10.35: - Calling a SALV report whilst creating a container
*                  automatically
*--------------------------------------------------------------------*
* Program flow is as follows:-
* ZCL_BC_VIEW_SALV_TABLE->CREATE_CONTAINER_PREPARE_DATA
* Function ZSALV_CSQT_CREATE_CONTAINER
* ZSALV_CSQT_CREATE_CONTAINER->FILL_CONTAINER_CONTENT
* ZCL_BC_VIEW_SALV_TABLE->PREPARE_DISPLAY_DATA
* --> INITIALISE (Generic)
* --> Application Specific Changes (in this program)
* --> Display (Generic)
      mo_view->create_container_prep_display(
    EXPORTING
      id_report_name        = ld_repid
      if_start_in_edit_mode = abap_true
      id_edit_control_field = mo_model->md_edit_control_field
      it_editable_fields    = mo_model->mt_editable_fields
      it_technicals         = mo_model->mt_technicals
      it_hidden             = mo_model->mt_hidden
      it_hotspots           = mo_model->mt_hotspots
      it_checkboxes         = mo_model->mt_checkboxes
      it_subtotal_fields    = mo_model->mt_subtotal_fields
      it_field_texts        = mo_model->mt_field_texts
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
          id_report_name     = ld_repid
          it_technicals      = mo_model->mt_technicals
          it_hidden          = mo_model->mt_hidden
          it_hotspots        = mo_model->mt_hotspots
          it_checkboxes      = mo_model->mt_checkboxes
          it_subtotal_fields = mo_model->mt_subtotal_fields
          it_field_texts     = mo_model->mt_field_texts
          it_user_commands   = mo_model->mt_user_commands
        CHANGING
          ct_data_table      = mo_model->mt_output_data ).
    ENDIF."Are we running in the background?

  ENDMETHOD.                                               "main

* To be called when the push channel notifies this report of
* new information the running program has subscribed to
  METHOD refresh.
    mo_view->refresh_display( ).
  ENDMETHOD.                    "refresh

  METHOD re_read_database.
    mo_model->data_retrieval( ).
  ENDMETHOD.

ENDCLASS.                    "lcl_application IMPLEMENTATION

*----------------------------------------------------------------------*
*       CLASS lcl_selections IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_selections IMPLEMENTATION.

  METHOD constructor.

    s_date  = is_date.
    p_cstl  = ip_cstl.
    p_vari  = ip_vari.

  ENDMETHOD.                    "constructor

ENDCLASS."Local Selections

*----------------------------------------------------------------------*
*       CLASS lcl_persistency_layer IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_persistency_layer IMPLEMENTATION.

  METHOD get_data.
*--------------------------------------------------------------------*
* EXPORTING et_output_data TYPE g_tt_output_data.
*--------------------------------------------------------------------*

    SELECT * ##too_many_itab_fields "in the world
      FROM z4c_monster_atrocity_monitor
      INTO CORRESPONDING FIELDS OF TABLE @rt_output_data.

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
      mo_persistency_layer = NEW #( ).
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

    mt_output_data = mo_persistency_layer->get_data( ).

  ENDMETHOD.                                               "data_retrieval

*--------------------------------------------------------------------*
* METHOD prepare_data_for_output
*--------------------------------------------------------------------*
* Get text names of objects, mapping, etc etc. As time goes by more and
* more of this sort of thing will be done at the database level
*----------------------------------------------------------------------*
  METHOD prepare_data_for_ouput ##NEEDED.

  ENDMETHOD.                                               "prepare_data_for_ouput

  METHOD fill_user_commands.
*--------------------------------------------------------------------*
* Listing 10.27: - Method in the Model Class to Define User Commands
*--------------------------------------------------------------------*
    CLEAR mt_user_commands.

    INSERT VALUE #(
    function  = 'ZCOMPLETE'
    icon      = icon_deceased_patient
    butn_type = 0                        "Normal Button
    text      = 'Deed is Done'(001) ) INTO TABLE mt_user_commands.

* One Haunted Castle has a ghost called the "blue (or radiant) boy" who according to the owners used to haunt the
* Pink Room in the castle
  ENDMETHOD.                                               "fill_user_commands

  METHOD fill_editable_fields ##NEEDED.
* No Editable Fields Here
  ENDMETHOD.                    "fill_editable_fields

  METHOD fill_hidden_fields ##NEEDED.
* No Hidden Fields
  ENDMETHOD.                    "fill_hidden_fields

  METHOD fill_technical_fields.
    APPEND 'MANDT' TO mt_technicals.
  ENDMETHOD.                    "fill_technical_fields

  METHOD fill_hotspot_fields ##NEEDED.
* No Hotspots
  ENDMETHOD.                    "fill_hotspot_fields

  METHOD fill_subtotal_fields ##NEEDED.
* No Subtotals
  ENDMETHOD.                    "fill_subtotal_fields

  METHOD fill_field_texts ##NEEDED.
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
        IF sy-subrc NE 0.
          RETURN.
        ENDIF.
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

    ls_options-varoption = 'Bolts-Through-Neck'(002).
    APPEND ls_options TO lt_options.

    ls_options-varoption = 'Creeping Terror'(003).
    APPEND ls_options TO lt_options.

    ls_options-varoption = 'Creature from the Black Lagoon'(004).
    APPEND ls_options TO lt_options.

    ls_options-varoption = 'Killer Klown'(005).
    APPEND ls_options TO lt_options.

    ls_options-varoption = 'Thing with Two Heads'(006).
    APPEND ls_options TO lt_options.

    ld_default_choice = 1.

    ls_titles-titel     = 'Choose Monster'(007).
    ls_titles-textline1 = 'Which Monster shall do This Deed, This Deed so Vile?'(008).

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
      WHEN '1' OR '2' OR '3' OR '4' OR '5'.
        ld_actual_choice = ld_answer.
      WHEN OTHERS.
        RETURN.
    ENDCASE.

    DATA: ls_monitor TYPE z4t_deliveries.

    SELECT *
      FROM z4t_deliveries UP TO 1 ROWS
      INTO CORRESPONDING FIELDS OF ls_monitor
      WHERE delivery_number = is_output_data-deliverynumber
      ORDER BY delivery_number.
    ENDSELECT.
    IF sy-subrc NE 0.
      RETURN.
    ENDIF.

    READ TABLE lt_options INTO ls_options INDEX ld_actual_choice.
    IF sy-subrc NE 0.
      RETURN.
    ENDIF.

    ls_monitor-delivery_number = is_output_data-deliverynumber.
    ls_monitor-monster_name    = ls_options-varoption.
    ls_monitor-current_status  = 'A'."Atrocity Ready to be Committed

* TO-DO
* The delivery business object persistency layer should update the delivery
    UPDATE z4t_deliveries FROM ls_monitor.
    IF sy-subrc <> 0.
      ROLLBACK WORK.                                   "#EC CI_ROLLBACK
      RETURN.
    ENDIF.

  ENDMETHOD.                    "allocate_monster

ENDCLASS.                    "lcl_model IMPLEMENTATION

*----------------------------------------------------------------------*
*       CLASS lcl_view IMPLEMENTATION
*------------------------------------------------------------------------*
* During the INITIALISATION method this method is called so that
* every row in the output table will be
* changed such that nominated columns have been made editable.
* Now we want to extend this logic to restrict the ability to change
* the task description. If a monster has always been assigned to the task,
* the nature of the task can no longer be changed.
* This does not actualy work by the way because the SALV does not allow it
*-------------------------------------------------------------------------*
CLASS lcl_view IMPLEMENTATION.

  METHOD make_column_editable.
*--------------------------------------------------------------------*
* ID_EDIT_CONTROL_FIELD Importing Type  LVC_FNAME
* IT_EDITABLE_FIELDS    Importing Type  LVC_T_FNAM
* CT_DATA_TABLE         Changing  Type  ANY TABLE
*--------------------------------------------------------------------*
* Local Variables
    DATA: ldo_table_line TYPE REF TO data.

    FIELD-SYMBOLS: <ls_data_table> TYPE any,
                   <lt_celltab>    TYPE lvc_t_styl,
                   <ld_status>     TYPE z4de_monster_delivery_status.

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
      "Determine the Order Status
      ASSIGN COMPONENT 'CURRENT_STATUS' OF STRUCTURE <ls_data_table> TO <ld_status>.
      CHECK sy-subrc = 0.
      "Based upon this, alter the CELLTAB nested table, to make the
      "cell read only if need be
      CHECK <ld_status> = 'C'."Foul Deed has been Requested
      "Orders in this status cannot have the task description changed
      ASSIGN COMPONENT 'CELLTAB' OF STRUCTURE <ls_data_table> TO <lt_celltab>.
      CHECK sy-subrc = 0.

      READ TABLE <lt_celltab> ASSIGNING FIELD-SYMBOL(<ls_celltab>) WITH KEY fieldname = 'TASK_DESCRIPTION'.

      IF sy-subrc <> 0.
        INSERT VALUE #( fieldname = 'TASK_DESCRIPTION' ) INTO TABLE <lt_celltab>.
        READ TABLE <lt_celltab> ASSIGNING <ls_celltab> WITH KEY fieldname = 'TASK_DESCRIPTION'.
        ASSERT sy-subrc EQ 0.
      ENDIF.

      <ls_celltab>-style = cl_gui_alv_grid=>mc_style_disabled."Read Only

    ENDLOOP."Data Table

  ENDMETHOD.

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

  METHOD on_user_command.
*--------------------------------------------------------------------*
* Listing 10.32 - User Command to Make a SALV Grid Editable
*--------------------------------------------------------------------*
* FOR EVENT added_function OF cl_salv_events
* IMPORTING ed_user_command
*           ed_row
*           ed_column.
*--------------------------------------------------------------------*
* Local Variables
    DATA: lo_alv  TYPE REF TO cl_gui_alv_grid ##NEEDED.

    CASE ed_user_command.
      WHEN 'ZSAVE'."A command to save your changes
        "Trick to update the internal table
        lo_alv->check_changed_data(
          IMPORTING
            e_valid = DATA(lf_valid) ) ##NEEDED.
        "Code to save the data
        MESSAGE i013(z4monsters)."Data saved
      WHEN OTHERS.
        "Let it go ... the model might need to respond even if the controller does not care
    ENDCASE.

    mo_model->user_command(
        id_user_command = ed_user_command
        id_column       = ed_column
        id_row          = ed_row ).

    mo_view->refresh_display( ).

  ENDMETHOD."User Command / Controller

  METHOD on_data_changed.

    mo_view->refresh_display( ).

  ENDMETHOD.                                               "on_data_changed

  METHOD make_column_editable.
*--------------------------------------------------------------------*
* IMPORTING id_column_name TYPE dd03l-fieldname
* CHANGING  ct_fcat        TYPE lvc_t_fcat.
*--------------------------------------------------------------------*

    LOOP AT mo_model->mt_output_data ASSIGNING FIELD-SYMBOL(<ls_output>).

      READ TABLE <ls_output>-celltab ASSIGNING FIELD-SYMBOL(<ls_celltab>) WITH KEY fieldname = id_column_name.

      IF sy-subrc <> 0.
        INSERT VALUE #( fieldname = id_column_name ) INTO TABLE <ls_output>-celltab.
        READ TABLE <ls_output>-celltab ASSIGNING <ls_celltab> WITH KEY fieldname = id_column_name.
        ASSERT sy-subrc EQ 0.
      ENDIF.

      <ls_celltab>-style = cl_gui_alv_grid=>mc_style_enabled.

    ENDLOOP.

    LOOP AT ct_fcat ASSIGNING FIELD-SYMBOL(<ls_fcat>) WHERE fieldname = id_column_name.
      <ls_fcat>-edit = abap_true.
    ENDLOOP.

  ENDMETHOD."make column editable

ENDCLASS.                    "lcl_controller IMPLEMENTATION

*--------------------------------------------------------------------*
* Class Implementation for receiving Push Channel Messages
*--------------------------------------------------------------------*
CLASS lcl_amc_receiver IMPLEMENTATION.
** <SIGNATURE>---------------------------------------------------------------------------------------+
** | Instance Public Method CL_AMC_TEST_BINARY->IF_AMC_MESSAGE_RECEIVER_BINARY~RECEIVE
** +-------------------------------------------------------------------------------------------------+
** | [--->] MESSAGE                        TYPE        XSTRING
** | [--->] CONTEXT                        TYPE REF TO IF_AMC_MESSAGE_CONTEXT
** +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD if_amc_message_receiver_pcp~receive.
*--------------------------------------------------------------------*
* Listing 14.05: - Coding PCP RECEIVE Method
*--------------------------------------------------------------------*
    "Fill up static class variables from incoming message
    mf_message_received = abap_true.

    TRY.
        md_message = i_message->get_text( ).
        i_message->get_fields( CHANGING c_fields = mt_pcp_fields ).
      CATCH cx_ac_message_type_pcp_error INTO DATA(pcp_error).
        md_message = |RECIEVE ERROR:{ pcp_error->get_text( ) }|.
    ENDTRY.

    "Just to prove a point, pop up message on users screen
    MESSAGE md_message TYPE 'I'.

    "Time to magically update the users screen....
    lcl_application=>re_read_database( ).
    lcl_application=>refresh( ).

  ENDMETHOD.        "if_amc_message_receiver_pcp~receive

ENDCLASS.
