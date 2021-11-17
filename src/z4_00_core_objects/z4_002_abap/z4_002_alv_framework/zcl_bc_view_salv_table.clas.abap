class ZCL_BC_VIEW_SALV_TABLE definition
  public
  create public .

public section.
  type-pools ABAP .

  interfaces ZIF_BC_ALV_REPORT_VIEW .
  interfaces IF_SALV_CSQT_CONTENT_MANAGER .
  interfaces ZIF_CREATED_VIA_OCP_FACTORY .

  aliases ADD_SORT_CRITERIA
    for ZIF_BC_ALV_REPORT_VIEW~ADD_SORT_CRITERIA .
  aliases APPLICATION_SPECIFIC_CHANGES
    for ZIF_BC_ALV_REPORT_VIEW~APPLICATION_SPECIFIC_CHANGES .
  aliases CREATE_CONTAINER_PREP_DISPLAY
    for ZIF_BC_ALV_REPORT_VIEW~CREATE_CONTAINER_PREP_DISPLAY .
  aliases DEFAULT_CLASS
    for ZIF_CREATED_VIA_OCP_FACTORY~DEFAULT_CLASS .
  aliases DISPLAY
    for ZIF_BC_ALV_REPORT_VIEW~DISPLAY .
  aliases GET_ALV_GRID_OBJECT
    for ZIF_BC_ALV_REPORT_VIEW~GET_ALV_GRID_OBJECT .
  aliases GET_MAIN_ALV_OBJECT
    for ZIF_BC_ALV_REPORT_VIEW~GET_MAIN_ALV_OBJECT .
  aliases INITIALISE
    for ZIF_BC_ALV_REPORT_VIEW~INITIALISE .
  aliases MD_EDIT_CONTROL_FIELD
    for ZIF_BC_ALV_REPORT_VIEW~MD_EDIT_CONTROL_FIELD .
  aliases MS_LAYOUT
    for ZIF_BC_ALV_REPORT_VIEW~MS_LAYOUT .
  aliases MT_CHECKBOXES
    for ZIF_BC_ALV_REPORT_VIEW~MT_CHECKBOXES .
  aliases MT_EDITABLE_FIELDS
    for ZIF_BC_ALV_REPORT_VIEW~MT_EDITABLE_FIELDS .
  aliases MT_FIELD_TEXTS
    for ZIF_BC_ALV_REPORT_VIEW~MT_FIELD_TEXTS .
  aliases MT_HIDDEN
    for ZIF_BC_ALV_REPORT_VIEW~MT_HIDDEN .
  aliases MT_HOTSPOTS
    for ZIF_BC_ALV_REPORT_VIEW~MT_HOTSPOTS .
  aliases MT_SORT_CRITERIA
    for ZIF_BC_ALV_REPORT_VIEW~MT_SORT_CRITERIA .
  aliases MT_SUBTOTAL_FIELDS
    for ZIF_BC_ALV_REPORT_VIEW~MT_SUBTOTAL_FIELDS .
  aliases MT_TECHNICALS
    for ZIF_BC_ALV_REPORT_VIEW~MT_TECHNICALS .
  aliases OPTIMISE_COLUMN_WIDTHS
    for ZIF_BC_ALV_REPORT_VIEW~OPTIMISE_COLUMN_WIDTHS .
  aliases PREPARE_DISPLAY_DATA
    for ZIF_BC_ALV_REPORT_VIEW~PREPARE_DISPLAY_DATA .
  aliases REFRESH_DISPLAY
    for ZIF_BC_ALV_REPORT_VIEW~REFRESH_DISPLAY .
  aliases SET_COLUMN_ATTRIBUTES
    for ZIF_BC_ALV_REPORT_VIEW~SET_COLUMN_ATTRIBUTES .
  aliases SET_LIST_HEADER
    for ZIF_BC_ALV_REPORT_VIEW~SET_LIST_HEADER .
  aliases ADD_COMMANDS_TO_TOOLBAR
    for ZIF_BC_ALV_REPORT_VIEW~ADD_COMMANDS_TO_TOOLBAR .
  aliases DISPLAY_BASIC_TOOLBAR
    for ZIF_BC_ALV_REPORT_VIEW~DISPLAY_BASIC_TOOLBAR .
  aliases FILL_CONTAINER_CONTENT
    for IF_SALV_CSQT_CONTENT_MANAGER~FILL_CONTAINER_CONTENT .
  aliases IS_THE_RIGHT_CLASS_TYPE_GIVEN
    for ZIF_CREATED_VIA_OCP_FACTORY~IS_THE_RIGHT_CLASS_TYPE_GIVEN .
  aliases MAKE_COLUMN_EDITABLE
    for ZIF_BC_ALV_REPORT_VIEW~MAKE_COLUMN_EDITABLE .
  aliases OPTIMISE_COLUMN_WIDTH
    for ZIF_BC_ALV_REPORT_VIEW~OPTIMISE_COLUMN_WIDTH .
  aliases SET_CHECKBOX
    for ZIF_BC_ALV_REPORT_VIEW~SET_CHECKBOX .
  aliases SET_COLUMN_AS_BUTTON
    for ZIF_BC_ALV_REPORT_VIEW~SET_COLUMN_AS_BUTTON .
  aliases SET_HANDLERS
    for ZIF_BC_ALV_REPORT_VIEW~SET_HANDLERS .
  aliases SET_HOTSPOT
    for ZIF_BC_ALV_REPORT_VIEW~SET_HOTSPOT .
  aliases SET_KEY
    for ZIF_BC_ALV_REPORT_VIEW~SET_KEY .
  aliases SET_LAYOUT
    for ZIF_BC_ALV_REPORT_VIEW~SET_LAYOUT .
  aliases SET_LONG_TEXT
    for ZIF_BC_ALV_REPORT_VIEW~SET_LONG_TEXT .
  aliases SET_MEDIUM_TEXT
    for ZIF_BC_ALV_REPORT_VIEW~SET_MEDIUM_TEXT .
  aliases SET_NO_MERGING
    for ZIF_BC_ALV_REPORT_VIEW~SET_NO_MERGING .
  aliases SET_SELECTIONS
    for ZIF_BC_ALV_REPORT_VIEW~SET_SELECTIONS .
  aliases SET_SHORT_TEXT
    for ZIF_BC_ALV_REPORT_VIEW~SET_SHORT_TEXT .
  aliases SET_STRIPED_PATTERN
    for ZIF_BC_ALV_REPORT_VIEW~SET_STRIPED_PATTERN .
  aliases SET_SUBTOTAL
    for ZIF_BC_ALV_REPORT_VIEW~SET_SUBTOTAL .
  aliases SET_TECHNICAL
    for ZIF_BC_ALV_REPORT_VIEW~SET_TECHNICAL .
  aliases SET_TOOLTIP
    for ZIF_BC_ALV_REPORT_VIEW~SET_TOOLTIP .
  aliases SET_VISIBLE
    for ZIF_BC_ALV_REPORT_VIEW~SET_VISIBLE .
  aliases USER_COMMAND_RECEIVED
    for ZIF_BC_ALV_REPORT_VIEW~USER_COMMAND_RECEIVED .

  data MT_DATA_TABLE type ref to DATA .
  data MD_REPORT_NAME type SY-REPID .
  data MS_VARIANT type DISVARIANT .
  data MT_USER_COMMANDS type TTB_BUTTON .
  data MO_SALV_MODEL type ref to ZCL_SALV_MODEL .
  data MO_ALV_GRID type ref to CL_SALV_TABLE .
  constants MC_CTX_UI_TECHNOLOGY type SEOCLNAME value 'CL_SALV_TABLE' ##NO_TEXT.

  methods HANDLE_LINK_CLICK
    for event LINK_CLICK of CL_SALV_EVENTS_TABLE
    importing
      !ROW
      !COLUMN .
  methods HANDLE_USER_COMMAND
    for event ADDED_FUNCTION of CL_SALV_EVENTS
    importing
      !E_SALV_FUNCTION .
protected section.
private section.

  data MO_AGGREGATIONS type ref to CL_SALV_AGGREGATIONS .
  data MO_COLUMN type ref to CL_SALV_COLUMN_TABLE .
  data MO_COLUMNS type ref to CL_SALV_COLUMNS_TABLE .
  data MO_EVENTS type ref to CL_SALV_EVENTS_TABLE .
  data MO_FUNCTIONS type ref to CL_SALV_FUNCTIONS_LIST .
  data MO_LAYOUT type ref to CL_SALV_LAYOUT .
  data MO_SELECTIONS type ref to CL_SALV_SELECTIONS .
  data MO_SORTS type ref to CL_SALV_SORTS .
  data MO_SETTINGS type ref to CL_SALV_DISPLAY_SETTINGS .
  data MF_START_IN_EDIT_MODE type ABAP_BOOL .

  methods GET_ALV_GRID
    returning
      value(RO_ALV_GRID) type ref to CL_GUI_ALV_GRID .
  methods STATUS_EXISTS
    importing
      !ID_CALLING_PROGRAM type SYREPID
      !ID_STATUS_NAME type SYPFKEY
    returning
      value(RF_EXISTS) type ABAP_BOOL .
ENDCLASS.



CLASS ZCL_BC_VIEW_SALV_TABLE IMPLEMENTATION.


METHOD get_alv_grid.
*--------------------------------------------------------------------*
* In full screen mode the following function will return the
* underlying ALV Grid from a SALV object
*--------------------------------------------------------------------*
* In actual fact we are very rarely in Full Screen Mode, but just so
* you know how to do this....
*--------------------------------------------------------------------*
    CALL FUNCTION 'GET_GLOBALS_FROM_SLVC_FULLSCR'
      IMPORTING
        e_grid = ro_alv_grid.

    IF ro_alv_grid IS BOUND.
      "We are in full screen mode, all is well
      RETURN.
    ENDIF.

*--------------------------------------------------------------------*
* Listing 09.33 - Getting an ALV Grid when we have a container
*--------------------------------------------------------------------*
* When we have a container, then the grid
* needs to be retrieved via the adapter
    ro_alv_grid = mo_salv_model->get_alv_grid( ).

  ENDMETHOD.                    "GET_ALV_GRID


METHOD handle_link_click.
*--------------------------------------------------------------------*
* Listing 10.20: - HANDLE_LINK_CLICK Method
*--------------------------------------------------------------------*
    "No type conversions needed
    RAISE EVENT user_command_received
    EXPORTING ed_user_command = '&IC1'
              ed_row          = row
              ed_column       = column.

ENDMETHOD.                    "handle_link_click


METHOD handle_user_command.
*--------------------------------------------------------------------*
* Listing 10.21: - HANDLE_USER_COMMAND Method
*--------------------------------------------------------------------*
    RAISE EVENT user_command_received
    EXPORTING ed_user_command = e_salv_function.

  ENDMETHOD.                    "handle_user_command


METHOD if_salv_csqt_content_manager~fill_container_content.
*------------------------------------------------------------------*
* Listing 10.24: - FILL_CONTAINER_CONTENT Method
*------------------------------------------------------------------*
* This gets called from function SALV_CSQT_CREATE_CONTAINER PBO
* module which creates a screen and a container, and passes us
* that container in the form of importing parameter R_CONTAINER
*------------------------------------------------------------------*
* Local Variables
    FIELD-SYMBOLS: <lt_data_table> TYPE ANY TABLE.

    ASSIGN mt_data_table->* TO <lt_data_table>.

    prepare_display_data(
      EXPORTING
        id_report_name        = md_report_name                    " Calling program
        if_start_in_edit_mode = mf_start_in_edit_mode
        id_edit_control_field = md_edit_control_field
        is_layout             = ms_layout
        it_editable_fields    = mt_editable_fields
        it_technicals         = mt_technicals
        it_hidden             = mt_hidden
        it_hotspots           = mt_hotspots
        it_checkboxes         = mt_checkboxes
        it_subtotal_fields    = mt_subtotal_fields
        it_sort_criteria      = mt_sort_criteria
        it_field_texts        = mt_field_texts
        io_container          = r_container                       " Container for Custom Controls in the Screen Area
        it_user_commands      = mt_user_commands                  " Toolbar Buttons
      CHANGING
        ct_data_table         = <lt_data_table> ).                " Data Table

  ENDMETHOD.                    "if_salv_csqt_content_manager~fill_container_content


METHOD status_exists.
*--------------------------------------------------------------------*
* The details of the various STATUSES (toolbars) for ABAP programs
* are hidden away in a cluster table
*--------------------------------------------------------------------*
* Local Variables
  DATA: ls_eukey    TYPE rseu1_key,
        lt_statuses TYPE STANDARD TABLE OF rsmpe_sta.

  "Set Default value
  rf_exists = abap_false.

  ls_eukey-name  = id_calling_program.
  ls_eukey-sprsl = scua_c_eudb_sprsl."German aways!

  IMPORT sta TO lt_statuses[] FROM DATABASE eudb(cu) ID ls_eukey
                              IGNORING CONVERSION ERRORS
                              ACCEPTING TRUNCATION.

  IF sy-subrc NE 0.
    RETURN.
  ENDIF.

  IF lt_statuses[] IS INITIAL.
    RETURN.
  ENDIF.

  IF line_exists( lt_statuses[ code = id_status_name ] ).
    rf_exists = abap_true.
  ENDIF.

ENDMETHOD.                    "STATUS_EXISTS


METHOD zif_bc_alv_report_view~add_commands_to_toolbar.
*--------------------------------------------------------------------*
* Listing 10.26: - Adding Commands Programatically
*--------------------------------------------------------------------*
  TRY.
      LOOP AT it_commands ASSIGNING FIELD-SYMBOL(<command_info>).
        CHECK <command_info>-function <> '&IC1'.
        mo_functions->add_function(
        name     = <command_info>-function
        icon     = CONV #( <command_info>-icon )
        text     = CONV #( <command_info>-text )
        tooltip  = CONV #( <command_info>-quickinfo )
        position = if_salv_c_function_position=>right_of_salv_functions ).
      ENDLOOP.

      "These two standard SAP exception classes are identical as far as I can see
    CATCH cx_salv_wrong_call INTO DATA(wrong_call).
      DATA(error_message) = wrong_call->get_text( ).
      "Object: &OBJECT&; name: &KEY& already exists (class: &CLASS&; method: &METHOD&)
      MESSAGE error_message TYPE 'E'.
    CATCH cx_salv_existing INTO DATA(duplicate_error).
      error_message = duplicate_error->get_text( ).
      "Object: &OBJECT&; name: &KEY& already exists (class: &CLASS&; method: &METHOD&)
      MESSAGE error_message TYPE 'E'.
  ENDTRY.

ENDMETHOD.                    "add_commands_to_toolbar


METHOD zif_bc_alv_report_view~add_sort_criteria.
*--------------------------------------------------------------------*
* Listing 10.18: - Custom SALV Class - Adding a Sort Criteria
*--------------------------------------------------------------------*

    IF if_descending = abap_true.
      DATA(sort_sequence) = if_salv_c_sort=>sort_down.
    ELSE.
      sort_sequence = if_salv_c_sort=>sort_up.
    ENDIF.

    TRY.
        mo_sorts = mo_alv_grid->get_sorts( ).

        mo_sorts->add_sort( columnname = id_columnname
                            position   = id_position
                            sequence   = sort_sequence
                            subtotal   = if_subtotal
                            group      = id_group
                            obligatory = if_obligatory ).

     CATCH cx_salv_not_found INTO DATA(not_found).
        DATA(an_error_occurred) = abap_true.
        "Object = Column
        "Key    = Field Name e.g. VBELN
        zcl_dbc=>require( that             = |{ not_found->object } { not_found->key } { 'must exist'(004) }|
                          which_is_true_if = xsdbool( an_error_occurred = abap_false ) ).
      CATCH cx_salv_data_error INTO DATA(salv_data_error).
        DATA(error_information) = salv_data_error->get_message( ).
        MESSAGE ID error_information-msgid TYPE 'E' NUMBER error_information-msgno
                WITH error_information-msgv1 error_information-msgv2
                     error_information-msgv3 error_information-msgv4.
      CATCH cx_salv_existing INTO DATA(duplicate_error).
        DATA(error_message) = duplicate_error->get_text( ).
        "Object: &OBJECT&; name: &KEY& already exists (class: &CLASS&; method: &METHOD&)
        MESSAGE error_message TYPE 'E'.
    ENDTRY.

  ENDMETHOD.                    "zif_bc_alv_report_view~add_sort_criteria


METHOD zif_bc_alv_report_view~application_specific_changes.
*--------------------------------------------------------------------*
* Listing 10.08: - Application-Specific Changes Method
*--------------------------------------------------------------------*
* The job of the model is to say what fields can be drilled into
* and what alternative names they have etc...
* Just like an application specific metadata CDS Extension
* The job of the view is to realise this technically
* Since this is CL_SALV_TABLE we cannot make fields editable here
* but we can do all the other adjustments needed
*--------------------------------------------------------------------*
  TRY.
      "Layout Related Settings
      set_list_header( is_layout-list_header ).
      set_layout( is_layout-variant ).
      IF is_layout-colwidth_optimize = abap_true.
        optimise_column_width( ).
      ENDIF.
       IF is_layout-striped_pattern = abap_true.
        set_striped_pattern( ).
      ENDIF.
      IF is_layout-no_cell_merging = abap_true.
        set_no_merging( ).
      ENDIF.
      "Technical Fields
      LOOP AT it_technicals ASSIGNING FIELD-SYMBOL(<field_name>).
        set_column_attributes( id_field_name   = <field_name>
                               if_is_technical = abap_true ).
      ENDLOOP.
      "Hidden Fields
      LOOP AT it_hidden ASSIGNING <field_name>.
        set_column_attributes( id_field_name = <field_name>
                               if_is_visible = abap_false ).
      ENDLOOP.
      "Hotspots
      LOOP AT it_hotspots ASSIGNING <field_name>.
        set_column_attributes( id_field_name = <field_name>
                               if_is_hotspot = abap_true ).
      ENDLOOP.
      "Renamed Fields / Tooltips
      LOOP AT it_field_texts ASSIGNING FIELD-SYMBOL(<alv_text_fields>).
        IF <alv_text_fields>-tooltip IS NOT INITIAL.
          set_column_attributes( id_field_name = <alv_text_fields>-field_name
                                 id_tooltip    = <alv_text_fields>-tooltip ).
        ENDIF.
        IF <alv_text_fields>-long_text IS NOT INITIAL.
          set_column_attributes( id_field_name = <alv_text_fields>-field_name
                                 id_long_text  = <alv_text_fields>-long_text ).
        ENDIF.
        IF <alv_text_fields>-medium_text IS NOT INITIAL.
          set_column_attributes( id_field_name  = <alv_text_fields>-field_name
                                 id_medium_text = <alv_text_fields>-medium_text ).
        ENDIF.
        IF <alv_text_fields>-short_text IS NOT INITIAL.
          set_column_attributes( id_field_name = <alv_text_fields>-field_name
                                 id_short_text = <alv_text_fields>-short_text ).
        ENDIF.
      ENDLOOP.
      "Checkboxes
      LOOP AT it_checkboxes ASSIGNING <field_name>.
        set_checkbox( <field_name> ).
      ENDLOOP.
      "Subtotals
      LOOP AT it_subtotals ASSIGNING <field_name>.
        set_column_attributes( id_field_name  = <field_name>
                               if_is_subtotal = abap_true ).
      ENDLOOP.
      "Sort Criteria
      LOOP AT it_sort_criteria ASSIGNING FIELD-SYMBOL(<sort_criteria>).
        add_sort_criteria(
            id_columnname = <sort_criteria>-columnname
            id_position   = CONV #( <sort_criteria>-position )
            if_descending = <sort_criteria>-descending
            if_subtotal   = <sort_criteria>-subtotal
            id_group      = <sort_criteria>-group
            if_obligatory = <sort_criteria>-obligatory ).
      ENDLOOP.

    CATCH cx_salv_not_found INTO DATA(not_found_exception).
      DATA(an_error_occurred) = abap_true.
      "Object = Column
      "Key    = Field Name e.g. VBELN
      zcl_dbc=>require(
      that             = |{ not_found_exception->object } { not_found_exception->key } { 'must exist'(004) }|
      which_is_true_if = xsdbool( an_error_occurred = abap_false ) ).
    CATCH cx_salv_data_error INTO DATA(data_error_exception).
      DATA(error_information) = data_error_exception->get_message( ).
      MESSAGE ID error_information-msgid TYPE 'E' NUMBER error_information-msgno
              WITH error_information-msgv1 error_information-msgv2
                   error_information-msgv3 error_information-msgv4.
    CATCH cx_salv_msg INTO DATA(generic_salv_exception).
      error_information = generic_salv_exception->get_message( ).
      MESSAGE ID error_information-msgid TYPE 'E' NUMBER error_information-msgno
              WITH error_information-msgv1 error_information-msgv2
                   error_information-msgv3 error_information-msgv4.
  ENDTRY.

ENDMETHOD."APPLICATION_SPECIFIC_CHANGES of ZCL_BC_VIEW_SALV_TABLE


METHOD zif_bc_alv_report_view~create_container_prep_display.
*--------------------------------------------------------------------*
* Listing 10.23: - Creating a Container Automatically
*--------------------------------------------------------------------*
* The below function creates a screen and a container, and then does
* a callback to method FILL CONTAINER CONTENT of interface
* IF_SALV_CSQT_CONTENT_MANAGER so that the calling class must
* implement method FILL_CONTAINER_CONTENT
* This way for CL_SALV_TABLE we can add our own functions without having
* to create a PF-STATUS
*--------------------------------------------------------------------*
  md_report_name        = id_report_name.
  md_edit_control_field = id_edit_control_field.
  mf_start_in_edit_mode = if_start_in_edit_mode.
  ms_variant-report     = id_report_name.
  ms_layout             = is_layout.
  mt_editable_fields[]  = it_editable_fields[].
  mt_technicals[]       = it_technicals[].
  mt_hidden[]           = it_hidden[].
  mt_hotspots[]         = it_hotspots[].
  mt_checkboxes[]       = it_checkboxes[].
  mt_subtotal_fields[]  = it_subtotal_fields[].
  mt_field_texts[]      = it_field_texts[].
  mt_sort_criteria[]    = it_sort_criteria[].
  mt_user_commands[]    = it_user_commands[].

  CREATE DATA mt_data_table LIKE ct_data_table.
  mt_data_table = REF #( ct_data_table ).

  CALL FUNCTION 'ZSALV_CSQT_CREATE_CONTAINER'
    EXPORTING
      r_content_manager = me
      title             = ms_layout-list_header.

ENDMETHOD.


METHOD zif_bc_alv_report_view~display.
*--------------------------------------------------------------------*
* Listing 10.19: - DISPLAY Method
*--------------------------------------------------------------------*
    mo_alv_grid->display( ).

* Gary is a BLUE Monster
  ENDMETHOD.                    "zif_bc_alv_report_view~display


METHOD zif_bc_alv_report_view~display_basic_toolbar.
*--------------------------------------------------------------------*
* LIsting 10.04: - Making Sure a Toolbar Appears at the Top of the Report
*--------------------------------------------------------------------*
  mo_functions = mo_alv_grid->get_functions( ).
  mo_functions->set_all( if_salv_c_bool_sap=>true ).

ENDMETHOD.                    "display_basic_toolbar


METHOD zif_bc_alv_report_view~get_alv_grid_object.
*----------------------------------------------------------------------*
* Listing 10.37: - GET_ALV_GRID_OBJECT Method of ZCL_BC_VIEW_SALV_TABLE
*----------------------------------------------------------------------*
  "We have a container, so
  "then the grid needs to be retrieved via the adapter
  ro_alv_grid = get_alv_grid( ).

ENDMETHOD.


METHOD zif_bc_alv_report_view~get_main_alv_object.

  ro_main_alv_instance = mo_alv_grid.

ENDMETHOD.


METHOD zif_bc_alv_report_view~initialise.
*--------------------------------------------------------------------*
* Listing 10.07: - INITIALIZE Method
*--------------------------------------------------------------------*
* Local Variables
  DATA: ld_status_name TYPE sypfkey VALUE 'ALV_STATUS'.

  IF mo_alv_grid IS BOUND.
    "Nothing needs doing
    RETURN.
  ENDIF.

  TRY.
*--------------------------------------------------------------------*
* Listing 10.03: - Creating the SALV Object without a Container and
* Listing 10.25: - Creating the SALV Object with a Container
*--------------------------------------------------------------------*
* If we have a container, then we can add our own user defined
* commands programtaically
*--------------------------------------------------------------------*
      IF io_container IS SUPPLIED AND
         io_container IS BOUND.
        cl_salv_table=>factory(
          EXPORTING
            r_container  = io_container
          IMPORTING
            r_salv_table = mo_alv_grid
          CHANGING
            t_table      = ct_data_table[] ).

        display_basic_toolbar( ).
        IF it_user_commands[] IS NOT INITIAL.
          add_commands_to_toolbar( it_user_commands ).
        ENDIF.
*--------------------------------------------------------------------*
* If we do not have a container, then we have to set up a self
* defined PF-STATUS with our extra commands
*--------------------------------------------------------------------*
      ELSEIF ct_data_table IS SUPPLIED.
        cl_salv_table=>factory(
          IMPORTING
            r_salv_table = mo_alv_grid
          CHANGING
            t_table      = ct_data_table[] ).

        display_basic_toolbar( ).
        "You get a dump adding commands to a TREE or a GRID
        "add_commands_to_toolbar( it_user_commands ).
        "so we add our own STATUS. That status has to exist though!
        IF status_exists( id_calling_program = id_report_name
                          id_status_name     = ld_status_name ) = abap_true.
          mo_alv_grid->set_screen_status(
              report        = id_report_name
              pfstatus      = ld_status_name
              set_functions = mo_alv_grid->c_functions_all ).
        ENDIF."does the status with extra buttons exist?
      ENDIF."Do we have a container?

      mo_columns = mo_alv_grid->get_columns( ).
      set_layout( id_variant ).
      set_handlers( ).

*--------------------------------------------------------------------*
* Listing 10.30: - Changing the INITIALIZE Method to store the
*                  ALV Grid Object
*--------------------------------------------------------------------*
      DATA: model_that_can_get_grid TYPE REF TO cl_salv_model.

      "Narrow casting
      "CL_SALV_MODEL is a superclass of CL_SALV_TABLE
      "Target = MODEL_THAT_CAN_GET_GRID = CL_SALV_MODEL
      "Source = MO_ALV_GRID             = CL_SALV_TABLE
      model_that_can_get_grid = mo_alv_grid.

      "Object to access underlying CL_GUI_ALV_GRID
      mo_salv_model = NEW zcl_salv_model( model_that_can_get_grid ).

      IF if_start_in_edit_mode = abap_true.
        "Prepare the Field Catalogue to be Editable
        mo_salv_model->set_editable(
        io_salv               = mo_alv_grid
        id_edit_control_field = id_edit_control_field
        it_editable_fields    = it_editable_fields ).
        "Prepare the Data Table to be Editable
        make_column_editable(
        EXPORTING id_edit_control_field = id_edit_control_field
                  it_editable_fields    = it_editable_fields
        CHANGING  ct_data_table         = ct_data_table ).
      ENDIF."Do we want to start in Editable Mode?

    CATCH cx_salv_msg.
      MESSAGE 'Report in Trouble'(003) TYPE 'E'.
  ENDTRY.

ENDMETHOD.                    "zif_bc_alv_report_view~initialise


METHOD zif_bc_alv_report_view~make_column_editable."of ZCL_BC_VIEW_SALV_TABLE
*--------------------------------------------------------------------*
* Listing 10.32: Preparing the data table to be editable
*--------------------------------------------------------------------*
* Local Variables
  DATA :cell_style_info      TYPE lvc_s_styl,
        table_line_reference TYPE REF TO data.

  FIELD-SYMBOLS: <data_table_row>   TYPE any,
                 <cell_style_table> TYPE lvc_t_styl.

  DATA(lo_control_field_data) = cl_abap_typedescr=>describe_by_data( id_edit_control_field ).

  zcl_dbc=>require( that             = 'The control field is of type LVC_S_STYL'(001)
                    which_is_true_if = xsdbool( lo_control_field_data->absolute_name = '\TYPE=LVC_FNAME' ) ).

  "Dynamically create work area for looping through the table
  "that was passed in
  CREATE DATA table_line_reference LIKE LINE OF ct_data_table.

  ASSIGN table_line_reference->*  TO <data_table_row>.

  LOOP AT ct_data_table ASSIGNING <data_table_row>.

    ASSIGN COMPONENT id_edit_control_field OF STRUCTURE <data_table_row> TO <cell_style_table>.

    IF sy-subrc <> 0.
      "Fatal Bug in the Program
      zcl_dbc=>require( that             = 'The edit control field is not in the ALV table'(002)
                        which_is_true_if = abap_false ).
    ENDIF.

    LOOP AT it_editable_fields ASSIGNING FIELD-SYMBOL(<editable_field>)."#EC CI_NESTED

      READ TABLE <cell_style_table> ASSIGNING FIELD-SYMBOL(<cell_style_info>)
      WITH KEY fieldname = <editable_field>.

      IF sy-subrc <> 0.
        DATA(insertion_position)  = sy-tabix.
        cell_style_info-fieldname = <editable_field>.
        INSERT cell_style_info INTO <cell_style_table> INDEX insertion_position.
        READ TABLE <cell_style_table> ASSIGNING <cell_style_info>
        WITH KEY fieldname = <editable_field>.
        ASSERT sy-subrc EQ 0.
      ENDIF.

      "Toggle between editable/read-only
      IF <cell_style_info>-style EQ cl_gui_alv_grid=>mc_style_enabled.
        <cell_style_info>-style = cl_gui_alv_grid=>mc_style_disabled."Read Only
      ELSE.
        <cell_style_info>-style = cl_gui_alv_grid=>mc_style_enabled."Editable
      ENDIF.

    ENDLOOP."List of Editable Fields
  ENDLOOP."Lines of the Data Table

ENDMETHOD."MAKE_COLUMN_EDITABLE of ZCL_BC_VIEW_SALV_TABLE


METHOD ZIF_BC_ALV_REPORT_VIEW~OPTIMISE_COLUMN_WIDTH.

    mo_columns = mo_alv_grid->get_columns( ).
    mo_columns->set_optimize( if_salv_c_bool_sap=>true ).

  ENDMETHOD.                    "optimise_column_width


METHOD zif_bc_alv_report_view~optimise_column_widths.

    optimise_column_width( ).

  ENDMETHOD.                    "zif_bc_alv_report_view~optimise_column_widths


METHOD zif_bc_alv_report_view~prepare_display_data.
*--------------------------------------------------------------------*
* Listing 10.02: - Preparing and Displaying Data Method
*--------------------------------------------------------------------*
* Step One - Generic - Set up the Basic Report
  initialise(
    EXPORTING
      id_report_name        = id_report_name                   " Calling program
      id_variant            = is_layout-variant                " Layout
      if_start_in_edit_mode = if_start_in_edit_mode
      id_edit_control_field = id_edit_control_field
      it_editable_fields    = it_editable_fields
      io_container          = io_container                     " Container for Custom Controls in the Screen Area
      it_user_commands      = it_user_commands                 " Toolbar Buttons
    CHANGING
      ct_data_table         = ct_data_table ).

* Step Two - Generic - Application Specific
  application_specific_changes(
    is_layout        = is_layout
    it_technicals    = it_technicals
    it_hidden        = it_hidden
    it_hotspots      = it_hotspots
    it_checkboxes    = it_checkboxes
    it_subtotals     = it_subtotal_fields
    it_field_texts   = it_field_texts
    it_sort_criteria = it_sort_criteria ).

* Step Three - Generic - Actually Display the Report
  display( ).

ENDMETHOD.                    "zif_bc_alv_report_view~prepare_display_data


METHOD zif_bc_alv_report_view~refresh_display.
*--------------------------------------------------------------------*
* Listing 10.22: - REFRESH_DISPLAY Method
*--------------------------------------------------------------------*
    "I am going to be a madman and suggest that when a user refreshes
    "the display because data has changed, they want the cursor to stay
    "where it is as opposed to jumping six pages up to the start of the
    "report
    mo_alv_grid->refresh( VALUE #(
    row = abap_true
    col = abap_true ) ).

ENDMETHOD.                    "zif_bc_alv_report_view~refresh_display


METHOD zif_bc_alv_report_view~set_checkbox.
*--------------------------------------------------------------------*
* Listing 10.10: - SET_CHECKBOX Method
*--------------------------------------------------------------------*
  TRY.
      mo_column ?= mo_columns->get_column( id_column_name ).

      mo_column->set_cell_type( if_salv_c_cell_type=>checkbox_hotspot ).

    CATCH cx_salv_not_found INTO DATA(not_found).
      DATA(an_error_occurred) = abap_true.
      "Object = Column
      "Key    = Field Name e.g. VBELN
      zcl_dbc=>require( that = |{ not_found->object } { not_found->key } { 'must exist'(004) } |
      which_is_true_if = xsdbool( an_error_occurred = abap_false ) ).
  ENDTRY.

* One Haunted Castle can be found at -7.8 degrees Longtitude (i.e. a negative value)
* Note that negative latitudes represent the southern hemisphere, and negative longitudes represent the
* western hemisphere
ENDMETHOD.                    "set_checkbox


METHOD zif_bc_alv_report_view~set_column_as_button.
*--------------------------------------------------------------------*
* Listing 10.14: - SET_COLUMN_AS_BUTTON Method
*--------------------------------------------------------------------*
  TRY.
      mo_column ?= mo_columns->get_column( id_field_name ).
      mo_column->set_icon( if_salv_c_bool_sap=>true ).

    CATCH cx_salv_not_found INTO DATA(not_found).
      DATA(an_error_occurred) = abap_true.
      zcl_dbc=>require( that = |{ not_found->object } { not_found->key } { 'must exist'(004) }|
      which_is_true_if = xsdbool( an_error_occurred = abap_false ) ).
  ENDTRY.

ENDMETHOD."set_column_as_button


METHOD zif_bc_alv_report_view~set_column_attributes.
*--------------------------------------------------------------------*
* Listing 10.09: - Custom ALV Class : SET_COLUMN_ATTRIBUTES Method
*--------------------------------------------------------------------*
* Preconditions
    IF id_field_name IS INITIAL.
      RETURN.
    ENDIF.

    IF if_is_a_checkbox = abap_true.
      set_checkbox( id_field_name ).
      set_hotspot( id_field_name ).
    ENDIF.

    IF if_is_hotspot = abap_true.
      set_hotspot( id_field_name ).
    ENDIF.

    IF if_is_visible IS SUPPLIED.
      set_visible( id_field_name = id_field_name
                   if_is_visible = if_is_visible ).
    ENDIF.

    IF if_is_technical = abap_true.
      set_technical( id_field_name ).
    ENDIF.

    IF if_is_a_button = abap_true.
      set_column_as_button( id_field_name ).
    ENDIF.

    IF if_is_subtotal = abap_true.
      set_subtotal( id_field_name ).
    ENDIF.

    IF id_long_text IS NOT INITIAL.
      set_long_text( id_field_name = id_field_name
                     id_long_text  = id_long_text ).
    ENDIF.

    IF id_medium_text IS NOT INITIAL.
      set_medium_text( id_field_name  = id_field_name
                       id_medium_text = id_medium_text ).
    ENDIF.

    IF id_short_text IS NOT INITIAL.
      set_short_text( id_field_name = id_field_name
                      id_short_text = id_short_text ).
    ENDIF.

    IF id_tooltip IS NOT INITIAL.
      set_tooltip( id_field_name = id_field_name
                   id_tooltip    = id_tooltip ).
    ENDIF.

  ENDMETHOD.                    "zif_bc_alv_report_view~set_column_attributes


METHOD zif_bc_alv_report_view~set_handlers.
*--------------------------------------------------------------------*
* Listing 10.06: - Custom ALV Object : SET_HANDLERS Method
*--------------------------------------------------------------------*
  mo_events = mo_alv_grid->get_event( ).

  SET HANDLER handle_link_click   FOR mo_events.

  SET HANDLER handle_user_command FOR mo_events.

ENDMETHOD.                    "set_handlers


METHOD zif_bc_alv_report_view~set_hotspot.
*--------------------------------------------------------------------*
* Listing 10.11: - SET_HOTSPOT Method
*--------------------------------------------------------------------*
  TRY.
      mo_column ?= mo_columns->get_column( id_column_name ).

      mo_column->set_cell_type( if_salv_c_cell_type=>hotspot ).

    CATCH cx_salv_not_found INTO DATA(not_found).
      DATA(an_error_occurred) = abap_true.
      "Object = Column
      "Key    = Field Name e.g. VBELN
      zcl_dbc=>require( that = |{ not_found->object } { not_found->key } { 'must exist'(004) }|
      which_is_true_if = xsdbool( an_error_occurred = abap_false ) ).
  ENDTRY.

ENDMETHOD."set_hotspot


METHOD ZIF_BC_ALV_REPORT_VIEW~SET_KEY.

    TRY.
        mo_column ?= mo_columns->get_column( id_column_name ).

        mo_column->set_key( abap_true ).

        mo_columns->set_key_fixation( abap_true ).

      CATCH cx_salv_not_found.
        MESSAGE s004(zmonsters)."Report in Trouble
    ENDTRY.

  ENDMETHOD.                    "set_key


METHOD zif_bc_alv_report_view~set_layout.
*--------------------------------------------------------------------*
* Listing 10.05: - Setting up the Layout
*--------------------------------------------------------------------*
* Local Variables
  DATA: layout_key_information TYPE salv_s_layout_key.

  mo_layout = mo_alv_grid->get_layout( ).

  layout_key_information-report = sy-cprog.

  mo_layout->set_key( layout_key_information ).

  mo_layout->set_default( 'X' ).

  IF id_variant IS NOT INITIAL.
    mo_layout->set_initial_layout( id_variant ).
  ENDIF.

  "Set save restriction
  "Check authority to change display variants.
  AUTHORITY-CHECK OBJECT 'Z_VARIANT1' ID 'ACTVT' FIELD '*' ##AUTH_OBJ_OK.

  IF sy-subrc = 0.   " does he ride a white horse?
    " yes, allow user and global display variants
    mo_layout->set_save_restriction( if_salv_c_layout=>restrict_none ).
  ELSE.
    mo_layout->set_save_restriction( if_salv_c_layout=>restrict_user_dependant ).
  ENDIF.

ENDMETHOD.                    "set_layout


METHOD zif_bc_alv_report_view~set_list_header.

    mo_settings = mo_alv_grid->get_display_settings( ).
    mo_settings->set_list_header( id_title ).

  ENDMETHOD.                    "zif_bc_alv_report_view~set_list_header


METHOD zif_bc_alv_report_view~set_long_text.
*--------------------------------------------------------------------*
* Listing 10.16: - SET_LONG_TEXT Method
*--------------------------------------------------------------------*

  IF mo_column IS NOT BOUND.
    mo_columns = mo_alv_grid->get_columns( ).
  ENDIF.

  TRY.
      mo_column ?= mo_columns->get_column( id_field_name ).
      mo_column->set_long_text( id_long_text ).

      IF strlen( id_long_text ) LE 20 ##NUMBER_OK.
        mo_column->set_medium_text( CONV #( id_long_text ) ).
      ENDIF.

      IF strlen( id_long_text ) LE 10 ##NUMBER_OK.
        mo_column->set_short_text( CONV #( id_long_text ) ).
      ENDIF.

    CATCH cx_salv_not_found INTO DATA(not_found).
      DATA(an_error_occurred) = abap_true.
      zcl_dbc=>require( that             = |{ not_found->object } { not_found->key } { 'must exist'(004) }|
                        which_is_true_if = xsdbool( an_error_occurred = abap_false ) ).
  ENDTRY.

ENDMETHOD.                    "set_long_text


METHOD ZIF_BC_ALV_REPORT_VIEW~SET_MEDIUM_TEXT.
* Local Variables
    DATA: ld_short_text TYPE scrtext_s.

    TRY.
        mo_column ?= mo_columns->get_column( id_field_name ).
        mo_column->set_medium_text( id_medium_text ).

        IF strlen( id_medium_text ) LE 10.
          ld_short_text = id_medium_text.
          mo_column->set_short_text( ld_short_text ).
        ENDIF.

      CATCH cx_salv_not_found INTO DATA(not_found).
        DATA(lf_error_occurred) = abap_true.
        "Object = Column
        "Key    = Field Name e.g. VBELN
        zcl_dbc=>require( that             = |{ not_found->object } { not_found->key } { 'must exist'(004) }|
                          which_is_true_if = xsdbool( lf_error_occurred = abap_false ) ).
    ENDTRY.

  ENDMETHOD.                    "SET_MEDIUM_TEXT


METHOD ZIF_BC_ALV_REPORT_VIEW~SET_NO_MERGING.
*--------------------------------------------------------------------*
* The default behaviour for any ALV grid is to merge cells which
* have the same value with the cells immediately below them, e.g.
* if every cell in a column had the same value, you would just see
* one great big, very tall, cell
* Sometimes you want to switch that setting off, so every value lives
* inside it's own cell
*--------------------------------------------------------------------*
  mo_settings = mo_alv_grid->get_display_settings( ).

  mo_settings->set_no_merging( abap_true ).

ENDMETHOD.


METHOD ZIF_BC_ALV_REPORT_VIEW~SET_SELECTIONS.
*--------------------------------------------------------------------*
* SINGLE      Individual selection      1
* MULTIPLE    Mult. Selection           2
* CELL        Cell Selection            3
* ROW_COLUMN  Line and Column Selection 4
* NONE        No Selection              0
*--------------------------------------------------------------------*
    mo_selections = mo_alv_grid->get_selections( ).
    mo_selections->set_selection_mode( cl_salv_selections=>single ).

  ENDMETHOD.                    "SET_SELECTIONS


METHOD ZIF_BC_ALV_REPORT_VIEW~SET_SHORT_TEXT.

    TRY.
        mo_column ?= mo_columns->get_column( id_field_name ).
        mo_column->set_short_text( id_short_text ).

      CATCH cx_salv_not_found INTO DATA(not_found).
        DATA(lf_error_occurred) = abap_true.
        "Object = Column
        "Key    = Field Name e.g. VBELN
        zcl_dbc=>require( that             = |{ not_found->object } { not_found->key } { 'must exist'(004) }|
                          which_is_true_if = xsdbool( lf_error_occurred = abap_false ) ).
    ENDTRY.

  ENDMETHOD.                    "SET_SHORT_TEXT


  METHOD ZIF_BC_ALV_REPORT_VIEW~SET_STRIPED_PATTERN.

    IF mo_settings IS NOT BOUND.
      mo_settings = mo_alv_grid->get_display_settings( ).
    ENDIF.

    mo_settings->set_striped_pattern( abap_true ).

  ENDMETHOD.


METHOD zif_bc_alv_report_view~set_subtotal.
*--------------------------------------------------------------------*
* Listing 10.15: - SET_SUBTOTAL Method
*--------------------------------------------------------------------*
  mo_aggregations = mo_alv_grid->get_aggregations( ).

  TRY.
      mo_aggregations->add_aggregation( id_field_name ).

    CATCH cx_salv_not_found INTO DATA(not_found).
      DATA(an_error_occurred) = abap_true.
      zcl_dbc=>require( that = |{ not_found->object } { not_found->key } { 'must exist'(004) }|
      which_is_true_if = xsdbool( an_error_occurred = abap_false ) ).

    CATCH cx_salv_data_error INTO DATA(salv_data_error).
      DATA(error_information) = salv_data_error->get_message( ).
      MESSAGE ID error_information-msgid TYPE 'E' NUMBER error_information-msgno
      WITH error_information-msgv1 error_information-msgv2
           error_information-msgv3 error_information-msgv4.

    CATCH cx_salv_existing INTO DATA(duplicate_error).
      DATA(error_message) = duplicate_error->get_text( ).
      "Object: &OBJECT&; name: &KEY& already exists (class: &CLASS&; method: &METHOD&)
      MESSAGE error_message TYPE 'E'.
  ENDTRY.

ENDMETHOD."SET_SUBTOTAL


METHOD zif_bc_alv_report_view~set_technical.
*--------------------------------------------------------------------*
* Listing 10.13: - SET_TECHNICAL Method
*--------------------------------------------------------------------*
  TRY.
      mo_column ?= mo_columns->get_column( id_field_name ).
      mo_column->set_technical( abap_true ).

    CATCH cx_salv_not_found INTO DATA(not_found).
      DATA(an_error_occurred) = abap_true.
      zcl_dbc=>require( that = |{ not_found->object } { not_found->key } { 'must exist'(004) }|
      which_is_true_if = xsdbool( an_error_occurred = abap_false ) ).
  ENDTRY.

ENDMETHOD."SET_TECHNICAL


METHOD zif_bc_alv_report_view~set_tooltip.
*--------------------------------------------------------------------*
* Listing 10.17: - SET_TOOLTIP Method
*--------------------------------------------------------------------*

  TRY.
      mo_column ?= mo_columns->get_column( id_field_name ).
      mo_column->set_tooltip( id_tooltip ).

    CATCH cx_salv_not_found INTO DATA(not_found).
      DATA(an_error_occurred) = abap_true.
      "Object = Column
      "Key    = Field Name e.g. VBELN
      zcl_dbc=>require( that             = |{ not_found->object } { not_found->key } { 'must exist'(004) }|
                        which_is_true_if = xsdbool( an_error_occurred = abap_false ) ).
  ENDTRY.

ENDMETHOD.                    "SET_TOOLTIP


METHOD zif_bc_alv_report_view~set_visible.
*--------------------------------------------------------------------*
* Listing 10.12: - SET_VISIBLE Method
*--------------------------------------------------------------------*
  TRY.
      mo_column ?= mo_columns->get_column( id_field_name ).
      mo_column->set_visible( if_is_visible ).

    CATCH cx_salv_not_found INTO DATA(not_found).
      DATA(an_error_occurred) = abap_true.
      "Object = Column
      "Key    = Field Name e.g. VBELN
      zcl_dbc=>require( that = |{ not_found->object } { not_found->key } { 'must exist'(004) }|
      which_is_true_if = xsdbool( an_error_occurred = abap_false ) ).
  ENDTRY.

ENDMETHOD."SET_VISIBLE


  METHOD zif_created_via_ocp_factory~is_the_right_class_type_given.

    READ TABLE it_context_data INTO DATA(context_data) WITH KEY name = 'UI_TECHNOLOGY'.

    IF sy-subrc NE 0.
      rf_yes_it_is = abap_false.
      RETURN.
    ENDIF.

    IF context_data-value NE mc_ctx_ui_technology.
      rf_yes_it_is = abap_false.
      RETURN.
    ENDIF.

    rf_yes_it_is = abap_true.

  ENDMETHOD.
ENDCLASS.
