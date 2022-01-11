class ZCL_BC_VIEW_GUI_ALV_GRID definition
  public
  create public .

public section.

  interfaces ZIF_BC_ALV_REPORT_VIEW .
  interfaces IF_SALV_CSQT_CONTENT_MANAGER .
  interfaces ZIF_CREATED_VIA_OCP_FACTORY .

  aliases DEFAULT_CLASS
    for ZIF_CREATED_VIA_OCP_FACTORY~DEFAULT_CLASS .
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
  aliases ADD_COMMANDS_TO_TOOLBAR
    for ZIF_BC_ALV_REPORT_VIEW~ADD_COMMANDS_TO_TOOLBAR .
  aliases ADD_SORT_CRITERIA
    for ZIF_BC_ALV_REPORT_VIEW~ADD_SORT_CRITERIA .
  aliases APPLICATION_SPECIFIC_CHANGES
    for ZIF_BC_ALV_REPORT_VIEW~APPLICATION_SPECIFIC_CHANGES .
  aliases CREATE_CONTAINER_PREP_DISPLAY
    for ZIF_BC_ALV_REPORT_VIEW~CREATE_CONTAINER_PREP_DISPLAY .
  aliases DISPLAY
    for ZIF_BC_ALV_REPORT_VIEW~DISPLAY .
  aliases DISPLAY_BASIC_TOOLBAR
    for ZIF_BC_ALV_REPORT_VIEW~DISPLAY_BASIC_TOOLBAR .
  aliases FILL_CONTAINER_CONTENT
    for IF_SALV_CSQT_CONTENT_MANAGER~FILL_CONTAINER_CONTENT .
  aliases GET_ALV_GRID_OBJECT
    for ZIF_BC_ALV_REPORT_VIEW~GET_ALV_GRID_OBJECT .
  aliases GET_MAIN_ALV_OBJECT
    for ZIF_BC_ALV_REPORT_VIEW~GET_MAIN_ALV_OBJECT .
  aliases INITIALISE
    for ZIF_BC_ALV_REPORT_VIEW~INITIALISE .
  aliases IS_THE_RIGHT_CLASS_TYPE_GIVEN
    for ZIF_CREATED_VIA_OCP_FACTORY~IS_THE_RIGHT_CLASS_TYPE_GIVEN .
  aliases MAKE_COLUMN_EDITABLE
    for ZIF_BC_ALV_REPORT_VIEW~MAKE_COLUMN_EDITABLE .
  aliases OPTIMISE_COLUMN_WIDTH
    for ZIF_BC_ALV_REPORT_VIEW~OPTIMISE_COLUMN_WIDTH .
  aliases OPTIMISE_COLUMN_WIDTHS
    for ZIF_BC_ALV_REPORT_VIEW~OPTIMISE_COLUMN_WIDTHS .
  aliases PREPARE_DISPLAY_DATA
    for ZIF_BC_ALV_REPORT_VIEW~PREPARE_DISPLAY_DATA .
  aliases REFRESH_DISPLAY
    for ZIF_BC_ALV_REPORT_VIEW~REFRESH_DISPLAY .
  aliases SET_CHECKBOX
    for ZIF_BC_ALV_REPORT_VIEW~SET_CHECKBOX .
  aliases SET_COLUMN_AS_BUTTON
    for ZIF_BC_ALV_REPORT_VIEW~SET_COLUMN_AS_BUTTON .
  aliases SET_COLUMN_ATTRIBUTES
    for ZIF_BC_ALV_REPORT_VIEW~SET_COLUMN_ATTRIBUTES .
  aliases SET_HANDLERS
    for ZIF_BC_ALV_REPORT_VIEW~SET_HANDLERS .
  aliases SET_HOTSPOT
    for ZIF_BC_ALV_REPORT_VIEW~SET_HOTSPOT .
  aliases SET_KEY
    for ZIF_BC_ALV_REPORT_VIEW~SET_KEY .
  aliases SET_LAYOUT
    for ZIF_BC_ALV_REPORT_VIEW~SET_LAYOUT .
  aliases SET_LIST_HEADER
    for ZIF_BC_ALV_REPORT_VIEW~SET_LIST_HEADER .
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

  constants MC_CTX_UI_TECHNOLOGY type SEOCLNAME value 'CL_GUI_ALV_GRID' ##NO_TEXT.
  data MT_DATA_TABLE type ref to DATA .
  data MD_REPORT_NAME type SY-REPID .
  data MS_VARIANT type DISVARIANT .
  data MT_USER_COMMANDS type TTB_BUTTON .
  data MO_ALV_GRID type ref to CL_GUI_ALV_GRID .
protected section.
private section.

  data MF_START_IN_EDIT_MODE type ABAP_BOOL .
ENDCLASS.



CLASS ZCL_BC_VIEW_GUI_ALV_GRID IMPLEMENTATION.


  METHOD IF_SALV_CSQT_CONTENT_MANAGER~FILL_CONTAINER_CONTENT.
*--------------------------------------------------------------------*
* The 20 Foot tall Monster has the Super-Power of X-Ray Vision
*--------------------------------------------------------------------*
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

  ENDMETHOD.


  method ZIF_BC_ALV_REPORT_VIEW~ADD_COMMANDS_TO_TOOLBAR.
* To Be Implemented
  endmethod.


  method ZIF_BC_ALV_REPORT_VIEW~ADD_SORT_CRITERIA.
* To Be Implemented
  endmethod.


  method ZIF_BC_ALV_REPORT_VIEW~APPLICATION_SPECIFIC_CHANGES.
* To Be Implemented
  endmethod.


  method ZIF_BC_ALV_REPORT_VIEW~CREATE_CONTAINER_PREP_DISPLAY.
* To Be Implemented
  endmethod.


  method ZIF_BC_ALV_REPORT_VIEW~DISPLAY.
* To Be Implemented
  endmethod.


  method ZIF_BC_ALV_REPORT_VIEW~DISPLAY_BASIC_TOOLBAR.
* To Be Implemented
  endmethod.


  method ZIF_BC_ALV_REPORT_VIEW~GET_ALV_GRID_OBJECT.
* To Be Implemented
  endmethod.


  method ZIF_BC_ALV_REPORT_VIEW~GET_MAIN_ALV_OBJECT.
* To Be Implemented
  endmethod.


  method ZIF_BC_ALV_REPORT_VIEW~INITIALISE.
* To Be Implemented
  endmethod.


  method ZIF_BC_ALV_REPORT_VIEW~MAKE_COLUMN_EDITABLE.
* To Be Implemented
  endmethod.


  method ZIF_BC_ALV_REPORT_VIEW~OPTIMISE_COLUMN_WIDTH.
* To Be Implemented
  endmethod.


  method ZIF_BC_ALV_REPORT_VIEW~OPTIMISE_COLUMN_WIDTHS.
* To Be Implemented
  endmethod.


  method ZIF_BC_ALV_REPORT_VIEW~PREPARE_DISPLAY_DATA.
* To Be Implemented
  endmethod.


  method ZIF_BC_ALV_REPORT_VIEW~REFRESH_DISPLAY.
* To Be Implemented
  endmethod.


  method ZIF_BC_ALV_REPORT_VIEW~SET_CHECKBOX.
* To Be Implemented
  endmethod.


  method ZIF_BC_ALV_REPORT_VIEW~SET_COLUMN_AS_BUTTON.
* To Be Implemented
  endmethod.


  method ZIF_BC_ALV_REPORT_VIEW~SET_COLUMN_ATTRIBUTES.
* To Be Implemented
  endmethod.


  method ZIF_BC_ALV_REPORT_VIEW~SET_HANDLERS.
* To Be Implemented
  endmethod.


  method ZIF_BC_ALV_REPORT_VIEW~SET_HOTSPOT.
* To Be Implemented
  endmethod.


  method ZIF_BC_ALV_REPORT_VIEW~SET_KEY.
* To Be Implemented
  endmethod.


  method ZIF_BC_ALV_REPORT_VIEW~SET_LAYOUT.
* To Be Implemented
  endmethod.


  method ZIF_BC_ALV_REPORT_VIEW~SET_LIST_HEADER.
* To Be Implemented
  endmethod.


  method ZIF_BC_ALV_REPORT_VIEW~SET_LONG_TEXT.
* To Be Implemented
  endmethod.


  method ZIF_BC_ALV_REPORT_VIEW~SET_MEDIUM_TEXT.
* To Be Implemented
  endmethod.


  method ZIF_BC_ALV_REPORT_VIEW~SET_NO_MERGING.
* To Be Implemented
  endmethod.


  method ZIF_BC_ALV_REPORT_VIEW~SET_SELECTIONS.
* To Be Implemented
  endmethod.


  method ZIF_BC_ALV_REPORT_VIEW~SET_SHORT_TEXT.
* To Be Implemented
  endmethod.


  method ZIF_BC_ALV_REPORT_VIEW~SET_STRIPED_PATTERN.
* To Be Implemented
  endmethod.


  method ZIF_BC_ALV_REPORT_VIEW~SET_SUBTOTAL.
* To Be Implemented
  endmethod.


  method ZIF_BC_ALV_REPORT_VIEW~SET_TECHNICAL.
* To Be Implemented
  endmethod.


  method ZIF_BC_ALV_REPORT_VIEW~SET_TOOLTIP.
* To Be Implemented
  endmethod.


  method ZIF_BC_ALV_REPORT_VIEW~SET_VISIBLE.
* To Be Implemented
  endmethod.


  METHOD ZIF_CREATED_VIA_OCP_FACTORY~IS_THE_RIGHT_CLASS_TYPE_GIVEN.
*-----------------------------------------------------------------------------------------*
* As this is exactly the same as the CL_SALV method it should be in an abstract base class
*-----------------------------------------------------------------------------------------*
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
