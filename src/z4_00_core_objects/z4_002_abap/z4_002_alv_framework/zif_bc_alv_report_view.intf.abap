interface ZIF_BC_ALV_REPORT_VIEW
  public .


  data MD_EDIT_CONTROL_FIELD type LVC_FNAME .
  data MS_LAYOUT type ZSBC_ALV_LAYOUT .
  data MT_EDITABLE_FIELDS type LVC_T_FNAM .
  data MT_TECHNICALS type LVC_T_FNAM .
  data MT_HIDDEN type LVC_T_FNAM .
  data MT_HOTSPOTS type LVC_T_FNAM .
  data MT_CHECKBOXES type LVC_T_FNAM .
  data MT_SUBTOTAL_FIELDS type LVC_T_FNAM .
  data MT_FIELD_TEXTS type ZTY_BC_ALV_TEXTS .
  data MT_SORT_CRITERIA type ZTY_BC_SORT_CRITERIA .

  events USER_COMMAND_RECEIVED
    exporting
      value(ED_USER_COMMAND) type SALV_DE_FUNCTION optional
      value(ED_ROW) type SALV_DE_ROW optional
      value(ED_COLUMN) type SALV_DE_COLUMN optional .

  methods ADD_COMMANDS_TO_TOOLBAR
    importing
      !IT_COMMANDS type TTB_BUTTON .
  methods CREATE_CONTAINER_PREP_DISPLAY
    importing
      !ID_REPORT_NAME type SY-REPID optional
      !IF_START_IN_EDIT_MODE type ABAP_BOOL optional
      !ID_EDIT_CONTROL_FIELD type LVC_FNAME optional
      !IS_LAYOUT type ZSBC_ALV_LAYOUT optional
      !IT_EDITABLE_FIELDS type LVC_T_FNAM optional
      !IT_TECHNICALS type LVC_T_FNAM optional
      !IT_HIDDEN type LVC_T_FNAM optional
      !IT_HOTSPOTS type LVC_T_FNAM optional
      !IT_CHECKBOXES type LVC_T_FNAM optional
      !IT_SUBTOTAL_FIELDS type LVC_T_FNAM optional
      !IT_FIELD_TEXTS type ZTY_BC_ALV_TEXTS optional
      !IT_SORT_CRITERIA type ZTY_BC_SORT_CRITERIA optional
      !IT_USER_COMMANDS type TTB_BUTTON optional
    changing
      !CT_DATA_TABLE type ANY TABLE .
  methods DISPLAY_BASIC_TOOLBAR .
  methods PREPARE_DISPLAY_DATA
    importing
      !ID_REPORT_NAME type SY-REPID optional
      !IF_START_IN_EDIT_MODE type ABAP_BOOL optional
      !ID_EDIT_CONTROL_FIELD type LVC_FNAME optional
      !IS_LAYOUT type ZSBC_ALV_LAYOUT optional
      !IT_EDITABLE_FIELDS type LVC_T_FNAM optional
      !IT_TECHNICALS type LVC_T_FNAM optional
      !IT_HIDDEN type LVC_T_FNAM optional
      !IT_HOTSPOTS type LVC_T_FNAM optional
      !IT_SUBTOTAL_FIELDS type LVC_T_FNAM optional
      !IT_CHECKBOXES type LVC_T_FNAM optional
      !IT_FIELD_TEXTS type ZTY_BC_ALV_TEXTS optional
      !IT_SORT_CRITERIA type ZTY_BC_SORT_CRITERIA optional
      !IO_CONTAINER type ref to CL_GUI_CONTAINER optional
      !IT_USER_COMMANDS type TTB_BUTTON optional
    changing
      !CT_DATA_TABLE type ANY TABLE optional .
  methods INITIALISE
    importing
      !ID_REPORT_NAME type SY-REPID optional
      !ID_VARIANT type DISVARIANT-VARIANT optional
      !IO_CONTAINER type ref to CL_GUI_CONTAINER optional
      !IT_USER_COMMANDS type TTB_BUTTON optional
      !IF_START_IN_EDIT_MODE type ABAP_BOOL optional
      !ID_EDIT_CONTROL_FIELD type LVC_FNAME optional
      !IT_EDITABLE_FIELDS type LVC_T_FNAM optional
    changing
      !CT_DATA_TABLE type ANY TABLE optional .
  methods APPLICATION_SPECIFIC_CHANGES
    importing
      !IS_LAYOUT type ZSBC_ALV_LAYOUT optional
      !IT_TECHNICALS type LVC_T_FNAM optional
      !IT_HIDDEN type LVC_T_FNAM optional
      !IT_HOTSPOTS type LVC_T_FNAM optional
      !IT_CHECKBOXES type LVC_T_FNAM optional
      !IT_SUBTOTALS type LVC_T_FNAM optional
      !IT_FIELD_TEXTS type ZTY_BC_ALV_TEXTS optional
      !IT_SORT_CRITERIA type ZTY_BC_SORT_CRITERIA optional .
  methods DISPLAY .
  methods OPTIMISE_COLUMN_WIDTH .
  methods SET_CHECKBOX
    importing
      !ID_COLUMN_NAME type LVC_FNAME .
  methods SET_COLUMN_ATTRIBUTES
    importing
      !ID_FIELD_NAME type LVC_FNAME
      !ID_TABLE_NAME type LVC_S_FCAT-REF_TABLE optional
      !IF_IS_HOTSPOT type ABAP_BOOL optional
      !IF_IS_VISIBLE type ABAP_BOOL optional
      !IF_IS_TECHNICAL type ABAP_BOOL optional
      !IF_IS_A_BUTTON type ABAP_BOOL optional
      !IF_IS_A_CHECKBOX type ABAP_BOOL optional
      !IF_IS_SUBTOTAL type ABAP_BOOL optional
      !ID_LONG_TEXT type SCRTEXT_L optional
      !ID_MEDIUM_TEXT type SCRTEXT_M optional
      !ID_SHORT_TEXT type SCRTEXT_S optional
      !ID_TOOLTIP type LVC_TIP optional .
  methods ADD_SORT_CRITERIA
    importing
      !ID_COLUMNNAME type LVC_FNAME
      !ID_POSITION type I optional
      !IF_DESCENDING type SAP_BOOL optional
      !IF_SUBTOTAL type SAP_BOOL default IF_SALV_C_BOOL_SAP=>FALSE
      !ID_GROUP type SALV_DE_SORT_GROUP default IF_SALV_C_SORT=>GROUP_NONE
      !IF_OBLIGATORY type SAP_BOOL default IF_SALV_C_BOOL_SAP=>FALSE .
  methods SET_COLUMN_AS_BUTTON
    importing
      !ID_FIELD_NAME type LVC_FNAME .
  methods OPTIMISE_COLUMN_WIDTHS .
  methods SET_HANDLERS .
  methods SET_HOTSPOT
    importing
      !ID_COLUMN_NAME type LVC_FNAME .
  methods SET_LIST_HEADER
    importing
      !ID_TITLE type LVC_TITLE .
  methods REFRESH_DISPLAY .
  methods SET_KEY
    importing
      !ID_COLUMN_NAME type LVC_FNAME .
  methods GET_MAIN_ALV_OBJECT
    returning
      value(RO_MAIN_ALV_INSTANCE) type ref to OBJECT .
  methods SET_LAYOUT
    importing
      !ID_VARIANT type DISVARIANT-VARIANT .
  methods GET_ALV_GRID_OBJECT
    returning
      value(RO_ALV_GRID) type ref to CL_GUI_ALV_GRID .
  methods SET_LONG_TEXT
    importing
      !ID_FIELD_NAME type LVC_FNAME
      !ID_LONG_TEXT type SCRTEXT_L .
  methods SET_MEDIUM_TEXT
    importing
      !ID_FIELD_NAME type LVC_FNAME
      !ID_MEDIUM_TEXT type SCRTEXT_M .
  methods SET_SELECTIONS
    importing
      !ID_SELECTION_TYPE type I default 0 .
  methods SET_SHORT_TEXT
    importing
      !ID_FIELD_NAME type LVC_FNAME
      !ID_SHORT_TEXT type SCRTEXT_S .
  methods SET_TECHNICAL
    importing
      !ID_FIELD_NAME type LVC_FNAME .
  methods SET_TOOLTIP
    importing
      !ID_FIELD_NAME type LVC_FNAME
      !ID_TOOLTIP type LVC_TIP .
  methods SET_VISIBLE
    importing
      !ID_FIELD_NAME type LVC_FNAME
      !IF_IS_VISIBLE type ABAP_BOOL .
  methods SET_SUBTOTAL
    importing
      !ID_FIELD_NAME type LVC_FNAME .
  methods SET_NO_MERGING .
  methods MAKE_COLUMN_EDITABLE
    importing
      !ID_EDIT_CONTROL_FIELD type LVC_FNAME
      !IT_EDITABLE_FIELDS type LVC_T_FNAM
    changing
      !CT_DATA_TABLE type ANY TABLE .
  methods SET_STRIPED_PATTERN .
endinterface.
