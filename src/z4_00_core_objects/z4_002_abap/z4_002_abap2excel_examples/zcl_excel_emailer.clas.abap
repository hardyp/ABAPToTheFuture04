class ZCL_EXCEL_EMAILER definition
  public
  create public .

public section.

  class-data MF_MACRO_ENABLED type ABAP_BOOL .
  constants MC_NORMAL type CHAR05 value '.XLSX' ##NO_TEXT.
  constants MC_MACRO_ENABLED type CHAR05 value '.XLSM' ##NO_TEXT.
  constants MC_FILTER_ROW type I value 4 ##NO_TEXT.
  constants MC_SUBTOTAL_ROW type I value 5 ##NO_TEXT.
  constants MC_HEADINGS_ROW type I value 6 ##NO_TEXT.
  constants MC_PRODUCTION type T000-CCCATEGORY value 'P' ##NO_TEXT.

  class-methods SEND
    importing
      !ID_FILE type XSTRING
      !IT_EMAIL_ADDRESSES type BCSY_SMTPA
      !ID_EMAIL_SUBJECT type SO_OBJ_DES
      !ID_ATTACHMENT_SUBJECT type SOOD-OBJDES
      !ID_EMAIL_BODY type BCSY_TEXT
      !IT_EMAIL_ADDRESS_OPTION type ZBC_T_EMAIL optional .
  class-methods SEND_ITAB
    importing
      !ID_TCODE type SY-TCODE
      !ID_REPORT_TITLE type CHAR80
      !ID_PROGRAM_NAME type SY-REPID
      !ID_EMAIL type AD_SMTPADR
      !IF_NO_AUTOFILTER type ABAP_BOOL optional
      !IT_RALD_FIELDCAT type SLIS_T_FIELDCAT_ALV optional
      !IT_RALD_SORTCAT type SLIS_T_SORTINFO_ALV optional
      !IS_RALD_LAYOUT type SLIS_LAYOUT_ALV optional
      !IT_EMAIL_BODY type BCSY_TEXT optional
    changing
      !CT_ITAB type STANDARD TABLE
      !CS_VARIANT type DISVARIANT .
  class-methods GET_EMAIL_ADDRESS
    importing
      !ID_USER_NAME type SY-UNAME default SY-UNAME
    returning
      value(RD_EMAIL_ADDRESS) type AD_SMTPADR .
  class-methods PREPARE_EMAIL_DETAILS
    importing
      !ID_TCODE type SY-TCODE
      !ID_REPORT_TITLE type CHAR80
      !ID_PROGRAM_NAME type SY-REPID
      !ID_ATTACHMENT_SUBJECT type SOOD-OBJDES optional
      !ID_EMAIL type AD_SMTPADR
      !IT_EMAIL_BODY type BCSY_TEXT optional
    exporting
      !ED_EMAIL_SUBJECT type SO_OBJ_DES
      !ED_ATTACHMENT_SUBJECT type SOOD-OBJDES
      !ET_EMAIL_BODY type BCSY_TEXT
      !ET_ADDRESSES type BCSY_SMTPA .
  class-methods CONVERT_TABLE_TO_SALV
    importing
      !ID_PROGRAM_NAME type SY-REPID
      !IT_RALD_FIELDCAT type SLIS_T_FIELDCAT_ALV optional
      !IT_RALD_SORTCAT type SLIS_T_SORTINFO_ALV optional
      !IS_RALD_LAYOUT type SLIS_LAYOUT_ALV optional
    exporting
      !EO_SALV type ref to CL_SALV_TABLE
    changing
      !CT_ITAB type STANDARD TABLE
      !CS_VARIANT type DISVARIANT optional .
  class-methods CONVERT_SALV_TO_EXCEL
    importing
      !ID_TITLE type ZEXCEL_SHEET_TITLE
      !IF_NO_AUTOFILTER type ABAP_BOOL default ABAP_FALSE
      !IF_NO_MACROS type ABAP_BOOL default ABAP_FALSE
      !IF_NO_FORMATTING type ABAP_BOOL default ABAP_FALSE
      !IF_AUTO_SIZE type ABAP_BOOL default ABAP_TRUE
      !ID_HEADINGS_ROW type I default 6
      !ID_START_COLUMN type I default 6
      !IO_SALV type ref to CL_SALV_TABLE optional
      !IO_ALV_GRID type ref to CL_GUI_ALV_GRID optional
      !IO_WORKSHEET type ref to ZCL_EXCEL_WORKSHEET optional
      !IT_TABLE type STANDARD TABLE
      !I_TABLE type ABAP_BOOL default ABAP_TRUE
    exporting
      !EO_CONVERTER type ref to ZCL_EXCEL_CONVERTER
    changing
      !CO_EXCEL type ref to ZCL_EXCEL .
  class-methods CONVERT_TABLE_TO_EXCEL
    importing
      !ID_PROGRAM_NAME type SY-REPID
      !ID_TITLE type ZEXCEL_SHEET_TITLE optional
      !IF_NO_AUTOFILTER type ABAP_BOOL optional
      !IF_NO_MACROS type ABAP_BOOL optional
      !IF_NO_FORMATTING type ABAP_BOOL optional
      !IT_RALD_FIELDCAT type SLIS_T_FIELDCAT_ALV optional
      !IT_RALD_SORTCAT type SLIS_T_SORTINFO_ALV optional
      !IS_RALD_LAYOUT type SLIS_LAYOUT_ALV optional
      !ID_START_COLUMN type I default 6
      !ID_HEADINGS_ROW type I default 6
    exporting
      !EO_CONVERTER type ref to ZCL_EXCEL_CONVERTER
      !EO_EXCEL type ref to ZCL_EXCEL
    changing
      !CT_ITAB type STANDARD TABLE
      !CS_VARIANT type DISVARIANT .
  class-methods SEND_EXCEL_OBJECT
    importing
      !IO_EXCEL type ref to ZCL_EXCEL optional
      !IO_CONVERTER type ref to ZCL_EXCEL_CONVERTER optional
      !IT_EMAIL_BODY type BCSY_TEXT optional
      !ID_TCODE type SY-TCODE
      !ID_REPORT_TITLE type CHAR80
      !ID_PROGRAM_NAME type SY-REPID
      !ID_EMAIL type AD_SMTPADR
      !ID_ATTACHMENT_SUBJECT type SOOD-OBJDES optional
      !IT_EMAIL type ZBC_T_EMAIL optional .
  class-methods FORMAT_WORKSHEET
    importing
      !ID_REPORT_TITLE type ZEXCEL_TITLE optional
      !ID_TAB_TITLE type ZEXCEL_SHEET_TITLE
      !IF_AUTO_SIZE type ABAP_BOOL default ABAP_TRUE
    changing
      !CO_EXCEL type ref to ZCL_EXCEL
      !CO_WORKSHEET type ref to ZCL_EXCEL_WORKSHEET .
  class-methods GET_MACRO_TEMPLATE
    returning
      value(RO_EXCEL) type ref to ZCL_EXCEL .
  class-methods GET_NEXT_WORKSHEET
    importing
      !ID_TARGET_SHEET type ZEXCEL_ACTIVE_WORKSHEET
    exporting
      !EO_WORKSHEET type ref to ZCL_EXCEL_WORKSHEET
    changing
      !CO_EXCEL type ref to ZCL_EXCEL .
  class-methods BUILD_HYPERLINK_URL
    importing
      !ID_TRANSACTION type SY-TCODE
      !ID_PARAMETERS type STRING
      !ID_OK_CODE type STRING
      !ID_USER_NAME type SY-UNAME default SY-UNAME
    returning
      value(RS_URL) type STRING .
  class-methods FIND_COLUMN
    importing
      !IO_WORKSHEET type ref to ZCL_EXCEL_WORKSHEET
      !ID_COLUMN_HEADER type STRING
    returning
      value(RD_COLUMN_ALPHA) type ZEXCEL_CELL_COLUMN_ALPHA .
  class-methods ADD_DRILL_DOWN
    importing
      !ID_TCODE type SY-TCODE
      !ID_VALUE type ANY
      !ID_USER_NAME type SY-UNAME
      !ID_COLUMN type ZEXCEL_CELL_COLUMN_ALPHA
      !ID_ROW type ZEXCEL_CELL_ROW
    changing
      !CO_WORKSHEET type ref to ZCL_EXCEL_WORKSHEET .
protected section.
private section.

  class-methods GET_EMAIL_VIA_USER
    importing
      !ID_USER_NAME type SY-UNAME default SY-UNAME
    returning
      value(RD_EMAIL_ADDRESS) type AD_SMTPADR ##RELAX.
  class-methods IS_PRODUCTION
    returning
      value(RESULT) type ABAP_BOOL .
ENDCLASS.



CLASS ZCL_EXCEL_EMAILER IMPLEMENTATION.


METHOD add_drill_down.
* Local Variables
  DATA:  ld_parameter    TYPE string,
         ld_ok_code      TYPE string,
         lo_hyperlink    TYPE REF TO zcl_excel_hyperlink,
         ld_url          TYPE string.

  CASE id_tcode.
    WHEN 'VA03'.
      ld_ok_code   = 'ENTE'.
      ld_parameter = 'VBAK-VBELN=' && id_value && ';'.
    WHEN OTHERS.
      RETURN.
  ENDCASE.

  TRY.
      ld_url = zcl_excel_emailer=>build_hyperlink_url( id_transaction = id_tcode
                                                       id_parameters  = ld_parameter
                                                       id_ok_code     = ld_ok_code
                                                       id_user_name   = id_user_name ).

      lo_hyperlink = zcl_excel_hyperlink=>create_external_link( ld_url ).

      co_worksheet->set_cell( ip_column    = id_column
                              ip_row       = id_row
                              ip_value     = id_value
                              ip_hyperlink = lo_hyperlink ).

      co_worksheet->change_cell_style( ip_column         = id_column
                                       ip_row            = id_row
                                       ip_font_color_rgb = zcl_excel_style_color=>c_blue
                                       ip_font_underline = abap_true ).

    CATCH zcx_excel.
      RETURN.
  ENDTRY.

ENDMETHOD.


METHOD build_hyperlink_url.
*--------------------------------------------------------------------*
* Listing 11.33: - Building the Hyperlink URL
*--------------------------------------------------------------------*
* This builds up a URL to log onto an SICF service. The SICF service will
* redirect to a java system which will authenticate the user via the
* Kerebros logon ticket created when the user logged onto windows in
* th morning. After authentication the java stack will redirect back
* to the SICf service which will then log the user on without asking
* for their user name or password, just like normal SAP SSO
* If the java service is not there, the user is asked for their name
* and password, not the end of the world
*--------------------------------------------------------------------*
* Local Variables
  DATA: ld_logical_system  TYPE t000-logsys,
        ld_rfc_destination TYPE rfcdest,
        ld_server_name     TYPE rfchost_ext,
        ld_url             TYPE string.

* T000 - Fully Buffered
  SELECT SINGLE logsys
    FROM t000
    INTO ld_logical_system
    WHERE mandt = sy-mandt.

  IF sy-subrc NE 0.
    RETURN.
  ENDIF.

  ld_rfc_destination = ld_logical_system.

  CALL FUNCTION 'DEST_RFC_ABAP_READ'
    EXPORTING
      name         = ld_rfc_destination
    IMPORTING
      server_name  = ld_server_name
    EXCEPTIONS
      read_failure = 1
      OTHERS       = 2.

  IF sy-subrc <> 0.
    RETURN.
  ENDIF.

  rs_url =
  'https://' &&
  ld_server_name &&
  ':44300/sap/bc/gui/sap/its/zwebgui?' &&  "44300 is the standard port for HTTPS
  '~transaction=*' &&
  id_transaction ##NO_TEXT.

  ld_url =
  id_parameters &&" e.g. USR02-BNAME=hardyp;
  'DYNP_OKCODE=' &&
  id_ok_code     &&
  '&sap-client=' &&
  sy-mandt.

  CONCATENATE rs_url ld_url INTO rs_url SEPARATED BY space.

ENDMETHOD.


METHOD convert_salv_to_excel.
* Convert SALV object into excel
  DATA: lo_worksheet    TYPE REF TO zcl_excel_worksheet,
        ld_sheet_title  TYPE zexcel_sheet_title,
        ld_headings_row TYPE i,
        l_ws            TYPE c LENGTH 10 VALUE 'ITS'.

  IF id_headings_row GT 0.
    ld_headings_row = id_headings_row.
  ELSE.
    ld_headings_row = mc_headings_row.
  ENDIF.

  "If we do this (call the convertor) in the foreground we get a dump
  "unless we do a dirty trick
  IF sy-batch EQ abap_false.
    EXPORT l_ws = l_ws TO MEMORY ID 'WWW_ALV_ITS'.
  ENDIF.

  "A macro makes no sense unless there is formatting also
  IF co_excel         IS INITIAL    AND
     if_no_formatting EQ abap_false AND
     if_no_macros     EQ abap_false.
    co_excel = get_macro_template( ).
  ENDIF.

  eo_converter = NEW #( ).

  TRY.
      IF io_salv IS NOT INITIAL.
        eo_converter->convert(
          EXPORTING
            io_alv        = io_salv
            it_table      = it_table[]
            i_row_int     = ld_headings_row
            i_column_int  = id_start_column
            i_table       = i_table
            i_style_table = zcl_excel_table=>builtinstyle_medium2
            io_worksheet  = io_worksheet
          CHANGING
            co_excel      = co_excel ).
      ELSEIF io_alv_grid IS NOT INITIAL.
        eo_converter->convert(
          EXPORTING
           io_alv        = io_alv_grid
           it_table      = it_table[]
           i_row_int     = ld_headings_row
           i_column_int  = id_start_column
           i_table       = abap_true
           i_style_table = zcl_excel_table=>builtinstyle_medium2
           io_worksheet  = io_worksheet
         CHANGING
           co_excel      = co_excel ).
      ELSE.
        RETURN.
      ENDIF.

      FREE MEMORY ID 'WWW_ALV_ITS'.

      IF io_worksheet IS SUPPLIED AND io_worksheet IS BOUND.
        lo_worksheet = io_worksheet.
      ELSE.
        lo_worksheet = co_excel->get_active_worksheet( ).
      ENDIF.

      IF if_no_formatting = abap_false.

        format_worksheet( EXPORTING id_tab_title = id_title
                                    if_auto_size = if_auto_size
                          CHANGING  co_excel     = co_excel
                                    co_worksheet = lo_worksheet ).
      ELSE.
        "Even if no formatting, still set the worksheet title
        "Make sure worksheet not already processed
        ld_sheet_title = lo_worksheet->get_title( ).
        IF ld_sheet_title NS 'Sheet'(010).                  "i.e. default value
          RETURN.
        ENDIF.
        ld_sheet_title = id_title.
        REPLACE ALL OCCURRENCES OF ':' IN ld_sheet_title WITH ''.
        CONDENSE ld_sheet_title.
        lo_worksheet->set_title( ld_sheet_title ).
      ENDIF.

    CATCH zcx_excel INTO DATA(lo_exception).
      DATA(ld_message) = lo_exception->get_text( ).
      MESSAGE ld_message TYPE 'I'.
  ENDTRY.

ENDMETHOD.


METHOD convert_table_to_excel.
* Local Variables
  DATA: ld_title TYPE zexcel_sheet_title.

  CLEAR: eo_converter,
         eo_excel.

  TRY.
      "Create SALV object
      zcl_excel_emailer=>convert_table_to_salv(
        EXPORTING
          id_program_name  = id_program_name
          it_rald_fieldcat = it_rald_fieldcat
          it_rald_sortcat  = it_rald_sortcat
          is_rald_layout   = is_rald_layout
        IMPORTING
          eo_salv          = DATA(lo_salv)
        CHANGING
          ct_itab          = ct_itab[]
          cs_variant       = cs_variant ).

      "Convert SALV Object into an XML Excel File
      IF id_title IS SUPPLIED.
        ld_title = id_title.
      ELSE.
        ld_title = sy-title.
      ENDIF.
      zcl_excel_emailer=>convert_salv_to_excel(
        EXPORTING
          id_title         = ld_title
          if_no_autofilter = if_no_autofilter
          if_no_macros     = if_no_macros
          if_no_formatting = if_no_formatting
          id_start_column  = id_start_column
          id_headings_row  = id_headings_row
          io_salv          = lo_salv
          it_table         = ct_itab[]
        IMPORTING
          eo_converter     = eo_converter
        CHANGING
          co_excel         = eo_excel ).

    CATCH zcx_excel INTO DATA(lo_exception).
      DATA(ld_message) = lo_exception->get_text( ).
      MESSAGE ld_message TYPE 'I'.
  ENDTRY.

ENDMETHOD.


METHOD convert_table_to_salv.
* Local Variables
  DATA: ls_key           TYPE        salv_s_layout_key,
        ld_variant       TYPE        slis_vari,
        lo_layout        TYPE REF TO cl_salv_layout,
        lt_new_fieldcat  TYPE slis_t_fieldcat_alv,
        lt_new_sortcat   TYPE slis_t_sortinfo_alv,
        lt_sortorder_tab TYPE abap_sortorder_tab,
        ls_sortorder_tab LIKE LINE OF lt_sortorder_tab.

* Create SALV object
  TRY.
      cl_salv_table=>factory(
        IMPORTING
          r_salv_table = eo_salv
        CHANGING
          t_table      = ct_itab[] ).

    CATCH cx_salv_msg.
      RETURN.
  ENDTRY.

* Attach a variant to this object
  lo_layout = eo_salv->get_layout( ).

* Set the Layout Key
  ls_key-report = id_program_name.
  lo_layout->set_key( ls_key ).

* Set usage of default Layouts
  lo_layout->set_default( 'X' ).

  "Problem occurs when variant structure is partly full
  IF cs_variant-variant IS INITIAL.
    CLEAR cs_variant.
  ELSE.
    ld_variant = cs_variant-variant.
    lo_layout->set_initial_layout( ld_variant ).
  ENDIF.

  IF it_rald_fieldcat[] IS INITIAL.
    RETURN.
  ENDIF.

  CALL FUNCTION 'REUSE_ALV_VARIANT_SELECT'
    EXPORTING
      i_dialog            = abap_false
      i_user_specific     = abap_true
      it_default_fieldcat = it_rald_fieldcat[]
      i_layout            = is_rald_layout
    IMPORTING
      et_fieldcat         = lt_new_fieldcat
      et_sort             = lt_new_sortcat
    CHANGING
      cs_variant          = cs_variant
    EXCEPTIONS
      wrong_input         = 1
      fc_not_complete     = 2
      not_found           = 3
      program_error       = 4
      OTHERS              = 5.

  IF sy-subrc <> 0.
    lt_new_fieldcat[] = it_rald_fieldcat[].
    lt_new_sortcat[]  = it_rald_sortcat[].
  ENDIF.

  IF lt_new_fieldcat[] IS INITIAL.
    lt_new_fieldcat[] = it_rald_fieldcat[].
  ENDIF.

  IF lt_new_sortcat[] IS INITIAL.
    lt_new_sortcat[] = it_rald_sortcat[].
  ENDIF.

* Set Initial Layout
  zcl_excel_fieldcat_converter=>convert_fieldcat( EXPORTING it_fieldcat = lt_new_fieldcat[]
                                                  CHANGING  co_salv     = eo_salv ).

  zcl_excel_fieldcat_converter=>convert_sortcat( EXPORTING it_sortcat = lt_new_sortcat[]
                                                 CHANGING  co_salv    = eo_salv ).

* We need to be sure the internal table is sorted the same as the spreadsheet, otherwise
* when we insert hyperlinks they go into the wrong line
  LOOP AT lt_new_sortcat ASSIGNING FIELD-SYMBOL(<ls_new_sortcat>).
* DESCENDING  = Sort Direction: 'X' (Descend.) or '  ' (Ascending (Default))
    ls_sortorder_tab-name = <ls_new_sortcat>-fieldname.
    IF <ls_new_sortcat>-down = abap_true.
      ls_sortorder_tab-descending = abap_true.
    ELSE.
      ls_sortorder_tab-descending = abap_false.
    ENDIF.
    INSERT ls_sortorder_tab INTO TABLE lt_sortorder_tab.
  ENDLOOP.

  IF lt_sortorder_tab[] IS INITIAL.
    RETURN.
  ENDIF.

  "If there is only one sort field e.g. ship-to name, and all the ship-to names in
  "the internal table are the same, a sort which is not STABLE will scramble the
  "internal table
  TRY.
      SORT ct_itab STABLE BY (lt_sortorder_tab).
    CATCH cx_sy_dyn_table_ill_comp_val.
      RETURN.
  ENDTRY.

ENDMETHOD.


METHOD find_column.
* Local Variables
  DATA: ld_current_column TYPE zexcel_cell_column,
        ld_cell_value     TYPE zexcel_cell_value,
        ld_column_header  TYPE string.

  TRY.
      "Detect the right-most column which has data inside it
      DATA(ld_highest_column) = io_worksheet->get_highest_column( ).

      ld_current_column = 6.

      WHILE ld_current_column LE ld_highest_column.
        io_worksheet->get_cell( EXPORTING ip_column  = ld_current_column
                                          ip_row     = mc_headings_row
                                IMPORTING ep_value   = ld_cell_value ).

        ld_column_header = to_upper( id_column_header ).
        ld_cell_value    = to_upper( ld_cell_value ).
        IF ld_cell_value CS ld_column_header.
          rd_column_alpha = zcl_excel_common=>convert_column2alpha( ld_current_column ).
          RETURN.
        ENDIF.
        ld_current_column = ld_current_column + 1.
      ENDWHILE.

    CATCH zcx_excel INTO DATA(lo_exception).
      DATA(ld_message) = lo_exception->get_text( ).
      MESSAGE ld_message TYPE 'I'.
  ENDTRY.

ENDMETHOD.


METHOD format_worksheet.
* Local Variables
  DATA: ls_header           TYPE zexcel_s_worksheet_head_foot,
        ls_footer           TYPE zexcel_s_worksheet_head_foot,
        ld_string           TYPE string,
        ld_value            TYPE zexcel_cell_value,
        ld_formula          TYPE zexcel_cell_formula,
        ld_column_alpha     TYPE zexcel_cell_column_alpha,
        ld_column_int       TYPE zexcel_cell_column,
        ld_row_int          TYPE zexcel_cell_row,
        ld_columns          TYPE zexcel_cell_column,
        ld_rows             TYPE zexcel_cell_row,
        ld_sheet_title      TYPE zexcel_sheet_title,
        ld_cell_style       TYPE zexcel_cell_style,
        lo_style_mixed      TYPE REF TO zcl_excel_style,
        lo_style_bold       TYPE REF TO zcl_excel_style,
        ld_style_mixed_guid TYPE zexcel_cell_style,
        ld_style_bold_guid  TYPE zexcel_cell_style,
        ld_freeze_column    TYPE zexcel_cell_column,
        ld_left             TYPE string,
        ld_right            TYPE string,
        ls_stylemapping     TYPE zexcel_s_stylemapping,
        ld_table_number     TYPE zexcel_active_worksheet,
        lf_subtotals        TYPE abap_bool,
        ld_record_count     TYPE sy-tabix,
        ld_count_string     TYPE string.

  TRY.
      "Make sure worksheet not already processed
      ld_sheet_title = co_worksheet->get_title( ).

      IF ld_sheet_title NS 'Sheet'(010).                "i.e. default value
        "Sheet name has already been changed
        RETURN.
      ENDIF.

      "Set Tab Name
      ld_sheet_title = id_tab_title.

      REPLACE ALL OCCURRENCES OF ':' IN ld_sheet_title WITH ''.

      CONDENSE ld_sheet_title.

      co_worksheet->set_title( ld_sheet_title ).

      "Page printing settings
      "Margins are to be set to the values for "narrow". I just copy
      "the values in the "narrow" option on the print preview
      co_worksheet->sheet_setup->set_page_margins( ip_top    = '1.91'
                                                   ip_bottom = '1.91'
                                                   ip_left   = '0.64'
                                                   ip_right  = '0.64'
                                                   ip_header = '0.76'
                                                   ip_footer = '0.76'
                                                   ip_unit   = 'cm' ) ##LITERAL.
      co_worksheet->sheet_setup->black_and_white = 'X'.

      "Requirement is to fit all columns on one sheet
      "You should turn "fit to page" on to activate fit_to_height and fit_to_width
      co_worksheet->sheet_setup->fit_to_page         = 'X'.
      co_worksheet->sheet_setup->fit_to_width        = 1.  " used only if ip_fit_to_page = 'X'
      co_worksheet->sheet_setup->orientation         = zcl_excel_sheet_setup=>c_orientation_landscape.
      co_worksheet->sheet_setup->page_order          = zcl_excel_sheet_setup=>c_ord_downthenover.
      co_worksheet->sheet_setup->paper_size          = zcl_excel_sheet_setup=>c_papersize_a4.
      co_worksheet->sheet_setup->scale               = 80 ##NUMBER_OK. " used only if ip_fit_to_page = SPACE
      co_worksheet->sheet_setup->horizontal_centered = abap_true.

      "Put Tab Name in Header Centre
      ls_header-center_value     = co_worksheet->get_title( ).
      ls_header-center_font      = ls_header-right_font.
      ls_header-center_font-size = 8.
      ls_header-center_font-name = zcl_excel_style_font=>c_name_arial.

      "Put last save date on footer left
      CONCATENATE sy-datum+6(2) sy-datum+4(2) sy-datum(4) INTO ld_string SEPARATED BY '/'.
      ls_footer-left_value = ld_string.
      ls_footer-left_font  = ls_header-center_font.

      "Put Spreadsheet path and name in Header Centre
      ls_footer-center_value = '&Z&F'.                     "Path / Filename
      ls_footer-center_font  = ls_header-center_font.

      "Put page X of Y on Footer Right
      ls_footer-right_value = 'page &P of &N' ##no_text.   "page x of y
      ls_footer-right_font  = ls_header-center_font.

      co_worksheet->sheet_setup->set_header_footer( ip_odd_header = ls_header
                                                    ip_odd_footer = ls_footer ).

      "When printing, repeat the first rows all the time up until the column headings
      co_worksheet->zif_excel_sheet_printsettings~set_print_repeat_rows(
            iv_rows_from = 1
            iv_rows_to   = mc_headings_row ).

      "This needs to be 3 times
      "the normal height so the autofilter icons don't get in the way. In fact
      "3 times does not cut it, needs to be a bit bigger
      co_worksheet->set_row_height( ip_row        = mc_headings_row
                                    ip_height_fix = 50 ) ##NUMBER_OK.

      "Default zoom should be 75%
      co_worksheet->zif_excel_sheet_properties~zoomscale        = 85 ##NUMBER_OK.
      co_worksheet->zif_excel_sheet_properties~zoomscale_normal = 85 ##NUMBER_OK.

      "Header Row must be left justified, centered vertically & wrapped
      lo_style_mixed                        = co_excel->add_new_style( ).
      lo_style_mixed->alignment->horizontal = zcl_excel_style_alignment=>c_horizontal_left.
      lo_style_mixed->alignment->vertical   = zcl_excel_style_alignment=>c_vertical_center.
      lo_style_mixed->alignment->wraptext   = abap_true.
      ld_style_mixed_guid                   = lo_style_mixed->get_guid( ).

      ld_rows    = co_worksheet->get_highest_row( ).
      ld_columns = co_worksheet->get_highest_column( ).

      DO ld_columns TIMES.
        ld_column_int   = ld_column_int + 1.
        ld_column_alpha = zcl_excel_common=>convert_column2alpha( ld_column_int ).

        co_worksheet->set_column_width( ip_column         = ld_column_alpha
                                        ip_width_autosize = if_auto_size ).

        DATA(lo_column) = co_worksheet->get_column( ld_column_alpha ).

        "We hide the first five columns, as that is where macros will live
        IF ld_column_int LE 5.
          lo_column = lo_column->set_visible( abap_false ).
          CONTINUE.
        ENDIF.

        IF ld_column_int EQ 6.
          "First Row holds the report title, we want the title in BOLD
          lo_style_bold               = co_excel->add_new_style( ).
          lo_style_bold->font->bold   = abap_true.
          lo_style_bold->font->name   = zcl_excel_style_font=>c_name_calibri.
          lo_style_bold->font->scheme = zcl_excel_style_font=>c_scheme_none.
          lo_style_bold->font->size   = 16.
          ld_style_bold_guid          = lo_style_bold->get_guid( ).
          IF id_report_title IS INITIAL.
            co_worksheet->set_cell( ip_row    = 1
                                    ip_column = ld_column_alpha
                                    ip_value  = ld_sheet_title
                                    ip_style  = ld_style_bold_guid ).
          ELSE.
            co_worksheet->set_cell( ip_row    = 1
                                    ip_column = ld_column_alpha
                                    ip_value  = id_report_title
                                    ip_style  = ld_style_bold_guid ).
          ENDIF.
          "Second row is the date run in SAP
          ld_string = |{ 'Date Run :'(015) } { sy-datum+6(2) } / { sy-datum+4(2) } / { sy-datum(4) }|.
          co_worksheet->set_cell( ip_row    = 2
                                  ip_column = ld_column_alpha
                                  ip_value  = ld_string ).
          "Third Row is user name
          ld_string = |{ 'Run by :'(014) } { sy-uname }|.
          co_worksheet->set_cell( ip_row    = 3
                                  ip_column = ld_column_alpha
                                  ip_value  = ld_string ).
          "Filter row is a constant text
          ld_string = 'Filters'(017).
          co_worksheet->set_cell( ip_row    = mc_filter_row
                                  ip_column = ld_column_alpha
                                  ip_value  = ld_string ).
          "Subtotal row is a constant text
          ld_string = 'Subtotals'(013).
          co_worksheet->set_cell( ip_row    = mc_subtotal_row
                                  ip_column = ld_column_alpha
                                  ip_value  = ld_string ).
        ENDIF."First visible column

        "Header Row must be left justified, centered vertically & wrapped
        co_worksheet->get_cell( EXPORTING ip_column = ld_column_alpha
                                          ip_row    = mc_headings_row
                                IMPORTING ep_value  = ld_value ).

        co_worksheet->set_cell( ip_row    = mc_headings_row
                                ip_column = ld_column_alpha
                                ip_value  = ld_value
                                ip_style  = ld_style_mixed_guid ).

        IF ld_column_int GT 6.
          "Copy everything in the last row to subtotal row
          co_worksheet->get_cell( EXPORTING ip_column  = ld_column_alpha
                                            ip_row     = ld_rows           "last row
                                  IMPORTING ep_guid    = ld_cell_style
                                            ep_formula = ld_formula ).

          IF ld_formula IS NOT INITIAL.
            IF ld_formula(9) = 'SUBTOTAL('.
              ld_table_number = co_excel->get_active_sheet_index( ).  "e.g. table1,table2 etc
              ld_string       = ld_table_number.
              CONDENSE ld_string.
              SPLIT ld_formula AT '[' INTO ld_left ld_right.
              ld_formula = ld_left && 'table' && ld_string && '[' && ld_right ##NO_TEXT.
            ENDIF."This is a subtotal
            co_worksheet->set_cell( ip_row     = mc_subtotal_row
                                    ip_column  = ld_column_alpha
                                    ip_style   = ld_cell_style
                                    ip_formula = ld_formula ).
            lf_subtotals = abap_true.
          ENDIF."We have a formula
        ENDIF."Column is GT 6 i.e. not hidden
      ENDDO."Columns

      ld_column_int = 0.
      "All cells within the table, but only these, must have borders
      DO ld_columns TIMES.
        ld_column_int = ld_column_int + 1.
        IF ld_column_int LE 5..
          "This is one of the four hidden columns on the left, leave it alone
          CONTINUE.
        ENDIF.
        ld_column_alpha = zcl_excel_common=>convert_column2alpha( ld_column_int ).
        ld_row_int      = 0.
        DO ld_rows TIMES.
          ld_row_int = ld_row_int + 1.
          "If we have subtotals format the entire subtotal row in the same colour
          IF ld_row_int = mc_subtotal_row AND lf_subtotals = abap_true.
            co_worksheet->change_cell_style( ip_column                  = ld_column_alpha
                                             ip_row                     = ld_row_int
                                             ip_fill_filltype           = zcl_excel_style_fill=>c_fill_solid
                                             ip_fill_fgcolor_theme      = zcl_excel_style_color=>c_theme_accent6
                                             ip_borders_top_style       = zcl_excel_style_border=>c_border_thin
                                             ip_borders_top_color_rgb   = zcl_excel_style_color=>c_black
                                             ip_borders_down_style      = zcl_excel_style_border=>c_border_thin
                                             ip_borders_down_color_rgb  = zcl_excel_style_color=>c_black
                                             ip_borders_left_style      = zcl_excel_style_border=>c_border_thin
                                             ip_borders_left_color_rgb  = zcl_excel_style_color=>c_black
                                             ip_borders_right_style     = zcl_excel_style_border=>c_border_thin
                                             ip_borders_right_color_rgb = zcl_excel_style_color=>c_black ).
          ELSEIF ld_row_int = mc_subtotal_row.
            "If there are no columns with subtotals it is silly to show the row, so we hide it
            DATA(lo_row) = co_worksheet->get_row( ip_row = mc_subtotal_row ).
            lo_row->set_visible( abap_false ).
          ELSEIF ld_row_int = mc_filter_row.
            co_worksheet->change_cell_style( ip_column                  = ld_column_alpha
                                             ip_row                     = ld_row_int
                                             ip_fill_filltype           = zcl_excel_style_fill=>c_fill_solid
                                             ip_fill_fgcolor_rgb        = 'FFFFFF00'  "Yellow
                                             ip_borders_top_style       = zcl_excel_style_border=>c_border_thin
                                             ip_borders_top_color_rgb   = zcl_excel_style_color=>c_green
                                             ip_borders_down_style      = zcl_excel_style_border=>c_border_thin
                                             ip_borders_down_color_rgb  = zcl_excel_style_color=>c_green
                                             ip_borders_left_style      = zcl_excel_style_border=>c_border_thin
                                             ip_borders_left_color_rgb  = zcl_excel_style_color=>c_green
                                             ip_borders_right_style     = zcl_excel_style_border=>c_border_thin
                                             ip_borders_right_color_rgb = zcl_excel_style_color=>c_green ).
          ENDIF.
          IF ld_row_int LE mc_headings_row.
            "Do not format anything in the header row, or in the hidden rows above it
            CONTINUE.
          ENDIF.

          co_worksheet->get_cell( EXPORTING ip_column = ld_column_alpha
                                            ip_row    = ld_row_int
                                  IMPORTING ep_guid   = ld_cell_style ).

          TRY.
              ls_stylemapping = co_worksheet->excel->get_style_to_guid( ld_cell_style ).
            CATCH zcx_excel.
              CLEAR ls_stylemapping.
          ENDTRY.

          IF ls_stylemapping-complete_style-number_format-format_code = '#,##0.00'.
            "This is a currency amount, Ian Petrovski says use the accounting conventions
            "which are to have negative numbers as red in brackets, and show zero values
            "as dashes, so as to focus the eye on the real numbers
            ls_stylemapping-complete_style-number_format-format_code = '#,##0.00;[Red](#,##0.00);-'.
            co_worksheet->change_cell_style( ip_column                    = ld_column_alpha
                                             ip_row                       = ld_row_int
                                             ip_number_format_format_code =
                                             ls_stylemapping-complete_style-number_format-format_code
                                             ip_borders_top_style         = zcl_excel_style_border=>c_border_thin
                                             ip_borders_top_color_rgb     = zcl_excel_style_color=>c_black
                                             ip_borders_down_style        = zcl_excel_style_border=>c_border_thin
                                             ip_borders_down_color_rgb    = zcl_excel_style_color=>c_black
                                             ip_borders_left_style        = zcl_excel_style_border=>c_border_thin
                                             ip_borders_left_color_rgb    = zcl_excel_style_color=>c_black
                                             ip_borders_right_style       = zcl_excel_style_border=>c_border_thin
                                             ip_borders_right_color_rgb   = zcl_excel_style_color=>c_black ).
          ELSEIF ls_stylemapping-complete_style-number_format-format_code = '#,##0'.
            ls_stylemapping-complete_style-number_format-format_code = '#,##0;[Red](#,##0);-'.
            co_worksheet->change_cell_style( ip_column                    = ld_column_alpha
                                             ip_row                       = ld_row_int
                                             ip_number_format_format_code =
                                             ls_stylemapping-complete_style-number_format-format_code
                                             ip_borders_top_style         = zcl_excel_style_border=>c_border_thin
                                             ip_borders_top_color_rgb     = zcl_excel_style_color=>c_black
                                             ip_borders_down_style        = zcl_excel_style_border=>c_border_thin
                                             ip_borders_down_color_rgb    = zcl_excel_style_color=>c_black
                                             ip_borders_left_style        = zcl_excel_style_border=>c_border_thin
                                             ip_borders_left_color_rgb    = zcl_excel_style_color=>c_black
                                             ip_borders_right_style       = zcl_excel_style_border=>c_border_thin
                                             ip_borders_right_color_rgb   = zcl_excel_style_color=>c_black ).
          ELSEIF ls_stylemapping-complete_style-number_format-format_code = '#,##0.000'.
            "This is a quantity, Rob Downing desires only one decimal place
            ls_stylemapping-complete_style-number_format-format_code = '#,##0.0'.
            co_worksheet->change_cell_style( ip_column                    = ld_column_alpha
                                             ip_row                       = ld_row_int
                                             ip_number_format_format_code =
                                             ls_stylemapping-complete_style-number_format-format_code
                                             ip_borders_top_style         = zcl_excel_style_border=>c_border_thin
                                             ip_borders_top_color_rgb     = zcl_excel_style_color=>c_black
                                             ip_borders_down_style        = zcl_excel_style_border=>c_border_thin
                                             ip_borders_down_color_rgb    = zcl_excel_style_color=>c_black
                                             ip_borders_left_style        = zcl_excel_style_border=>c_border_thin
                                             ip_borders_left_color_rgb    = zcl_excel_style_color=>c_black
                                             ip_borders_right_style       = zcl_excel_style_border=>c_border_thin
                                             ip_borders_right_color_rgb   = zcl_excel_style_color=>c_black ).
          ELSEIF ls_stylemapping-complete_style-number_format-format_code = 'h:mm:ss' ##NO_TEXT.
            "This is a time with seconds, Rob Downing hates that, lose the seconds
            ls_stylemapping-complete_style-number_format-format_code = 'hh:mm' ##NO_TEXT.
            co_worksheet->change_cell_style( ip_column                    = ld_column_alpha
                                             ip_row                       = ld_row_int
                                             ip_number_format_format_code =
                                             ls_stylemapping-complete_style-number_format-format_code
                                             ip_borders_top_style         = zcl_excel_style_border=>c_border_thin
                                             ip_borders_top_color_rgb     = zcl_excel_style_color=>c_black
                                             ip_borders_down_style        = zcl_excel_style_border=>c_border_thin
                                             ip_borders_down_color_rgb    = zcl_excel_style_color=>c_black
                                             ip_borders_left_style        = zcl_excel_style_border=>c_border_thin
                                             ip_borders_left_color_rgb    = zcl_excel_style_color=>c_black
                                             ip_borders_right_style       = zcl_excel_style_border=>c_border_thin
                                             ip_borders_right_color_rgb   = zcl_excel_style_color=>c_black ).
          ELSE.
            co_worksheet->change_cell_style( ip_column                  = ld_column_alpha
                                             ip_row                     = ld_row_int
                                             ip_borders_top_style       = zcl_excel_style_border=>c_border_thin
                                             ip_borders_top_color_rgb   = zcl_excel_style_color=>c_black
                                             ip_borders_down_style      = zcl_excel_style_border=>c_border_thin
                                             ip_borders_down_color_rgb  = zcl_excel_style_color=>c_black
                                             ip_borders_left_style      = zcl_excel_style_border=>c_border_thin
                                             ip_borders_left_color_rgb  = zcl_excel_style_color=>c_black
                                             ip_borders_right_style     = zcl_excel_style_border=>c_border_thin
                                             ip_borders_right_color_rgb = zcl_excel_style_color=>c_black ).
          ENDIF.
        ENDDO."Rows
      ENDDO."Columns

      "If any columns are frozen on the left, keep them frozen
      co_worksheet->get_freeze_cell( IMPORTING ep_column = ld_freeze_column ).

      "We have five hidden columns at the start, remember... the original
      "ALV report would not have been expecting that one!
      ld_freeze_column = ld_freeze_column + 5.

      "For some reason the standard ABAP2XLS code freezes one too many columns
      IF ld_freeze_column GT 6.
        ld_freeze_column = ld_freeze_column - 1.
      ENDIF.

      co_worksheet->freeze_panes( ip_num_columns = ld_freeze_column
                                  ip_num_rows    = mc_headings_row ).   "Freeze column headers when scrolling

      "Baron F wants the number of records shown at the top
      IF lf_subtotals = abap_true.
        "Subtract five header rows and the subtotal row
        ld_record_count = ld_rows - mc_headings_row - 1.
      ELSE.
        "Subtract five header rows
        ld_record_count = ld_rows - mc_headings_row.
      ENDIF.

      ld_count_string = |{ ld_record_count } { 'Records Found'(016) }|.

      co_worksheet->set_cell( ip_column = 'I'
                              ip_row    = 2
                              ip_value  = ld_count_string ).

    CATCH zcx_excel INTO DATA(lo_exception).
      DATA(ld_message) = lo_exception->get_text( ).
      MESSAGE ld_message TYPE 'I'.
      ld_message = lo_exception->error.
      MESSAGE ld_message TYPE 'I'.
  ENDTRY.

ENDMETHOD.


METHOD get_email_address ##NEEDED.
*-----------------------------------------------------------------------*
* The below code will only work in a system which has the SAP HR Module
* If you have a SuccessFactors HR system or some such then you will have
* to do a remote call to get the users email address
*-----------------------------------------------------------------------*
* Local Variables
*  DATA : ld_pernr         TYPE prelp-pernr,
*         ld_infty         TYPE prelp-infty VALUE '0105',
*         ld_begda         TYPE prelp-begda,
*         ld_subrc         TYPE sy-subrc,
*         lt_p0105         TYPE STANDARD TABLE OF p0105,
*         ls_p0105         TYPE                   p0105.
*
*  ld_begda = sy-datum.
*
*  CALL FUNCTION 'HRWPC_AP_GET_EMPLOYEE_FOR_USER'
*    EXPORTING
*      user_name               = id_user_name
*    IMPORTING
*      employeenumber          = ld_pernr
*    EXCEPTIONS
*      no_employeenumber_found = 1
*      subtype_not_available   = 2
*      OTHERS                  = 3.
*
*  IF sy-subrc <> 0.
*    rd_email_address = get_email_via_user( id_user_name ).
*    RETURN.
*  ENDIF.
*
*  CALL FUNCTION 'HR_READ_INFOTYPE'
*    EXPORTING
*      pernr           = ld_pernr
*      infty           = ld_infty
*      begda           = ld_begda
*    IMPORTING
*      subrc           = ld_subrc
*    TABLES
*      infty_tab       = lt_p0105
*    EXCEPTIONS
*      infty_not_found = 1
*      OTHERS          = 2.
*
*  IF sy-subrc <> 0.
*    rd_email_address = get_email_via_user( id_user_name ).
*    RETURN.
*  ENDIF.
*
*  IF ld_subrc <> 0.
*    rd_email_address = get_email_via_user( id_user_name ).
*    RETURN.
*  ENDIF.
*
*  READ TABLE lt_p0105 INTO ls_p0105 WITH KEY pernr = ld_pernr
*                                             subty = '0010'
*                                             endda = '99991231'.
*
*  IF sy-subrc <> 0.
*    rd_email_address = get_email_via_user( id_user_name ).
*    RETURN.
*  ENDIF.
*
*  MOVE ls_p0105-usrid_long TO rd_email_address.

ENDMETHOD.


METHOD get_email_via_user.
* Local Variables
  DATA: lt_addsmtp TYPE STANDARD TABLE OF	bapiadsmtp,
        lt_return  TYPE STANDARD TABLE OF bapiret2 ##needed.

  CALL FUNCTION 'BAPI_USER_GET_DETAIL'
    EXPORTING
      username = id_user_name
    TABLES
      return   = lt_return
      addsmtp  = lt_addsmtp.

  IF lt_addsmtp[] IS INITIAL.
    RETURN.
  ENDIF.

  READ TABLE lt_addsmtp INTO DATA(ls_addsmtp) INDEX 1.
  ASSERT sy-subrc EQ 0.

  rd_email_address = ls_addsmtp-e_mail.

ENDMETHOD.


METHOD get_macro_template.
* Local Variables
  DATA: ls_template     TYPE ztexcel_template,
        ld_template     TYPE zde_bc_excel_template,
        lo_excel_reader TYPE REF TO zif_excel_reader,
        lo_iterator     TYPE REF TO cl_object_collection_iterator,
        lo_worksheet    TYPE REF TO zcl_excel_worksheet.

  TRY.
      "Use some sort of customising to set the default value, or a factory method, or whatever....
      ld_template = 'DEFAULT'.

      lo_excel_reader = NEW zcl_excel_reader_xlsm( ).

      SELECT SINGLE *
        FROM ztexcel_template
        INTO CORRESPONDING FIELDS OF ls_template
        WHERE template_name = ld_template.

      IF sy-subrc NE 0.
        RETURN.
      ENDIF.

      ro_excel = lo_excel_reader->load( ls_template-raw_data ).

      mf_macro_enabled = abap_true.

* We have lots of sheets in the template just to be safe, but have no way of knowing
* how many sheets the calling program wants. They will usually only want one
* So we hide the last seven sheets, or however many sheets we have. The first is always
* visible
      lo_iterator = ro_excel->get_worksheets_iterator( ).
      WHILE lo_iterator->if_object_collection_iterator~has_next( ) EQ abap_true.
        lo_worksheet ?= lo_iterator->if_object_collection_iterator~get_next( ).
        IF sy-index = 1.
          "The first sheet is always visible
          lo_worksheet->zif_excel_sheet_properties~hidden = zif_excel_sheet_properties=>c_visible.
        ELSE.
          lo_worksheet->zif_excel_sheet_properties~hidden = zif_excel_sheet_properties=>c_hidden.
        ENDIF.
      ENDWHILE.

    CATCH zcx_excel.
      RETURN.
  ENDTRY.

ENDMETHOD.


METHOD get_next_worksheet.

  CLEAR eo_worksheet.

  TRY.
      DATA(ld_current_highest_sheet) = co_excel->get_worksheets_size( ).

      IF id_target_sheet LE ld_current_highest_sheet.
        "Worksheet exists with the target number, so we use that one
        co_excel->set_active_sheet_index( id_target_sheet ).
        eo_worksheet = co_excel->get_active_worksheet( ).
        eo_worksheet->zif_excel_sheet_properties~hidden = zif_excel_sheet_properties=>c_visible.
      ELSE.
        "No existing worksheet with the target number, so we need to
        "create at least one new one
        co_excel->add_new_worksheet( ).
        "Recursive Call
        get_next_worksheet( EXPORTING id_target_sheet = id_target_sheet
                            IMPORTING eo_worksheet    = eo_worksheet
                            CHANGING  co_excel        = co_excel ).
      ENDIF.

    CATCH zcx_excel INTO DATA(lo_exception).
      DATA(ld_string) = lo_exception->get_text( ).
      MESSAGE ld_string TYPE 'I'.
      ld_string = lo_exception->error.
      MESSAGE ld_string TYPE 'I'.
      MESSAGE 'Could not create new worksheet'(012) TYPE 'E'.
  ENDTRY.

ENDMETHOD.


  METHOD is_production.
* T000 - Fully Buffered
    SELECT SINGLE cccategory
      FROM  t000
      INTO  @DATA(ld_client_function)
      WHERE mandt EQ @sy-mandt.

    IF ld_client_function = mc_production.
      result = abap_true.
    ELSE.
      result = abap_false.
    ENDIF.

  ENDMETHOD.


METHOD prepare_email_details.
* Local Variables
  DATA: ls_soli            TYPE soli,
        ld_curr_report     TYPE rsvar-report,
        lt_selection_table TYPE STANDARD TABLE OF rsparams,
        lt_sscr            TYPE STANDARD TABLE OF rsscr,
        ld_tabname         TYPE dd03m-tabname,
        ld_fieldname       TYPE dd03m-fieldname,
        ld_program         TYPE rs38m-programm ##NEEDED,
        ld_key             TYPE textpool-key ##NEEDED,
        ld_entry           TYPE textpool-entry,
        ld_sign            TYPE string,
        ld_option          TYPE string.

* Clear Return parameters
  CLEAR: ed_email_subject,
         ed_attachment_subject,
         et_email_body,
         et_addresses.

**********************************************************************
* Email Subject e.g. Widget report
**********************************************************************
  ed_email_subject = id_report_title.

  REPLACE ALL OCCURRENCES OF ':' IN ed_email_subject WITH ''.
  CONDENSE ed_email_subject.

  "Add system id to the title of Non production systems so recipient knows it is not real data
  IF is_production( )      EQ abap_false AND
     ed_email_subject+0(3) NE sy-sysid.
    CONCATENATE sy-sysid ed_email_subject INTO ed_email_subject SEPARATED BY space.
  ENDIF.

**********************************************************************
* Attachment Subject e.g. ZS19_20100101.xlsx
* This has to end in .XLSX or the world will end
**********************************************************************
  IF id_attachment_subject IS NOT INITIAL.
    ed_attachment_subject = id_attachment_subject.
  ELSE.
    CONCATENATE id_tcode sy-datum INTO ed_attachment_subject SEPARATED BY '_'.
  ENDIF.

  IF mf_macro_enabled = abap_true.
    CONCATENATE ed_attachment_subject mc_macro_enabled INTO ed_attachment_subject ##no_text.
  ELSE.
    CONCATENATE ed_attachment_subject mc_normal        INTO ed_attachment_subject ##no_text.
  ENDIF.

**********************************************************************
* Email Address
**********************************************************************
  APPEND id_email TO et_addresses.

**********************************************************************
* Email Body
**********************************************************************
  IF it_email_body[] IS NOT INITIAL.
    et_email_body[] = it_email_body[].
    RETURN.
  ENDIF.

  CONCATENATE
  'Please find attached an email attachment showing the output from the'(001)
  'report in the form of a spreadsheet.'(002)
  INTO ls_soli SEPARATED BY space.
  APPEND ls_soli TO et_email_body.

  "Get a table of all the selection criteria for this program
  ld_curr_report = id_program_name.

  "The table LT_SSCR will list the selection criteria in the order they appear on the screen
  LOAD REPORT ld_curr_report PART 'SSCR' INTO lt_sscr.

  "The table LT_SELECTION_TABLE will contain the values of the various selection criteria
  CALL FUNCTION 'RS_REFRESH_FROM_SELECTOPTIONS'
    EXPORTING
      curr_report     = ld_curr_report
    TABLES
      selection_table = lt_selection_table
    EXCEPTIONS
      not_found       = 1
      no_report       = 2
      OTHERS          = 3.

  IF sy-subrc NE 0.
    RETURN.
  ENDIF.

  IF lt_selection_table[] IS INITIAL.
    RETURN.
  ENDIF.

  ls_soli =  'The selection criteria were as follows:-'(003).
  APPEND ls_soli TO et_email_body.

  "Blank Line
  CLEAR  ls_soli.
  APPEND ls_soli TO et_email_body.

  LOOP AT lt_sscr ASSIGNING FIELD-SYMBOL(<ls_criteria_names>).
    LOOP AT lt_selection_table ASSIGNING FIELD-SYMBOL(<ls_selection_table>) WHERE selname = <ls_criteria_names>-name.
      CHECK <ls_selection_table>-low IS NOT INITIAL.

      IF <ls_selection_table>-kind = 'P'.
        "This is a Parameter
        ld_sign = 'selection equals'(018).
      ELSEIF <ls_selection_table>-sign = 'I'.
        ld_sign = 'selection includes'(004).
      ELSEIF <ls_selection_table>-sign = 'E'.
        ld_sign = 'selection excludes'(005).
      ENDIF.

      IF <ls_selection_table>-kind = 'P'.
        CLEAR ld_option.
      ELSEIF <ls_selection_table>-option IS NOT INITIAL.
        CASE <ls_selection_table>-option.
          WHEN 'EQ'.
            ld_option = 'the value of'(019).
          WHEN 'BT'.
            IF <ls_selection_table>-high IS INITIAL.
              ld_option = 'the value of'(019).
            ELSE.
              ld_option = 'values between'(007).
            ENDIF.
          WHEN 'CP'.
            ld_option = 'values containing the pattern'(008).
          WHEN OTHERS.
            ld_option = 'values'(009).
        ENDCASE.
      ENDIF.

      "Get the text name of the selection criteria
      ld_program = id_program_name.
      ld_key     = <ls_selection_table>-selname.
      CLEAR ld_entry.
*--------------------------------------------------------------------*
* The below function module only exists in ECC systems
* If you are installing this in an ECC system remove the RETURN and
* then uncomment the function module call
*--------------------------------------------------------------------*
      RETURN.
*      CALL FUNCTION 'READ_TEXTELEMENT_FROM_REPORT'
*        EXPORTING
*          program = ld_program
*          key     = ld_key
*        IMPORTING
*          entry   = ld_entry.

      IF ld_entry CS 'D       .'.
        LOOP AT lt_sscr ASSIGNING FIELD-SYMBOL(<ls_sscr>) WHERE name = <ls_selection_table>-selname.
          SPLIT <ls_sscr>-dbfield AT '-' INTO ld_tabname ld_fieldname.
          CONDENSE ld_fieldname NO-GAPS.                           "Most likely not needed
          "Read Database View
          SELECT ddtext
            FROM dd03m UP TO 1 ROWS
            INTO ld_entry
            WHERE fieldname  = ld_fieldname
            AND   tabname    = ld_tabname
            AND   ddlanguage = sy-langu.
          ENDSELECT.
        ENDLOOP.
      ENDIF.

      ls_soli = ld_entry.
      CONDENSE ls_soli.

      "Remove the horrible looking ICON symbols e.g. @KG@
      IF strlen( ls_soli ) GT 4 AND
         ls_soli(1) = '@'."It's an ICON
          ls_soli(4) = '    '.
          CONDENSE ls_soli.
      ENDIF."String length greater than four

      IF <ls_criteria_names>-dtyp = 'DATS'.
        <ls_selection_table>-low = <ls_selection_table>-low+6(2) && '/' &&
                                   <ls_selection_table>-low+4(2) && '/' &&
                                   <ls_selection_table>-low(4).
        IF <ls_selection_table>-high IS NOT INITIAL.
          <ls_selection_table>-high = <ls_selection_table>-high+6(2) && '/' &&
                                      <ls_selection_table>-high+4(2) && '/' &&
                                      <ls_selection_table>-high(4).
        ENDIF.
      ENDIF.

      IF <ls_criteria_names>-miscell EQ 'ALPHA'.
        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
          EXPORTING
            input  = <ls_selection_table>-low
          IMPORTING
            output = <ls_selection_table>-low.
      ENDIF.

      CONCATENATE ls_soli                                          "Plant
                  ld_sign                                          "selection includes
                  ld_option                                        "values between
                  <ls_selection_table>-low                         "3000 and .....
      INTO ls_soli SEPARATED BY space.
      IF <ls_selection_table>-high IS NOT INITIAL.
        CONCATENATE ls_soli <ls_selection_table>-high INTO ls_soli SEPARATED BY ' AND '.
      ENDIF.
      CONDENSE ls_soli.
      APPEND ls_soli TO et_email_body.
      "Blank Line
      CLEAR  ls_soli.
      APPEND ls_soli TO et_email_body.
    ENDLOOP.

  ENDLOOP."List of criteria in the order they appear on the SELECTION-SCREEN

ENDMETHOD.


METHOD send.
*--------------------------------------------------------------------*
* Listing 11.31: - Sending a Spreadsheet by Email
*--------------------------------------------------------------------*
* Local Variables
  DATA: lo_send_request  TYPE REF TO cl_bcs,
        lo_document      TYPE REF TO cl_document_bcs,
        lo_recipient     TYPE REF TO if_recipient_bcs,
        lt_main_text     TYPE bcsy_text,
        lf_sent_to_all   TYPE os_boolean,
        ld_bytecount     TYPE i,
        ld_maxbytecount  TYPE i,
        ld_filelen       TYPE so_obj_len,
        ld_email         TYPE ad_smtpadr.

  TRY.
      "Convert to binary
      DATA(lt_file_tab) = cl_bcs_convert=>xstring_to_solix( iv_xstring  = id_file ).
      ld_bytecount      = xstrlen( id_file ).
      ld_maxbytecount   = 10000000 ##NUMBER_OK.

*-------- create persistent send request ------------------------
      lo_send_request = cl_bcs=>create_persistent( ).

*-------- create and set document with attachment ---------------
      "Create document object from internal table with text
      lt_main_text = id_email_body.
      IF ld_bytecount >= ld_maxbytecount.
        APPEND INITIAL LINE TO lt_main_text ASSIGNING FIELD-SYMBOL(<ls_main_text>).
        <ls_main_text>-line =
        |{ 'The excel extract of the report cannot be sent because the resulting file is beyond the'(020) } | &&
        |{ ld_maxbytecount / 1000000 DECIMALS = 0 }{ 'MB limit'(021) }|.
      ENDIF.
      lo_document = cl_document_bcs=>create_document( i_type    = 'RAW'
                                                      i_text    = lt_main_text
                                                      i_subject = id_email_subject ).

      IF ld_bytecount < ld_maxbytecount.

        "Add the spread sheet as attachment to document object
        ld_filelen = ld_bytecount.

        DATA: lt_att_head    TYPE soli_tab,
              lv_text_line   TYPE soli.
        CONCATENATE '&SO_FILENAME=' id_attachment_subject INTO lv_text_line.
        APPEND lv_text_line TO lt_att_head.

        lo_document->add_attachment( i_attachment_type    = 'EXT'
                                     i_attachment_subject = id_attachment_subject
                                     i_attachment_size    = ld_filelen
                                     i_att_content_hex    = lt_file_tab
                                     i_attachment_header  = lt_att_head ) ##NO_TEXT.

      ENDIF.

      "Add document object to send request
      lo_send_request->set_document( lo_document ).

*--------- add recipient (e-mail address) -----------------------
      LOOP AT it_email_addresses ASSIGNING FIELD-SYMBOL(<ld_email>).
        "Create recipient object
        lo_recipient = cl_cam_address_bcs=>create_internet_address( <ld_email> ).

        "Add recipient object to send request
        lo_send_request->add_recipient( lo_recipient ).

      ENDLOOP.
*--------- add recipient (e-mail address) with options -----------------------
      LOOP AT it_email_address_option ASSIGNING FIELD-SYMBOL(<ls_email_opt>).
        "Create recipient object
        lo_recipient = cl_cam_address_bcs=>create_internet_address( <ls_email_opt>-email ).

        "Add recipient object to send request
        lo_send_request->add_recipient( i_recipient  = lo_recipient
                                        i_copy       = <ls_email_opt>-copy
                                        i_blind_copy = <ls_email_opt>-blind_copy ).

      ENDLOOP.

      lo_send_request->set_status_attributes( 'N' )."Never

*---------- send document ---------------------------------------
      lf_sent_to_all = lo_send_request->send( 'X' )."With Error Screen

      COMMIT WORK.

      IF lf_sent_to_all IS INITIAL.
        MESSAGE i500(sbcoms) WITH ld_email."Document not sent to &1
      ELSE.
        MESSAGE s022(so)."Document sent
        "kick off the send job so the email goes out immediately
        WAIT UP TO 2 SECONDS.     "ensure the mail has been queued
        SUBMIT rsconn01
          WITH mode   = '*'       "process everything you find.
          WITH output = ' '
        AND RETURN."#EC CI_SUBMIT
      ENDIF.

*------------ exception handling ----------------------------------
    "Replace this rudimentary exception handling with your own one !!!
    CATCH cx_bcs INTO DATA(lo_bcs_exception).
      "Error occurred during transmission - return code: <&>
      MESSAGE i865(so) WITH lo_bcs_exception->error_type.
  ENDTRY.

ENDMETHOD.


METHOD send_excel_object.
* Local Variables
  DATA: ld_email_subject      TYPE so_obj_des,
        ld_attachment_subject TYPE sood-objdes, "make sure this ends in XLSX
        lt_email_body         TYPE bcsy_text,
        lt_addresses          TYPE bcsy_smtpa,
        ld_xml_file           TYPE xstring,
        lo_excel_writer       TYPE REF TO zif_excel_writer.

* Preconditions
  zcl_dbc=>require( that             = 'a valid EXCEL is object is passed in'(011)
                    which_is_true_if = xsdbool( io_excel     IS SUPPLIED OR
                                                io_converter IS SUPPLIED ) ).
  DATA(lt_email_opt) = it_email.
  TRY.
      "Prepare details needed for sending an email
      zcl_excel_emailer=>prepare_email_details(
        EXPORTING
          id_tcode              = id_tcode
          id_report_title       = id_report_title
          id_program_name       = id_program_name
          id_email              = id_email
          it_email_body         = it_email_body
          id_attachment_subject = id_attachment_subject
        IMPORTING
          ed_email_subject      = ld_email_subject
          ed_attachment_subject = ld_attachment_subject
          et_email_body         = lt_email_body
          et_addresses          = lt_addresses ).

      DELETE lt_email_opt WHERE email = id_email AND copy = '' AND blind_copy = ''.
      "Transform Excel Data into XML Format
      IF io_excel IS NOT SUPPLIED OR
         io_excel IS NOT BOUND.
        io_converter->get_file( IMPORTING e_file = ld_xml_file ).
        ld_attachment_subject = to_upper( ld_attachment_subject ).
        REPLACE ALL OCCURRENCES OF mc_macro_enabled IN ld_attachment_subject WITH mc_normal.
      ELSEIF io_excel->zif_excel_book_vba_project~codename_pr IS NOT INITIAL.
        mf_macro_enabled = abap_true.
        lo_excel_writer  = NEW zcl_excel_writer_xlsm( ).
        ld_xml_file      = lo_excel_writer->write_file( io_excel ).
        REPLACE ALL OCCURRENCES OF mc_normal IN ld_attachment_subject WITH mc_macro_enabled.
      ELSE.
        mf_macro_enabled = abap_false.
        lo_excel_writer  = NEW zcl_excel_writer_2007( ).
        ld_xml_file      = lo_excel_writer->write_file( io_excel ).
        REPLACE ALL OCCURRENCES OF mc_macro_enabled IN ld_attachment_subject WITH mc_normal.
      ENDIF.

      "Send Email
      zcl_excel_emailer=>send(
          id_file                 = ld_xml_file
          it_email_addresses      = lt_addresses
          id_email_subject        = ld_email_subject
          id_attachment_subject   = ld_attachment_subject
          id_email_body           = lt_email_body
          it_email_address_option = lt_email_opt ).

    CATCH zcx_excel INTO DATA(lo_exception).
      DATA(ld_message) = lo_exception->get_text( ).
      MESSAGE ld_message TYPE 'I'.
  ENDTRY.

ENDMETHOD.


METHOD send_itab.

  TRY.
      "Step 1 - convert internal table into an EXCEL object
      zcl_excel_emailer=>convert_table_to_excel(
        EXPORTING
          id_program_name  = id_program_name
          it_rald_fieldcat = it_rald_fieldcat[]
          it_rald_sortcat  = it_rald_sortcat[]
          is_rald_layout   = is_rald_layout
        IMPORTING
          eo_converter     = DATA(lo_converter)
          eo_excel         = DATA(lo_excel)
        CHANGING
          ct_itab          = ct_itab[]
          cs_variant       = cs_variant ).

      "Step 2 - send the amended EXCEL object as an email
      zcl_excel_emailer=>send_excel_object(
          io_excel        = lo_excel
          io_converter    = lo_converter
          id_tcode        = id_tcode
          it_email_body   = it_email_body
          id_report_title = id_report_title
          id_program_name = id_program_name
          id_email        = id_email ).

    CATCH zcx_excel INTO DATA(lo_exception).
      DATA(message) = lo_exception->get_text( ).
      MESSAGE message TYPE 'I'.
  ENDTRY.

ENDMETHOD.
ENDCLASS.
