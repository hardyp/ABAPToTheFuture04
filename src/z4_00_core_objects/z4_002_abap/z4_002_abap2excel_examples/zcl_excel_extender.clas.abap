class ZCL_EXCEL_EXTENDER definition
  public
  create public .

public section.

  constants MC_HEADER_ROW type I value 6 ##NO_TEXT.

  class-methods CALCULATE_CELL_WIDTH
    importing
      !IO_WORKSHEET type ref to ZCL_EXCEL_WORKSHEET
      !IP_COLUMN type SIMPLE
      !IP_ROW type ZEXCEL_CELL_ROW
      !IP_CURRENT_MAX type FLOAT optional
    returning
      value(EP_WIDTH) type I
    raising
      ZCX_EXCEL .
  class-methods CALCULATE_COLUMN_WIDTHS
    importing
      !IO_WORKSHEET type ref to ZCL_EXCEL_WORKSHEET
    raising
      ZCX_EXCEL .
protected section.
private section.
ENDCLASS.



CLASS ZCL_EXCEL_EXTENDER IMPLEMENTATION.


METHOD calculate_cell_width.
  DATA: cell_value   TYPE zexcel_cell_value,
        guid         TYPE zexcel_cell_style,
        stylemapping TYPE zexcel_s_stylemapping,
        ld_format    TYPE zexcel_number_format,
        ld_zeroes    TYPE string,
        ld_decimals  TYPE i,
        ld_word_001  TYPE string,
        ld_word_002  TYPE string,
        ld_word_003  TYPE string,
        ld_formula   TYPE zexcel_cell_formula.

  io_worksheet->get_cell( EXPORTING ip_column  = ip_column              " Cell Column
                                    ip_row     = ip_row                " Cell Row
                          IMPORTING ep_value   = cell_value
                                    ep_guid    = guid
                                    ep_formula = ld_formula ).

  ep_width = strlen( cell_value ).

  "For the headers column, we count the length as the longest word
  IF ip_row EQ mc_header_row.
    SPLIT cell_value AT ' ' INTO ld_word_001 ld_word_002 ld_word_003.
    ep_width = strlen( ld_word_001 ).
    IF strlen( ld_word_002 ) GT ep_width.
      ep_width = strlen( ld_word_002 ).
    ENDIF.
    IF strlen( ld_word_003 ) GT ep_width.
      ep_width = strlen( ld_word_003 ).
    ENDIF.
  ENDIF.

  TRY.
      stylemapping = io_worksheet->excel->get_style_to_guid( guid ).
    CATCH zcx_excel.
      RETURN.  " Do nothing if no style was found
  ENDTRY.

  ld_format = stylemapping-complete_style-number_format-format_code.

  IF ld_format = 'STD_DATE' OR
     ld_format = 'DD.MM.YYYY'.
    "Stops date getting truncated
    ep_width = 11 ##NUMBER_OK.
  ELSEIF ld_format = 'h:mm:ss' OR
         ld_format = 'hh:mm:ss' ##NO_TEXT.
    "Shrinks time values
    ep_width = 9.
  ELSEIF ld_format = 'hh:mm' ##NO_TEXT.
    ep_width = 6.
  ELSEIF ld_format CS '#,##0.00;'.                         "Number with 2 decimal places
    ld_decimals = 2.
    SPLIT cell_value AT '.' INTO cell_value ld_zeroes.
    IF sy-subrc = 0.
      ep_width = strlen( cell_value ) + ld_decimals + 1.   "Add one for the decimal point
    ENDIF.
    IF cell_value < 0.
      ep_width = ep_width + 2.                             "To take the brackets into account
    ENDIF.
  ELSEIF ld_format CS '#,##0.'.                            "Number with decimal places
    SPLIT ld_format AT '#,##0.' INTO ld_format ld_zeroes.
    ld_decimals = strlen( ld_zeroes ).
    SPLIT cell_value AT '.' INTO cell_value ld_zeroes.
    IF sy-subrc = 0.
      ep_width = strlen( cell_value ) + ld_decimals + 1.   "Add one for the decimal point
    ENDIF.
    IF cell_value < 0.
      ep_width = ep_width + 2.                             "To take the brackets into account
    ENDIF.
  ELSE.
    "Stops last bit of column getting hidden
    ep_width = ep_width + 1.
  ENDIF.

  "For subtotals,widen the column, as total will most likley be
  "longer than individaul entries
  IF ld_formula           IS NOT INITIAL   AND
     strlen( ld_formula ) GT 8             AND
     ld_formula(9)        EQ 'SUBTOTAL('   AND
     ip_row               GT mc_header_row AND
     ip_current_max       GT 0.
    ep_width = ip_current_max + 1.
  ENDIF.

  IF stylemapping-complete_stylex-font-size = 'X'.
    ep_width = ep_width * stylemapping-complete_style-font-size / 11 ##NUMBER_OK.
  ENDIF.

ENDMETHOD.


METHOD calculate_column_widths.
  TYPES:
    BEGIN OF t_auto_size,
      col_index TYPE int4,
      width     TYPE float,
    END   OF t_auto_size.
  TYPES: tt_auto_size TYPE STANDARD TABLE OF t_auto_size.

  DATA: auto_size   TYPE flag,
        auto_sizes  TYPE tt_auto_size,
        count       TYPE int4,
        highest_row TYPE int4,
        width       TYPE i,
        lo_column   TYPE REF TO zcl_excel_column.

  FIELD-SYMBOLS: <auto_size> LIKE LINE OF auto_sizes.

  DATA(columns)         = io_worksheet->get_columns( ).
  DATA(column_iterator) = columns->get_iterator( ).

  WHILE column_iterator->has_next( ).
    lo_column ?= column_iterator->get_next( ).
    auto_size = lo_column->get_auto_size( ).
    IF auto_size = abap_true.
      APPEND INITIAL LINE TO auto_sizes ASSIGNING <auto_size>.
      <auto_size>-col_index = lo_column->get_column_index( ).
      <auto_size>-width     = -1.
    ENDIF.
  ENDWHILE.

  "There is only something to do if there are some auto-size columns
  IF auto_sizes IS NOT INITIAL.
    highest_row = io_worksheet->get_highest_row( ).
    LOOP AT auto_sizes ASSIGNING <auto_size>.
      count = 1.
      WHILE count <= highest_row.
        IF <auto_size>-col_index = 6 OR                  "Column with headers
           <auto_size>-col_index = 9.                    "Column with extra header information
          IF count LT mc_header_row.
            "The first four rows in the first column are headers and should not be taken into account
            count = count + 1.
            CONTINUE.
          ENDIF.
        ENDIF.

        width = calculate_cell_width( io_worksheet   = io_worksheet
                                      ip_column      = <auto_size>-col_index
                                      ip_row         = count
                                      ip_current_max = <auto_size>-width ).
        width = width + 1.

        IF width > <auto_size>-width.
          <auto_size>-width = width.
        ENDIF.
        count = count + 1.
      ENDWHILE.
      DATA(column_dimension) = io_worksheet->get_column( <auto_size>-col_index ).
      column_dimension->set_width( <auto_size>-width ).
    ENDLOOP.
  ENDIF.

ENDMETHOD.
ENDCLASS.
