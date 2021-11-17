class ZCL_EXCEL_FIELDCAT_CONVERTER definition
  public
  create public .

public section.

  class-methods CONVERT_FIELDCAT
    importing
      !IT_FIELDCAT type SLIS_T_FIELDCAT_ALV
    changing
      !CO_SALV type ref to CL_SALV_TABLE .
  class-methods CONVERT_SORTCAT
    importing
      !IT_SORTCAT type SLIS_T_SORTINFO_ALV
    changing
      !CO_SALV type ref to CL_SALV_TABLE .
protected section.
private section.

  class-data MO_COLUMNS type ref to CL_SALV_COLUMNS_TABLE .
  class-data MO_COLUMN type ref to CL_SALV_COLUMN_TABLE .
  class-data MO_SORT type ref to CL_SALV_SORTS .
  class-data MO_AGGREGATIONS type ref to CL_SALV_AGGREGATIONS .
ENDCLASS.



CLASS ZCL_EXCEL_FIELDCAT_CONVERTER IMPLEMENTATION.


METHOD convert_fieldcat.
* Local Variables
  DATA: ld_column_name TYPE lvc_fname,
        lt_salv_list   TYPE salv_t_column_ref,
        ls_salv_list   TYPE salv_s_column_ref,
        ld_long_text   TYPE scrtext_l,
        ld_med_text    TYPE scrtext_m,
        ld_short_text  TYPE scrtext_s.

  "When ordering the columns, it works better when the fieldcat is
  "in col_pos sequence.
  DATA(lt_fieldcat) = it_fieldcat[].
  "don't re-order hidden fields
  DELETE lt_fieldcat WHERE no_out EQ abap_true.
  SORT lt_fieldcat BY col_pos ASCENDING.

  "Get the columns from the SALV object
  mo_columns = co_salv->get_columns( ).

  "The fields will be in the order of the internal table. Let us
  "re-arrange them to the order in the field catalogue, which is
  "usually different
  LOOP AT lt_fieldcat ASSIGNING FIELD-SYMBOL(<ls_fieldcat>).
    ld_column_name = <ls_fieldcat>-fieldname.
    mo_columns->set_column_position( columnname = ld_column_name
                                     position   = <ls_fieldcat>-col_pos ).
  ENDLOOP.

  "Hotspots
  LOOP AT it_fieldcat ASSIGNING <ls_fieldcat> WHERE hotspot = abap_true.
    ld_column_name = <ls_fieldcat>-fieldname.
    TRY.
        mo_column ?= mo_columns->get_column( ld_column_name ).

        mo_column->set_cell_type( if_salv_c_cell_type=>hotspot ).

      CATCH cx_salv_not_found.
        CONTINUE.
    ENDTRY.
  ENDLOOP.  "Hotspots

  "Key Fields
  LOOP AT it_fieldcat ASSIGNING <ls_fieldcat> WHERE key = abap_true.
    ld_column_name = <ls_fieldcat>-fieldname.
    TRY.
        mo_column ?= mo_columns->get_column( ld_column_name ).

        mo_column->set_key( abap_true ).

        mo_columns->set_key_fixation( abap_true ).

      CATCH cx_salv_not_found.
        CONTINUE.
    ENDTRY.
  ENDLOOP.

  "Hidden Fields
  LOOP AT it_fieldcat ASSIGNING <ls_fieldcat>.
    ld_column_name = <ls_fieldcat>-fieldname.
    TRY.
        mo_column ?= mo_columns->get_column( ld_column_name ).

        IF <ls_fieldcat>-no_out = abap_true.
          mo_column->set_technical( if_salv_c_bool_sap=>true ).
        ELSE.
          mo_column->set_visible( if_salv_c_bool_sap=>true ).
        ENDIF.

      CATCH cx_salv_not_found.
        CONTINUE.
    ENDTRY.
  ENDLOOP.  "Hidden Fields

  "Technical Fields
  LOOP AT it_fieldcat ASSIGNING <ls_fieldcat> WHERE tech = abap_true.
    ld_column_name = <ls_fieldcat>-fieldname.
    TRY.
        mo_column ?= mo_columns->get_column( ld_column_name ).

        mo_column->set_technical( if_salv_c_bool_sap=>true ).

      CATCH cx_salv_not_found.
        CONTINUE.
    ENDTRY.
  ENDLOOP.  "Technical Fields

  "Subtotals
  LOOP AT it_fieldcat ASSIGNING <ls_fieldcat> WHERE do_sum = abap_true.

    mo_aggregations = co_salv->get_aggregations( ).
    "Add TOTAL for COLUMN
    TRY.
        mo_aggregations->add_aggregation( columnname  = <ls_fieldcat>-fieldname
                                          aggregation = if_salv_c_aggregation=>total ).
      CATCH cx_salv_data_error
            cx_salv_not_found
            cx_salv_existing.
        CONTINUE.
    ENDTRY.

  ENDLOOP.  "Subtotals

  "Rename the fields
  LOOP AT it_fieldcat ASSIGNING <ls_fieldcat>.
    CHECK <ls_fieldcat>-seltext_l IS NOT INITIAL.
    ld_column_name = <ls_fieldcat>-fieldname.
    ld_long_text   = condense( <ls_fieldcat>-seltext_l ).
    ld_med_text    = condense( <ls_fieldcat>-seltext_m ).
    ld_short_text  = condense( <ls_fieldcat>-seltext_s ).
    TRY.
        mo_column ?= mo_columns->get_column( ld_column_name ).

        IF ld_long_text IS NOT INITIAL.
          mo_column->set_long_text( ld_long_text ).
        ENDIF.

        "If the medium text is blank, the long text will be used
        "I only want this when a word gets truncated half way through
        "e.g. 'default load siz' instead of 'default load size'
        IF strlen( ld_long_text ) GT strlen( ld_med_text ) AND
           strlen( ld_long_text ) GT 20 ##NUMBER_OK.
          IF ld_long_text(20)   EQ ld_med_text(20) AND
             ld_long_text+20(1) NE ' '.
            mo_column->set_medium_text( space ).
            mo_column->set_short_text( space ).
            CLEAR ld_short_text.
          ELSE.
            mo_column->set_medium_text( ld_med_text ).
          ENDIF.
        ELSEIF ld_med_text IS NOT INITIAL.
          mo_column->set_medium_text( ld_med_text ).
        ENDIF.

        IF ld_short_text IS NOT INITIAL.
          mo_column->set_short_text( ld_short_text ).
        ENDIF.

      CATCH cx_salv_not_found.
        CONTINUE.
    ENDTRY.
  ENDLOOP.  "Rename the fields

  "There will be some entries which are in the SALV object but not in the
  "field catalogue. We want to hide them.
  lt_salv_list = mo_columns->get( ).

  LOOP AT lt_salv_list ASSIGNING FIELD-SYMBOL(<ls_salv_list>).
    READ TABLE it_fieldcat INTO DATA(ls_fieldcat) WITH KEY fieldname = <ls_salv_list>-columnname.
    IF sy-subrc <> 0.
      "Field in internal table, but must not be displayed
      ld_column_name = ls_fieldcat-fieldname.
      TRY.
          ls_salv_list-r_column->set_technical( if_salv_c_bool_sap=>true ).
        CATCH cx_salv_not_found.
          CONTINUE.
      ENDTRY.
    ENDIF.
  ENDLOOP.

ENDMETHOD.


METHOD convert_sortcat.
* Local Variables
  DATA: ld_sequence TYPE salv_de_sort_sequence,
        ld_position TYPE i,
        ld_group    TYPE salv_de_sort_group.

* Preconditions
  IF it_sortcat[] IS INITIAL.
    RETURN.
  ENDIF.

  mo_sort = co_salv->get_sorts( ).

  LOOP AT it_sortcat ASSIGNING FIELD-SYMBOL(<ls_sortcat>).
    TRY.
        "For some mad reason the RALD sort position is text!
        ld_position = <ls_sortcat>-spos.

        IF <ls_sortcat>-up = abap_true.
          ld_sequence = if_salv_c_sort=>sort_up.
        ELSEIF <ls_sortcat>-down = abap_true.
          ld_sequence = if_salv_c_sort=>sort_down.
        ELSE.
          ld_sequence = if_salv_c_sort=>sort_none.
        ENDIF.

        IF <ls_sortcat>-group = 'UL'.
          ld_group = if_salv_c_sort=>group_with_underline.
        ELSEIF <ls_sortcat>-group = '*'.
          ld_group = if_salv_c_sort=>group_with_newpage.
        ELSE.
          ld_group = if_salv_c_sort=>group_none.
        ENDIF.

        mo_sort->add_sort( columnname = <ls_sortcat>-fieldname
                           position   = ld_position
                           sequence   = ld_sequence
                           subtotal   = <ls_sortcat>-subtot
                           group      = ld_group
                           obligatory = <ls_sortcat>-obligatory ).

      CATCH cx_salv_not_found
            cx_salv_data_error
            cx_salv_existing.
        CONTINUE."With next loop pass
    ENDTRY.
  ENDLOOP.

ENDMETHOD.
ENDCLASS.
