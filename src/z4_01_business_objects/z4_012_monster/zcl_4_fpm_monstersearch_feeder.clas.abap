class ZCL_4_FPM_MONSTERSEARCH_FEEDER definition
  public
  create public .

public section.

  interfaces IF_FPM_GUIBB .
  interfaces IF_FPM_GUIBB_SEARCH .

  data MS_MONSTER_HEADER type Z4SC_MONSTER_HEADER .
  data MT_MONSTER_HEADERS type Z4_TT_MONSTER_HEADER .
protected section.
private section.
ENDCLASS.



CLASS ZCL_4_FPM_MONSTERSEARCH_FEEDER IMPLEMENTATION.


  METHOD if_fpm_guibb_search~check_config ##NEEDED.

  ENDMETHOD.


  METHOD if_fpm_guibb_search~flush ##NEEDED.

  ENDMETHOD.


  METHOD if_fpm_guibb_search~get_data.
*--------------------------------------------------------------------*
* Listing 12.08: - Get Data Method of Feeder Class
*--------------------------------------------------------------------*
    CLEAR: et_messages,
           ev_search_criteria_changed,
           et_result_list,
           ev_result_list_title,
           ev_field_usage_changed,
           ev_search_criteria_changed.

    IF io_event->mv_event_id NE if_fpm_guibb_search=>fpm_execute_search.
      RETURN.
    ENDIF.

    et_result_list = mt_monster_headers.

  ENDMETHOD.


  METHOD if_fpm_guibb_search~get_default_config ##NEEDED.

  ENDMETHOD.


  METHOD if_fpm_guibb_search~get_definition.

    CLEAR:
    eo_field_catalog_attr,
    et_field_description_attr,
    eo_field_catalog_result,
    et_field_description_result,
    ev_result_table_selection_mode,
    es_message,
    ev_additional_error_info,
    es_options,
    et_action_definition,
    et_special_groups.
*--------------------------------------------------------------------*
* Listing 12.06: - Get_Defintion Method of Feeder Class
*--------------------------------------------------------------------*
    "What are the search criteria going to be?
    eo_field_catalog_attr ?= cl_abap_structdescr=>describe_by_data( ms_monster_header ).

    "What columns are going to be displayed in a table?
    eo_field_catalog_result ?= cl_abap_tabledescr=>describe_by_data( mt_monster_headers ).

    "What are the column headings going to be?
    DATA(table_of_fields) = eo_field_catalog_attr->get_ddic_field_list( ).

    "Nice example of transforming an internal table into a
    "similar one with a slightly different format
    et_field_description_result = VALUE #(
     FOR field_info IN table_of_fields
         ( name = field_info-fieldname
           text = field_info-fieldtext ) ).

  ENDMETHOD.


  METHOD if_fpm_guibb_search~process_event.
*--------------------------------------------------------------------*
* Listing 12.07: - Process_Event Method of Feeder Class
*--------------------------------------------------------------------*
    CLEAR: et_messages,
           ev_result.

    TRY.
        cl_fpm_guibb_search_conversion=>to_abap_select_where_tab(
          EXPORTING
            it_fpm_search_criteria = it_fpm_search_criteria
            iv_table_name          = 'Z4T_MONSTER_HEAD'
          IMPORTING
            et_abap_select_table   = DATA(where_clause_table) ).

      CATCH cx_fpmgb INTO DATA(guibb_error).
        INSERT VALUE #(
        plaintext = guibb_error->get_text( ) )
        INTO TABLE et_messages.
        RETURN.
    ENDTRY.

* In real life I would transform the where clause into
* a table that the monster model could handle
* but to speed things up - direct read!
* In fact I would use the Business Object persistency layer class - BOPF or RAP - and thus the transient fields
* would be filled as well
* Of course in real life I would rather die than use any form of Web Dynpro - BUT LET'S NOT GO THERE!
    SELECT *
      FROM z4t_monster_head
      INTO CORRESPONDING FIELDS OF TABLE mt_monster_headers
      WHERE (where_clause_table) ##too_many_itab_fields.

    IF sy-subrc NE 0.
      RETURN.
    ENDIF.

  ENDMETHOD."Process Event


  METHOD if_fpm_guibb~get_parameter_list ##NEEDED.

  ENDMETHOD.


  METHOD if_fpm_guibb~initialize ##NEEDED.

  ENDMETHOD.
ENDCLASS.
