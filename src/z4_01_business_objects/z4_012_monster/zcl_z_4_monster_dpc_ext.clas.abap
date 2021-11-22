class ZCL_Z_4_MONSTER_DPC_EXT definition
  public
  inheriting from ZCL_Z_4_MONSTER_DPC
  create public .

public section.

  methods CONSTRUCTOR .

  methods /IWBEP/IF_MGW_APPL_SRV_RUNTIME~CREATE_DEEP_ENTITY
    redefinition .
protected section.

  methods MONSTERITEMS_GET_ENTITYSET
    redefinition .
  methods MONSTERS_DELETE_ENTITY
    redefinition .
  methods MONSTERS_GET_ENTITY
    redefinition .
  methods MONSTERS_GET_ENTITYSET
    redefinition .
private section.

  data MO_MONSTER_MODEL type ref to ZCL_4_MONSTER_MODEL .
ENDCLASS.



CLASS ZCL_Z_4_MONSTER_DPC_EXT IMPLEMENTATION.


  METHOD /iwbep/if_mgw_appl_srv_runtime~create_deep_entity.
*--------------------------------------------------------------------*
* In traditional DYNPRO programs you update the header table and the
* item table and any other related tables in a single LUW
* The obvious example is VBAK / VBAP / VBEP / VBKD etc which all get
* updated together when a sales document is saved.
* Naturally we want to do the exact same thing when processing a
* request to create a new business object via SAP Gateway
* The terminology here is CREATE_DEEP_ENTITY
*--------------------------------------------------------------------*
* The bulk of the comments below are copied verbatim from the standard
* SAP example code
*--------------------------------------------------------------------*
    "Local Variables
    TYPES: monster_header_row TYPE z4sc_monster_header_ex,
           monster_item_row   TYPE z4sc_monster_items_ex,
           monster_item_list  TYPE STANDARD TABLE OF monster_item_row.

    CONSTANTS: monster_set_name TYPE string VALUE `MONSTERS`.

    DATA: new_monster_header_entry TYPE monster_header_row,
          new_monster_item_stack   TYPE monster_item_list,
          new_monster_item_entry   LIKE LINE OF new_monster_item_stack,
          entityset_name           TYPE string,
          " The following structure is used to receive the header and multiple
          " rows acquired by the call to method io_data_provider->read_entry_data.
          " That method will move values, column by column, from the sending
          " structure to this receiving structure through a process that dynamically
          " identifies both sender column and receiver column. Accordingly, there
          " are constraints that must be observed when defining this receiving
          " structure, otherwise one or more columns of the the receiving structure
          " will not be populated with counterpart values from the corresponding
          " sending structure.
          " This structure needs to be defined as a deep structure, meaning that
          " it contains at least one field that is itself defined as an internal
          " table.  The structure is to be arranged as follows:
          "   o The name of this structure can be any name at all.
          "   o The next dependent fields of this structure must contain fields with
          "       the same name and type as those fields defined as the properties
          "       of the primary entity type.
          "   o The next dependent field following the final primary entity type field
          "       must be the same name as the navigation property associating the
          "       primary entity type to its dependent entity type.  This field itself
          "       must be defined as an internal table using a structure containing
          "       fields with the same name and type as those fields defined as the
          "       properties of the dependent entity type.
          "   o The sequence of the primary entity type and dependent entity type
          "       fields within this structure is not important - only the names of
          "       the fields must match their counterpart entity type properties.
          "       Indeed, the internal table defined for holding the rows of the
          "       dependent entity type can be interspersed between two other fields
          "       describing the properties of the primary entity type.  For the sake
          "       of easy maintenance and debugging, it is recommended to define these
          "       fields in the following sequence:
          "         - all fields correlating to properties of the primary entity type
          "         - the name of an internal table correlating to the navigation property
          "             from the primary entity type to the dependent entity type, with
          "             the name of its corresponding structure being one that contains
          "             the names of the properties of the dependent entity type
          BEGIN OF monster_with_items.
            "   Next entries must indicate the names and respective types of the
            "     properties defined for the primary entity type:
            INCLUDE TYPE monster_header_row.
            "   Next entry must indicate the name of the navigation property from the
            "     primary entity type to the dependent entity type:
            DATA: frommonstertomonsteritem
                  "   Next entry must indicate an internal table describing the names and
                  "     respective types of the properties defined for the dependent entity type:
                  TYPE STANDARD TABLE OF monster_item_row,
          END OF monster_with_items.

    CLEAR er_deep_entity.

    entityset_name = to_upper( io_tech_request_context->get_entity_set_name( ) ).

    CASE entityset_name.
      WHEN monster_set_name.
        io_data_provider->read_entry_data( IMPORTING es_data = monster_with_items ).
      WHEN OTHERS.
        "Notify the front end of the problem encountered:
        DATA(message_container) = /iwbep/if_mgw_conv_srv_runtime~get_message_container( ).
        message_container->add_message_text_only(
          iv_msg_type = 'E'
          iv_msg_text = 'Unexpected Entity Set Type'(001) ).
        RETURN.
    ENDCASE.

    new_monster_header_entry = CORRESPONDING #( monster_with_items ).

    LOOP AT monster_with_items-frommonstertomonsteritem ASSIGNING FIELD-SYMBOL(<frommonstertomonsteritem>).
      new_monster_item_entry = CORRESPONDING #( <frommonstertomonsteritem> ).
      INSERT new_monster_item_entry INTO TABLE new_monster_item_stack.
    ENDLOOP.

    TRY.
        "As always, outsource the actual work to a re-usable business object class
        mo_monster_model->create_monster_record(
           VALUE #( header = new_monster_header_entry
                    items  = new_monster_item_stack ) ).
      CATCH zcx_4_monster_exceptions_mc INTO DATA(exception).
        "Undo any work that might already have been performed during the execution of this method:
        ROLLBACK WORK."#EC CI_ROLLBACK
        "Notify the front end of the problem encountered:
        message_container = /iwbep/if_mgw_conv_srv_runtime~get_message_container( ).
        message_container->add_message_text_only(
          iv_msg_type = 'E'
          iv_msg_text = CONV #( exception->get_text( ) ) ).
        RETURN.
    ENDTRY.

    "Provide this newly created monster with items in the response:
    copy_data_to_ref(
      EXPORTING is_data = monster_with_items
      CHANGING  cr_data = er_deep_entity ).

*--------------------------------------------------------------------*
* The 20 foor tall monster cannot turn invisible at will
*--------------------------------------------------------------------*
  ENDMETHOD.


  METHOD constructor.
*-----------------------------------------------------------------------*
* Listing 09.01: - Embedding Monster Model Class into Data Provider Class
*-----------------------------------------------------------------------*
    super->constructor( ).

    mo_monster_model = NEW #( ).

  ENDMETHOD.


  METHOD monsteritems_get_entityset.
*--------------------------------------------------------------------*
* Listing 09.03: - Coding Method to Get All Monster Items
*--------------------------------------------------------------------*
    DATA: monster_header LIKE LINE OF et_entityset.

    CLEAR: es_response_context.

    io_tech_request_context->get_converted_source_keys(
      IMPORTING es_key_values = monster_header ).

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = monster_header-monster_number
      IMPORTING
        output = monster_header-monster_number.

    TRY.
        et_entityset =
        mo_monster_model->derive_monster_record(
        monster_header-monster_number )-items.

      CATCH zcx_4_monster_exceptions_mc INTO DATA(monster_exception).
        "Create Bottle...
        DATA(message_container) =
        /iwbep/if_mgw_conv_srv_runtime~get_message_container( ).

        "Put message in bottle....
        "Use CONV to convert from string to TEXT220
        message_container->add_message_text_only(
          iv_msg_type = /iwbep/if_message_container=>gcs_message_type-error
          iv_msg_text = CONV #( monster_exception->get_text( ) ) ).

        "Throw bottle into sea....
        RAISE EXCEPTION NEW /iwbep/cx_mgw_busi_exception(
          message_container = message_container ).

    ENDTRY.

  ENDMETHOD."MonsterItems Get Entityset


  METHOD monsters_delete_entity.
*--------------------------------------------------------------------*
* Listing 09.05: - Passing Exception Back to Calling Application
*--------------------------------------------------------------------*
    DATA: monster_number TYPE z4de_monster_number.

    DATA(monster_keys) = io_tech_request_context->get_keys( ).
    monster_number = monster_keys[ name = 'MONSTER_NUMBER' ]-value.

    DATA(message_container) =
     /iwbep/if_mgw_conv_srv_runtime~get_message_container( ).

    message_container->add_message_text_only(
    iv_msg_type = /iwbep/if_message_container=>gcs_message_type-error
    iv_msg_text = |{ 'Monster No'(003) } { monster_number ALPHA = OUT } { 'does not want to be deleted'(002) }| ).

    RAISE EXCEPTION NEW /iwbep/cx_mgw_busi_exception( message_container = message_container ).

  ENDMETHOD."Monsters Delete Entity


  METHOD monsters_get_entity.
*--------------------------------------------------------------------*
* Listing 09.04: Monsters Get Entity Method
*--------------------------------------------------------------------*
    DATA: monster_number TYPE z4de_monster_number.

    CLEAR: es_response_context.

    DATA(monster_keys) = io_tech_request_context->get_keys( ).
    monster_number = monster_keys[ name = 'MONSTER_NUMBER' ]-value.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = monster_number
      IMPORTING
        output = monster_number.

    TRY.
        er_entity =
        mo_monster_model->derive_monster_record( monster_number )-header.

        "If the time stamps are empty you get a short dump
        IF er_entity-createdat IS INITIAL.
          GET TIME STAMP FIELD er_entity-createdat.
        ENDIF.

        IF er_entity-lastchangedat IS INITIAL.
          GET TIME STAMP FIELD er_entity-lastchangedat.
        ENDIF.

      CATCH zcx_4_monster_exceptions_mc INTO DATA(monster_exception).
        "Create Bottle...
        DATA(message_container) =
        /iwbep/if_mgw_conv_srv_runtime~get_message_container( ).

        "Put message in bottle....
        "Use CONV to convert from string to TEXT220
        message_container->add_message_text_only(
          iv_msg_type = /iwbep/if_message_container=>gcs_message_type-error
          iv_msg_text = CONV #( monster_exception->get_text( ) ) ).

        "Throw bottle into sea....
        RAISE EXCEPTION NEW /iwbep/cx_mgw_busi_exception(
          message_container = message_container ).

    ENDTRY.

  ENDMETHOD.


  METHOD monsters_get_entityset.
*--------------------------------------------------------------------*
* Listing 09.02: - Monsters Get Entity Set – Method Implementation
*--------------------------------------------------------------------*
    CLEAR: et_entityset,
           es_response_context.

    "See if we have any selection criteria passed in
    DATA(odata_filter) = io_tech_request_context->get_filter( ).
    DATA(odata_filter_select_options) =
    odata_filter->get_filter_select_options( ).

    "We adapt the ODATA structure to the generic COSEL
    "selection structure used throughout ABAP
    DATA: abap_select_options TYPE ztt_bc_coseltab.

    IF odata_filter_select_options[] IS NOT INITIAL.
      LOOP AT odata_filter_select_options
       ASSIGNING FIELD-SYMBOL(<odata_sel_option_structure>).
        APPEND INITIAL LINE TO abap_select_options
        ASSIGNING FIELD-SYMBOL(<abap_select_option>).
        <abap_select_option>-field =
        <odata_sel_option_structure>-property.
        LOOP AT <odata_sel_option_structure>-select_options
          ASSIGNING FIELD-SYMBOL(<odata_select_option>)."#EC CI_NESTED
          <abap_select_option>-option = <odata_select_option>-option.
          <abap_select_option>-sign   = <odata_select_option>-sign.
          <abap_select_option>-low    = <odata_select_option>-low.
          <abap_select_option>-high   = <odata_select_option>-high.
        ENDLOOP."Selection options for field being queried
      ENDLOOP."List of fields being queried
    ELSE.
      "No selection criteria have been passed in
      "Set selection criteria so that all records are returned
      APPEND INITIAL LINE TO abap_select_options
      ASSIGNING <abap_select_option>.
      <abap_select_option>-field   = 'MONSTER_NUMBER'.
      <abap_select_option>-option  = 'GT'.
      <abap_select_option>-sign    = 'I'.
      <abap_select_option>-low     = '0000000001'.
    ENDIF."Were any selection criteria passed in?

    "The below needs buffering, in case of client side paging
    "i.e. don't re-read the database when the user pages down
    DATA(table_of_filtered_monsters) =
    mo_monster_model->retrieve_headers_by_attribute( abap_select_options ).

    "Extract any instructions how to sort the result list from
    "the incoming request
    DATA(odata_sort_order_fields) =
    io_tech_request_context->get_orderby( ).

    "Now we build a dynamic table which we will then use to
    "sort the result a la SORTCAT in the ALV
    DATA: abap_sort_order_fields TYPE abap_sortorder_tab.

    LOOP AT odata_sort_order_fields
      ASSIGNING FIELD-SYMBOL(<odata_sort_order>).
      APPEND INITIAL LINE TO abap_sort_order_fields
      ASSIGNING FIELD-SYMBOL(<abap_sort_order>).
      <abap_sort_order>-name = <odata_sort_order>-property.
      IF <odata_sort_order>-order = 'desc'.
        <abap_sort_order>-descending = abap_true.
      ENDIF.
      "Some header fields are defined as CHAR
      IF <odata_sort_order>-property = 'SCARINESS' OR
         <odata_sort_order>-property = 'EVILNESS'  OR
         <odata_sort_order>-property = 'COLOR'     OR
         <odata_sort_order>-property = 'BRAIN_SIZE'.
        <abap_sort_order>-astext = abap_true.
      ENDIF.
    ENDLOOP."Sort Order from Incoming Request

    SORT table_of_filtered_monsters BY (abap_sort_order_fields).

    "Query the incoming URL to see if we have to start from
    "a specific point, and how many rows to display
    DATA(paging_skip) = io_tech_request_context->get_skip( ).

    "The URL may contain text like "$skip=5"
    IF paging_skip IS NOT INITIAL.
      DATA(start_row) = paging_skip + 1.
    ELSE.
      start_row = 1.
    ENDIF.

    DATA(paging_top) = io_tech_request_context->get_top( ).

    "The URL may contain text like "$top=10"
    IF paging_top IS NOT INITIAL.
      DATA(end_row) = paging_skip + paging_top.
    ELSE.
      end_row = lines( table_of_filtered_monsters ).
    ENDIF.

    "Export the final result
    LOOP AT table_of_filtered_monsters FROM start_row TO end_row
      ASSIGNING FIELD-SYMBOL(<filtered_monster_header>).
      APPEND INITIAL LINE TO et_entityset
      ASSIGNING FIELD-SYMBOL(<monster_header_record>).
      <monster_header_record> = CORRESPONDING #( <filtered_monster_header> ).
      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          input  = <monster_header_record>-monster_number
        IMPORTING
          output = <monster_header_record>-monster_number.
    ENDLOOP.

* Change security settings to allow local testing as per
* http://scn.sap.com/community/gateway/blog/2014/09/23/solve-cors-with-gateway-and-chrome
    DATA(http_name_value_pair) = VALUE ihttpnvp(
    name = 'Access-Control-Allow-Origin'
    value = '*' ) ##NO_TEXT.

    /iwbep/if_mgw_conv_srv_runtime~set_header( http_name_value_pair ).

  ENDMETHOD."Monsters Get EntitySet
ENDCLASS.
