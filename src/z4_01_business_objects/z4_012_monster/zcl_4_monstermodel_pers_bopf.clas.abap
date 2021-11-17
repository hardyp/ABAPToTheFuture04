class ZCL_4_MONSTERMODEL_PERS_BOPF definition
  public
  create private

  global friends ZCL_4_MONSTER_FACTORY .

public section.

  interfaces ZIF_4_MONSTERMODEL_PERS_LAYER .

  aliases CREATE_MONSTER_RECORD
    for ZIF_4_MONSTERMODEL_PERS_LAYER~CREATE_MONSTER_RECORD .
  aliases DERIVE_MONSTER_RECORD_4_UPDATE
    for ZIF_4_MONSTERMODEL_PERS_LAYER~DERIVE_MONSTER_RECORD_4_UPDATE .
  aliases GET_BOPF_KEY_4_MONSTER_NUMBER
    for ZIF_4_MONSTERMODEL_PERS_LAYER~GET_BOPF_KEY_4_MONSTER_NUMBER .
  aliases RETRIEVE_HEADERS_BY_ATTRIBUTE
    for ZIF_4_MONSTERMODEL_PERS_LAYER~DERIVE_HEADERS_BY_ATTRIBUTE .
  aliases RETRIEVE_MONSTER_RECORD
    for ZIF_4_MONSTERMODEL_PERS_LAYER~DERIVE_MONSTER_RECORD .
  aliases UPDATE_MONSTER_RECORD
    for ZIF_4_MONSTERMODEL_PERS_LAYER~UPDATE_MONSTER_RECORD .

  methods CONSTRUCTOR .
protected section.
private section.

  data MO_TRANSACTION_MANAGER type ref to /BOBF/IF_TRA_TRANSACTION_MGR .
  data MO_SERVICE_MANAGER type ref to /BOBF/IF_TRA_SERVICE_MANAGER .
  data MO_OBJECT_CONFIGURATION type ref to /BOBF/IF_FRW_CONFIGURATION .
  data MO_BOPF_PL_HELPER type ref to ZCL_4_BC_BOPF_PL_HELPER .

  methods EXTERNAL_HEADERS_VIEW
    importing
      !IT_BOPF_HEADER_RECORDS type Z4TT_MONSTER_HEADER
    returning
      value(RT_EXTERNAL_HEADER_RECORDS) type Z4TT_MONSTER_HEADER_EX .
  methods EXTERNAL_HEADER_VIEW
    importing
      !IS_BOPF_HEADER_RECORD type Z4SC_MONSTER_HEADER
    returning
      value(RS_EXTERNAL_HEADER_RECORD) type Z4SC_MONSTER_HEADER_EX .
  methods EXTERNAL_ITEM_VIEW
    importing
      !IT_BOPF_ITEM_RECORDS type Z4TT_MONSTER_ITEMS
    returning
      value(RT_EXTERNAL_ITEM_RECORDS) type Z4TT_MONSTER_ITEMS_EX .
ENDCLASS.



CLASS ZCL_4_MONSTERMODEL_PERS_BOPF IMPLEMENTATION.


  METHOD constructor.
*--------------------------------------------------------------------*
* Listing 07.02: - BOPF Specific Persistency Layer Constructor
*--------------------------------------------------------------------*
    TRY.

        mo_transaction_manager =
        /bobf/cl_tra_trans_mgr_factory=>get_transaction_manager( ).

        mo_service_manager =
        /bobf/cl_tra_serv_mgr_factory=>get_service_manager(
        zif_4_monster_c=>sc_bo_key ).

        mo_object_configuration =
        /bobf/cl_frw_factory=>get_configuration(
        zif_4_monster_c=>sc_bo_key ).

        mo_bopf_pl_helper = NEW #(
            io_transaction_manager  = mo_transaction_manager
            io_service_manager      = mo_service_manager
            io_object_configuration = mo_object_configuration ).

      CATCH /bobf/cx_frw INTO DATA(bopf_exception).
        RAISE EXCEPTION NEW zcx_4_monster_exceptions_mc(
          textid = bopf_exception->if_t100_message~t100key ).
    ENDTRY.

  ENDMETHOD.


  METHOD external_headers_view.

    rt_external_header_records[] = CORRESPONDING #( it_bopf_header_records[] ).

  ENDMETHOD.


  METHOD external_header_view.

    rs_external_header_record = CORRESPONDING #( is_bopf_header_record ).

  ENDMETHOD.


  METHOD external_item_view.

    rt_external_item_records[] = CORRESPONDING #( it_bopf_item_records[] ).

  ENDMETHOD.


METHOD zif_4_monstermodel_pers_layer~create_monster_record.
*--------------------------------------------------------------------*
* Listing 07.18: - Creating New Monster Record
*--------------------------------------------------------------------*
* Local variables
  DATA: all_changes_to_be_made     TYPE /bobf/t_frw_modification,
        bopf_monster_header_record TYPE REF TO z4sc_monster_header.

  FIELD-SYMBOLS: <bopf_monster_header_record> TYPE z4sc_monster_header.

*------------------------------------------------------------*
* Create Header Record
*------------------------------------------------------------*
  TRY.
      "The data component of the change to be made is typed as TYPE REF TO DATA
      CREATE DATA bopf_monster_header_record.

      ASSIGN bopf_monster_header_record->* TO <bopf_monster_header_record>.

      <bopf_monster_header_record> = CORRESPONDING #( is_monster_record-header ).

      "I've got a brand new pair of roller skates, you've got a  brand new key
      <bopf_monster_header_record>-key = /bobf/cl_frw_factory=>get_new_key( ).

      APPEND INITIAL LINE TO all_changes_to_be_made
      ASSIGNING FIELD-SYMBOL(<change_to_be_made>).
      <change_to_be_made>-node = zif_4_monster_c=>sc_node-monster_header.
      <change_to_be_made>-change_mode = /bobf/if_frw_c=>sc_modify_create.
      <change_to_be_made>-key         = bopf_monster_header_record->key.
      <change_to_be_made>-data        = bopf_monster_header_record.

*------------------------------------------------------------*
* Time for the item table
*------------------------------------------------------------*
      DATA: bopf_monster_item_record TYPE REF TO z4sc_monster_items.

      FIELD-SYMBOLS: <bopf_monster_item_record> TYPE z4sc_monster_items.

      LOOP AT is_monster_record-items ASSIGNING FIELD-SYMBOL(<monster_item_record_ex>).

        CREATE DATA bopf_monster_item_record.

        ASSIGN bopf_monster_item_record->* TO <bopf_monster_item_record>.

        <bopf_monster_item_record> = CORRESPONDING #( <monster_item_record_ex> ).
        <bopf_monster_item_record>-key = /bobf/cl_frw_factory=>get_new_key( ).

        APPEND INITIAL LINE TO all_changes_to_be_made
        ASSIGNING <change_to_be_made>.
        <change_to_be_made>-node        = zif_4_monster_c=>sc_node-monster_items.
        <change_to_be_made>-change_mode = /bobf/if_frw_c=>sc_modify_create.
        <change_to_be_made>-source_node = zif_4_monster_c=>sc_node-monster_header.
        <change_to_be_made>-association =
        zif_4_monster_c=>sc_association-monster_header-monster_items.
        <change_to_be_made>-source_key = bopf_monster_header_record->key.
        <change_to_be_made>-key        = bopf_monster_item_record->key.
        <change_to_be_made>-data       = bopf_monster_item_record.

      ENDLOOP."Monster items

*------------------------------------------------------------*
* Here we go!
*------------------------------------------------------------*
      mo_bopf_pl_helper->change_data_in_memory( all_changes_to_be_made ).

      mo_bopf_pl_helper->change_data_in_database( ).

    CATCH /bobf/cx_frw INTO DATA(bobf_exception).
      "Need to extract actual error information from the BOBF error
      "Both BOBF exception and monster exception implement the
      "T100 interface; we can just pass the information on directly
      RAISE EXCEPTION TYPE zcx_4_monster_exceptions_mc
        EXPORTING
          textid = bobf_exception->if_t100_message~t100key.
  ENDTRY.

ENDMETHOD."Create Monster Record / ZCL_4_MONSTERMODEL_PERS_BOPF


  METHOD zif_4_monstermodel_pers_layer~derive_headers_by_attribute.

    "Local variables
    DATA bopf_selection_parameters TYPE /bobf/t_frw_query_selparam.

    "Adapt generic query structure to specific BOPF query structure
    LOOP AT it_selections ASSIGNING FIELD-SYMBOL(<generic_selection_parameter>).

      APPEND INITIAL LINE TO bopf_selection_parameters
      ASSIGNING FIELD-SYMBOL(<bopf_selection_parameter>).

      <bopf_selection_parameter>-attribute_name = <generic_selection_parameter>-field.
      <bopf_selection_parameter>-sign           = <generic_selection_parameter>-sign.
      <bopf_selection_parameter>-option         = <generic_selection_parameter>-option.
      <bopf_selection_parameter>-low            = <generic_selection_parameter>-low.
      <bopf_selection_parameter>-high           = <generic_selection_parameter>-high.

    ENDLOOP.

    IF bopf_selection_parameters[] IS INITIAL.
      RETURN.
    ENDIF.

    DATA: bopf_monster_header_records TYPE z4tt_monster_header.

    mo_service_manager->query(
      EXPORTING
        iv_query_key            = zif_4_monster_c=>sc_query-monster_header-select_by_elements
        it_selection_parameters = bopf_selection_parameters
        iv_fill_data            = abap_true
      IMPORTING
        et_data                 = bopf_monster_header_records ).

    rt_monster_headers = external_headers_view( bopf_monster_header_records ).

    LOOP AT rt_monster_headers ASSIGNING FIELD-SYMBOL(<ls_headers>).
      "Gateway dumps if ay date field is initial
      IF <ls_headers>-createdat IS INITIAL.
        GET TIME STAMP FIELD <ls_headers>-createdat.
      ENDIF.
      IF <ls_headers>-lastchangedat IS INITIAL.
        GET TIME STAMP FIELD <ls_headers>-lastchangedat.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.


  METHOD zif_4_monstermodel_pers_layer~derive_monster_record.
*--------------------------------------------------------------------*
* Listing 07.03: - Model Class Data Retrieval Method
*--------------------------------------------------------------------*
* We could do this the traditional way via a SELECT statement
* But that’s just what they’re EXPECTING us to do!
* Instead, let’s go down the BOPF path...
*--------------------------------------------------------------------*
    TRY.
        "To get a BOPF object, we need a key, not a number
        DATA(monster_key) = get_bopf_key_4_monster_number( id_monster_number ).

        "The header record lives in the header(root) node
        DATA(bopf_monster_header) = CAST z4sc_monster_header(
                mo_bopf_pl_helper->get_node_row(
                id_object_key      = monster_key
                id_node_type       = zif_4_monster_c=>sc_node-monster_header
                id_node_row_number = 1
                id_edit_mode       = /bobf/if_conf_c=>sc_edit_read_only ) ).

        FIELD-SYMBOLS: <bopf_monster_header> TYPE z4sc_monster_header.

        ASSIGN bopf_monster_header->* TO <bopf_monster_header>.

        rs_monster_record-header = external_header_view( <bopf_monster_header> ).

        "The item table records live in one or more child nodes of
        "the header-level node
        DATA(bopf_monster_items) = CAST z4tt_monster_items(
        mo_bopf_pl_helper->get_child_node_table(
        id_object_key       = monster_key
        id_parent_node_type = zif_4_monster_c=>sc_node-monster_header
        id_child_node_type  =
        zif_4_monster_c=>sc_association-monster_header-monster_items ) ).

        FIELD-SYMBOLS: <bopf_monster_items> TYPE z4tt_monster_items.

        ASSIGN bopf_monster_items->* TO <bopf_monster_items>.

        rs_monster_record-items = external_item_view( <bopf_monster_items> ).

      CATCH /bobf/cx_frw INTO DATA(bopf_exception).
        RAISE EXCEPTION NEW zcx_4_monster_exceptions_mc(
          textid = bopf_exception->if_t100_message~t100key ).
    ENDTRY.

  ENDMETHOD."Derive Monster Record ZCL_4_MONSTERMODEL_PERS_BOPF


  METHOD zif_4_monstermodel_pers_layer~derive_monster_record_4_update.

    TRY.
        "To get a BOPF object, we need a key, not a number
        DATA(monster_key) = get_bopf_key_4_monster_number( id_monster_number ).

        "The header record lives in the header(root) node
        DATA(bopf_monster_header) = CAST z4sc_monster_header(
                mo_bopf_pl_helper->get_node_row(
                id_object_key      = monster_key
                id_node_type       = zif_4_monster_c=>sc_node-monster_header
                id_node_row_number = 1
                id_edit_mode       = /bobf/if_conf_c=>sc_edit_exclusive ) ).

        FIELD-SYMBOLS: <bopf_monster_header> TYPE z4sc_monster_header.

        ASSIGN bopf_monster_header->* TO <bopf_monster_header>.

        rs_monster_record-header = external_header_view( <bopf_monster_header> ).

        "The item table records live in one or more child nodes of
        "the header-level node
        DATA(bopf_monster_items) = CAST z4tt_monster_items(
        mo_bopf_pl_helper->get_child_node_table(
        id_object_key       = monster_key
        id_parent_node_type = zif_4_monster_c=>sc_node-monster_header
        id_child_node_type  =
        zif_4_monster_c=>sc_association-monster_header-monster_items ) ).

        FIELD-SYMBOLS: <bopf_monster_items> TYPE z4tt_monster_items.

        ASSIGN bopf_monster_items->* TO <bopf_monster_items>.

        rs_monster_record-items = external_item_view( <bopf_monster_items> ).

      CATCH /bobf/cx_frw INTO DATA(bopf_exception).
        RAISE EXCEPTION NEW zcx_4_monster_exceptions_mc(
          textid = bopf_exception->if_t100_message~t100key ).
    ENDTRY.

  ENDMETHOD.


  METHOD ZIF_4_MONSTERMODEL_PERS_LAYER~GET_BOPF_KEY_4_MONSTER_NUMBER.
*--------------------------------------------------------------------------*
* Listing 07.04: - Using a Custom Query to turn a Monster Number into a Key
*--------------------------------------------------------------------------*
    "Local Variables
    DATA: bopf_selection_parameters TYPE /bobf/t_frw_query_selparam.

    "This builds the dynamic WHERE clause for the database read
    APPEND INITIAL LINE TO bopf_selection_parameters
    ASSIGNING FIELD-SYMBOL(<bopf_selection_parameter>).

    <bopf_selection_parameter> = VALUE #(
    attribute_name =
    zif_4_monster_c=>sc_query_attribute-monster_header-select_by_elements-monster_number
    sign   = 'I'
    option = 'EQ'
    low    = id_monster_number ).

    "This builds a fully dynamic SQL Statement
    mo_service_manager->query(
      EXPORTING
        iv_query_key            =
        zif_4_monster_c=>sc_query-monster_header-select_by_elements
        it_selection_parameters = bopf_selection_parameters
      IMPORTING
        et_key                  = DATA(monster_keys) ).

    "The result is always a table, but we have a unique key, so
    "as someone once said THERE CAN BE ONLY ONE!
    READ TABLE monster_keys INDEX 1
    ASSIGNING FIELD-SYMBOL(<monster_key>).

    IF sy-subrc EQ 0.
      rd_bopf_key = <monster_key>-key.
    ENDIF.
*--------------------------------------------------------------------*
* One Haunted Castle can be found at 134.6 degrees Longtitude
*--------------------------------------------------------------------*
  ENDMETHOD.


METHOD zif_4_monstermodel_pers_layer~update_monster_record.
*--------------------------------------------------------------------*
* Listing 07.21: - Updating (Changing) Existing BOPF Record
*--------------------------------------------------------------------*
  "Local variables
  DATA: all_changes_to_be_made TYPE /bobf/t_frw_modification.

  FIELD-SYMBOLS: <bopf_monster_header> TYPE z4sc_monster_header.

  DATA: bopf_monster_header TYPE REF TO z4sc_monster_header.

*------------------------------------------------------------*
* Update header
*------------------------------------------------------------*
  TRY.
      CREATE DATA bopf_monster_header.

      ASSIGN bopf_monster_header->* TO <bopf_monster_header>.

      <bopf_monster_header> = CORRESPONDING #( is_monster_record-header ).

      APPEND INITIAL LINE TO all_changes_to_be_made
      ASSIGNING FIELD-SYMBOL(<change_to_be_made>).
      <change_to_be_made>-node        = zif_4_monster_c=>sc_node-monster_header.
      <change_to_be_made>-change_mode = /bobf/if_frw_c=>sc_modify_update.
      <change_to_be_made>-key         = bopf_monster_header->key.
      <change_to_be_made>-data        = bopf_monster_header.

*------------------------------------------------------------*
* Update items
*------------------------------------------------------------*
      DATA: bopf_monster_item_record TYPE REF TO z4sc_monster_items.

      FIELD-SYMBOLS: <bopf_monster_item_record> TYPE z4sc_monster_items.

      LOOP AT is_monster_record-items ASSIGNING FIELD-SYMBOL(<monster_item_record_ex>).

        CREATE DATA bopf_monster_item_record.

        ASSIGN bopf_monster_item_record->* TO <bopf_monster_item_record>.

        <bopf_monster_item_record> = CORRESPONDING #( <monster_item_record_ex> ).

        APPEND INITIAL LINE TO all_changes_to_be_made
        ASSIGNING <change_to_be_made>.
        <change_to_be_made>-node        = zif_4_monster_c=>sc_node-monster_items.
        <change_to_be_made>-change_mode = /bobf/if_frw_c=>sc_modify_update.
        <change_to_be_made>-source_node = zif_4_monster_c=>sc_node-monster_header.
        <change_to_be_made>-association =
        zif_4_monster_c=>sc_association-monster_header-monster_items.
        <change_to_be_made>-source_key = bopf_monster_header->key.
        <change_to_be_made>-key        = bopf_monster_item_record->key.
        <change_to_be_made>-data       = bopf_monster_item_record.

      ENDLOOP."Monster items

      mo_bopf_pl_helper->change_data_in_memory( all_changes_to_be_made ).

      mo_bopf_pl_helper->change_data_in_database( ).

    CATCH /bobf/cx_frw INTO DATA(bopf_exception).
      RAISE EXCEPTION NEW zcx_4_monster_exceptions_mc(
        textid = bopf_exception->if_t100_message~t100key ).
  ENDTRY.

ENDMETHOD."Update Monster Record / ZCL_4_MONSTER_MODEL_PERS_BOPF
ENDCLASS.
