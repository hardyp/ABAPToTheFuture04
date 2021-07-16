class ZCL_4_BC_BOPF_PL_HELPER definition
  public
  create public .

public section.

  methods CONSTRUCTOR
    importing
      !IO_TRANSACTION_MANAGER type ref to /BOBF/IF_TRA_TRANSACTION_MGR
      !IO_SERVICE_MANAGER type ref to /BOBF/IF_TRA_SERVICE_MANAGER
      !IO_OBJECT_CONFIGURATION type ref to /BOBF/IF_FRW_CONFIGURATION .
  methods GET_CHILD_NODE_ROW
    importing
      !ID_OBJECT_KEY type /BOBF/CONF_KEY
      !ID_PARENT_NODE_TYPE type /BOBF/OBM_NODE_KEY
      !ID_CHILD_NODE_TYPE type /BOBF/OBM_ASSOC_KEY
      !ID_INDEX type I default '1'
    returning
      value(RDO_DATA) type ref to DATA
    raising
      /BOBF/CX_FRW .
  methods GET_CHILD_NODE_TABLE
    importing
      !ID_OBJECT_KEY type /BOBF/CONF_KEY
      !ID_PARENT_NODE_TYPE type /BOBF/OBM_NODE_KEY
      !ID_CHILD_NODE_TYPE type /BOBF/OBM_ASSOC_KEY
    returning
      value(RDO_DATA) type ref to DATA
    raising
      /BOBF/CX_FRW .
  methods GET_NODE_ROW
    importing
      !ID_OBJECT_KEY type /BOBF/CONF_KEY
      !ID_NODE_TYPE type /BOBF/OBM_NODE_KEY
      !ID_EDIT_MODE type /BOBF/CONF_EDIT_MODE default /BOBF/IF_CONF_C=>SC_EDIT_READ_ONLY
      !ID_NODE_ROW_NUMBER type I default '1'
    returning
      value(RDO_ROW_DATA) type ref to DATA
    raising
      /BOBF/CX_FRW .
  methods GET_NODE_TABLE
    importing
      !ID_OBJECT_KEY type /BOBF/CONF_KEY
      !ID_NODE_TYPE type /BOBF/OBM_NODE_KEY
      !ID_EDIT_MODE type /BOBF/CONF_EDIT_MODE default /BOBF/IF_CONF_C=>SC_EDIT_READ_ONLY
    returning
      value(RDO_DATA_TABLE) type ref to DATA
    raising
      /BOBF/CX_FRW .
  methods CHANGE_DATA_IN_MEMORY
    importing
      !IT_CHANGES_TO_BE_MADE type /BOBF/T_FRW_MODIFICATION
    raising
      /BOBF/CX_FRW .
  methods CHANGE_DATA_IN_DATABASE
    raising
      /BOBF/CX_FRW .
  class-methods PUT_MESSAGE_IN_BOTTLE
    importing
      !I_HOPE_THAT_SOMEONE_GETS_MY type ref to /BOBF/CM_FRW
    changing
      !CO_BOTTLE type ref to /BOBF/IF_FRW_MESSAGE .
protected section.
private section.

  data MO_TRANSACTION_MANAGER type ref to /BOBF/IF_TRA_TRANSACTION_MGR .
  data MO_SERVICE_MANAGER type ref to /BOBF/IF_TRA_SERVICE_MANAGER .
  data MO_OBJECT_CONFIGURATION type ref to /BOBF/IF_FRW_CONFIGURATION .
ENDCLASS.



CLASS ZCL_4_BC_BOPF_PL_HELPER IMPLEMENTATION.


METHOD change_data_in_database.
*--------------------------------------------------------------------*
* Listing 07.22 - Updating a BOPF Object in the Database
*--------------------------------------------------------------------*
  "Off we go!
  mo_transaction_manager->save(
  IMPORTING eo_message  = DATA(bottle_of_messages)
            ev_rejected = DATA(change_has_been_rejected) ).

  IF change_has_been_rejected EQ abap_false.
    "Happy Path - Database Updated
    RETURN.
  ENDIF.

  RAISE EXCEPTION NEW /bobf/cx_dac( mo_message = bottle_of_messages ).

ENDMETHOD."Change Data in Database / ZCL_4_BC_BOPF_PL_HELPER


METHOD CHANGE_DATA_IN_MEMORY.
*--------------------------------------------------------------------*
* Listing 07.21 - Method to Change the BOPF Data in Memory
*--------------------------------------------------------------------*

  "Change Data in Memory
  mo_service_manager->modify(
  EXPORTING it_modification = it_changes_to_be_made
  IMPORTING eo_change       = DATA(actual_changes_made)
            eo_message      = DATA(bottle_of_messages) ).

  IF xsdbool( actual_changes_made->has_failed_changes( ) ) = abap_false.
    "Data in Memory changed OK (Happy Path)
    RETURN.
  ENDIF.

  IF bottle_of_messages IS NOT BOUND.
    "Should be impossible
    RAISE EXCEPTION NEW zcx_4_monster_exceptions_mc(
    textid = zcx_4_monster_exceptions_mc=>memory_update_failure ).
  ENDIF.

  "CHECK = Returns true if the msg obj has messages with severity error
  IF bottle_of_messages->check( ) EQ abap_false.
    "Warning or Information or Success, let it go
    RETURN.
  ENDIF.

  RAISE EXCEPTION NEW /bobf/cx_dac( mo_message = bottle_of_messages ).

ENDMETHOD."Change Data in Memory/ ZCL_4_BC_BOPF_PL_HELPER


METHOD CONSTRUCTOR.

  mo_transaction_manager  = io_transaction_manager.
  mo_service_manager      = io_service_manager.
  mo_object_configuration = io_object_configuration.

ENDMETHOD.


METHOD get_child_node_row.
  "Local Variables
  DATA: lt_ref_to_data TYPE REF TO data.

  FIELD-SYMBOLS: <lt_data> TYPE INDEX TABLE,
                 <ls_row>  TYPE any.

  "Get the node, the whole node, and nothing but the node
  lt_ref_to_data      = get_child_node_table(
  id_object_key       = id_object_key
  id_parent_node_type = id_parent_node_type
  id_child_node_type  = id_child_node_type ).

  IF lt_ref_to_data IS NOT BOUND.
    RAISE EXCEPTION TYPE /bobf/cx_dac."Data Access Error
  ENDIF.

  "We bind the data table to the return parameter
  ASSIGN lt_ref_to_data->* TO <lt_data>.
  READ TABLE <lt_data> INDEX id_index ASSIGNING <ls_row>.

  IF sy-subrc EQ 0.
    rdo_data = REF #( <ls_row> ).
  ELSE.
    RAISE EXCEPTION TYPE /bobf/cx_dac."Data Access Error
  ENDIF.

ENDMETHOD.


METHOD GET_CHILD_NODE_TABLE.
*--------------------------------------------------------------------*
* Listing 07.07 - Getting the Item Table (Child Node Table)
*--------------------------------------------------------------------*
  "Local Variables
  DATA: node_confguration_data TYPE /bobf/s_confro_node.

  FIELD-SYMBOLS: <result_table> TYPE INDEX TABLE.

  "Find out all about the child node
  mo_object_configuration->get_assoc(
    EXPORTING iv_assoc_key = id_child_node_type
              iv_node_key  = id_parent_node_type
    IMPORTING es_assoc     = DATA(association_data) ).

  IF association_data-target_node IS NOT BOUND.
    RAISE EXCEPTION TYPE /bobf/cx_dac."Error Messages of the data access
  ENDIF.

  "The target node contains the name of the database table
  node_confguration_data = association_data-target_node->*.

  "Now we know that we can bind the result paramater
  "to a dynamic internal table
  CREATE DATA rdo_data TYPE (node_confguration_data-data_table_type).
  ASSIGN rdo_data->* TO <result_table>.

  "Have to put the key in a table so set up a table with one line
  DATA(table_of_object_keys) = VALUE /bobf/t_frw_key( ( key = id_object_key ) ).

  "Off we go!
  mo_service_manager->retrieve_by_association(
    EXPORTING iv_node_key    = id_parent_node_type
              it_key         = table_of_object_keys
              iv_association = id_child_node_type
              iv_fill_data   = abap_true
    IMPORTING eo_message     = DATA(bottle_of_messages)
              et_data        = <result_table> ).

  "Error Handling
  IF bottle_of_messages IS NOT BOUND.
    RETURN.
  ENDIF.

  "Baa Baa Black Sheep, have we any fatal errors?
  IF bottle_of_messages->check( ) EQ abap_false.
    RETURN.
  ENDIF.

  "Data Access Exception
  RAISE EXCEPTION NEW /bobf/cx_dac( mo_message = bottle_of_messages ).

ENDMETHOD."Get Child Node Table


METHOD GET_NODE_ROW.
*--------------------------------------------------------------------*
* Listing 07.05 - Getting a Header Record (Node Row)
*--------------------------------------------------------------------*
  "Local Variables
  DATA ref_to_data_table TYPE REF TO data.

  FIELD-SYMBOLS: <data_table>     TYPE INDEX TABLE,
                 <table_row_data> TYPE any.

  ref_to_data_table = get_node_table(
  id_object_key     = id_object_key
  id_node_type      = id_node_type
  id_edit_mode      = id_edit_mode ).

  IF ref_to_data_table IS NOT BOUND.
    RAISE EXCEPTION TYPE /bobf/cx_dac."Data Access Exception
  ENDIF.

  ASSIGN ref_to_data_table->* TO <data_table>.
  READ TABLE <data_table> INDEX id_node_row_number ASSIGNING <table_row_data>.

  IF sy-subrc EQ 0.
    rdo_row_data = REF #( <table_row_data> ).
  ELSE.
    RAISE EXCEPTION TYPE /bobf/cx_dac."Error Messages of the data access
  ENDIF.

ENDMETHOD."Get Node Row


METHOD get_node_table.
*--------------------------------------------------------------------*
* Listing 07.06 - Getting a Table that Contains the Header Record
*--------------------------------------------------------------------*
  "Local Variables
  FIELD-SYMBOLS: <result_table> TYPE INDEX TABLE.

  "Get all details about the node, what we are interested
  "in the data table name
  mo_object_configuration->get_node(
    EXPORTING iv_node_key = id_node_type
    IMPORTING es_node     = DATA(node_configuration_data) ).

  "Now we know the database table name, we can create a dynamic
  "internal table bound to the returning parameter
  CREATE DATA rdo_data_table TYPE (node_configuration_data-data_table_type).
  ASSIGN rdo_data_table->* TO <result_table>.

  "Retrieve the target node:
  DATA(table_of_object_keys) =
  VALUE /bobf/t_frw_key( ( key = id_object_key ) ).

  "Off we go! We call a standard BOPF class/method
  mo_service_manager->retrieve(
    EXPORTING iv_node_key  = id_node_type
              it_key       = table_of_object_keys
              iv_edit_mode = id_edit_mode
    IMPORTING eo_message   = DATA(bottle_of_messages)
              et_data      = <result_table> ).

  "Error Handling
  IF bottle_of_messages IS NOT BOUND.
    RETURN.
  ENDIF.

  "Baa Baa Black Sheep, have we any fatal errors?
  IF bottle_of_messages->check( ) EQ abap_false.
    RETURN.
  ENDIF.

  "Data Access Exception
  RAISE EXCEPTION NEW /bobf/cx_dac( mo_message = bottle_of_messages ).

ENDMETHOD."Get Node Table


METHOD put_message_in_bottle.

  IF co_bottle IS INITIAL.
    "Create the bottle for our message
    co_bottle = /bobf/cl_frw_factory=>get_message( ).
  ENDIF.

  "Put the message in the bottle
  co_bottle->add_cm( i_hope_that_someone_gets_my ).

ENDMETHOD.
ENDCLASS.
