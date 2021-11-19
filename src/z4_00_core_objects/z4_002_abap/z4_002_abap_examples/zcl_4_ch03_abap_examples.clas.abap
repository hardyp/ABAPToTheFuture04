class ZCL_4_CH03_ABAP_EXAMPLES definition
  public
  create public .

public section.
protected section.
private section.

  methods L03_01_QUERY_WITHOUT_VALUE  ##RELAX.
  methods L03_02_QUERY_WITH_VALUE  ##RELAX.
  methods L03_03_VALUE_TO_FILL_ITAB  ##RELAX.
  methods L03_04_FILLING_ITAB_HARDCODE  ##RELAX.
  methods L03_05_FILLING_ITAB_VIA_FOR  ##RELAX.
  methods L03_06_SHORT_LIVED_VARIABLES  ##RELAX.
  methods L03_07_BASIC_ENUMERATION  ##RELAX.
  methods L03_08_NON_INTEGER_ENUMERATION  ##RELAX.
  methods L03_09_STRUCTURED_ENUMERATION  ##RELAX.
  methods L03_10_ALPHA_INPUT_OUTPUT   ##RELAX
    raising
      /IWBEP/CX_MGW_BUSI_EXCEPTION .
  methods L03_11_ALPHA_FOMATTING_OPTION   ##RELAX
    raising
      /IWBEP/CX_MGW_BUSI_EXCEPTION .
  methods L03_12_METHOD_GUESS_DATA_TYPE  ##RELAX.
  methods L03_13_METHOD_INLINE_DECS  ##RELAX.
  methods L03_14_METHOD_CALL_W_HELPER   ##RELAX
    raising
      CX_AMC_ERROR .
  methods L03_15_METHOD_CALL_W_CONV   ##RELAX
    raising
      CX_AMC_ERROR .
  methods L03_16_TYPE_REF_TO_OLD
    importing
      !IS_CTX type /BOBF/S_FRW_CTX_DET
      !IO_MODIFY type ref to /BOBF/IF_FRW_MODIFY .
  methods L03_17_ABAP_TRUE .
  methods L03_18_NO_ABAP_TRUE .
  methods L03_19_IF_WRONG_CHECK .
  methods L03_20_BOOLC
    returning
      value(RT_RESULT) type BAPIRET2_TAB .
  methods L03_21_NEGATIVE_TESTING .
  methods L03_22_XSDBOOL .
  methods L03_23_CASE_STATEMENT
    importing
      !ID_EDIT_MODE type LRM_CRUD_MODE .
  methods L03_24_SWITCH_STATEMENT
    importing
      !ID_EDIT_MODE type LRM_CRUD_MODE .
  methods L03_26_CASE_STATEMENT
    changing
      !CS_MONSTER_HEADER type Z4SC_MONSTER_HEADER .
  methods L03_27_COND_STATEMENT
    changing
      !CS_MONSTER_HEADER type Z4SC_MONSTER_HEADER .
  methods L03_28_COND_STATEMENT_UPDATED
    changing
      !CS_MONSTER_HEADER type Z4SC_MONSTER_HEADER_EX .
  methods L03_29_INTERNAL_TABLES_WA .
  methods L03_30_INTERNAL_TABLES_FS .
  methods L03_31_READING_TABLE_LINE_OLD .
  methods L03_32_READING_TABLE_LINE_NEW .
  methods L03_33_READING_TABLE_OPTIONAL .
  methods L03_34_COPYING_ITABS_OLD .
  methods L03_35_COPYING_ITABS_NEW .
  methods L03_36_COPY_TO_HASHED_ITAB
    importing
      !ID_ORDER_NUMBER type Z4DE_MONSTER_ORDER_NUMBER
      !ID_ITEM_NUMBER type Z4DE_MONSTER_ORDER_ITEM_NUMBER .
  methods L03_37_DEEP_STRUCTURE .
  methods L03_38_CORRESPONDING_OLD .
  methods L03_39_CORRESPONDING_OLD_V2 .
  methods L03_40_CORRESPONDING_NEW .
  methods L03_41_CORRESPONDING_DYNAMIC
    returning
      value(RS_BEST_BRAINS) type Z4ST_MONSTER_BEST_BRAINS .
  methods L03_42_GETTING_ROW_NO_OLD .
  methods L03_43_GETTING_ROW_NO_NEW .
  methods L03_44_GETTING_ROW_NO_NEWER .
  methods L03_45_ITAB_ROW_EXISTS_OLD .
  methods L03_46_ITAB_ROW_EXISTS_NEW .
  methods L03_47_ITAB_ROW_EXISTS_NEWER .
  methods L03_48_REDUCE .
  methods L03_49_GROUP_BY .
  methods L03_50_TABLE_EXTRACTION_OLD .
  methods L03_51_TABLE_EXTRACTION_NEW .
  methods L03_52_FAE_DATABASE .
  methods L03_53_FAE_ITAB .
  methods L03_54_VIRTUAL_SORTING .
  methods L03_55_VIRTUAL_SORTING_PART_2 .
  methods L03_56_QUERY_DDIC_NO_CAST .
  methods L03_57_QUERY_DDIC_WITH_CAST .
  methods L03_58_FIND_A_SUBCLASS_OLD
    importing
      !IO_SALV_ADAPTER type ref to CL_SALV_ADAPTER
    returning
      value(RO_ALV_GRID) type ref to CL_GUI_ALV_GRID .
  methods L03_59_FIND_A_SUBCLASS_NEW
    importing
      !IO_SALV_ADAPTER type ref to CL_SALV_ADAPTER
    returning
      value(RO_ALV_GRID) type ref to CL_GUI_ALV_GRID .
  methods L03_60_FIND_A_SUBCLASS_NEW_V2
    importing
      !IO_SALV_ADAPTER type ref to CL_SALV_ADAPTER
    returning
      value(RO_ALV_GRID) type ref to CL_GUI_ALV_GRID .
  methods L03_61_FUNCTIONAL_METHOD_PLUS .
ENDCLASS.



CLASS ZCL_4_CH03_ABAP_EXAMPLES IMPLEMENTATION.


  METHOD l03_01_query_without_value.
*-----------------------------------------------------------------------------*
* Listing 03.01:  Database Query without VALUE
*-----------------------------------------------------------------------------*
* This is the BAD example so is not found anywhere in the monster applications
*-----------------------------------------------------------------------------*
    DATA:
      monster_type_range            TYPE ztt_bc_coseltab,
      monster_type_selection_option LIKE LINE OF monster_type_range.

    monster_type_selection_option-field  = 'EVILNESS'.
    monster_type_selection_option-option = 'EQ'.
    monster_type_selection_option-sign   = 'I'.
    monster_type_selection_option-low    = 'EVIL'."Evil Monster
    APPEND monster_type_selection_option TO monster_type_range.

    monster_type_selection_option-field  = 'EVILNESS'.
    monster_type_selection_option-option = 'EQ'.
    monster_type_selection_option-sign   = 'I'.
    monster_type_selection_option-low    = 'VERY'."Very Evil Monster
    APPEND monster_type_selection_option TO monster_type_range.

    DATA(monster)         = NEW zcl_4_monster_model( ).
    DATA(monster_headers) =
    monster->retrieve_headers_by_attribute( monster_type_range ).

  ENDMETHOD.


  METHOD l03_02_query_with_value.
*--------------------------------------------------------------------*
* Listing 03.02:  Database Query with VALUE
*--------------------------------------------------------------------*
    DATA(monster_headers) =
    NEW zcl_4_monster_model( )->retrieve_headers_by_attribute(
    VALUE ztt_bc_coseltab(
          ( field  = 'EVILNESS' )
          ( option = 'EQ' )
          ( sign   = 'I' )
          ( low    = 'EVIL' )     "Evil Monster
          ( low    = 'VERY' ) ) )."Very Evil Monster

  ENDMETHOD.


  METHOD l03_03_value_to_fill_itab.
*----------------------------------------------------------------------------*
* Listing 03.03:  Using VALUE to Fill Multiple Lines of Internal Table
*----------------------------------------------------------------------------*
* This is used in the unit tests for program ZALL_PAIRS
*----------------------------------------------------------------------------*
* You can see it in the LCL_MOCK_PERSISTENCY_LAYER class in method
* GET_DATA
*---------------------------------------------------------------------------*
    TYPES: BEGIN OF l_typ_configuration,
             variable_name  TYPE string,
             count          TYPE i,
             possible_value TYPE string,
           END OF   l_typ_configuration.

    TYPES: l_tt_configuration TYPE STANDARD TABLE OF l_typ_configuration
                              WITH KEY variable_name count.

    DATA(lt_configuration) = VALUE l_tt_configuration(
    ( variable_name = 'Monster Model'(002) count = 1 possible_value = 'BTNK' ) "Bolts Through Neck
    ( variable_name = 'Monster Model'(002) count = 2 possible_value = 'KLKL' ) "Killer Klown
    ( variable_name = 'Monster Model'(002) count = 3 possible_value = 'ISDD' ) "Ice Skating Dead
    ( variable_name = 'Evilness'(003)      count = 1 possible_value = 'EVIL' )
    ( variable_name = 'Evilness'(003)      count = 2 possible_value = 'VERY' )
    ( variable_name = 'Brain Size'(004)    count = 1 possible_value = 'SMALL' )
    ( variable_name = 'Brain Size'(004)    count = 2 possible_value = 'MICRO' )
    ( variable_name = 'Scariness'(005)     count = 1 possible_value = 'NORM' )
    ( variable_name = 'Scariness'(005)     count = 2 possible_value = 'BANK' )
    ( variable_name = 'Usage'(006)         count = 1 possible_value = 'NORM' )
    ( variable_name = 'Usage'(006)         count = 2 possible_value = 'PLUM' )"IT'S THE PLUMBER!!!!
    ( variable_name = 'Color'(007)         count = 1 possible_value = 'BLUE' )
    ( variable_name = 'Color'(007)         count = 2 possible_value = 'GREEN' ) ).

  ENDMETHOD.


  METHOD l03_04_filling_itab_hardcode.
*--------------------------------------------------------------------*
* Listing 03.04:  Fill Internal Table using Hard Coding
*--------------------------------------------------------------------*
* This is the bad example so is not used in any monster applications
*--------------------------------------------------------------------*
    DATA(table_of_monsters) = VALUE z4tt_monster_header(
   ( name = 'JIMMY' monster_number = 1 )
   ( name = 'ROLF ' monster_number = 2 ) ).

  ENDMETHOD.


  METHOD l03_05_filling_itab_via_for.
*--------------------------------------------------------------------*
* Listing 03.05:  Filling Internal Tables from Other Tables
*--------------------------------------------------------------------*
    SELECT *
    FROM z4t_monster_head
    INTO TABLE @DATA(all_monsters).

    DATA(neurotic_monsters) = VALUE z4tt_monster_header(
     FOR monster_details IN all_monsters WHERE ( sanity_percentage < 20 )
         ( name           = monster_details-name
           monster_number = monster_details-monster_number ) ).

  ENDMETHOD.


  METHOD l03_06_short_lived_variables.
*--------------------------------------------------------------------*
* Listing 03.06:  Creating Short-Lived Variables
*--------------------------------------------------------------------*
    SELECT *
          FROM z4t_monster_head
          INTO TABLE @DATA(all_monsters).

    DATA(iterator) = NEW lcl_weapon_iterator( ).

    DO lines( all_monsters[] ) TIMES.
      DATA(arming_description) = CONV string(
        LET weapon_name  = iterator->get_next_weapon( )
            monster_name = all_monsters[ sy-index ]-name
            date_string  =
           |{ sy-datum } DATE = USER|
        IN |{ 'Monster'(018) } { monster_name } { 'was issued a'(019) } | &&
           |{ weapon_name } { 'on'(020) } { date_string }| ).
      MESSAGE arming_description TYPE 'I'.
    ENDDO.

  ENDMETHOD.


  METHOD l03_07_basic_enumeration.
*--------------------------------------------------------------------*
* Listing 03.07:  Basic Enumeration
*--------------------------------------------------------------------*
    TYPES: BEGIN OF ENUM monster_brain_size,
             normal,
             small,
             micro,
           END OF ENUM monster_brain_size.

    DATA: brain_size TYPE monster_brain_size.

  ENDMETHOD.


  METHOD l03_08_non_integer_enumeration.
*--------------------------------------------------------------------*
* Listing 03.08:  Non-Integer Enumeration
*--------------------------------------------------------------------*
    TYPES:
      brain_size TYPE c LENGTH 8,
      BEGIN OF ENUM monster_brain_size BASE TYPE brain_size,
        normal VALUE IS INITIAL,
        small  VALUE 'SMALL',
        micro  VALUE 'MICRO',
      END OF ENUM monster_brain_size.

    DATA(brain_size) = NEW monster_brain_size( small ).

  ENDMETHOD.


  METHOD l03_09_structured_enumeration.
*--------------------------------------------------------------------*
* Listing 03.09:  Enumeration with Structure
*--------------------------------------------------------------------*
    TYPES: BEGIN OF ENUM monster_brain_size
             STRUCTURE brain_sizes,
             normal,
             small,
             micro,
           END OF ENUM monster_brain_size
           STRUCTURE brain_sizes.

    DATA(brain_size) = brain_sizes-small.

* One Haunted Castle can be found at 53 degrees Latitude
  ENDMETHOD.


  METHOD l03_10_alpha_input_output.
*-----------------------------------------------------------------------------*
* Listing 03.10:  Removing and Adding Leading Zeroes by Function Call
*-----------------------------------------------------------------------------*
* This is the bad example so is not found anywhere in the monster applications
*-----------------------------------------------------------------------------*

    DATA: monster_number TYPE z4de_monster_number VALUE '0000000001'.

    DATA(segw_service) = NEW zcl_z_4_monster_dpc_ext( ).

    DATA(message_container) =
    segw_service->/iwbep/if_mgw_conv_srv_runtime~get_message_container( ).

    "Remove Leading Zeroes ready for Output to User
    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
      EXPORTING
        input  = monster_number
      IMPORTING
        output = monster_number.

    message_container->add_message_text_only(
    EXPORTING
    iv_msg_type = /iwbep/if_message_container=>gcs_message_type-error
    iv_msg_text = |Monster No { monster_number } | &&
                  |does not want to be deleted| ).

    "Add back leading zeroes in case you need to read database
    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = monster_number
      IMPORTING
        output = monster_number.

    RAISE EXCEPTION NEW /iwbep/cx_mgw_busi_exception(
        message_container = message_container ).

  ENDMETHOD.


  METHOD l03_11_alpha_fomatting_option.
*-----------------------------------------------------------------------*
* Listing 03.11:  Removing Leading Zeroes via Formatting Option
*-----------------------------------------------------------------------*
* Used in method MONSTERS_DELETE_ENTITY of Class ZCL_Z_4_MONSTER_DPC_EXT
*-----------------------------------------------------------------------*

    DATA: monster_number TYPE z4de_monster_number VALUE '0000000001'.

    DATA(segw_service) = NEW zcl_z_4_monster_dpc_ext( ).

    DATA(message_container) =
    segw_service->/iwbep/if_mgw_conv_srv_runtime~get_message_container( ).

    message_container->add_message_text_only(
    EXPORTING
    iv_msg_type = /iwbep/if_message_container=>gcs_message_type-error
    iv_msg_text = |Monster No { monster_number ALPHA = OUT } does not want to be deleted| ).

    RAISE EXCEPTION NEW /iwbep/cx_mgw_busi_exception(
        message_container = message_container ).

  ENDMETHOD.


  METHOD l03_12_method_guess_data_type.
*-------------------------------------------------------------------------------------*
* Listing 03.12:  Declaring Variables with (Hopefully) Same Types as Method Parameters
*-------------------------------------------------------------------------------------*
* This is the bad example so is not found anywhere in the monster applications
*-------------------------------------------------------------------------------------*
    DATA: changes_to_be_made  TYPE /bobf/t_frw_modification,
          actual_changes_made TYPE REF TO /bobf/if_tra_change,
          bottle_of_messages  TYPE REF TO /bobf/if_frw_message.

    DATA(bopf_service_manager) =
         /bobf/cl_tra_serv_mgr_factory=>get_service_manager(
         zif_4_monster_c=>sc_bo_key ).

    "Change Data in Memory
    bopf_service_manager->modify(
    EXPORTING it_modification = changes_to_be_made
    IMPORTING eo_change       = actual_changes_made
              eo_message      = bottle_of_messages ).

  ENDMETHOD.


  METHOD l03_13_method_inline_decs.
*------------------------------------------------------------------------------*
* Listing 03.13:  Using Inline Declarations to Avoid Possible Type Mismatches
*------------------------------------------------------------------------------*
* This is used in method CHANGE_DATA_IN_MEMORY of Class ZCL_4_BC_BOPF_PL_HELPER
*------------------------------------------------------------------------------*
    DATA: changes_to_be_made TYPE /bobf/t_frw_modification.

    DATA(bopf_service_manager) =
         /bobf/cl_tra_serv_mgr_factory=>get_service_manager(
         zif_4_monster_c=>sc_bo_key ).

    "Change Data in Memory
    bopf_service_manager->modify(
    EXPORTING it_modification = changes_to_be_made
    IMPORTING eo_change       = DATA(actual_changes_made)
              eo_message      = DATA(bottle_of_messages) ).

  ENDMETHOD.


  METHOD l03_14_method_call_w_helper.
*--------------------------------------------------------------------*
* Listing 03.14:  Moving Variable into Helper Variable
*--------------------------------------------------------------------*
* As an example the incoming Castle Number came from an external system
* in the form of NUMC4, but our method wants a CHAR60
*--------------------------------------------------------------------*
    DATA: helper        TYPE c LENGTH 60,
          castle_number TYPE n LENGTH 4 VALUE '0001'.

    helper = castle_number.

    "In the below the EXTENSION_ID is CHAR60
    DATA(message_bottle) = CAST if_amc_message_producer_pcp(
        cl_amc_channel_manager=>create_message_producer(
          i_application_id       = 'ZAMC_4_MONSTERS'
          i_channel_id           = '/monsters'
          i_channel_extension_id = helper ) ).

  ENDMETHOD.


  METHOD l03_15_method_call_w_conv.
*---------------------------------------------------------------------------------*
* Listing 03.15:  Converting String with Constructor Variable
*---------------------------------------------------------------------------------*
* This is used in Program Z4_MONSTER_ATROCITY_DUE_LIST in method ALLOCATE_MONSTER
*---------------------------------------------------------------------------------*
    DATA: castle_number TYPE n LENGTH 4 VALUE '0001'.

    "In the below the EXTENSION_ID is CHAR60
    DATA(message_bottle) = CAST if_amc_message_producer_pcp(
        cl_amc_channel_manager=>create_message_producer(
          i_application_id       = 'ZAMC_4_MONSTERS'
          i_channel_id           = '/monsters'
          i_channel_extension_id = CONV #( castle_number ) ) ).

  ENDMETHOD.


  METHOD l03_16_type_ref_to_old.
*--------------------------------------------------------------------*
* Listing 03.16:  Filling TYPE REF TO DATA Parameter
*--------------------------------------------------------------------*
* This is used in method /BOBF/IF_FRW_DETERMINATION~EXECUTE of
* class ZCL_4_D_MONSTERHEADER_TEXTS
*--------------------------------------------------------------------*
    DATA: bopf_monster_header_records TYPE z4tt_monster_header,
          header_record_reference     TYPE REF TO data.

    LOOP AT bopf_monster_header_records INTO DATA(bopf_monster_header_record).
      "Before 7.4
      CREATE DATA header_record_reference LIKE bopf_monster_header_record.
      GET REFERENCE OF bopf_monster_header_record INTO header_record_reference.

      "After 7.4
      header_record_reference = REF #( bopf_monster_header_record ).

      "IS_DATA is type REF TO DATA
      io_modify->update(
        iv_node = is_ctx-node_key
        iv_key  = bopf_monster_header_record-key
        is_data = header_record_reference ).

    ENDLOOP.

  ENDMETHOD.


  METHOD l03_17_abap_true.
*--------------------------------------------------------------------*
* Listing 03.17:  ABAP_TRUE
*--------------------------------------------------------------------*
    DATA(monster) = zcl_4_monster_model=>get_instance( '0000000001' ).

    IF monster->is_scary( ) = abap_true.
      MESSAGE 'Oh No! Send for the Fire Brigade!'(008) TYPE 'I'.
    ENDIF.

  ENDMETHOD.


  METHOD l03_18_no_abap_true.
*--------------------------------------------------------------------*
* Listing 03.18:  Omitting ABAP_TRUE
*--------------------------------------------------------------------*
    DATA(monster) =
    zcl_4_monster_model=>get_instance( '0000000001' ).

    IF monster->is_scary( ).
      MESSAGE 'Oh No! Send for the Fire Brigade!'(008) TYPE 'I'.
    ENDIF.

* Each of the five suspect Monsters is a different COLOR
  ENDMETHOD.


  METHOD l03_19_if_wrong_check.
*--------------------------------------------------------------------*
* Listing 03.19:  IF Statement without Proper Check
*--------------------------------------------------------------------*
    DATA(monster) = NEW zcl_4_monster_model( ).

    IF monster->wants_to_blow_up_world( ).
      DATA(massive_atom_bomb) = NEW lcl_atom_bomb( ).
      massive_atom_bomb->explode( ).
    ENDIF.

  ENDMETHOD.


  METHOD l03_20_boolc.
*--------------------------------------------------------------------*
* Listing 03.20:  BOOLC
*--------------------------------------------------------------------*
* Do some groovy things
* Get some results

* Postconditions
    zcl_dbc=>ensure( that = 'A result table is returned'(009)
    which_is_true_if = boolc( rt_result[] IS NOT INITIAL ) ).

  ENDMETHOD.


  METHOD l03_21_negative_testing.
*--------------------------------------------------------------------*
* Listing 03.21:  Testing for Negative
*--------------------------------------------------------------------*
    DATA: empty_table TYPE STANDARD TABLE OF z4t_monster_head.

    IF boolc( empty_table[] IS NOT INITIAL ) = abap_false.
      WRITE:/ 'This table is empty'(010).
    ELSE.
      WRITE:/ 'This table is as full as full can be'(011).
    ENDIF.
    IF boolc( 1 = 2 ) = abap_false.
      WRITE:/ '1 does not equal 2'(012).
    ELSE.
      WRITE:/ '1 equals 2, and the world is made of snow'(013).
    ENDIF.

  ENDMETHOD.


  METHOD l03_22_xsdbool.
*--------------------------------------------------------------------*
* Listing 03.22:  Using XSDBOOL for Correct Logic Test Results
*--------------------------------------------------------------------*
    DATA: empty_table TYPE STANDARD TABLE OF z4t_monster_head.

    "Then do the same using XSDBOOL.
    IF xsdbool( empty_table[] IS NOT INITIAL ) = abap_false.
      WRITE:/ 'This table is empty'(010).
    ELSE.
      WRITE:/ 'This table is as full as full can be'(011).
    ENDIF.
    IF xsdbool( 1 = 2 ) = abap_false.
      WRITE:/ '1 does not equal 2'(012).
    ELSE.
      WRITE:/ '1 equals 2, and the world is made of snow'(013).
    ENDIF.

  ENDMETHOD.


  METHOD l03_23_case_statement.
*--------------------------------------------------------------------*
* Listing 03.23:  Filling in Variable Using CASE Statement
*--------------------------------------------------------------------*
    "Use adapter pattern to translate human readable CRUD standard to
    "the BOPF equivalent
    DATA: bopf_edit_mode TYPE /bobf/conf_edit_mode.

    CASE id_edit_mode.
      WHEN 'R'."Read
        bopf_edit_mode = /bobf/if_conf_c=>sc_edit_read_only.
      WHEN 'U'."Update
        bopf_edit_mode = /bobf/if_conf_c=>sc_edit_exclusive.
      WHEN OTHERS.
        "Unexpected Situation
        RAISE EXCEPTION TYPE zcx_4_monster_exceptions.
    ENDCASE.

  ENDMETHOD.


  METHOD L03_24_SWITCH_STATEMENT.
*--------------------------------------------------------------------*
* Listing 03.24:  Filling in Variable Using SWITCH Statement
*--------------------------------------------------------------------*
* Use adapter pattern to translate human readable CRUD
* standard values to the BOPF equivalent
    DATA(bopf_edit_mode) = SWITCH /bobf/conf_edit_mode( id_edit_mode
      WHEN 'R' THEN /bobf/if_conf_c=>sc_edit_read_only  "Read
      WHEN 'U' THEN /bobf/if_conf_c=>sc_edit_exclusive  "Update
      ELSE THROW zcx_4_monster_exceptions( ) ).           "Unexpected Situation

  ENDMETHOD.


  METHOD l03_26_case_statement.
*--------------------------------------------------------------------*
* Listing 03.26:  CASE Statement to Evaluate Monster Sanity
*--------------------------------------------------------------------*
* This is the bad example so is not used in any monster applications
*--------------------------------------------------------------------*
    "Fill the Sanity Description
    CASE cs_monster_header-sanity_percentage.
      WHEN 5.
        cs_monster_header-sanity_description = 'VERY SANE'.
      WHEN 4.
        cs_monster_header-sanity_description = 'SANE'.
      WHEN 3.
        cs_monster_header-sanity_description = 'SLIGHTLY MAD'.
      WHEN 2.
        cs_monster_header-sanity_description = 'VERY MAD'.
      WHEN 1.
        cs_monster_header-sanity_description = 'BONKERS'.
      WHEN OTHERS.
        cs_monster_header-sanity_description = 'RENAMES SAP PRODUCTS'.
    ENDCASE.

  ENDMETHOD.


  METHOD l03_27_cond_statement.
*--------------------------------------------------------------------*
* Listing 03.27:  Using COND Constructor Operator
*--------------------------------------------------------------------*
* Used in method DERIVE_HEADER_FIELDS of class ZCL_4_MONSTER_MODEL
*--------------------------------------------------------------------*
    "Fill the Sanity Description
    cs_monster_header-sanity_description =
    COND #(
    WHEN cs_monster_header-sanity_percentage > 75 THEN 'VERY SANE'
    WHEN cs_monster_header-sanity_percentage > 50 THEN 'SANE'
    WHEN cs_monster_header-sanity_percentage > 25 THEN 'SLIGHTLY MAD'
    WHEN cs_monster_header-sanity_percentage > 12 THEN 'VERY MAD'
    WHEN cs_monster_header-sanity_percentage > 1  THEN 'BONKERS'
    ELSE 'RENAMES SAP PRODUCTS' ).

* In one Haunted Castle, in the Chapel, beside the Great Hall, the voices of two men are often heard talking.
* It is never possible to follow their words, and they stop talking if one makes serious efforts to trace them
  ENDMETHOD.


  METHOD l03_28_cond_statement_updated.
*--------------------------------------------------------------------*
* Listing 03.28:  COND Constructor Operator with Updated Logic
*--------------------------------------------------------------------*
    DATA: day TYPE char10.
    day = 'Tuesday'(014)."Lenny Henry!
    "Fill the Sanity Description
    cs_monster_header-sanity_description =
    COND #(
    WHEN cs_monster_header-sanity_percentage = 5 THEN 'VERY SANE'
    WHEN cs_monster_header-sanity_percentage = 4 THEN 'SANE'
    WHEN cs_monster_header-sanity_percentage = 3 THEN 'SLIGHTLY MAD'
    WHEN cs_monster_header-sanity_percentage = 2 THEN 'VERY MAD'
    WHEN cs_monster_header-sanity_percentage = 1 AND
         day = 'Tuesday'(014)                    THEN 'HAVING AN OFF DAY'
    WHEN cs_monster_header-sanity_percentage = 1 THEN 'BONKERS'
    ELSE 'RENAMES SAP PRODUCTS' ).

  ENDMETHOD.


  METHOD l03_29_internal_tables_wa.
*--------------------------------------------------------------------*
* Listing 03.29:  Reading into Work Area and Looping through Table
*--------------------------------------------------------------------*
    DATA: table_of_monsters TYPE STANDARD TABLE OF z4t_monster_head.

    READ TABLE table_of_monsters
    WITH KEY color = 'RED'
    INTO DATA(red_monster_details).

    LOOP AT table_of_monsters INTO DATA(loopy_monster_details) ##into_ok.
    ENDLOOP.

  ENDMETHOD.


  METHOD l03_30_internal_tables_fs.
*--------------------------------------------------------------------*
* Listing 03.30:  Field Symbols for Work Area
*--------------------------------------------------------------------*
    DATA: table_of_monsters TYPE STANDARD TABLE OF z4t_monster_head.

    READ TABLE table_of_monsters
    WITH KEY color = 'RED'
    ASSIGNING FIELD-SYMBOL(<red_monster_details>).

    LOOP AT table_of_monsters ASSIGNING FIELD-SYMBOL(<loopy_monster_details>).
    ENDLOOP.

  ENDMETHOD.


  METHOD l03_31_reading_table_line_old.
*--------------------------------------------------------------------*
* Listing 03.31:  Reading Line from Internal Table before 7.4
*--------------------------------------------------------------------*
* Local Variables
    DATA: monster_name      TYPE z4de_monster_name VALUE 'JIMMY',
          table_of_monsters TYPE STANDARD TABLE OF z4t_monster_head,
          monster_details   LIKE LINE OF table_of_monsters,
          monster           TYPE REF TO zif_4_monster_model.

    READ TABLE table_of_monsters INTO monster_details
    WITH KEY name = monster_name.

    monster = zcl_4_monster_model=>get_instance( monster_details-monster_number ).

  ENDMETHOD.


  METHOD l03_32_reading_table_line_new.
*--------------------------------------------------------------------*
* Listing 03.32:  Reading Line from Internal Table after 7.40
*--------------------------------------------------------------------*
* Local Variables
    DATA: monster_name      TYPE z4de_monster_name VALUE 'HUBERT',
          table_of_monsters TYPE STANDARD TABLE OF z4t_monster_head.

    DATA(monster) = zcl_4_monster_model=>get_instance( table_of_monsters[ monster_name ]-monster_number  ).

  ENDMETHOD.


  METHOD l03_33_reading_table_optional.
*--------------------------------------------------------------------*
* Listing 03.33:  Reading Internal Table with OPTIONAL/DEFAULT
*--------------------------------------------------------------------*
* Local Variables
    DATA: monster_name      TYPE z4de_monster_name VALUE 'JIMMY',
          table_of_monsters TYPE STANDARD TABLE OF z4t_monster_head.

    DATA(message1) =
    |{ monster_name }{ '''s Monster Number is'(022) } | &&
    |{ VALUE #( table_of_monsters[ monster_name ]-monster_number OPTIONAL ) }|.

    DATA(message2) =
    |{ monster_name }{ '''s Monster Number is'(022) } | &&
    |{ VALUE #( table_of_monsters[ monster_name ]-monster_number DEFAULT '9999999999' ) }|.

  ENDMETHOD.


  METHOD l03_34_copying_itabs_old.
*--------------------------------------------------------------------*
* Listing 03.34:  Moving One Table to Another before 7.4
*--------------------------------------------------------------------*
* Local Variables
    DATA: green_monsters TYPE STANDARD TABLE OF z4t_monster_head,
          blue_monsters  TYPE STANDARD TABLE OF z4t_monster_head.

    FIELD-SYMBOLS:
      <green_monsters> LIKE LINE OF green_monsters,
      <blue_monsters>  LIKE LINE OF blue_monsters.

    LOOP AT green_monsters ASSIGNING <green_monsters>.
      APPEND INITIAL LINE TO blue_monsters
      ASSIGNING <blue_monsters>.
      MOVE-CORRESPONDING <green_monsters> TO <blue_monsters>.
      CLEAR <blue_monsters>-evilness.
      <blue_monsters>-early_age_strength =
      <green_monsters>-strength.
    ENDLOOP.

  ENDMETHOD.


  METHOD l03_35_copying_itabs_new.
*--------------------------------------------------------------------*
* Listing 03.35:  Moving One Table to Another in 7.4
*--------------------------------------------------------------------*
* Local Variables
    DATA: green_monsters TYPE STANDARD TABLE OF z4t_monster_head,
          blue_monsters  TYPE STANDARD TABLE OF z4t_monster_head.

    green_monsters = CORRESPONDING #(
    blue_monsters
    MAPPING early_age_strength = strength
    EXCEPT evilness ).

  ENDMETHOD.


  METHOD l03_36_copy_to_hashed_itab.
*--------------------------------------------------------------------*
* Listing 03.36: Moving Corresponding Fields to a HASHED Table
*--------------------------------------------------------------------*
* Local Variables
    DATA: monster_order_items TYPE HASHED TABLE OF z4t_order_items
                              WITH UNIQUE KEY order_number order_item.

    DATA(changed_item) = VALUE z4t_order_items(
    order_number     = '0000000001'
    order_item       = '000010'
    task_description = 'Do the Boogoloo' ).

    "HASHED table
    READ TABLE monster_order_items ASSIGNING FIELD-SYMBOL(<order_item>)
    WITH TABLE KEY order_number = id_order_number
                   order_item   = id_item_number.

    IF sy-subrc = 0.
      "Cannot over-write key fields of a Hashed Table
      <order_item> = CORRESPONDING #( BASE ( <order_item> ) changed_item
      EXCEPT order_number order_item ).
    ELSE.
      INSERT changed_item INTO TABLE monster_order_items.
    ENDIF.

* Each of the five suspect Monsters has a different HOBBY and each of those hobbies is really boring
  ENDMETHOD.


  METHOD l03_37_deep_structure.
*--------------------------------------------------------------------*
* Listing 03.37:  Deep Structure
*--------------------------------------------------------------------*
    TYPES: BEGIN OF l_typ_monsters,
             monster_number TYPE z4de_monster_number,
             monster_name   TYPE z4de_monster_name,
             t_items        TYPE z4tt_monster_items,
           END OF l_typ_monsters.
    DATA: monster_table TYPE STANDARD TABLE OF l_typ_monsters.

  ENDMETHOD.


  METHOD L03_38_CORRESPONDING_OLD.
*--------------------------------------------------------------------*
* Listing 03.38:  MOVE-CORRESPONDING Attempt
*--------------------------------------------------------------------*
    TYPES: BEGIN OF l_typ_european_monsters,
             monster_name      TYPE string,
             monster_iban_code TYPE string,
           END OF l_typ_european_monsters.

    TYPES: l_tt_european_monsters
    TYPE HASHED TABLE OF l_typ_european_monsters
    WITH UNIQUE KEY monster_name.

    DATA: iban_code_record TYPE l_typ_european_monsters.

    TYPES: BEGIN OF l_typ_european_results,
             laboratory TYPE string,
             t_result   TYPE l_tt_european_monsters,
           END OF l_typ_european_results.

    TYPES: l_tt_european_results
    TYPE STANDARD TABLE OF l_typ_european_results.

    DATA: european_result  TYPE l_typ_european_results,
          european_results TYPE l_tt_european_results.

    TYPES: BEGIN OF l_typ_us_monsters,
             monster_name         TYPE string,
             monster_lockbox_code TYPE string,
           END OF l_typ_us_monsters.

    TYPES: l_tt_us_monsters TYPE HASHED TABLE OF l_typ_us_monsters
                            WITH UNIQUE KEY monster_name.

    TYPES: BEGIN OF l_typ_us_results,
             laboratory TYPE string,
             t_result   TYPE l_tt_us_monsters,
           END OF l_typ_us_results.

    TYPES: l_tt_us_results TYPE STANDARD TABLE OF l_typ_us_results.

    DATA: us_result  TYPE l_typ_us_results,
          us_results TYPE l_tt_us_results.

    european_result-laboratory         = 'SECRET LABORATORY 51'.
    iban_code_record-monster_name      = 'FRED'.
    iban_code_record-monster_iban_code = 'AL47212110090000000235698741'.
    INSERT iban_code_record INTO TABLE european_result-t_result.

*--------------------------------------------------------------------*
* Important Bit - This DOES NOT work
*--------------------------------------------------------------------*
    MOVE-CORRESPONDING european_result TO us_result.

  ENDMETHOD.


  METHOD l03_39_corresponding_old_v2.
*--------------------------------------------------------------------*
* Listing 03.39:  Copying between Internal Tables with Different Structures before 7.4
*--------------------------------------------------------------------*
    TYPES: BEGIN OF l_typ_european_monsters,
             monster_name      TYPE string,
             monster_iban_code TYPE string,
           END OF l_typ_european_monsters.

    TYPES: l_tt_european_monsters
    TYPE HASHED TABLE OF l_typ_european_monsters
    WITH UNIQUE KEY monster_name.

    DATA: iban_code_record TYPE l_typ_european_monsters.

    TYPES: BEGIN OF l_typ_european_results,
             laboratory TYPE string,
             t_result   TYPE l_tt_european_monsters,
           END OF l_typ_european_results.

    TYPES: l_tt_european_results
    TYPE STANDARD TABLE OF l_typ_european_results.

    DATA: european_result  TYPE l_typ_european_results,
          european_results TYPE l_tt_european_results.

    TYPES: BEGIN OF l_typ_us_monsters,
             monster_name         TYPE string,
             monster_lockbox_code TYPE string,
           END OF l_typ_us_monsters.

    TYPES: l_tt_us_monsters TYPE HASHED TABLE OF l_typ_us_monsters
                            WITH UNIQUE KEY monster_name.

    TYPES: BEGIN OF l_typ_us_results,
             laboratory TYPE string,
             t_result   TYPE l_tt_us_monsters,
           END OF l_typ_us_results.

    TYPES: l_tt_us_results TYPE STANDARD TABLE OF l_typ_us_results.

    DATA: us_result  TYPE l_typ_us_results,
          us_results TYPE l_tt_us_results.

    european_result-laboratory         = 'SECRET LABORATORY 51'.
    iban_code_record-monster_name      = 'FRED'.
    iban_code_record-monster_iban_code = 'AL47212110090000000235698741'.
    INSERT iban_code_record INTO TABLE european_result-t_result.
    INSERT european_result  INTO TABLE european_results.

*--------------------------------------------------------------------*
* Important Bit - This Works - Takes 4 Lines
*--------------------------------------------------------------------*
    LOOP AT european_results ASSIGNING FIELD-SYMBOL(<european_result>).
      APPEND INITIAL LINE TO us_results ASSIGNING FIELD-SYMBOL(<us_result>).
      MOVE-CORRESPONDING <european_result> TO <us_result>.
    ENDLOOP.

  ENDMETHOD.


  METHOD l03_40_corresponding_new.
*--------------------------------------------------------------------*
* Listing 03.40:  Copying between Internal Tables with Different Structures in 7.4
*--------------------------------------------------------------------*
* Local Variables
    TYPES: BEGIN OF l_typ_european_monsters,
             monster_name      TYPE string,
             monster_iban_code TYPE string,
           END OF l_typ_european_monsters.

    TYPES: l_tt_european_monsters
    TYPE HASHED TABLE OF l_typ_european_monsters
    WITH UNIQUE KEY monster_name.

    DATA: iban_code_record TYPE l_typ_european_monsters.

    TYPES: BEGIN OF l_typ_european_results,
             laboratory TYPE string,
             t_result   TYPE l_tt_european_monsters,
           END OF l_typ_european_results.

    TYPES: l_tt_european_results
    TYPE STANDARD TABLE OF l_typ_european_results.

    DATA: european_result  TYPE l_typ_european_results,
          european_results TYPE l_tt_european_results.

    TYPES: BEGIN OF l_typ_us_monsters,
             monster_name         TYPE string,
             monster_lockbox_code TYPE string,
           END OF l_typ_us_monsters.

    TYPES: l_tt_us_monsters TYPE HASHED TABLE OF l_typ_us_monsters
                            WITH UNIQUE KEY monster_name.

    TYPES: BEGIN OF l_typ_us_results,
             laboratory TYPE string,
             t_result   TYPE l_tt_us_monsters,
           END OF l_typ_us_results.

    TYPES: l_tt_us_results TYPE STANDARD TABLE OF l_typ_us_results.

    DATA: us_result  TYPE l_typ_us_results,
          us_results TYPE l_tt_us_results.

    european_result-laboratory         = 'SECRET LABORATORY 51'.
    iban_code_record-monster_name      = 'FRED'.
    iban_code_record-monster_iban_code = 'AL47212110090000000235698741'.
    INSERT iban_code_record INTO TABLE european_result-t_result.
    INSERT european_result  INTO TABLE european_results.

*--------------------------------------------------------------------*
* Important Bit - This Works - Takes Only 1 Line
*--------------------------------------------------------------------*
    MOVE-CORRESPONDING european_results TO us_results.

  ENDMETHOD.


  METHOD l03_41_corresponding_dynamic.
*--------------------------------------------------------------------*
* Listing 03.41:  Dynamic MOVE-CORRESPONDING Usage
*--------------------------------------------------------------------*
* This example demonstrates building up the mapping rules dynamically
* at runtime to aid in the make (monster) to order business process
*--------------------------------------------------------------------*
* Incoming Parameters are
* - a record describing the customer requirements (Z4T_ORDER_ITEMS)
*   this comes from sales (taking customer orders)
* - a record describing possible brains available (Z4ST_MONSTER_POSSIBLE_BRAINS)
*   this comes from procurement (digging up graves)
* - the returning parameter is a ranked list of preferred brains
*   this is sent to production (Baron Frankenstein)
*--------------------------------------------------------------------*
* Local Variables
    DATA: mapping_record TYPE cl_abap_corresponding=>mapping_info,
          mapping_table  TYPE cl_abap_corresponding=>mapping_table.

    "Simulate incoming customer requirements from sales
    DATA(is_customer_requirements) = VALUE z4t_order_items(
      usage_desired      = 'MORI'    "Morris Dancer
      brain_size_desired = 'MICRO' )."Microscopic

    "Simulate incoming possible brains from procurement
    DATA(is_possible_brains) = VALUE z4st_monster_possible_brains(
      monster_number = '0000000001'
      monster_name   = 'JIMMY'
      biggest_brain  = 'EINSTIEN'
      smallest_brain = 'RENAMER'
      weirdest_brain = 'YANKOVICH'
      evilest_brain  = 'BANKER' ).

    mapping_record-level = 0.
    mapping_record-kind  = cl_abap_corresponding=>mapping_component.

    mapping_record-dstname = 'BEST_BRAIN_01'.

    IF is_customer_requirements-brain_size_desired = 'NORM'."Normal Brain
      mapping_record-srcname = 'BIGGEST_BRAIN'.
    ELSE.
      mapping_record-srcname = 'SMALLEST_BRAIN'.
    ENDIF.

    APPEND mapping_record TO mapping_table.

    mapping_record-dstname = 'BEST_BRAIN_02'.

    IF is_customer_requirements-usage_desired = 'MORT'."Mortgage Salesman
      mapping_record-srcname = 'EVILEST_BRAIN'.
    ELSEIF is_customer_requirements-usage_desired = 'MORI'."Morris Dancer
      mapping_record-srcname = 'WEIRDEST_BRAIN'.
    ENDIF.

    APPEND mapping_record TO mapping_table.

    TRY.
        DATA(dynamic_mapper) =
        cl_abap_corresponding=>create(
            source            = is_possible_brains
            destination       = rs_best_brains
            mapping           = mapping_table ).

        dynamic_mapper->execute(
          EXPORTING source      = is_possible_brains
          CHANGING  destination = rs_best_brains    ).

      CATCH cx_corr_dyn_error ##NO_HANDLER."In the example, in real life you need one
        "Raise a Fatal Error Message
        RETURN.
    ENDTRY.

    "If the RETURNING parameter is a structure, as in this method, but you are only interested
    "in one field of that structure then you call the method as follows:-
    DATA(best_brain) = l03_41_corresponding_dynamic( )-best_brain_01.

  ENDMETHOD.


  METHOD l03_42_getting_row_no_old.
*--------------------------------------------------------------------*
* Listing 03.42:  Reading Internal Table to Get Row Number in 7.02
*--------------------------------------------------------------------*
    DATA:
      start_row         TYPE sy-tabix,
      table_of_monsters TYPE STANDARD TABLE OF z4t_monster_head,
      monster_number    TYPE z4de_monster_number VALUE '0000000001'.

    READ TABLE table_of_monsters
    WITH KEY monster_number = monster_number
    TRANSPORTING NO FIELDS.

    IF sy-subrc = 0.
      start_row = sy-tabix.
    ENDIF.

  ENDMETHOD.


  METHOD l03_43_getting_row_no_new.
*--------------------------------------------------------------------*
* Listing 03.43:  LINE_INDEX
*--------------------------------------------------------------------*
* Post 7.40 Syntax
*--------------------------------------------------------------------*
* Local Variables
    DATA:
      table_of_monsters TYPE STANDARD TABLE OF z4t_monster_head,
      monster_number    TYPE z4de_monster_number VALUE '0000000001'.

    DATA(start_row) = line_index( table_of_monsters[ monster_number = monster_number ] ).

    LOOP AT table_of_monsters FROM start_row ASSIGNING FIELD-SYMBOL(<monster_details>).
      "Do Something
    ENDLOOP.

  ENDMETHOD.


  METHOD l03_44_getting_row_no_newer.
*--------------------------------------------------------------------*
* Listing 03.44:  Built-In Function LINE_INDEX at Operand Position
*--------------------------------------------------------------------*
* Post 7.40 Syntax - V2
*--------------------------------------------------------------------*
* Local Variables
    DATA:
      table_of_monsters TYPE STANDARD TABLE OF z4t_monster_head,
      monster_number    TYPE z4de_monster_number VALUE '0000000001'.

    LOOP AT table_of_monsters
      FROM line_index( table_of_monsters[ monster_number = monster_number ] )
      ASSIGNING FIELD-SYMBOL(<monster_details>).
      "Do Something
    ENDLOOP.

* One Haunted Castle can be found at 50.4 degrees Latitude
  ENDMETHOD.


  METHOD l03_45_itab_row_exists_old.
*--------------------------------------------------------------------*
* Listing 03.45:  Checking If Internal Table Line Exists before 7.4
*--------------------------------------------------------------------*
* Pre 7.4 Syntax
*--------------------------------------------------------------------*
    DATA:
      table_of_monsters TYPE STANDARD TABLE OF z4t_monster_head,
      monster_number    TYPE z4de_monster_number VALUE '0000000001'.

    FIELD-SYMBOLS: <monster_details> LIKE LINE OF table_of_monsters.

    READ TABLE table_of_monsters ASSIGNING <monster_details>
    WITH KEY monster_number = monster_number.

    IF sy-subrc NE 0.
      APPEND INITIAL LINE TO table_of_monsters
      ASSIGNING <monster_details>.
    ENDIF.
    ADD 1 TO <monster_details>-sanity_percentage.

  ENDMETHOD.


  METHOD l03_46_itab_row_exists_new.
*--------------------------------------------------------------------*
* Listing 03.46:  Checking If Internal Table Line Exists in 7.4
*--------------------------------------------------------------------*
* Post 7.4 Syntax
*--------------------------------------------------------------------*
    DATA:
      table_of_monsters TYPE STANDARD TABLE OF z4t_monster_head,
      monster_number    TYPE z4de_monster_number VALUE '0000000001'.

    IF line_exists( table_of_monsters[ monster_number = monster_number ] ).
      READ TABLE table_of_monsters ASSIGNING FIELD-SYMBOL(<monster_details>)
      WITH KEY monster_number = monster_number.
    ELSE.
      APPEND INITIAL LINE TO table_of_monsters
      ASSIGNING <monster_details>.
    ENDIF.

    <monster_details>-sanity_percentage = <monster_details>-sanity_percentage + 1.

  ENDMETHOD.


  METHOD l03_47_itab_row_exists_newer.
*--------------------------------------------------------------------*
* Listing 03.47:  Code All in One Line, with No Reliance on SY-SUBRC
*--------------------------------------------------------------------*
* Post 7.4 Syntax V2
*--------------------------------------------------------------------*
    DATA:
      table_of_monsters TYPE STANDARD TABLE OF z4t_monster_head,
      monster_number    TYPE z4de_monster_number VALUE '0000000001'.

    IF line_exists( table_of_monsters[ monster_number = monster_number ] ).
      "Do Something
    ENDIF.

  ENDMETHOD.


  METHOD l03_48_reduce.
*--------------------------------------------------------------------*
* Listing 03.48:  How Many Really Mad Monsters?
*--------------------------------------------------------------------*
* Local Variables
    DATA: neurotic_monsters TYPE STANDARD TABLE OF z4t_monster_head.
*--------------------------------------------------------------------*
* Fill up NEUROTIC_MONSTERS table, then...
*--------------------------------------------------------------------*
    DATA(mad_monsters_count) = REDUCE sy-tabix(
    INIT result = 0
    FOR  monster_details IN neurotic_monsters
    NEXT result = result +
    lcl_utilities=>add_1_if_true(
    zcl_4_monster_model=>is_mad( monster_details-monster_number ) ) ).

  ENDMETHOD.


  METHOD l03_49_group_by.
*--------------------------------------------------------------------*
* Listing 03.49:  GROUP BY
*--------------------------------------------------------------------*
* In this example we are trying to prove that bonkers monsters with
* bolts througth their neck have more heads per monster on average
* compared to bonkers monsters who like to ice skate.
* This is most likely a 100% correlation with the business problem
* you are trying to solve at work right at this moment.
*--------------------------------------------------------------------*
    TYPES: tt_monsters TYPE STANDARD TABLE OF z4t_monster_head
                       WITH DEFAULT KEY.

    DATA: monster_sub_set TYPE tt_monsters,
          total_heads     TYPE i.

    DATA(table_of_monsters) = VALUE tt_monsters(
    ( monster_number = '1' model = 'BTNK' no_of_heads = 3 )
    ( monster_number = '2' model = 'BTNK' no_of_heads = 4 )
    ( monster_number = '3' model = 'BTNK' no_of_heads = 2 )
    ( monster_number = '4' model = 'ISDD' no_of_heads = 1 )
    ( monster_number = '5' model = 'ISDD' no_of_heads = 1 )
     ).

    LOOP AT table_of_monsters ASSIGNING FIELD-SYMBOL(<monster_details>)
    GROUP BY ( model          = <monster_details>-model
               is_it_crackers = zcl_4_monster_model=>is_mad( <monster_details>-monster_number ) )
    ASSIGNING FIELD-SYMBOL(<monster_group_record>).

      CHECK <monster_group_record>-is_it_crackers = abap_true.

      CLEAR monster_sub_set.

      LOOP AT GROUP <monster_group_record> ASSIGNING FIELD-SYMBOL(<bonkers_monsters>).
        monster_sub_set =
        VALUE #( BASE monster_sub_set ( <bonkers_monsters> ) ).
      ENDLOOP.

      CLEAR total_heads.

      LOOP AT monster_sub_set ASSIGNING FIELD-SYMBOL(<sub_set_record>).
        total_heads = total_heads + <sub_set_record>-no_of_heads.
      ENDLOOP.

      WRITE:/ 'Bonkers Monsters of Type'(015),<monster_group_record>-model,' have '(016),total_heads,' heads'(017).

    ENDLOOP.

  ENDMETHOD.


  METHOD l03_50_table_extraction_old.
*--------------------------------------------------------------------*
* Listing 03.50:  Extracting One Table from Another before 7.4
*--------------------------------------------------------------------*
    DATA: "Source Table
          all_monsters
          TYPE SORTED TABLE OF z4t_monster_head
          WITH NON-UNIQUE KEY monster_number
          WITH NON-UNIQUE SORTED KEY bonkers_ness
          COMPONENTS sanity_percentage,
          "Target Table
          averagely_mad_monsters   TYPE STANDARD TABLE OF z4t_monster_head,
          an_averagely_mad_monster LIKE LINE OF averagely_mad_monsters.

    "Extract Source to Target
    LOOP AT all_monsters ASSIGNING FIELD-SYMBOL(<monster_record>)
      WHERE sanity_percentage < 75.
      CLEAR an_averagely_mad_monster.
      MOVE-CORRESPONDING <monster_record> TO an_averagely_mad_monster.
      APPEND an_averagely_mad_monster     TO averagely_mad_monsters.
    ENDLOOP."All Monsters

  ENDMETHOD.


  METHOD l03_51_table_extraction_new.
*--------------------------------------------------------------------*
* Listing 03.51:  Extracting One Table from Another in 7.40
*--------------------------------------------------------------------*
    DATA: all_monsters
             TYPE SORTED TABLE OF z4t_monster_head
             WITH NON-UNIQUE KEY monster_number
             WITH NON-UNIQUE SORTED KEY bonkers_ness
             COMPONENTS sanity_percentage.

    DATA(averagely_mad_monsters) =
    FILTER #( all_monsters USING KEY bonkers_ness
    WHERE sanity_percentage < CONV #( 75 ) ).

  ENDMETHOD.


  METHOD l03_52_fae_database.
*--------------------------------------------------------------------*
* Listing 03.52:  FOR ALL ENTRIES during Database Read
*--------------------------------------------------------------------*
    DATA: monster_deliveries
          TYPE SORTED TABLE OF z4t_deliveries
          WITH NON-UNIQUE KEY monster_number,
          all_monsters       TYPE STANDARD TABLE OF z4t_monster_head.

* Fill up table ALL_MONSTERS

    SELECT *
    FROM z4t_deliveries
    INTO CORRESPONDING FIELDS OF TABLE monster_deliveries
    FOR ALL ENTRIES IN all_monsters
    WHERE monster_number = all_monsters-monster_number.

  ENDMETHOD.


  METHOD l03_53_fae_itab.
*--------------------------------------------------------------------*
* Listing 03.53:  FOR ALL ENTRIES on Internal Table
*--------------------------------------------------------------------*
* In my humble opinion the requirement that both tables need to be
* SORTED or HASHED dramatically reduces the usefulness of this
*--------------------------------------------------------------------*
    DATA: monster_deliveries TYPE SORTED TABLE OF z4t_deliveries
                             WITH NON-UNIQUE KEY monster_number,
          all_monsters       TYPE SORTED TABLE OF z4t_monster_head
                             WITH UNIQUE KEY monster_number.

*--------------------------------------------------------------------*
* Fill up table ALL_MONSTERS
*--------------------------------------------------------------------*

*--------------------------------------------------------------------*
* Do a FAE on the interal table....
*--------------------------------------------------------------------*
    DATA(deliveries_our_monsters) =
    FILTER #( monster_deliveries IN all_monsters
    WHERE monster_number = monster_number ).

* The Monster whose hobby is Spotting Trains is Colored Red
  ENDMETHOD.


  METHOD l03_54_virtual_sorting.
*--------------------------------------------------------------------*
* Listing 03.54:  Virtual Sorting
*--------------------------------------------------------------------*
* This is a problem that actually happened...
*--------------------------------------------------------------------*
    TYPES: BEGIN OF l_typ_disputes,
             dispute_number TYPE c LENGTH 10,
           END OF l_typ_disputes.

    TYPES: BEGIN OF l_typ_credit_notes,
             dispute_number TYPE c LENGTH 10,
             credit_value   TYPE alv_curr,
             rejected       TYPE abap_bool,
           END OF   l_typ_credit_notes.

    DATA: dispute_table     TYPE STANDARD TABLE OF l_typ_disputes,
          credit_note_table TYPE STANDARD TABLE OF l_typ_credit_notes.

*--------------------------------------------------------------------*
* Fill up the two tables
*--------------------------------------------------------------------*

* Whatever....

*--------------------------------------------------------------------*
* Virtual Sorting...
*--------------------------------------------------------------------*
    DATA(dispute_indexes) = cl_abap_itab_utilities=>virtual_sort(
    im_virtual_source =
    VALUE #(
    ( source = REF #( credit_note_table )
    components =
    VALUE #( ( name = 'credit_value'
    descending = abap_true ) ) )
    ( source = REF #( dispute_table )
    components =
    VALUE #( ( name = 'dispute_number' ) ) ) ) ).

  ENDMETHOD.


  METHOD l03_55_virtual_sorting_part_2.
*--------------------------------------------------------------------*
* Listing 03.55: Using Virtual Sort Result
*--------------------------------------------------------------------*
* This is a problem that actually happened...
*--------------------------------------------------------------------*
    TYPES: BEGIN OF l_typ_disputes,
             dispute_number TYPE c LENGTH 10,
           END OF l_typ_disputes.

    TYPES: BEGIN OF l_typ_credit_notes,
             dispute_number TYPE c LENGTH 10,
             credit_value   TYPE alv_curr,
             rejected       TYPE abap_bool,
           END OF   l_typ_credit_notes.

    DATA: dispute_table     TYPE STANDARD TABLE OF l_typ_disputes,
          credit_note_table TYPE STANDARD TABLE OF l_typ_credit_notes.

*--------------------------------------------------------------------*
* Fill up the two tables
*--------------------------------------------------------------------*

* Whatever....

*--------------------------------------------------------------------*
* Virtual Sorting...
*--------------------------------------------------------------------*
    DATA(dispute_indexes) = cl_abap_itab_utilities=>virtual_sort(
    im_virtual_source =
    VALUE #(
    ( source = REF #( credit_note_table )
    components =
    VALUE #( ( name = 'credit_value'
    descending = abap_true ) ) )
    ( source = REF #( dispute_table )
    components =
    VALUE #( ( name = 'dispute_number' ) ) ) ) ).

*--------------------------------------------------------------------*
* Evaluate Results
*--------------------------------------------------------------------*
    DATA top_disputes TYPE STANDARD TABLE OF l_typ_disputes.

    LOOP AT dispute_indexes ASSIGNING FIELD-SYMBOL(<index>).
      APPEND dispute_table[ <index> ] TO top_disputes.
    ENDLOOP.

  ENDMETHOD.


  METHOD l03_56_query_ddic_no_cast.
*-----------------------------------------------------------------------*
* Listing 03.56:  Components of Specific Dictionary Structure without CAST
*-----------------------------------------------------------------------*
    DATA structure_description TYPE REF TO cl_abap_structdescr.
    structure_description
    ?= cl_abap_typedescr=>describe_by_name( 'Z4SP_MONSTER_HEADER_D' ).
    DATA structure_components TYPE abap_compdescr_tab.
    structure_components = structure_description->components.

  ENDMETHOD.


  METHOD l03_57_query_ddic_with_cast.
*--------------------------------------------------------------------*
* Listing 03.57:  Components of Specific Dictionary Structure with CAST
*--------------------------------------------------------------------*
    DATA(structure_components) = CAST cl_abap_structdescr(
    cl_abap_typedescr=>describe_by_name( 'Z4SP_MONSTER_HEADER_D' ) )->components.

  ENDMETHOD.


  METHOD l03_58_find_a_subclass_old.
*--------------------------------------------------------------------*
* Listing 03.58:  Trying to Find Subclass before 7.5
*--------------------------------------------------------------------*
* See method GET_ALV_GRID in clasas ZCL_SALV_MODEL in Chapter 10
*--------------------------------------------------------------------*
    DATA: full_screen_adapter TYPE REF TO cl_salv_fullscreen_adapter,
          container_adapter   TYPE REF TO cl_salv_grid_adapter.
    TRY.
        "Presume full screen mode (No Container)
        "Target FULL_SCREEN_ADAPTER = CL_SALV_FULLSCREEN_ADAPTER
        "CL_SALV_FULLSCREEN_ADAPTER is a subclass of CL_SALV_ADAPTER
        full_screen_adapter ?= io_salv_adapter.
        "Get the Grid
        ro_alv_grid = full_screen_adapter->get_grid( ).
      CATCH cx_sy_move_cast_error.
        "We must be in container mode
        "Target CONTAINER_ADAPTER = CL_SALV_GRID_ADAPTER
        "CL_SALV_GRID_ADAPTER is a subclass of CL_SALV_ADAPTER
        container_adapter ?= io_salv_adapter.
        ro_alv_grid = container_adapter->get_grid( ).
    ENDTRY.

  ENDMETHOD.


  METHOD l03_59_find_a_subclass_new.
*--------------------------------------------------------------------*
* Listing 03.59:  Trying to Find Subclass in 7.5
*--------------------------------------------------------------------*
* See method GET_ALV_GRID in clasas ZCL_SALV_MODEL in Chapter 10
*--------------------------------------------------------------------*
    DATA: full_screen_adapter TYPE REF TO cl_salv_fullscreen_adapter,
          container_adapter   TYPE REF TO cl_salv_grid_adapter.

    IF io_salv_adapter IS INSTANCE OF cl_salv_fullscreen_adapter.
      full_screen_adapter ?= io_salv_adapter.
      ro_alv_grid = full_screen_adapter->get_grid( ).
    ELSEIF io_salv_adapter IS INSTANCE OF cl_salv_grid_adapter.
      container_adapter ?= io_salv_adapter.
      ro_alv_grid = container_adapter->get_grid( ).
    ENDIF.

  ENDMETHOD.


  METHOD l03_60_find_a_subclass_new_v2.
*--------------------------------------------------------------------*
* Listing 03.60:  Another Way to Find Subclass in 7.50
*--------------------------------------------------------------------*
* See method GET_ALV_GRID in clasas ZCL_SALV_MODEL in Chapter 10
*--------------------------------------------------------------------*

    CASE TYPE OF io_salv_adapter.
      WHEN TYPE cl_salv_fullscreen_adapter
      INTO DATA(full_screen_adapter).
        ro_alv_grid = full_screen_adapter->get_grid( ).
      WHEN TYPE cl_salv_grid_adapter
      INTO DATA(container_adapter).
        ro_alv_grid = container_adapter->get_grid( ).
      WHEN OTHERS.
        RETURN.
    ENDCASE.

  ENDMETHOD.


  METHOD l03_61_functional_method_plus.
*--------------------------------------------------------------------*
* Listing 03.61:  CHANGING and EXPORTING Parameters
*--------------------------------------------------------------------*
    DATA: something_spurious  TYPE string,
          something_unrelated TYPE string.

    DATA(bomb_name) = lcl_atom_bomb=>get_details(
    EXPORTING id_bomb_number         = '0000000001'
    IMPORTING ed_something_spurious  = something_spurious
    CHANGING  cd_something_unrelated = something_unrelated ).

  ENDMETHOD.
ENDCLASS.
