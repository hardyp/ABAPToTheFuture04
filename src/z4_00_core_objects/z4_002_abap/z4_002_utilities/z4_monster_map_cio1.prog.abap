*&---------------------------------------------------------------------*
*&  Include           Z_MONSTER_ADL_IO1
*&---------------------------------------------------------------------*
* Local Class Implementations
*--------------------------------------------------------------------*
CLASS lcl_application IMPLEMENTATION.

  METHOD main.
* Local Variables
    DATA: ld_repid TYPE sy-repid.

    "Hard Coded here - would use configuration or similar in
    "real life
    DATA(context_data_list) = VALUE wdr_simple_name_value_list( (
    name  = 'UI_TECHNOLOGY'
    value = 'CL_SALV_TABLE' ) ).

    mo_model = NEW #( ).

    zcl_ocp_factory=>return_object_given(
      EXPORTING it_context_data = context_data_list
      CHANGING  co_object       = mo_view ).

    mo_controller = NEW #( io_model = mo_model
                           io_view  = mo_view ).

    mo_model->data_retrieval( ).
    mo_model->prepare_data_for_ouput( ).

    "It is bad news to pass system variables as parameters
    ld_repid = sy-repid.

    IF sy-batch IS INITIAL.
*--------------------------------------------------------------------*
* Listing 10.31 - Calling a SALV report whilst creating a container
*                 automatically
*--------------------------------------------------------------------*
* Program flow is as follows:-
* ZCL_BC_VIEW_SALV_TABLE->CREATE_CONTAINER_PREPARE_DATA
* Function ZSALV_CSQT_CREATE_CONTAINER
* ZSALV_CSQT_CREATE_CONTAINER->FILL_CONTAINER_CONTENT
* ZCL_BC_VIEW_SALV_TABLE->PREPARE_DISPLAY_DATA
* --> INITIALISE (Generic)
* --> Application Specific Changes (Generic)
* --> Display (Generic)
      mo_view->create_container_prep_display(
    EXPORTING
      id_report_name        = ld_repid
      if_start_in_edit_mode = abap_false
      is_layout             = mo_model->ms_layout
      id_edit_control_field = mo_model->md_edit_control_field
      it_editable_fields    = mo_model->mt_editable_fields
      it_technicals         = mo_model->mt_technicals
      it_hidden             = mo_model->mt_hidden
      it_hotspots           = mo_model->mt_hotspots
      it_checkboxes         = mo_model->mt_checkboxes
      it_subtotal_fields    = mo_model->mt_subtotal_fields
      it_field_texts        = mo_model->mt_field_texts
      it_user_commands      = mo_model->mt_user_commands
    CHANGING
      ct_data_table         = mo_model->mt_output_data ).

    ELSE.
* If this is running in the background there is no way
* in the world we want/need a container, as there is no
* chance for the user to press any user command buttons or
* edit the data, as there is no user, and no screen for the
* container to live on for that matter
      mo_view->prepare_display_data(
        EXPORTING
          id_report_name     = ld_repid
          it_technicals      = mo_model->mt_technicals
          it_hidden          = mo_model->mt_hidden
          it_hotspots        = mo_model->mt_hotspots
          it_checkboxes      = mo_model->mt_checkboxes
          it_subtotal_fields = mo_model->mt_subtotal_fields
          it_field_texts     = mo_model->mt_field_texts
          it_user_commands   = mo_model->mt_user_commands
        CHANGING
          ct_data_table      = mo_model->mt_output_data ).
    ENDIF."Are we running in the background?

  ENDMETHOD.                                               "main

ENDCLASS.                    "lcl_application IMPLEMENTATION

*----------------------------------------------------------------------*
*       CLASS lcl_selections IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_selections IMPLEMENTATION.

  METHOD constructor.

    s_chap  = is_chap.
    p_vari  = ip_vari.

  ENDMETHOD.                    "constructor

ENDCLASS."Local Selections

*----------------------------------------------------------------------*
*       CLASS lcl_persistency_layer IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_persistency_layer IMPLEMENTATION.

  METHOD constructor ##NEEDED.

  ENDMETHOD.                    "constructor

  METHOD get_data.
*--------------------------------------------------------------------*
* EXPORTING et_output_data TYPE g_tt_output_data.
*--------------------------------------------------------------------*
* This is going to be hard coding, through and through
*--------------------------------------------------------------------*
    DEFINE add_listing.
      APPEND VALUE #(
      chapter     = &1
      listing     = &2
      name        = &3
      object_type = &4
      object_name = &5
      subobject   = &6
      )
      TO et_output_data.
    END-OF-DEFINITION.

    add_listing :
*--------------------------------------------------------------------*
* Chapter 03 : ABAP
*--------------------------------------------------------------------*
 3  1 'Query without VALUE' 'CLAS' 'ZCL_4_CH03_ABAP_EXAMPLES' 'L03_01_QUERY_WITHOUT_VALUE',
 3  2 'Query with VALUE' 'CLAS' 'ZCL_4_CH03_ABAP_EXAMPLES' 'L03_02_QUERY_WITH_VALUE',
 3  3 'Using VALUE to fill Internal Table' 'CLAS' 'ZCL_4_CH03_ABAP_EXAMPLES' 'L03_03_VALUE_TO_FILL_ITAB',
 3  4 'Fill Internal Table using Hard Coding' 'CLAS' 'ZCL_4_CH03_ABAP_EXAMPLES' 'L03_04_FILLING_ITAB_HARDCODE',
 3  5 'Filling Internal Tables using FOR' 'CLAS' 'ZCL_4_CH03_ABAP_EXAMPLES' 'L03_05_FILLING_ITAB_VIA_FOR',
 3  6 'Creating Short-Lived Variables (LET)' 'CLAS' 'ZCL_4_CH03_ABAP_EXAMPLES' 'L03_06_SHORT_LIVED_VARIABLES',
 3  7 'Basic Enumeration' 'CLAS' 'ZCL_4_CH03_ABAP_EXAMPLES' 'L03_07_BASIC_ENUMERATION',
 3  8 'Non-Integer Enumeration' 'CLAS' 'ZCL_4_CH03_ABAP_EXAMPLES' 'L03_08_NON_INTEGER_ENUMERATION',
 3  9 'Enumeration with Structure' 'CLAS' 'ZCL_4_CH03_ABAP_EXAMPLES' 'L03_09_STRUCTURED_ENUMERATION',
 3 10 'Remove/Add Leading Zeroes by Function' 'CLAS' 'ZCL_4_CH03_ABAP_EXAMPLES' 'L03_10_ALPHA_INPUT_OUTPUT',
 3 11 'ALPHA Formatting Option' 'CLAS' 'ZCL_4_CH03_ABAP_EXAMPLES' 'L03_11_ALPHA_FOMATTING_OPTION',
 3 12 'Call Method / Guess Data Types' 'CLAS' 'ZCL_4_CH03_ABAP_EXAMPLES' 'L03_12_METHOD_GUESS_DATA_TYPE',
 3 13 'Call Method + Inline Declarations' 'CLAS' 'ZCL_4_CH03_ABAP_EXAMPLES' 'L03_13_METHOD_INLINE_DECS',
 3 14 'Method Call with Helper Variable' 'CLAS' 'ZCL_4_CH03_ABAP_EXAMPLES' 'L03_14_METHOD_CALL_W_HELPER',
 3 15 'Method Call with CONV' 'CLAS' 'ZCL_4_CH03_ABAP_EXAMPLES' 'L03_15_METHOD_CALL_W_CONV',
 3 16 'Using TYPE REF TO previously' 'CLAS' 'ZCL_4_CH03_ABAP_EXAMPLES' 'L03_16_TYPE_REF_TO_OLD',
 3 17 'Needing ABAP_TRUE' 'CLAS' 'ZCL_4_CH03_ABAP_EXAMPLES' 'L03_17_ABAP_TRUE',
 3 18 'Not Needing ABAP_TRUE' 'CLAS' 'ZCL_4_CH03_ABAP_EXAMPLES' 'L03_18_NO_ABAP_TRUE',
 3 19 'IF with Incorrect Check' 'CLAS' 'ZCL_4_CH03_ABAP_EXAMPLES' 'L03_19_IF_WRONG_CHECK',
 3 20 'Demonstration of BOOLC' 'CLAS' 'ZCL_4_CH03_ABAP_EXAMPLES' 'L03_20_BOOLC',
 3 21 'Testing for a Negative' 'CLAS' 'ZCL_4_CH03_ABAP_EXAMPLES' 'L03_21_NEGATIVE_TESTING',
 3 22 'Using XSDBOOL for Correct Logic Test Results' 'CLAS' 'ZCL_4_CH03_ABAP_EXAMPLES' 'L03_22_XSDBOOL',
 3 23 'Filling in a Variable Using CASE Statement' 'CLAS' 'ZCL_4_CH03_ABAP_EXAMPLES' 'L03_23_CASE_STATEMENT',
 3 24 'Filling in a Variable Using SWITCH Statement' 'CLAS' 'ZCL_4_CH03_ABAP_EXAMPLES' 'L03_24_SWTCH_STATEMENT',
* Listing 25 uses VBPA whic deos not exist in a standalone system like PHX
 3 26 'CASE Statement to Determine Monster Sanity' 'CLAS' 'ZCL_4_CH03_ABAP_EXAMPLES' 'L03_26_CASE_STATEMENT',
 3 27 'Using COND Constructor Operator' 'CLAS' 'ZCL_4_CH03_ABAP_EXAMPLES' 'L03_27_COND_STATEMENT',
 3 28 'COND Constructor Operator with Updated Logic' 'CLAS' 'ZCL_4_CH03_ABAP_EXAMPLES' 'L03_28_COND_STATEMENT_UPDATED',
 3 29 'Reading into Work Area and Looping' 'CLAS' 'ZCL_4_CH03_ABAP_EXAMPLES' 'L03_29_INTERNAL_TABLES_WA',
 3 30 'Field Symbols for Work Area' 'CLAS' 'ZCL_4_CH03_ABAP_EXAMPLES' 'L03_30_INTERNAL_TABLES_FS',
 3 31 'Reading Line from Internal Table before 7.4' 'CLAS' 'ZCL_4_CH03_ABAP_EXAMPLES' 'L03_31_READING_TABLE_LINE_OLD',
 3 32 'Reading Line from Internal Table after 7.4' 'CLAS' 'ZCL_4_CH03_ABAP_EXAMPLES' 'L03_32_READING_TABLE_LINE_NEW',
 3 33 'Reading Line from Interal Table - OPTIONAL' 'CLAS' 'ZCL_4_CH03_ABAP_EXAMPLES' 'L03_33_READING_TABLE_OPTIONAL',
 3 34 'Moving One Table to Another before 7.4' 'CLAS' 'ZCL_4_CH03_ABAP_EXAMPLES' 'L03_34_COPYING_ITABS_OLD',
 3 35 'Moving One Table to Another after 7.4' 'CLAS' 'ZCL_4_CH03_ABAP_EXAMPLES' 'L03_35_COPYING_ITABS_NEW',
 3 36 'Moving Corresponding Fields to a HASHED ITAB' 'CLAS' 'ZCL_4_CH03_ABAP_EXAMPLES' 'L03_36_COPY_TO_HASHED_ITAB',
 3 37 'Deep Structure' 'CLAS' 'ZCL_4_CH03_ABAP_EXAMPLES' 'L03_37_DEEP_STRUCTURE',
 3 38 'MOVE-CORRESPONDING (Before) V1' 'CLAS' 'ZCL_4_CH03_ABAP_EXAMPLES' 'L03_38_CORRESPONDING_OLD',
 3 39 'MOVE-CORRESPONDING (Before) V2' 'CLAS' 'ZCL_4_CH03_ABAP_EXAMPLES' 'L03_39_CORRESPONDING_OLD_V2',
 3 40 'MOVE_CORRESPONDING (After)' 'CLAS' 'ZCL_4_CH03_ABAP_EXAMPLES' 'L03_40_CORRESPONDING_NEW',
 3 41 'Dynamic MOVE-CORRESPONDING Usage' 'CLAS' 'ZCL_4_CH03_ABAP_EXAMPLES' 'L03_41_CORRESPONDING_DYNAMIC',
 3 42 'Reading Internal Table to Get Row Number' 'CLAS' 'ZCL_4_CH03_ABAP_EXAMPLES' 'L03_42_GETTING_ROW_NO_OLD',
 3 43 'Get Row Number using LINE_INDEX' 'CLAS' 'ZCL_4_CH03_ABAP_EXAMPLES' 'L03_43_GETTING_ROW_NO_NEW',
 3 44 'LINE_INDEX at Operand Position' 'CLAS' 'ZCL_4_CH03_ABAP_EXAMPLES' 'L03_44_GETTING_ROW_NO_NEWER',
 3 45 'Checking if Internal Table Line Exists1' 'CLAS' 'ZCL_4_CH03_ABAP_EXAMPLES' 'L03_45_ITAB_ROW_EXISTS_OLD',
 3 46 'Checking if Internal Table Line Exists2' 'CLAS' 'ZCL_4_CH03_ABAP_EXAMPLES' 'L03_46_ITAB_ROW_EXISTS_NEW',
 3 47 'Checking if Internal Table Line Exists3' 'CLAS' 'ZCL_4_CH03_ABAP_EXAMPLES' 'L03_47_ITAB_ROW_EXISTS_NEWER',
 3 48 'Using REDUCE to Count Table Entries' 'CLAS' 'ZCL_4_CH03_ABAP_EXAMPLES' 'L03_48_REDUCE',
 3 49 'Using GROUP_BY' 'CLAS' 'ZCL_4_CH03_ABAP_EXAMPLES' 'L03_49_GROUP_BY',
 3 50 'Table Extraction before 7.4' 'CLAS' 'ZCL_4_CH03_ABAP_EXAMPLES' 'L03_50_TABLE_EXTRACTION_OLD',
 3 51 'Table Extraction after 7.4' 'CLAS' 'ZCL_4_CH03_ABAP_EXAMPLES' 'L03_51_TABLE_EXTRACTION_NEW',
 3 52 'FOR ALL ENTRIES on Database Table' 'CLAS' 'ZCL_4_CH03_ABAP_EXAMPLES' 'L03_52_FAE_DATABASE',
 3 53 'FOR ALL ENTRIES on Internal Table' 'CLAS' 'ZCL_4_CH03_ABAP_EXAMPLES' 'L03_53_FAE_ITAB',
 3 54 'Virtual Sorting' 'CLAS' 'ZCL_4_CH03_ABAP_EXAMPLES' 'L03_54_VIRTUAL_SORTING',
 3 55 'Virtual Sorting Results' 'CLAS' 'ZCL_4_CH03_ABAP_EXAMPLES' 'L03_55_VIRTUAL_SORTING_PART_2',
 3 56 'Get Structure Info without CAST' 'CLAS' 'ZCL_4_CH03_ABAP_EXAMPLES' 'L03_56_QUERY_DDIC_NO_CAST',
 3 57 'Get Structure Info with CAST' 'CLAS' 'ZCL_4_CH03_ABAP_EXAMPLES' 'L03_57_QUERY_DDIC_WITH_CAST',
 3 58 'Finding a Subclass pre 7.5'  'CLAS' 'ZCL_4_CH03_ABAP_EXAMPLES' 'L03_58_FIND_A_SUBCLASS_OLD',
 3 59 'Finding a Subclass post 7.5' 'CLAS' 'ZCL_4_CH03_ABAP_EXAMPLES' 'L03_59_FIND_A_SUBCLASS_NEW',
 3 60 'Finding a Subclass post 7.5 (Alternate)' 'CLAS' 'ZCL_4_CH03_ABAP_EXAMPLES' 'L03_60_FIND_A_SUBCLASS_NEW_V2',
 3 61 'Functional Method + Extra Parameters' 'CLAS' 'ZCL_4_CH03_ABAP_EXAMPLES' 'L03_61_FUNCTIONAL_METHOD_PLUS',
*--------------------------------------------------------------------*
* Chapter 04 : Exception Handling
*--------------------------------------------------------------------*
     4  1 'Handling an Exception Locally'    'PROG' 'Z4_EXCEPTION_EXAMPLES'    'SOMETHING_STATIC',
     4  2 'NO_CHECK Exception'               'PROG' 'Z4_EXCEPTION_EXAMPLES'    'USER_COMMAND',
     4  3 'TRY / CATCH / CLEANUP'            'PROG' 'Z4_EXCEPTION_EXAMPLES'    'TRY_CATCH_BLOCK',
     4  4 'Exporting Vital Information'      'PROG' 'Z4_EXCEPTION_EXAMPLES'    'DO_SOMETHING',
     4  5 'Converting Classical Exceptions'  'PROG' 'Z4_EXCEPTION_EXAMPLES'    'CLASSIC_CONVERSION',
     4  6 'Wrapping a Function Module'       'PROG' 'Z4_EXCEPTION_EXAMPLES'    'WRAPPING_A_FUNCTION',
     4  7 'CLEANUP (Wrong)'                  'PROG' 'Z4_EXCEPTION_EXAMPLES'    'HESD_SWAP_OPERATION_WRONG',
     4  8 'CLEANUP (Right)'                  'PROG' 'Z4_EXCEPTION_EXAMPLES'    'HEAD_SWAP_OPERATION_RIGHT',
     4  9 'CLEANUP (Right#2)'                'PROG' 'Z4_EXCEPTION_EXAMPLES'    'REPLACE_EVERYTHING',
     4 10 'RETRY'                            'PROG' 'Z4_EXCEPTION_EXAMPLES'    'RETRY',
     4 11 'RESUME'                           'PROG' 'Z4_EXCEPTION_EXAMPLES'    'RESUMABLE',
* 4/12 is not in the ABAP language but rather EIFFEL
     4 13 'Design by Contract'               'CLAS' 'ZCL_4_EXCEPTIONAL_MONSTER' 'OPEN_MONSTERS_EYES',
     4 14 'Building Error Message'           'CLAS' 'ZCL_DBC'                   'ENSURE',
     4 15 'Custom Constraint'                'PROG' 'Z4_EXCEPTION_EXAMPLES'     'LOCAL CLASS DEFINITIONS',
     4 16 'Class Invariant'                  'PROG' 'Z4_EXCEPTION_EXAMPLES'     'OPEN_MONSTERS_EYES',
     4 17 'Exception Class > Short Dump'     'PROG' 'Z4_EXCEPTION_EXAMPLES'     'RAISE_SHORT_DUMP',
*--------------------------------------------------------------------*
* Chapter 05 : TDD / ABAP Unit
*--------------------------------------------------------------------*
     5  1 'Dependencies'                     'PROG' 'ZL05_01_DEPENDENCIES'        'FIRE_NUCLEAR_MISSILE',
     5  2 'Test Seams'                       'PROG' 'ZL05_02_TEST_SEAMS'          'START-OF-SELECTION',
     5  3 'Broken Dependencies'              'PROG' 'ZL05_03_BROKEN_DEPENDENCIES' 'FIRE_NUCLEAR_MISSILE',
     5  4 'Test Seams#2'                     'PROG' 'LZCH05_01_02_TEST_SEAMST99'  'FIRE_NUCLEAR_MISSILE',
     5  5 'Test Double Definition'           'CLAS' 'ZCL_4_MONSTER_SIMULATOR'     'DEFINITION',
     5  6 'Helper Classes'                   'CLAS' 'ZCL_4_MONSTER_SIMULATOR'     'DEFINITION',
     5  7 'Helpers as Private Members'       'CLAS' 'ZCL_4_MONSTER_SIMULATOR'     'DEFINITION',
     5  8 'Bad Way to Create Objects'        'CLAS' 'ZCL_4_MONSTER_SIMULATOR'     'CONSTRUCTOR',
     5  9 'Better Way to Create Objects'     'CLAS' 'ZCL_4_MONSTER_SIMULATOR'     'CONSTRUCTOR',
     5 10 'Best Way to Create Objects'       'CLAS' 'ZCL_4_MONSTER_SIMULATOR'     'CONSTRUCTOR',
     5 11 'Factory Definition'               'CLAS' 'ZCL_4_MONSTER_FACTORY'       'DEFINITION',
     5 12 'Injector Example'                 'CLAS' 'ZCL_4_MONSTER_INJECTOR'      'INJECT_MONSTER_MAKING_MACHINE',
     5 13 'Getting an Instance'              'CLAS' 'ZCL_4_MONSTER_FACTORY'       'GET_MONSTER_MAKING_MACHINE',
     5 14 'Dependency Lookup Injecction'     'CLAS' 'ZCL_4_MONSTER_SIMULATOR'     'SETUP',
     5 15 'Defining Test Class'              'CLAS' 'ZCL_4_MONSTER_SIMULATOR'     'LOCAL_TEST_CLASSES',
     5 16 'Test Class General Settings'      'CLAS' 'ZCL_4_MONSTER_SIMULATOR'     'LOCAL_TEST_CLASS_DEFINITION',
     5 17 'Defining Test Doubles'            'CLAS' 'ZCL_4_MONSTER_SIMULATOR'     'LOCAL_TEST_CLASS_DEFINITION',
     5 18 'Test Class Variables'             'CLAS' 'ZCL_4_MONSTER_SIMULATOR'     'LOCAL_TEST_CLASS_DEFINITION',
     5 19 'IT SHOULD Methods'                'CLAS' 'ZCL_4_MONSTER_SIMULATOR'     'LOCAL_TEST_CLASS_DEFINITION',
     5 20 'GIVEN / WHEN / THEN'              'CLAS' 'ZCL_4_MONSTER_SIMULATOR'     'LOCAL_TEST_CLASS_DEFINITION',
     5 21 'Implementation of Test Class'     'CLAS' 'ZCL_4_MONSTER_SIMULATOR'     'RETURN_A_BOM_FOR_A_MONSTER',
     5 22 'SETUP Method Implementation'      'CLAS' 'ZCL_4_MONSTER_SIMULATOR'     'SETUP',
     5 23 'GIVEN Simulating External Input'  'CLAS' 'ZCL_4_MONSTER_SIMULATOR'     'GIVEN_MONSTER_DETAILS_ENTERED',
     5 24 'WHEN Calling Production Code'     'CLAS' 'ZCL_4_MONSTER_SIMULATOR'     'WHEN_BOM_IS_CALCULATED',
     5 25 'Check for Basic Errors'           'CLAS' 'ZCL_4_MONSTER_SIMULATOR'     'RETURN_A_BOM_FOR_A_MONSTER',
     5 26 'THEN Assertions in Tests'         'CLAS' 'ZCL_4_MONSTER_SIMULATOR'     'THEN_RESULTING_BOM_IS_CORRECT',
     5 27 'ASSERT_THAT Class Definiton'      'CLAS' 'ZCL_4_MONSTER_SIMULATOR'     'LOCAL_TEST_CLASS_DEFINITION',
     5 28 'ASSERT_THAT Class Implementation' 'CLAS' 'ZCL_4_MONSTER_SIMULATOR'     'LOCAL_TEST_CLASS_IMPLEMENTATION',
     5 29 'ASSERT_THAT In Action'            'CLAS' 'ZCL_4_MONSTER_SIMULATOR'     'MOCKING_FRAMEWORK_TEST',
* 30 - Defining and Implementing Interface in Test Class before 7.4
* 31 - Defining and Implementing Interface in Test Class in 7.4(PARTIALLY IMPLEMENTED)
* 32 - Test Double Implementations before 7.4
* 33 - Test Double Implementations using COND
* 34 - Building Up Complex Object before 7.4
* 35 - Building Up Complex Object in 7.4
     5 36 'ATDF - GIVEN Methods'             'CLAS' 'ZCL_4_MONSTER_SIMULATOR'     'MOCKING_FRAMEWORK_TEST',
     5 37 'ATDF - WHEN Methods'              'CLAS' 'ZCL_4_MONSTER_SIMULATOR'     'MOCKING_FRAMEWORK_TEST',
     5 38 'ATDF - THEN Methods'              'CLAS' 'ZCL_4_MONSTER_SIMULATOR'     'MOCKING_FRAMEWORK_TEST',
     5 39 'ATDF - Test Method'               'CLAS' 'ZCL_4_MONSTER_SIMULATOR'     'MOCKING_FRAMEWORK_TEST',
     5 40 'ATDF Exception Test'              'CLAS' 'ZCL_4_MONSTER_SIMULATOR'     'MOCKING_EXECEPTION_TEST',
* 41 - Eclipse - Definition of Test Class for Authority Checking Class
* 42 - Eclipse - Test Methods for Authority-Check Test Class
* 43 - Eclipse - Production Code using a test double instead of an AUTHORITY-CHECK statement
* 44 - Eclipse - Restricting User Authorizations during a Unit Test
     5 45 'Multiple Test Cases'              'CLAS' 'ZCL_4_MONSTER_SIMULATOR'     'MOCKUP_LOADER',
*--------------------------------------------------------------------*
* Chapter 06 : Database Programming - ABAP SQL / CDS Entities / AMDPs
*--------------------------------------------------------------------*
     6  1 'Traditional ABAP SQL'             'CLAS' 'ZCL_4_MONSTER_DATABASE_EXAMPLE' 'DERIVE_OLD_FASHIONED_MONSTERS',
     6  2 'CASE Statements in SQL'           'CLAS' 'ZCL_4_MONSTER_DATABASE_EXAMPLE' 'DERIVE_MONSTER_SCARINESS',
     6  3 'Business Data in 2 Steps'         'CLAS' 'ZCL_4_MONSTER_DATABASE_EXAMPLE' 'DERIVE_MONSTER_BUSINESS_V1',
     6  4 'Business Data in 1 Step'          'CLAS' 'ZCL_4_MONSTER_DATABASE_EXAMPLE' 'DERIVE_MONSTER_BUSINESS_V2',
     6  5 'Calculations outside SQL'         'CLAS' 'ZCL_4_MONSTER_DATABASE_EXAMPLE' 'DERIVE_SCARY_RATIO_OLD',
     6  6 'Calculations inside SQL'          'CLAS' 'ZCL_4_MONSTER_DATABASE_EXAMPLE' 'DERIVE_SCARY_RATIO_NEW',
     6  7 'Cool WHERE clauses'               'CLAS' 'ZCL_4_MONSTER_DATABASE_EXAMPLE' 'DERIVE_VIA_COOL_WHERE_CLAUSES',
     6  8 'String Functions in SQL'          'CLAS' 'ZCL_4_MONSTER_DATABASE_EXAMPLE' 'DERIVE_STRINGY_MONSTERS',
     6  9 'IS INITIAL in WHERE Clause'       'CLAS' 'ZCL_4_MONSTER_DATABASE_EXAMPLE' 'DERIVE_MONSTER_INITIALS',
     6 10 'SQL Function FLOOR'               'CLAS' 'ZCL_4_MONSTER_DATABASE_EXAMPLE' 'DERIVE_MONSTER_DANCE_FLOOR',
     6 11 'SQL Function in Query'            'CLAS' 'ZCL_4_MONSTER_DATABASE_EXAMPLE' 'DERIVE_TENTACLED_MONSTERS',
     6 12 'Case Insensitive Search'          'CLAS' 'ZCL_4_MONSTER_DATABASE_EXAMPLE' 'DERIVE_VILLAGES_BY_DESCRIPTION',
     6 13 'LAG and LEAD'                     'CLAS' 'ZCL_4_MONSTER_DATABASE_EXAMPLE' 'DERIVE_ADJACENT_VILLAGES',
     6 14 'Set Indicators'                   'CLAS' 'ZCL_4_MONSTER_DATABASE_EXAMPLE' 'UPDATE_GET_SET_GO',
     6 15 'Manual Internal Table'            'CLAS' 'ZCL_4_MONSTER_DATABASE_EXAMPLE' 'DERIVE_MANUAL_MONSTERS',
     6 16 'Automatic Internal Table'         'CLAS' 'ZCL_4_MONSTER_DATABASE_EXAMPLE' 'DERIVE_AUTOMATIC_MONSTERS',
     6 17 'Existence Check'                  'CLAS' 'ZCL_4_MONSTER_DATABASE_EXAMPLE' 'DERIVE_MONSTER_EXISTENCE',
     6 18 'INNER JOIN with Field List'       'CLAS' 'ZCL_4_MONSTER_DATABASE_EXAMPLE' 'DERIVE_RES_THE_HARD_WAY',
     6 19 'INNER JOIN with Wild Card'        'CLAS' 'ZCL_4_MONSTER_DATABASE_EXAMPLE' 'DERIVE_RES_THE_EASY_WAY',
     6 20 'ON CLAUSE Improvements'           'CLAS' 'ZCL_4_MONSTER_DATABASE_EXAMPLE' 'DERIVE_VIA_COOL_ON_CLAUSE',
     6 21 'UNION in ABAP'                    'CLAS' 'ZCL_4_MONSTER_DATABASE_EXAMPLE' 'DERIVE_UNION_MEMBERSHIP',
     6 22 'INSERT Aggregated Data'           'CLAS' 'ZCL_4_MONSTER_DATABASE_EXAMPLE' 'DERIVE_BY_AGGREGATION',
     6 23 'Common Table Expressions 1'       'CLAS' 'ZCL_4_MONSTER_DATABASE_EXAMPLE' 'DERIVE_DELS_BY_NO_OF_HEADS',
     6 24 'Common Table Expressions 2'       'CLAS' 'ZCL_4_MONSTER_DATABASE_EXAMPLE' 'DERIVE_DELS_BY_NO_OF_HEADS',
     6 25 'IS NOT INITIAL in SELECT'         'CLAS' 'ZCL_4_MONSTER_DATABASE_EXAMPLE' 'DERIVE_BLANK_LOOKS',
     6 26 'Filling a Range Table'            'CLAS' 'ZCL_4_MONSTER_DATABASE_EXAMPLE' 'DERIVE_MONSTER_FILLINGS',
     6 27 'Global Temporary Table'           'CLAS' 'ZCL_4_MONSTER_DATABASE_EXAMPLE' 'DERIVE_GLOBAL_MONSTERS',
     6 28 'SELECT from Internal Table'       'CLAS' 'ZCL_4_MONSTER_DATABASE_EXAMPLE' 'DERIVE_TABLED_MOTION',
     6 29 'OSQL Setup/Teardown'              'CLAS' 'ZCL_4_MONSTER_DATABASE_EXAMPLE' 'LOCAL_TEST_METHODS',
     6 30 'OSQL Unit Test'                   'CLAS' 'ZCL_4_MONSTER_DATABASE_EXAMPLE' 'LOCAL_TEST_METHODS',
* 31 - CDS Entity Code generated from Template
* 32 - CDS Entity with JOINS
* 33 - List of Fields for CDS Entity to Get from Database
* 34 - CASE Statement within CDS Entity
* 35 - WHERE Clause in CDS Entity
* 36 - Complete DDL Source for Generating CDS Entity
* 37 - Monster Root View Entity
* 38 - Monster Items CDS Entity associated to its Parent
* 39 - Coding DCL Source
     6 40 'Reading CDS View from ABAP'       'CLAS' 'ZCL_4_MONSTER_DATABASE_EXAMPLE' 'DERIVE_GREEN_MONSTERS',
* 41 - Abstract CDS Entity
* 42 - Using a Virtual Element inside a CDS Entity
* 43 - Defining Hierarchy Source CDS Entity with Self-Association
* 44 - CDS Entity to Expose a Hierarchy
* 45 - Reading from a CDS Hierarchy
     6 46 'CDS Test Double Setup/Teardown'   'CLAS' 'ZCL_4_MONSTER_DATABASE_EXAMPLE' 'LOCAL_TEST_METHODS',
     6 47 'CDS Test Double Unit Test'        'CLAS' 'ZCL_4_MONSTER_DATABASE_EXAMPLE' 'LOCAL_TEST_METHODS',
* 48 - Class Definition with Interface to Enable Use of AMDPs
* 49 - Implementing Method to Run inside Database
* 50 - Coding Database SELECT inside AMDP
* 51 - Calling AMDP from ABAP Program
* 52 - Coding CDS View Table Function
* 53 - Defining CDS Table Function in ABAP Class
* 54 - SQLScript Coding for Table Function
* 55 - Reading Data from CDS View with Table Function (inside ABAP)
* 56 - Removing Bad Debt Records from Table on Application Server
* 57 - AMDP Method Definition to Get Monster Cleared Items
* 58 - AMDP Method Implementation to Get Monster Cleared Items
*--------------------------------------------------------------------*
* Chapter 07 : BOPF
*--------------------------------------------------------------------*
* 01 - Adding BOPF Annotations to CDS View
     7  2 'BOPF P.Layer Constructor'         'CLAS' 'ZCL_4_MONSTERMODEL_PERS_BOPF'   'CONSTRUCTOR',
     7  3 'Retrieve Monster Record'          'CLAS' 'ZCL_4_MONSTERMODEL_PERS_BOPF'   'DERIVE_MONSTER_RECORD',
     7  4 'Getting a BOPF Key'               'CLAS' 'ZCL_4_MONSTERMODEL_PERS_BOPF'   'GET_BOPF_KEY_4_MONSTER_NUMBER',
     7  5 'Get Node Row'                     'CLAS' 'ZCL_4_BC_BOPF_PL_HELPER'        'GET_NODE_ROW',
     7  6 'Get Node Table'                   'CLAS' 'ZCL_4_BC_BOPF_PL_HELPER'        'GET_NODE_TABLE',
     7  7 'Get Item Table'                   'CLAS' 'ZCL_4_BC_BOPF_PL_HELPER'        'GET_CHILD_NODE_TABLE',
* 08 - Exclusive Lock
     7  9 'Determination of Header Texts'    'CLAS' 'ZCL_4_MONSTER_MODEL'            'DERIVE_HEADER_FIELDS',
     7 10 'Checking Changed Fields'          'CLAS' 'ZCL_4_D_MONSTERHEADER_TEXTS'    'CHECK DELTA',
     7 11 'Check Method'                     'CLAS' 'ZCL_4_D_MONSTERHEADER_TEXTS'    'CHECK',
     7 12 'Execute Determination'            'CLAS' 'ZCL_4_D_MONSTERHEADER_TEXTS'    'EXECUTE',
     7 13 'Validation - Monster Model'       'CLAS' 'ZCL_4_MONSTER_MODEL'            'VALIDATE_MONSTER_HEADER',
     7 14 'Validation - BOPF'                'CLAS' 'ZCL_4_V_MONSTERHEADER_CON_CHK'  'EXECUTE',
     7 15 'Howling Method in Model Class'    'CLAS' 'ZCL_4_MONSTER_MODEL'            'ACTION_HOWL_AT_THE_MOON',
     7 16 'Howling Methdod in BOPF'          'CLAS' 'ZCL_4_A_HOWL_AT_THE_MOON'       'EXECUTE',
     7 17 'Action Validation in BOPF'        'CLAS' 'ZCL_4_A_V_CHECK_HOWLING_STATUS' 'EXECUTE',
     7 18 'Creating Record in BOPF'          'CLAS' 'ZCL_4_MONSTERMODEL_PERS_BOPF'   'CREATE_MONSTER_RECORD',
     7 19 'Change Data in Memory'            'CLAS' 'ZCL_4_BC_BOPF_PL_HELPER'        'CHANGE_DATA_IN_MEMORY',
     7 20 'Change Data in Database'          'CLAS' 'ZCL_4_BC_BOPF_PL_HELPER'        'CHANGE_DATA_IN_DATABASE',
     7 21 'Changing Record in BOPF'          'CLAS' 'ZCL_4_MONSTERMODEL_PERS_BOPF'   'UPDATE_MONSTER_RECORD',
* 22 - Redefining Standard BOPF Change Document Method (Only works in an ERP system)
     7 23 'BUnit Test Defintion'             'CLAS' 'ZCL_4_A_EXPLODE_ALL_HEADS'      'LOCAL_TEST_CLASSES',
     7 24 'BUnit Test Implementation'        'CLAS' 'ZCL_4_A_EXPLODE_ALL_HEADS'      'LOCAL_TEST_CLASSES',
*--------------------------------------------------------------------*
* Chapter 08 : RAP Business Logic
*--------------------------------------------------------------------*
* Everything lives in the BTP ABAP Environment
*--------------------------------------------------------------------*
* Chapter 09 : Service Layer
*--------------------------------------------------------------------*
     9  1 'Monster DPC Constructor'          'CLAS' 'ZCL_Z_4_MONSTER_DPC_EXT'       'CONSTRUCTOR',
     9  2 'Monsters Get Entity Set'          'CLAS' 'ZCL_Z_4_MONSTER_DPC_EXT'       'MONSTERS_GET_ENTITYSET',
     9  3 'Monsters Items Get Entity Set'    'CLAS' 'ZCL_Z_4_MONSTER_DPC_EXT'       'MONSTERITEMS_GET_ENTITYSET',
     9  4 'Monsters Get Entity'              'CLAS' 'ZCL_Z_4_MONSTER_DPC_EXT'       'MONSTERS_GET_ENTITY',
     9  5 'Monsters Delete Entity'           'CLAS' 'ZCL_Z_4_MONSTER_DPC_EXT'       'MONSTERS_DELETE_ENTITY',
     9  6 'Auto-Generated DPC Method'        'CLAS' 'ZCL_Z_4_MONSTER_DELIVE_DPC'    'Z4CDS_MONSTER_DE_GET_ENTITY',
* 07 - CDS View which Generates a BOPF Object and SEGW Service (Eclipse 7.52 ABAP)
* 08 - Monster Service Definition (BTP ABAP)
* 09 - Test Class Generated from Service Binding (BTP ABAP)
* 10 - Monster Sales Order JSON Configuration File (BTP)
*--------------------------------------------------------------------*
* Chapter 10 : SALV
*--------------------------------------------------------------------*
    10  1 'MAIN Method in SALV Reports'      'PROG' 'Z4_MONSTER_ADL_IO1'            'MAIN',
    10  2 'Preparing and Displaying Data'    'CLAS' 'ZCL_BC_VIEW_SALV_TABLE'        'PREPARE_DISPLAY_DATA',
    10  3 'Creating SALV Object'             'CLAS' 'ZCL_BC_VIEW_SALV_TABLE'        'INITIALISE',
    10  4 'Display Basic Toolbar'            'CLAS' 'ZCL_BC_VIEW_SALV_TABLE'        'DISPLAY_BASIC_TOOLBAR',
    10  5 'Setting up the Layout'            'CLAS' 'ZCL_BC_VIEW_SALV_TABLE'        'SET_LAYOUT',
    10  6 'Setting up Event Handlers'        'CLAS' 'ZCL_BC_VIEW_SALV_TABLE'        'SET_HANDLERS',
    10  7 'Full Initlization Method '        'CLAS' 'ZCL_BC_VIEW_SALV_TABLE'        'INITIALISE',
    10  8 'Application Specific Changes'     'CLAS' 'ZCL_BC_VIEW_SALV_TABLE'        'APPLICATION_SPECIFIC_CHANGES',
    10  9 'Set Column Attributes'            'CLAS' 'ZCL_BC_VIEW_SALV_TABLE'        'SET_COLUMN_ATTRIBUTES',
    10 10 'Set Checkbox'                     'CLAS' 'ZCL_BC_VIEW_SALV_TABLE'        'SET_CHECKBOX',
    10 11 'Set Hotspot'                      'CLAS' 'ZCL_BC_VIEW_SALV_TABLE'        'SET_HOTSPOT',
    10 12 'Set Visible'                      'CLAS' 'ZCL_BC_VIEW_SALV_TABLE'        'SET_VISIBLE',
    10 13 'Set Technical'                    'CLAS' 'ZCL_BC_VIEW_SALV_TABLE'        'SET_TECHNICAL',
    10 14 'Set Column as Push Button'        'CLAS' 'ZCL_BC_VIEW_SALV_TABLE'        'SET_COLUMN_AS_BUTTON',
    10 15 'Set Sub-Total'                    'CLAS' 'ZCL_BC_VIEW_SALV_TABLE'        'SET_SUBTOTAL',
    10 16 'Set Long Text'                    'CLAS' 'ZCL_BC_VIEW_SALV_TABLE'        'SET_LONG_TEXT',
    10 17 'Set Tooltip'                      'CLAS' 'ZCL_BC_VIEW_SALV_TABLE'        'SET_TOOLTIP',
    10 18 'Adding Sort Criteria'             'CLAS' 'ZCL_BC_VIEW_SALV_TABLE'        'ADD_SORT_CRITERIA',
    10 19 'Displaying the SALV Grid'         'CLAS' 'ZCL_BC_VIEW_SALV_TABLE'        'DISPLAY',
    10 20 'Responding to Double Click'       'CLAS' 'ZCL_BC_VIEW_SALV_TABLE'        'HANDLE_LINK_CLICK',
    10 21 'Responding to User Command'       'CLAS' 'ZCL_BC_VIEW_SALV_TABLE'        'HANDLE_USER_COMMAND',
    10 22 'Refresh Display'                  'CLAS' 'ZCL_BC_VIEW_SALV_TABLE'        'REFRESH_DISPLAY',
    10 23 'Create Container Automatically'   'CLAS' 'ZCL_BC_VIEW_SALV_TABLE'        'CREATE_CONTAINER_PREP_DISPLAY',
    10 24 'Fill Container Content'           'CLAS' 'ZCL_BC_VIEW_SALV_TABLE'        'FILL_CONTAINER_CONTENT',
    10 25 'Creating SALV with Container'     'CLAS' 'ZCL_BC_VIEW_SALV_TABLE'        'INITIALISE',
    10 26 'Adding Custom User Commands'      'CLAS' 'ZCL_BC_VIEW_SALV_TABLE'        'ADD_COMMANDS_TO_TOOLBAR',
    10 27 'Model adding User Commands'       'REPS' 'Z4_MONSTER_AM_IO1'             'FILL_USER_COMMANDS',
    10 28 'Enhancing SAP UI Class'           'CLAS' 'CL_SALV_GRID_ADAPTER'          'ZEI_C10_SALV',
    10 29 'Subclassing SALV Model'           'CLAS' 'ZCL_SALV_MODEL'                'CONSTRUCTOR',
    10 30 'Change Initalisation Method'      'CLAS' 'ZCL_BC_VIEW_SALV_TABLE'        'INITIALISE',
    10 31 'Prepare for Grid Refresh Event'   'CLAS' 'ZCL_SALV_MODEL'                'SET_EDITABLE',
    10 32 'Make Fields Editable'             'CLAS' 'ZCL_BC_VIEW_SALV_TABLE'        'MAKE_COLUMN_EDITABLE',
    10 33 'Get Underlying Grid Object'       'CLAS' 'ZCL_SALV_MODEL'                'GET_ALV_GRID',
    10 34 'Handle AFTER REFRESH event'       'CLAS' 'ZCL_BC_SALV_EVENT_HANDLER'     'ON_AFTER_REFRESH',
    10 35 'Report Calling SALV + Container'  'REPS' 'Z4_MONSTER_ADL_IO1'            'MAIN',
    10 36 'User Command ZEDIT (Controller)'  'REPS' 'Z4_MONSTER_ADL_IO1'            'ON_USER_COMMAND',
    10 37 'Get Underlying Grid Object'       'CLAS' 'ZCL_BC_VIEW_SALV_TABLE'        'GET_ALV_GRID_OBJECT',
    10 38 'Making Column Editable (Cont)'    'REPS' 'Z4_MONSTER_ADL_IO1'            'MAKE_COLUMN_EDITABLE',
    10 39 'SALV IDA Demo - with Criteria'    'REPS' 'Z4_MONSTER_ADL_IO1'            'IDA_DEMO',
    10 40 'SALV IDA Demo - Basic'            'REPS' 'Z4_MONSTER_ADL_IO1'            'IDA_DEMO',
    10 41 'SALV IDA Demo - CDS View'         'REPS' 'Z4_MONSTER_ADL_IO1'            'IDA_DEMO',
    10 42 'IDA Calculated Field Def'         'REPS' 'Z4_MONSTER_ADL_CD01'           'CLASS DEFINITIONS',
    10 43 'IDA Calculated Field Imp'         'REPS' 'Z4_MONSTER_ADL_IO1'            'CLASS IMPLEMENTATIONS',
    10 44 'IDA with Field Calculator'        'REPS' 'Z4_MONSTER_ADL_IO1'            'IDA_DEMO2',
*--------------------------------------------------------------------*
* Chapter 11 : ABAP2XLSX
*--------------------------------------------------------------------*
* Listing 1.1: Creating the Excel Object and Navigating to the First Worksheet (Generic Example)
* Listing 1.2: Adding Desired Attributes to a Spreadsheet Object (Generic Example)
* Listing 1.3: Converting an Excel Object to XML (Generic Example)
    11 04 'Downloading Spreadsheet'  'REPS' 'Z4_MONSTER_ADL_IO1' 'ON_USER_COMMAND',
    11 05 'Convert SALv to Excel'    'REPS' 'Z4_MONSTER_ADL_IO1' 'ON_USER_COMMAND',
    11 06 'Data Declarations'        'REPS' 'Z4_MONSTER_ADL_IO1' 'ADJUST SPREADSHEET',
    11 07 'Looping Through Cells'    'REPS' 'Z4_MONSTER_ADL_IO1' 'ADJUST SPREADSHEET',
    11 08 'Finding Cell Style'       'REPS' 'Z4_MONSTER_ADL_IO1' 'ADJUST SPREADSHEET',
    11 09 'Cell Formatting'          'REPS' 'Z4_MONSTER_ADL_IO1' 'ADJUST SPREADSHEET',
    11 10 'Landscape Orientation'    'REPS' 'Z4_MONSTER_ADL_IO1' 'ADJUST SPREADSHEET',
    11 11 'Header and Footer'        'REPS' 'Z4_MONSTER_ADL_IO1' 'ADJUST SPREADSHEET',
    11 12 'Header Row Repeats'       'REPS' 'Z4_MONSTER_ADL_IO1' 'ADJUST SPREADSHEET',
    11 13 'Column Constants'         'REPS' 'Z4_MONSTER_ADL_TOP' 'DATA DECLARATIONS',
    11 13 'Creating Colors'          'REPS' 'Z4_MONSTER_ADL_IO1' 'ADJUST SPREADSHEET',
    11 15 'Conditional Formatting'   'REPS' 'Z4_MONSTER_ADL_IO1' 'ADJUST SPREADSHEET',
    11 16 'Testing String Start'     'REPS' 'Z4_MONSTER_ADL_IO1' 'ADJUST SPREADSHEET',
* Listing 1.17: Generated XML Code (in XML strangely enough!)
* Listing 1.18: Building Up XML for New Type of Conditional Formatting (ZCL_EXCEL_WRITER_2007 / CREATE_XL_SHEET))
* Listing 1.19: Standard ABAP2XLSX Code Linking Nodes Together ZCL_EXCEL_WRITER_2007 / CREATE_XL_SHEET))
    11 20 'I Like Traffic Lights'    'REPS' 'Z4_MONSTER_ADL_IO1' 'ADJUST SPREADSHEET',
    11 21 'Negative => Positive'     'REPS' 'Z4_MONSTER_ADL_IO1' 'ADJUST SPREADSHEET',
* Listing 1.22: Creating a Second Worksheet (Generic Example)
    11 23 'Pie Chart Data Worksheet' 'REPS' 'Z4_MONSTER_ADL_IO1' 'ADJUST SPREADSHEET',
    11 23 'Pie Chart Worksheet'      'REPS' 'Z4_MONSTER_ADL_IO1' 'ADJUST SPREADSHEET',
* Listing 1.25: Macro to Change the Name of a Worksheet (Visual Basic)
* Listing 1.26: Turning a Blank Spreadsheet into an XSTRING (Generic Example)
* Listing 1.27: Storing the Spreadsheet Template in the Database (Generic Example)
    11 28 'Upload a Template'         'REPS' 'Z4_MONSTER_ADL_IO1' 'ON_USER_COMMAND',
    11 29 'Fill Template with Data'   'REPS' 'Z4_MONSTER_ADL_IO1' 'ON_USER_COMMAND',
    11 30 'Download Extension Name'   'REPS' 'Z4_MONSTER_ADL_IO1' 'ON_USER_COMMAND',
    11 31 'Send Spreadsheet by Email' 'CLAS' 'ZCL_EXCEL_EMAILER'  'SEND',
    11 32 'Inserting a Hyperlink'     'REPS' 'Z4_MONSTER_ADL_IO1' 'ADJUST SPREADSHEET',
    11 33 'Build Hyperlink URL'       'CLAS' 'ZCL_EXCEL_EMAILER'  'BUILD_HYPERLINK_URL',
* Listing 1.34: Coding an Overwrite Method - Oh Dear! The method to be over-written no longer exists!
    11 35 'Generated TYPE Defintions' 'PROG' 'Z4_MONSTER_WORD_INVOICES' 'DEFINITIONS',
    11 36 'Dynamic Word Document'     'PROG' 'Z4_MONSTER_WORD_INVOICES' 'MAIN',
*--------------------------------------------------------------------*
* Chapter 12 : Web Dynpro ABAP
*--------------------------------------------------------------------*
    12 01 'Reacting to FIND MONSTERS'        'WDYN' 'ZWDC_4_MONSTER_LIST'            'ONACTIONFIND_MONSTERS',
    12 02 'Controller gets Monster Data'     'WDYN' 'ZWDC_4_MONSTER_LIST'            'RETRIEVE_HEADERS_BY_ATTRIBUTE',
    12 03 'Action to Display Monster'        'WDYN' 'ZWDC_4_MONSTER_LIST'            'ONACTIONSHOW_MONSTER',
    12 04 'Handling Inbound Plug'            'WDYN' 'ZWDC_4_MONSTER_LIST'            'HANDLEIP_FROM_HEADER_TABLE',
    12 05 'Controller Reads Monster Data'    'WDYN' 'ZWDC_4_MONSTER_LIST'            'RETRIEVE_MONSTER',
    12 06 'FPM Feeder Class Defintion'       'CLAS' 'ZCL_4_FPM_MONSTERSEARCH_FEEDER' 'GET_DEFINITION',
    12 07 'FPM Feeder Where Clause'          'CLAS' 'ZCL_4_FPM_MONSTERSEARCH_FEEDER' 'PROCESS_EVENT',
    12 08 'FPM Feeder Return Result'         'CLAS' 'ZCL_4_FPM_MONSTERSEARCH_FEEDER' 'GET_DATA',
    12 09 'WDA Unit Test Setup'              'CLAS' 'ZCL_4_WDA_MONSTER_TEST_CLASS'   'SETUP',
    12 10 'WDA Unit Test Execution'          'CLAS' 'ZCL_4_WDA_MONSTER_TEST_CLASS'   'GIVEN_USER_TICKS_BOX',
*--------------------------------------------------------------------*
* Chapter 13 : UI5
*--------------------------------------------------------------------*
* Everything lives in the BTP ABAP Environment
*--------------------------------------------------------------------*
* Chapter 14 : Push Channels
*--------------------------------------------------------------------*
     14 01 'Define Target Channel'         'REPS' 'Z4_MONSTER_ADL_IO1'             'ALLOCATE MONSTER',
     14 02 'Check Channel Exists'          'REPS' 'Z4_MONSTER_ADL_IO1'             'ALLOCATE MONSTER',
     14 03 'Prepare/Send Message'          'REPS' 'Z4_MONSTER_ADL_IO1'             'ALLOCATE MONSTER',
     14 04 'Receiver Class Definition'     'REPS' 'Z4_MONSTER_AM_CD01'             'LCL_AMC_RECEIVER',
     14 05 'Receiver Class Implementation' 'REPS' 'Z4_MONSTER_AM_IO1'              'RECEIVE',
     14 06 'Subscribing to Messages'       'REPS' 'Z4_MONSTER_AM_IO1'              'MAIN',
     14 07 'ON_START Method'               'CLAS' 'ZCL_APC_WSP_EXT_ZAPC_4_MONSTER' 'ON_START',
     14 08 'ON_MESSAGE Method'             'CLAS' 'ZCL_APC_WSP_EXT_ZAPC_4_MONSTER' 'ON_MESSAGE',
     14 09 'Binding APC to AMC'            'CLAS' 'ZCL_APC_WSP_EXT_ZAPC_4_MONSTER' 'ON_START'.
* Listing 1.10:  Establish WebSocket Connection from SAPUI5 (JavaScript)
* Listing 1.11:  Sending Message from SAPUI5 into SAP (JavaScript)
*--------------------------------------------------------------------*

    DELETE et_output_data WHERE chapter NOT IN go_selections->s_chap[].

    LOOP AT et_output_data ASSIGNING FIELD-SYMBOL(<ls_output_data>).
      CASE <ls_output_data>-object_type.
        WHEN 'PROG' OR 'REPS'.
          SELECT SINGLE devclass
            FROM tadir
            INTO <ls_output_data>-devclass
            WHERE pgmid    EQ 'R3TR'
            AND   object   EQ 'PROG'
            AND   obj_name EQ <ls_output_data>-object_name.
        WHEN 'CLAS' OR 'WDYN'.
          SELECT SINGLE devclass
            FROM tadir
            INTO <ls_output_data>-devclass
            WHERE pgmid    EQ 'R3TR'
            AND   object   EQ <ls_output_data>-object_type
            AND   obj_name EQ <ls_output_data>-object_name.
        WHEN OTHERS.
          CONTINUE.
      ENDCASE.
      "It is just not worth figuring out how to get from A to B in this case
      IF <ls_output_data>-object_name EQ 'LZCH05_01_02_TEST_SEAMST99'.
        <ls_output_data>-devclass = 'Z4_002_TDD_EXAMPLES'.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.                                               "get_data

ENDCLASS.                    "lcl_persistency_layer IMPLEMENTATION

*----------------------------------------------------------------------*
*       CLASS lcl_model IMPLEMENTATION
*----------------------------------------------------------------------*
* When creating the model for real we do not fill the import parameter
* and thus the data is read for real
* When creating the model within a unit test, we pass in a reference to
* the fake database access class
*----------------------------------------------------------------------*
CLASS lcl_model IMPLEMENTATION.

  METHOD constructor.

    super->constructor( ).

    IF io_access_class IS SUPPLIED.
      mo_persistency_layer = io_access_class.
    ELSE.
      mo_persistency_layer = NEW #( ).
    ENDIF.

    fill_user_commands( ).

    fill_layout_data( ).

    fill_technical_fields( ).

    fill_hidden_fields( ).

    fill_hotspot_fields( ).

    fill_subtotal_fields( ).

    fill_field_texts( ).

    fill_editable_fields( ).

    fill_checkbox_fields( ).

    set_edit_control_field( ).

  ENDMETHOD.                                               "constructor

  METHOD data_retrieval.

    mt_output_data = mo_persistency_layer->get_data( ).

  ENDMETHOD.                                               "data_retrieval

*--------------------------------------------------------------------*
* METHOD prepare_data_for_output
*--------------------------------------------------------------------*
* Get text names of objects, mapping, etc etc
*--------------------------------------------------------------------*
  METHOD prepare_data_for_ouput ##NEEDED.

  ENDMETHOD.                                               "prepare_data_for_ouput

  METHOD fill_user_commands.

    CLEAR mt_user_commands.

  ENDMETHOD.                                               "fill_user_commands

  METHOD fill_editable_fields ##NEEDED.

  ENDMETHOD.                    "fill_editable_fields

  METHOD fill_hidden_fields ##NEEDED.
    "No Hidden Fields
  ENDMETHOD.                    "fill_hidden_fields

  METHOD fill_technical_fields ##NEEDED.

  ENDMETHOD.                    "fill_technical_fields

  METHOD fill_hotspot_fields.
    APPEND 'LISTING' TO mt_hotspots.
  ENDMETHOD.                    "fill_hotspot_fields

  METHOD fill_subtotal_fields ##NEEDED.
    "No Subtotals
  ENDMETHOD.                    "fill_subtotal_fields

  METHOD fill_field_texts.
    DEFINE add_text.
      APPEND VALUE #(
      field_name = &1
      long_text  = &2
      tooltip    = &3 )
      TO mt_field_texts.
    END-OF-DEFINITION.

    add_text:
    'CHAPTER'     'Chapter'             'Chapter Number',
    'LISTING'     'Listing'             'Listing Number',
    'NAME'        'Listing Description' 'Listing Description',
    'OBJECT_NAME' 'Object Name'         'Program/Class Name',
    'SUBOBJECT'   'Routine Name'        'Routine/Method Name'.

  ENDMETHOD.                    "fill_field_texts

  METHOD fill_checkbox_fields ##NEEDED.

  ENDMETHOD.                    "fill_checkbox_fields

  METHOD fill_layout_data.

    ms_layout = VALUE #( list_header       = 'Monster Listing Mapping'
                         variant           = go_selections->p_vari
                         colwidth_optimize = abap_true
                         striped_pattern   = abap_true
                         no_cell_merging   = abap_true ).

  ENDMETHOD.

  METHOD user_command.

    TYPES: BEGIN OF ty_ztab,
             prog TYPE programm,
           END OF ty_ztab.

    TYPES: BEGIN OF ty_zprog,
             line TYPE char255,
           END OF ty_zprog.

    TYPES: BEGIN OF ty_zprogdet,
             prog     TYPE programm,
             devclass TYPE tadir-devclass,
             linenr   TYPE i,
             line     TYPE char72,
           END OF ty_zprogdet.

    DATA: i_prog            TYPE STANDARD TABLE OF ty_ztab,
          i_zprog           TYPE STANDARD TABLE OF ty_zprog,
          i_zprogdet        TYPE STANDARD TABLE OF ty_zprogdet,
          ld_search_progs   TYPE c LENGTH 30,
          chapter_as_numc   TYPE n LENGTH 2,
          listing_as_numc   TYPE n LENGTH 2,
          chapter_as_string TYPE string,
          listing_as_string TYPE string,
          both_as_string    TYPE string,
          start_line        TYPE c LENGTH 6.

    CASE id_user_command.
      WHEN '&IC1'.
        READ TABLE mt_output_data ASSIGNING FIELD-SYMBOL(<ls_output>) INDEX id_row.
        IF sy-subrc NE 0.
          RETURN.
        ENDIF.
        CASE id_column.
          WHEN 'LISTING'.
            "Drill Down
            CASE <ls_output>-object_type.
              WHEN 'PROG' OR 'REPS'.
                ld_search_progs   = <ls_output>-object_name.
              WHEN 'CLAS'.
                IF strlen( <ls_output>-object_name ) = 30.
                  ld_search_progs = <ls_output>-object_name(29) && '%'.
                ELSE.
                  ld_search_progs = <ls_output>-object_name && '%'.
                ENDIF.
              WHEN 'WDYN'.
                "I am amazed I got this to work. To say the link between the WDA name and the actual INCLUDE in which
                "the method lives is obscure is a collosal understatement
                cl_wdy_wb_naming_service=>get_classname_for_component(
                  EXPORTING
                    p_component        = CONV #( <ls_output>-object_name )
                  RECEIVING
                    p_classname        = DATA(ld_classname)
                  EXCEPTIONS
                    no_generation_info = 1
                    OTHERS             = 2 ).
                IF sy-subrc <> 0.
                  MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                    WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
                ENDIF.
                ld_search_progs = ld_classname(8) && 'B_' && ld_classname+8(11) && '%'.
              WHEN OTHERS.
                RETURN.
            ENDCASE."Object Type - Class, Program etc...
            chapter_as_numc   = <ls_output>-chapter.
            chapter_as_string = |{ chapter_as_numc }.|.
            chapter_as_string = condense( chapter_as_string ).
            listing_as_numc   = <ls_output>-listing.
            listing_as_string = |{ listing_as_numc }:|.
            listing_as_string = condense( listing_as_string ).
            both_as_string = chapter_as_string && listing_as_string.
            SELECT prog FROM d010sinf
            INTO TABLE i_prog
            WHERE prog LIKE ld_search_progs.
            IF sy-subrc NE 0.
              RETURN.
            ENDIF.
            LOOP AT i_prog ASSIGNING FIELD-SYMBOL(<wa_prog>).
              READ REPORT <wa_prog>-prog INTO i_zprog.
              IF sy-subrc = 0.

                LOOP AT i_zprog ASSIGNING FIELD-SYMBOL(<wa_zprog>). "#EC CI_NESTED

                  CHECK <wa_zprog>-line CS 'Listing'.
                  CHECK <wa_zprog>-line CS chapter_as_string.
                  CHECK <wa_zprog>-line CS listing_as_string.
                  CHECK <wa_zprog>-line CS both_as_string.

                  INSERT VALUE #(
                  prog   = <wa_prog>-prog
                  line   = <wa_zprog>-line
                  linenr = sy-tabix ) INTO TABLE i_zprogdet.

                ENDLOOP."One Program
              ENDIF."Could we read a program?
            ENDLOOP."All Programs
            IF lines( i_zprogdet ) EQ 0.
              RETURN.
            ELSE.
              READ TABLE i_zprogdet INTO DATA(wa_zprogdet) INDEX 1.
              ASSERT sy-subrc EQ 0.
            ENDIF.
            start_line = wa_zprogdet-linenr.
            CALL FUNCTION 'EDITOR_PROGRAM'
              EXPORTING
                appid       = 'PG'
                display     = 'X'
                program     = wa_zprogdet-prog
                line        = start_line
              EXCEPTIONS
                application = 1
                OTHERS      = 2.

            IF sy-subrc <> 0.
              RETURN.
            ENDIF.
          WHEN OTHERS.
            RETURN.
        ENDCASE."What column was selected for drill down?

      WHEN OTHERS.
        RETURN.
    ENDCASE."What user command was chosen?

  ENDMETHOD."User Command / Model

ENDCLASS.                    "lcl_model IMPLEMENTATION

*----------------------------------------------------------------------*
*       CLASS lcl_view IMPLEMENTATION
*----------------------------------------------------------------------*
CLASS lcl_view IMPLEMENTATION.

ENDCLASS.                    "lcl_view IMPLEMENTATION

*----------------------------------------------------------------------*
*       CLASS lcl_controller IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_controller IMPLEMENTATION.

  METHOD constructor.

    super->constructor( ).

    mo_model = io_model.
    mo_view  = io_view.

    "Make the controller react to the views events
    SET HANDLER on_user_command FOR mo_view.
    "If the model changes some data, then it needs to
    "tell the controller, so it can tell the view
    "to refresh the data
    SET HANDLER on_data_changed FOR mo_model.

  ENDMETHOD.                    "constructor

  METHOD on_user_command.
*--------------------------------------------------------------------*
* FOR EVENT added_function OF cl_salv_events
* IMPORTING ed_user_command
*           ed_row
*           ed_column.
*--------------------------------------------------------------------*

    mo_model->user_command(
        id_user_command = ed_user_command
        id_column       = ed_column
        id_row          = ed_row ).

    mo_view->refresh_display( ).

  ENDMETHOD."User Command / Controller

  METHOD on_data_changed.

    mo_view->refresh_display( ).

  ENDMETHOD.                                               "on_data_changed

ENDCLASS.                    "lcl_controller IMPLEMENTATION
