*"* use this source file for your ABAP unit test classes
*--------------------------------------------------------------------*
* Listing 05.15 - Defining a Test Class
*--------------------------------------------------------------------*
CLASS ltc_monster_simulator DEFINITION DEFERRED.

"Need to make the class under test "friends" with the test class
"in order to enable testing for private methods
CLASS zcl_4_monster_simulator DEFINITION LOCAL FRIENDS ltc_monster_simulator.

CLASS ltd_pers_layer DEFINITION FOR TESTING.
  PUBLIC SECTION.
    INTERFACES zif_4_monster_sim_pers_layer PARTIALLY IMPLEMENTED.

    ALIASES derive_monsters_under_bed FOR
            zif_4_monster_sim_pers_layer~derive_monsters_under_bed.
ENDCLASS.

CLASS ltd_pers_layer IMPLEMENTATION.

  METHOD derive_monsters_under_bed.

  ENDMETHOD.

ENDCLASS.

CLASS ltd_logger DEFINITION FOR TESTING.
  PUBLIC SECTION.
    INTERFACES zif_4_monster_logger PARTIALLY IMPLEMENTED.
ENDCLASS.

CLASS ltd_mmm DEFINITION FOR TESTING.
  PUBLIC SECTION.
    INTERFACES zif_4_monster_making_machine PARTIALLY IMPLEMENTED.
ENDCLASS.

CLASS ltd_mmm IMPLEMENTATION.

ENDCLASS.

*----------------------------------------------------------------------*
*       CLASS lcl_my_contstraint DEFINITION
*----------------------------------------------------------------------*
* Listing 05.27 - ASSERT THAT
* This constraint is used to demonstrate how to code a custom assertion
* which can be used when your test logic is more complex than the
* standard SAP assertions can cope with
*----------------------------------------------------------------------*
CLASS lcl_monster_constraint DEFINITION.

  PUBLIC SECTION.
    INTERFACES if_constraint.

ENDCLASS."Monster Constraint Defintion

*----------------------------------------------------------------------*
*       CLASS ltc_monster_simulator DEFINITION
*----------------------------------------------------------------------*

*--------------------------------------------------------------------*
* Listing 05.16 - Test Class General Settings
*--------------------------------------------------------------------*
CLASS ltc_monster_simulator DEFINITION FOR TESTING
   RISK LEVEL HARMLESS
   DURATION SHORT
   FINAL.

*--------------------------------------------------------------------*
* Listing 05.17 - Defining Test Double Classes for Injecting into Test Class
* Listing 05.18 - Variables for the Test Class Definition
*--------------------------------------------------------------------*
  PUBLIC SECTION.

  PRIVATE SECTION.
    CONSTANTS: pers_layer_interface TYPE seoclsname VALUE 'ZIF_4_MONSTER_SIM_PERS_LAYER',
               logger_interface     TYPE seoclsname VALUE 'ZIF_4_MONSTER_LOGGER',
               mmm_interface        TYPE seoclsname VALUE 'ZIF_4_MONSTER_MAKING_MACHINE',
               my_bed               TYPE string VALUE 'MY_BED',
               insomniac_eater      TYPE string VALUE 'INSOMNIAC_EATER',
               three_heads          TYPE int4   VALUE 3,
               five_monsters        TYPE int4   VALUE 5,
               fifteen_heads        TYPE int4   VALUE 15.

    DATA: mo_class_under_test   TYPE REF TO zcl_4_monster_simulator,
          mo_mock_pers_layer    TYPE REF TO zif_4_monster_sim_pers_layer,
          mo_mock_logger        TYPE REF TO zif_4_monster_logger,
          mo_mock_mmm           TYPE REF TO zif_4_monster_making_machine,
          ms_input_data         TYPE z4s_monster_input_data,
          mt_bom_data           TYPE z4tt_monster_items,
          total_heads_under_bed TYPE int4.

    METHODS: setup,
*--------------------------------------------------------------*
* Specifications
*--------------------------------------------------------------*
* Listing 05.19 - Test Methods that Define what an Application
*                 SHOULD do (as in IT SHOULD......)
*--------------------------------------------------------------*
      "IT is the Monster Simulator hence LTC_MONSTER_SIMULATOR
      "IT SHOULD.....................
      "User Acceptance Tests (Use Cases)
      return_a_bom_for_a_monster     FOR TESTING,
      make_the_monster_sing          FOR TESTING,
      make_the_monster_dance         FOR TESTING,
      make_the_monster_go_to_france  FOR TESTING,
      calculate_heads_under_bed      FOR TESTING,
      mocking_framework_test         FOR TESTING,
      mocking_exception_test         FOR TESTING,
      mockup_loader                  FOR TESTING,
*--------------------------------------------------------------*
* Listing 05.20 - The GIVEN / WHEN / THEN Pattern for Unit Tests
*--------------------------------------------------------------*
      "GIVEN.........................
      given_monster_details_entered,
      "WHEN..........................
      when_bom_is_calculated,
      "THEN..........................
      then_resulting_bom_is_correct,
      "The structure then repeats again and again....
      given_customizing_that_says
        IMPORTING for_monster_type        TYPE string
                  no_of_heads_is_normally TYPE int4,
      given_monster_numbers
        IMPORTING
          in_bed    TYPE string
          of_type   TYPE string
          there_are TYPE int4,
      when_head_nos_are_calculated,
      then_no_of_heads_should_be
        IMPORTING id_heads TYPE int4.

ENDCLASS."Test Class Definition

*----------------------------------------------------------------------*
*       CLASS lcl_monster_constraint IMPLEMENTATION
*----------------------------------------------------------------------*
* Listing 05.28 Implementation of a Custom Constraint Class
* Custom logic to implement a more complex assertion than is possible
* using standard SAP assertions
*----------------------------------------------------------------------*
CLASS lcl_monster_constraint IMPLEMENTATION.

  METHOD if_constraint~is_valid.
*--------------------------------------------------------------------*
* IMPORTING data_object TYPE data
* RETURNING result      TYPE abap_bool
*--------------------------------------------------------------------*
    DATA(monster) = CAST zcl_monster_simulator( data_object ).

    result = abap_false.

    CHECK monster->md_scariness     CS 'SCARY'.
    CHECK monster->md_bolts_in_neck EQ 2.
    CHECK monster->md_fluffiness    EQ 0.
    CHECK monster->md_color         NE 'PINK'.

    result = abap_true.

  ENDMETHOD.                    "IF_CONSTRAINT~is_valid

  METHOD if_constraint~get_description.
*--------------------------------------------------------------------*
* RETURNING result TYPE string_table
*--------------------------------------------------------------------*
    DATA(message) = |'Monster is no longer a monster!'(001)|.

    APPEND message TO result.

  ENDMETHOD.                    "IF_CONSTRAINT~get_description

ENDCLASS."Monster Constraint Implementation
*----------------------------------------------------------------------*
*       CLASS ltc_monster_simulator IMPLEMENTATION
*----------------------------------------------------------------------*
CLASS ltc_monster_simulator IMPLEMENTATION.
*--------------------------------------------------------------------*
* Listing 05.21 - Implementation of a Test Class
* Listing 05.25 - Unit Test to Check for Basic Errors
*--------------------------------------------------------------------*
  METHOD return_a_bom_for_a_monster.

    TRY.

        given_monster_details_entered( ).

        when_bom_is_calculated( ).

        then_resulting_bom_is_correct( ).

      CATCH zcx_violated_precondition.
        cl_abap_unit_assert=>fail( 'Violated Contract Precondition' ).
      CATCH zcx_violated_postcondition.
        cl_abap_unit_assert=>fail( 'Violated Contract Postcondition' ).
    ENDTRY.

  ENDMETHOD."Return a BOM for a Monster (Test Class)

  METHOD make_the_monster_sing.
  ENDMETHOD.                    "make_the_monster_sing

  METHOD make_the_monster_dance.
  ENDMETHOD.                    "make_the_monster_dance

  METHOD make_the_monster_go_to_france.
  ENDMETHOD.                    "make_the_monster_go_to_france

  METHOD calculate_heads_under_bed.

    given_customizing_that_says( for_monster_type        = insomniac_eater
                                 no_of_heads_is_normally = three_heads ).

    given_monster_numbers( in_bed    = my_bed
                           of_type   = insomniac_eater
                           there_are = five_monsters ).

    when_head_nos_are_calculated( ).

    then_no_of_heads_should_be( fifteen_heads ).

  ENDMETHOD.

*--------------------------------------------------------------------*
* Listing 05.36 - Coding a Unit Test without Definitions and Implementations
*--------------------------------------------------------------------*
  METHOD mocking_framework_test.
* Local Variables
    DATA: interface_name         TYPE seoclsname VALUE 'ZIF_4_MONSTER_SIMULATOR',
          mock_monster_simulator TYPE REF TO zif_4_monster_simulator,
          scariness_description  TYPE string.

    "Create the Test Double Instance
    mock_monster_simulator ?= cl_abap_testdouble=>create( interface_name ).

    "What result do we expect back from the called method?
    cl_abap_testdouble=>configure_call( mock_monster_simulator )->returning( 'REALLY SCARY' )->and_expect(
     )->is_called_times( 1 ).

    "Prepare the simulated input details e.g. monster strength
    given_monster_details_entered( ).

    "Say what method we are mocking and the input values
    TRY.
        mock_monster_simulator->calculate_scariness( is_bom_input_data = ms_input_data ).

        "Invoke the production code to be tested
        scariness_description = mock_monster_simulator->calculate_scariness( ms_input_data ).
      CATCH zcx_violated_precondition_stat.
        cl_abap_unit_assert=>fail( msg = 'Contract Precondition Violation' ).
    ENDTRY.

    "Was the correct value returned?
    cl_abap_unit_assert=>assert_equals( exp = 'REALLY SCARY'
                                        act = scariness_description
                                        msg = 'Monster is not scary enough' ).

    "Listen very carefully - was the method only called once?
    cl_abap_testdouble=>verify_expectations( mock_monster_simulator ).

*--------------------------------------------------------------------*
* Listing 05.29 - Calling a Custom Assertion
*--------------------------------------------------------------------*
* After having done some standard SAP assertions we now perform
* a user-defined assertion, where we program our own rules to see
* if a test passed or not
*--------------------------------------------------------------------*
    DATA(custom_constraint) = NEW lcl_monster_constraint( ).

    cl_abap_unit_assert=>assert_that( exp = custom_constraint
                                      act = scariness_description ).

  ENDMETHOD."Mocking Framework Test

*--------------------------------------------------------------------*
* Listing 05.37 Mocking Exception using ATDF
*--------------------------------------------------------------------*
  METHOD mocking_exception_test.
* Local Variables
    DATA: interface_name         TYPE seoclsname VALUE 'ZIF_4_MONSTER_SIMULATOR',
          mock_monster_simulator TYPE REF TO zif_4_monster_simulator,
          scariness_description  TYPE string.

    "Create the Test Double Instance
    mock_monster_simulator ?= cl_abap_testdouble=>create( interface_name ).

    "What result do we expect back from the called method?
    DATA(lo_violation) = NEW zcx_violated_precondition_stat( ).
    cl_abap_testdouble=>configure_call( mock_monster_simulator )->raise_exception( lo_violation ).

    "Prepare the simulated input details e.g. monster strength
    CLEAR ms_input_data.

    "Say what method we are mocking and the input values
    TRY.
        mock_monster_simulator->calculate_scariness( is_bom_input_data = ms_input_data ).

        "Invoke the production code to be tested
        scariness_description = mock_monster_simulator->calculate_scariness( ms_input_data ).

      CATCH zcx_violated_precondition_stat.
        "All is well, we wanted the exception to be raised
        RETURN.
    ENDTRY.

    "Was the correct value returned?
    cl_abap_unit_assert=>fail( msg = 'Expected Exception was not Raised' ).

  ENDMETHOD."Mocking Exception Test

*--------------------------------------------------------------------*
* Listing 05.38 Test Method to Load Multiple Test Cases
*--------------------------------------------------------------------*
  METHOD mockup_loader.
* Local Variables
    TYPES: BEGIN OF l_typ_monster_test_data,
             strength   TYPE  zde_monster_strength,
             brain_size TYPE  zde_monster_brain_size,
             sanity     TYPE  zde_monster_sanity,
             ssatn      TYPE  zde_component_type_percentage,
             sspdt      TYPE  zde_component_type_percentage,
           END OF   l_typ_monster_test_data.

* Need to specify the type of the table, to make sure
* correct tests are done on the data loaded from MIME
    DATA test_cases_table TYPE TABLE OF l_typ_monster_test_data.

    "Name of Entry in SMW0
    zcl_mockup_loader=>class_set_source(
      i_type = 'MIME'
      i_path = 'ZMONSTER_TEST_DATA' ).

    TRY.
        DATA(mockup_loader) = zcl_mockup_loader=>get_instance( ).
      CATCH zcx_mockup_loader_error INTO DATA(loader_exception).
        cl_abap_unit_assert=>fail( loader_exception->get_text( ) ).
    ENDTRY.

    TRY.
        "Load test cases. The format is SPREADSHEET NAME/Sheet Name
        mockup_loader->load_data(
         EXPORTING i_obj       = 'MONSTER_TEST_DATA/monster_tests'
         IMPORTING e_container = test_cases_table ).

      CATCH zcx_mockup_loader_error INTO loader_exception.
        cl_abap_unit_assert=>fail( loader_exception->get_text( ) ).
    ENDTRY.

    LOOP AT test_cases_table INTO DATA(test_case).
      mo_class_under_test->get_component_split(
        EXPORTING
          id_strength   = test_case-strength
          id_brain_size = test_case-brain_size
          id_sanity     = test_case-sanity
        IMPORTING
          id_ssatn      = DATA(actual_percentage_ssatn)
          id_sspdt      = DATA(actual_percentage_sspdt) ).

      cl_abap_unit_assert=>assert_equals(
      exp = test_case-ssatn
      act = actual_percentage_ssatn
      msg = |{ test_case-strength } + { test_case-brain_size } + { test_case-sanity } gets incorrect SSATN %age| ).

      cl_abap_unit_assert=>assert_equals(
      exp = test_case-sspdt
      act = actual_percentage_sspdt
      msg = |{ test_case-strength } + { test_case-brain_size } + { test_case-sanity } gets incorrect SSPDT %age| ).

    ENDLOOP."Test Cases

  ENDMETHOD."Mockup Loader

*--------------------------------------------------------------------*
* Listing 05.14 - Dependency Lookup: Injection
* Listing 05.22 - Create an Instance of the Class Under Test and
*                 clear all global variables
*--------------------------------------------------------------------*
  METHOD: setup."Called before every test

    "Create Test Doubles - with manually defined test double classes
    CREATE OBJECT mo_mock_pers_layer TYPE ltd_pers_layer.
    CREATE OBJECT mo_mock_logger TYPE ltd_logger.
    CREATE OBJECT mo_mock_mmm TYPE ltd_mmm.

    "OR Create Test Doubles - using ATDF
    mo_mock_pers_layer ?= cl_abap_testdouble=>create( pers_layer_interface ).
    mo_mock_logger     ?= cl_abap_testdouble=>create( logger_interface ).
    mo_mock_mmm        ?= cl_abap_testdouble=>create( mmm_interface ).

    "Create Instance of Class Under Test
    "Via Constructor Injection
    mo_class_under_test = NEW #(
      io_pers_layer             = mo_mock_pers_layer
      io_logger                 = mo_mock_logger
      io_monster_making_machine = mo_mock_mmm ).

    "OR Create Instance of Class Under Test
    "Via Injection via Factory (Dependency Lookup)
    DATA(lo_injector) = NEW zcl_4_monster_injector( ).
    lo_injector->inject_monster_sim_pl( mo_mock_pers_layer ).
    lo_injector->inject_logger( mo_mock_logger ).
    lo_injector->inject_monster_making_machine( mo_mock_mmm ).

    mo_class_under_test = NEW #( ).

    "Clear all global variables
    CLEAR: ms_input_data,
           mt_bom_data.

  ENDMETHOD."Setup - Implementation

  "GIVEN.........................
  METHOD given_customizing_that_says.

    cl_abap_testdouble=>configure_call( mo_mock_pers_layer )->returning( no_of_heads_is_normally ).

    mo_mock_pers_layer->derive_heads_per_monster_type( for_monster_type ).

  ENDMETHOD.

  METHOD given_monster_numbers.

    cl_abap_testdouble=>configure_call( mo_mock_pers_layer )->returning(
    VALUE zif_4_monster_simulator=>m_tt_monsters_under_bed(
    ( bed_name      = in_bed
      monster_type  = of_type
      total_of_type = there_are ) ) ).

    mo_mock_pers_layer->derive_monsters_under_bed( in_bed ).

  ENDMETHOD.

*--------------------------------------------------------------------*
* Listing 05.23 - Preparing Test Data by Simulating External Input
*--------------------------------------------------------------------*
  METHOD given_monster_details_entered.

    ms_input_data = VALUE #(
    model_desired              = 'BTNK'
    color_desired              = 'GREEN'
    brain_size_desired         = 'MICRO'
    early_age_strength_desired = 14
    eas_days_desired           = 7 ).

  ENDMETHOD."Monster Details Entered – Implementation

*--------------------------------------------------------------------*
* Listing 05.24 - Calling the Production Code to be Tested
*--------------------------------------------------------------------*
  "WHEN..........................
  METHOD when_head_nos_are_calculated.

    total_heads_under_bed = mo_class_under_test->derive_all_heads_under_bed( my_bed ).

  ENDMETHOD.

  METHOD when_bom_is_calculated.

    mo_class_under_test->simulate_monster_bom(
    EXPORTING is_bom_input_data = ms_input_data
    IMPORTING et_bom_data       = mt_bom_data ).

  ENDMETHOD."when_bom_is_calculated

*--------------------------------------------------------------------*
* Listing 05.26 - Using Assertions to See if the Test Passed
*--------------------------------------------------------------------*
  "THEN..........................
  METHOD then_no_of_heads_should_be.

    cl_abap_unit_assert=>assert_equals(
        msg  = 'Number of Heads under the Bed is Incorrect'
        exp  = id_heads
        act  = total_heads_under_bed ).

  ENDMETHOD.

  METHOD then_resulting_bom_is_correct.

    DATA(bom_item_details) = mt_bom_data[ 1 ].

    cl_abap_unit_assert=>assert_equals( msg  = 'Monster has wrong number of Heads'
                                        exp  = 1
                                        act  = bom_item_details-part_quantity
                                        quit = if_aunit_constants=>no ).

    bom_item_details = mt_bom_data[ 2 ].

    cl_abap_unit_assert=>assert_equals( msg  = 'Monster has wrong number of Arms'
                                        exp  = 2
                                        act  = bom_item_details-part_quantity
                                        quit = if_aunit_constants=>no ).

    bom_item_details = mt_bom_data[ 3 ].

    cl_abap_unit_assert=>assert_equals( msg  = 'Monster has wrong number of Legs'
                                        exp  = 1
                                        act  = bom_item_details-part_quantity
                                        quit = if_aunit_constants=>no ).

*--------------------------------------------------------------------*
* See Chapter 4 about exception handling for an explanation of
* "design by contract" as implemented using ZCL_DBC
* What we are doing here is coding a "class invariant"
* After every method call, the monster must remain a monster
* Listing 06.14 - Calling a Class Invariant at the end of each Method Call
*--------------------------------------------------------------------*
    DATA(monster_constraint) = NEW lcl_monster_constraint( ).

    TRY.

        zcl_dbc=>ensure(
          EXPORTING
            that             = 'The Monster is still a Monster'
            which_is_true_if = monster_constraint->if_constraint~is_valid( mo_class_under_test ) ).

      CATCH zcx_violated_postcondition.
        DATA(message_table) = monster_constraint->if_constraint~get_description( ).
        cl_abap_unit_assert=>fail( msg = message_table[ 1 ] ).
    ENDTRY.

  ENDMETHOD."Then Resulting BOM is correct - Implementation

ENDCLASS."Test Class Implementation
