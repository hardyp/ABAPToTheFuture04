class ZCL_4_WDA_MONSTER_TEST_CLASS definition
  public
  create private
  for testing
  duration short
  risk level harmless .

public section.
  PROTECTED SECTION.
  PRIVATE SECTION.

    DATA mo_tester TYPE REF TO cl_wd_web_dynpro_tester .

    METHODS: setup,
      allow_box_to_be_checked FOR TESTING,
      given_user_ticks_box,
      when_round_trip_occurs,
      then_box_is_ticked.

ENDCLASS.



CLASS ZCL_4_WDA_MONSTER_TEST_CLASS IMPLEMENTATION.


  METHOD allow_box_to_be_checked."for TESTING
*--------------------------------------------------------------------*
* Listing 12.10: - Executing WDA Unit Test
*--------------------------------------------------------------------*
    given_user_ticks_box( ).

    when_round_trip_occurs( ).

    then_box_is_ticked( ).

* In one Haunted Castle in what is called ‘The Inner Pantry’, a frail figure in white still appears.  The silver was
* stored here and a footman employed to sleep here and guard it. Historically, one night, when the footman had turned
* in to sleep, he was accosted by this lady in white. Very pale, she begged him for water. Thinking it was one of the
* castle guests, he turned to obey. Suddenly he remembered he was locked in and no visitor could have
* possibly entered!
  ENDMETHOD.


  METHOD given_user_ticks_box.
*--------------------------------------------------------------------*
* Listing 12.10 : - Executing WDA Unit Test (1/3)
*--------------------------------------------------------------------*
    DATA: checkbox TYPE REF TO if_wdt_checkbox.

    checkbox ?= mo_tester->get_ui_element_tester(
      view_id        = 'V_2_SELECT_OPTIONS'
      uielement_id   = 'CBOX_BED_HIDER' ).

    checkbox->raise_change( abap_true ).

  ENDMETHOD.


  METHOD setup.
*--------------------------------------------------------------------*
* Listing 12.09: - Setting Up WDA Unit Test
*--------------------------------------------------------------------*
    mo_tester = cl_wd_web_dynpro_tester=>create( 'ZWDC_4_MONSTER_LIST' ).

  ENDMETHOD.


  METHOD then_box_is_ticked.
*--------------------------------------------------------------------*
* Listing 12.10 : Executing WDA Unit Test (3/3)
*--------------------------------------------------------------------*
    DATA: checked TYPE abap_bool.

    DATA(root_node) =
    mo_tester->get_context_root_node( controller_id = 'V_2_SELECT_OPTIONS' ).

    DATA(node) = root_node->get_child_node( 'BED_HIDER_FLAG' ).

    node->get_attribute( EXPORTING name  = 'BED_HIDER_FLAG'
                         IMPORTING value = checked ).

    cl_abap_unit_assert=>assert_equals(
        exp  = abap_true
        act  = checked ).

  ENDMETHOD.


  METHOD when_round_trip_occurs.
*--------------------------------------------------------------------*
* Listing 12.10 : Executing WDA Unit Test (2/3)
*--------------------------------------------------------------------*
    mo_tester->execute_request( ).

  ENDMETHOD.
ENDCLASS.
