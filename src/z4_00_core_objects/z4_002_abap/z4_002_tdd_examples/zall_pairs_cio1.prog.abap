*&---------------------------------------------------------------------*
*& Include          ZALL_PAIRS_CIO1
*&---------------------------------------------------------------------*
* Local Class Implemntations
*----------------------------------------------------------------------*
CLASS lcl_all_pairs IMPLEMENTATION.

  METHOD main.
*--------------------------------------------------------------------*
* IMPORTING it_configuration     TYPE g_tt_configuration
* RETURNING VALUE(rt_test_cases) TYPE g_tt_output_data.
*--------------------------------------------------------------------*
* The unit test example
* would give us 3 x 2 x 2 x 2 x 2 x 2 = 96 test cases using exhaustive testing
* The all pairs will return a lower value i.e. 8
* The theory is that testing those 8 cases will give you just an accurate
* result as testing all 96
*--------------------------------------------------------------------*
    mt_configuration = it_configuration.

    calculate_mapping( ).

    DATA(no_of_variables) = lines( mt_mapping ).

    process_first_two_columns( ).

    DATA(new_column) = 2.

    WHILE new_column LT no_of_variables.

      new_column = new_column + 1.

      insert_new_column( new_column ).

    ENDWHILE.

    rt_test_cases = mt_test_cases.

  ENDMETHOD.

  METHOD calculate_mapping.
*-----------------------------------------------------------------------------------------------------------------*
* Here we see which variable has the highest number of values, then we put the variables with more possibilities
* in the left most columns. The minimum amount of test cases is COUNT( column_one) * COUNT( column_two )
*-----------------------------------------------------------------------------------------------------------------*
    LOOP AT mt_configuration ASSIGNING FIELD-SYMBOL(<configuration>).

      READ TABLE mt_mapping ASSIGNING FIELD-SYMBOL(<mapping>)
      WITH KEY variable COMPONENTS variable_name = <configuration>-variable_name.

      IF sy-subrc NE 0.
        APPEND INITIAL LINE TO mt_mapping ASSIGNING <mapping>.
      ENDIF.

      <mapping>-variable_name = <configuration>-variable_name.
      <mapping>-value_count   = <mapping>-value_count + 1.

    ENDLOOP.

* The mapping table should now have one line for each variables saying how many distinct values
* there are. There is probably an even more compact way to do this like REDUCE. If so then it is
* a valid example in my book (in more ways than one).
    SORT mt_mapping BY value_count DESCENDING.

    LOOP AT mt_mapping ASSIGNING <mapping>.
      <mapping>-column_number = sy-tabix.
    ENDLOOP.

  ENDMETHOD.

  METHOD process_first_two_columns.
*--------------------------------------------------------------------*
* This is the easy bit. The initial version of the test case table
* just contains the pairs from the first two columns i.e. if there
* are six different values in column one and three different values
* in column two, we will have 18 lines, none of them arbitrary
*--------------------------------------------------------------------*
    DATA: all_pairs       LIKE LINE OF mt_pairs,
          ld_first_start  TYPE sy-tabix,
          ld_second_start TYPE sy-tabix.

    READ TABLE mt_mapping INTO DATA(mapping) WITH KEY column_number = 1.
    ASSERT sy-subrc EQ 0.

    DATA(first_variable) = mapping-variable_name.

    READ TABLE mt_mapping INTO mapping WITH KEY column_number = 2.
    ASSERT sy-subrc EQ 0.

    DATA(second_variable) = mapping-variable_name.

    SORT mt_configuration BY variable_name possible_value.

    READ TABLE mt_configuration TRANSPORTING NO FIELDS WITH KEY variable_name = first_variable
                                                       BINARY SEARCH.
    IF sy-subrc EQ 0.
      ld_first_start = sy-tabix.
    ELSE.
      "Should be Impossible
      ld_first_start = 1.
    ENDIF.

    READ TABLE mt_configuration TRANSPORTING NO FIELDS WITH KEY variable_name = second_variable
                                                       BINARY SEARCH.
    IF sy-subrc EQ 0.
      ld_second_start = sy-tabix.
    ELSE.
      "Should be Impossible
      ld_second_start = 1.
    ENDIF.

    LOOP AT mt_configuration ASSIGNING FIELD-SYMBOL(<first_column>) FROM ld_first_start.

      IF <first_column>-variable_name NE first_variable.
        EXIT."From Loop
      ENDIF.

      LOOP AT mt_configuration ASSIGNING FIELD-SYMBOL(<second_column>) FROM ld_second_start."#EC CI_NESTED

        IF <second_column>-variable_name NE second_variable.
          EXIT."From Inner Loop
        ENDIF.

        APPEND INITIAL LINE TO mt_test_cases ASSIGNING FIELD-SYMBOL(<test_case>).

        <test_case>-case_number  = lines( mt_test_cases[] ).
        <test_case>-column_01    = <first_column>-possible_value.
        <test_case>-arbitrary_01 = abap_false.
        <test_case>-column_02    = <second_column>-possible_value.
        <test_case>-arbitrary_01 = abap_false.

        all_pairs-test_case  = <test_case>-case_number.
        all_pairs-variable_1 = first_variable.
        all_pairs-value_1    = <first_column>-possible_value.
        all_pairs-variable_2 = second_variable.
        all_pairs-value_2    = <second_column>-possible_value.

        INSERT all_pairs INTO TABLE mt_pairs.

      ENDLOOP."Values for Second Column
    ENDLOOP."Values for First Column

*--------------------------------------------------------------------*
* At this point we have all possible combinations of the first and second variables in the
* test case table, and also in the "all pairs" table
* We check if this is the case using a Unit Test
*--------------------------------------------------------------------*
  ENDMETHOD.

  METHOD insert_new_column.
*--------------------------------------------------------------------*
* IMPORTING new_column TYPE i
*--------------------------------------------------------------------*

    DATA(left_column) = new_column - 1.

    populate( left_populated_column = left_column
              right_blank_column    = new_column ).

    left_column = left_column - 1.

    WHILE left_column GT 1.

      compare_columns( left_column  = left_column
                       right_column = new_column ).

      left_column = left_column - 1.

    ENDWHILE.

  ENDMETHOD.

  METHOD pair_is_in_test_case.
*--------------------------------------------------------------------*
* IMPORTING variable_1         TYPE string
*           first_value        TYPE string
*           variable_2         TYPE string
*           second_value       TYPE string
* RETURNING VALUE(yes_it_is)   TYPE abap_bool.
*--------------------------------------------------------------------*
    DATA: column_01 TYPE string,
          column_02 TYPE string,
          number_a  TYPE n LENGTH 2,
          number_b  TYPE n LENGTH 2.

    FIELD-SYMBOLS: <value_a> TYPE any,
                   <value_b> TYPE any.

    DATA(column_no_1) = get_column_for_variable( variable_1 ).
    DATA(column_no_2) = get_column_for_variable( variable_2 ).

    number_a = column_no_1.
    number_b = column_no_2.

    column_01    = 'COLUMN_' && number_a.
    column_02    = 'COLUMN_' && number_b.

    LOOP AT mt_test_cases ASSIGNING FIELD-SYMBOL(<test_case>).

      UNASSIGN <value_a>.
      ASSIGN COMPONENT column_01 OF STRUCTURE <test_case> TO <value_a>.
      IF sy-subrc NE 0.
        CONTINUE.
      ENDIF.
      CHECK <value_a> IS ASSIGNED.

      UNASSIGN <value_b>.
      ASSIGN COMPONENT column_02 OF STRUCTURE <test_case> TO <value_b>.
      IF sy-subrc NE 0.
        CONTINUE.
      ENDIF.
      CHECK <value_b> IS ASSIGNED.

      CHECK <value_a> = first_value.
      CHECK <value_b> = second_value.

      yes_it_is = abap_true.
      RETURN.

    ENDLOOP."Test Cases

  ENDMETHOD.

  METHOD compare_columns.
*--------------------------------------------------------------------*
* IMPORTING left_column TYPE i
*           right_column TYPE i
*--------------------------------------------------------------------*
* For each combination (pair) of the possible values in columns A and B
* we make sure that pair exists somewhere in the test case table
* If not, then naturally we need to add it. If we can add it to an
* existing row, sweet, otherwise we need a new row
*--------------------------------------------------------------------*
    DATA: all_pairs     LIKE LINE OF mt_pairs,
          pair_inserted TYPE abap_bool,
          first_start   TYPE sy-tabix,
          second_start  TYPE sy-tabix.

    READ TABLE mt_mapping INTO DATA(mapping) WITH KEY column_number = left_column.
    ASSERT sy-subrc EQ 0.

    DATA(first_variable) = mapping-variable_name.

    READ TABLE mt_mapping INTO mapping WITH KEY column_number = right_column.
    ASSERT sy-subrc EQ 0.

    DATA(second_variable) = mapping-variable_name.

    DATA: left_column_name  TYPE string,
          right_column_name TYPE string,
          arbitrary_01      TYPE string,
          arbitrary_02      TYPE string,
          number_a          TYPE n LENGTH 2,
          number_b          TYPE n LENGTH 2.

    number_a = left_column.
    number_b = right_column.

    left_column_name  = 'COLUMN_' && number_a.
    right_column_name = 'COLUMN_' && number_b.
    arbitrary_01      = 'ARBITRARY_' && number_a.
    arbitrary_02      = 'ARBITRARY_' && number_b.

    FIELD-SYMBOLS: <value_a>           TYPE any,
                   <value_b>           TYPE any,
                   <arbitrary_01_flag> TYPE any,
                   <arbitrary_02_flag> TYPE any.

    "MT_CONGIGURATION has been sorted by VARIABLE_NAME
    READ TABLE mt_configuration TRANSPORTING NO FIELDS WITH KEY variable_name = first_variable
                                                       BINARY SEARCH.
    IF sy-subrc EQ 0.
      first_start = sy-tabix.
    ELSE.
      "Should be Impossible
      first_start = 1.
    ENDIF.

    READ TABLE mt_configuration TRANSPORTING NO FIELDS WITH KEY variable_name = second_variable
                                                       BINARY SEARCH.
    IF sy-subrc EQ 0.
      second_start = sy-tabix.
    ELSE.
      "Should be Impossible
      second_start = 1.
    ENDIF.

    LOOP AT mt_configuration ASSIGNING FIELD-SYMBOL(<first_column>) FROM first_start.

      IF <first_column>-variable_name NE first_variable.
        EXIT."From Outer Loop
      ENDIF.

      LOOP AT mt_configuration ASSIGNING FIELD-SYMBOL(<second_column>) FROM second_start."#EC CI_NESTED

        IF <second_column>-variable_name NE second_variable.
          EXIT."From Inner Loop
        ENDIF.

        IF line_exists( mt_pairs[  variable_1 = first_variable
                                   value_1    = <first_column>-possible_value
                                   variable_2 = second_variable
                                   value_2    = <second_column>-possible_value ] ).
          "We already have this pair of values in a test case, all good
          CONTINUE."With next pair
        ENDIF.

        pair_inserted = abap_false.

        LOOP AT mt_test_cases ASSIGNING FIELD-SYMBOL(<test_case>)."#EC CI_NESTED

          UNASSIGN <value_a>.
          ASSIGN COMPONENT left_column_name OF STRUCTURE <test_case> TO <value_a>.
          IF sy-subrc NE 0.
            CONTINUE.
          ENDIF.
          CHECK <value_a> IS ASSIGNED.

          CHECK <value_a> EQ <first_column>-possible_value.

          UNASSIGN <value_b>.
          ASSIGN COMPONENT right_column_name OF STRUCTURE <test_case> TO <value_b>.
          IF sy-subrc NE 0.
            CONTINUE.
          ENDIF.
          CHECK <value_b> IS ASSIGNED.

          UNASSIGN <arbitrary_01_flag>.
          ASSIGN COMPONENT arbitrary_01 OF STRUCTURE <test_case> TO <arbitrary_01_flag>.
          IF sy-subrc NE 0.
            CONTINUE.
          ENDIF.
          CHECK <arbitrary_01_flag> IS ASSIGNED.

          UNASSIGN <arbitrary_02_flag>.
          ASSIGN COMPONENT arbitrary_02 OF STRUCTURE <test_case> TO <arbitrary_02_flag>.
          IF sy-subrc NE 0.
            CONTINUE.
          ENDIF.
          CHECK <arbitrary_02_flag> IS ASSIGNED.

*--------------------------------------------------------------------*
* The value in the second column is blank. Thus we can add a new pair
*--------------------------------------------------------------------*
          IF <value_b> IS INITIAL.
            <value_a>            = <first_column>-possible_value. "It already is, just making this obvious
            <arbitrary_01_flag>  = abap_false.
            <value_b>            = <second_column>-possible_value.
            <arbitrary_02_flag>  = abap_false.
            all_pairs-test_case  = <test_case>-case_number.
            all_pairs-variable_1 = first_variable.
            all_pairs-value_1    = <first_column>-possible_value.
            all_pairs-variable_2 = second_variable.
            all_pairs-value_2    = <second_column>-possible_value.

            INSERT all_pairs INTO TABLE mt_pairs.
            pair_inserted = abap_true.
            EXIT."From list of test cases
          ENDIF.

*--------------------------------------------------------------------*
* This combination exists already thus we can add it to the all
* pairs table as a valid combination
*--------------------------------------------------------------------*
          IF <value_b> = <second_column>-possible_value.
            <arbitrary_01_flag>  = abap_false.          "No longer arbitrary
            <arbitrary_02_flag>  = abap_false.          "No longer arbitrary
            all_pairs-test_case  = <test_case>-case_number.
            all_pairs-variable_1 = first_variable.
            all_pairs-value_1    = <first_column>-possible_value.
            all_pairs-variable_2 = second_variable.
            all_pairs-value_2    = <second_column>-possible_value.

            INSERT all_pairs INTO TABLE mt_pairs.
            pair_inserted = abap_true.
            EXIT."From List of Test Cases
          ENDIF.

*--------------------------------------------------------------------*
* If the value in the second column is an arbitrary value, we can replace it
*--------------------------------------------------------------------*
          CHECK <arbitrary_02_flag> EQ abap_true.

          <value_b>           = <second_column>-possible_value.
          <arbitrary_01_flag> = abap_false.             "No longer arbitrary
          <arbitrary_02_flag> = abap_false.             "No longer arbitrary

          all_pairs-test_case  = <test_case>-case_number.
          all_pairs-variable_1 = first_variable.
          all_pairs-value_1    = <first_column>-possible_value.
          all_pairs-variable_2 = second_variable.
          all_pairs-value_2    = <second_column>-possible_value.

          INSERT all_pairs INTO TABLE mt_pairs.
          pair_inserted = abap_true.
          EXIT."From list of test cases
        ENDLOOP."Existing Test Cases

        IF pair_inserted EQ abap_true.
          CONTINUE."With next pair of values
        ENDIF.

*--------------------------------------------------------------------*
* If we get here, then we could not add our unique pair of values to the
* existing list of test cases, so we need a new one
*--------------------------------------------------------------------*
        DATA: random_number TYPE qfranint.

        GET TIME."To make sure random number generator works

        CALL FUNCTION 'QF05_RANDOM_INTEGER'
          EXPORTING
            ran_int_max   = lines( mt_test_cases[] )
            ran_int_min   = 1
          IMPORTING
            ran_int       = random_number
          EXCEPTIONS
            invalid_input = 1
            OTHERS        = 2.

        IF sy-subrc <> 0.
          random_number = lines( mt_test_cases[] ) / 2.
        ENDIF.

* The idea is to pick one of the existing lines. As more columns get added
* that new line will have the arbitrary values replaced with real ones
        READ TABLE mt_test_cases INTO DATA(last_test_case) INDEX random_number.

        ASSERT sy-subrc EQ 0.

        APPEND INITIAL LINE TO mt_test_cases ASSIGNING <test_case>.

        <test_case> = CORRESPONDING #( last_test_case ).

        <test_case>-case_number  = lines( mt_test_cases[] ).
        <test_case>-arbitrary_01 = abap_true.
        <test_case>-arbitrary_02 = abap_true.
        <test_case>-arbitrary_03 = abap_true.
        <test_case>-arbitrary_04 = abap_true.
        <test_case>-arbitrary_05 = abap_true.

        UNASSIGN <value_a>.
        ASSIGN COMPONENT left_column_name OF STRUCTURE <test_case> TO <value_a>.
        IF sy-subrc NE 0.
          CONTINUE.
        ENDIF.
        IF <value_a> IS NOT ASSIGNED.
          CONTINUE.
        ENDIF.

        UNASSIGN <value_b>.
        ASSIGN COMPONENT right_column_name OF STRUCTURE <test_case> TO <value_b>.
        IF sy-subrc NE 0.
          CONTINUE.
        ENDIF.
        IF <value_b> IS NOT ASSIGNED.
          CONTINUE.
        ENDIF.

        UNASSIGN <arbitrary_01_flag>.
        ASSIGN COMPONENT arbitrary_01 OF STRUCTURE <test_case> TO <arbitrary_01_flag>.
        IF sy-subrc NE 0.
          CONTINUE.
        ENDIF.
        IF <arbitrary_01_flag> IS NOT ASSIGNED.
          CONTINUE.
        ENDIF.

        UNASSIGN <arbitrary_02_flag>.
        ASSIGN COMPONENT arbitrary_02 OF STRUCTURE <test_case> TO <arbitrary_02_flag>.
        IF sy-subrc NE 0.
          CONTINUE.
        ENDIF.
        IF <arbitrary_02_flag> IS NOT ASSIGNED.
          CONTINUE.
        ENDIF.

        <value_a>           = <first_column>-possible_value.
        <value_b>           = <second_column>-possible_value.
        <arbitrary_01_flag> = abap_false.
        <arbitrary_02_flag> = abap_false.

        all_pairs-test_case  = <test_case>-case_number.
        all_pairs-variable_1 = first_variable.
        all_pairs-value_1    = <first_column>-possible_value.
        all_pairs-variable_2 = second_variable.
        all_pairs-value_2    = <second_column>-possible_value.

        INSERT all_pairs INTO TABLE mt_pairs.
      ENDLOOP."Possible Values of Column B
    ENDLOOP."Possible Values of Column A

  ENDMETHOD.

  METHOD populate.
*--------------------------------------------------------------------*
* IMPORTING right_blank_column TYPE i
*           left_populated_column TYPE i
*--------------------------------------------------------------------*
* The idea is to loop through the test cases - the new column is blank
* at this point
* We keep iterating through the possible values of the new column
* If we can find a new pair with the eixtsing value in the comparison
* column and the value we are checking then great, add to test case
* and all pairs table
* If not try again with next possible value
* Once we have all possible pairs, fill the remaining test cases with
* arbitrary values, but looping through the possible values of the
* new column
*--------------------------------------------------------------------*
    "What is the variable for the compariosn column?
    DATA(full_column_variable) = get_variable_for_column( left_populated_column ).

    "What is the variable we are going to populate the column with?
    DATA(right_blank_column_variable) = get_variable_for_column( right_blank_column ).

    "How many different values do we have for the new column?
    DATA(number_of_new_values) = get_number_of_new_values_for( right_blank_column_variable ).

    DATA: current_new_value_index TYPE i.

    "Loop through test cases, filling new blank column
    LOOP AT mt_test_cases ASSIGNING FIELD-SYMBOL(<ls_test_case>).

      DO number_of_new_values TIMES."#EC CI_NESTED

        current_new_value_index = current_new_value_index + 1.

        IF current_new_value_index GT number_of_new_values.
          current_new_value_index = 1.
        ENDIF.

        "What value is it we are trying to insert?
        DATA(proposed_value) = get_proposed_value( variable_name = right_blank_column_variable
                                                   index         = current_new_value_index ).

        "What value is it in the comparison column?
        get_current_value( EXPORTING test_case = <ls_test_case>
                                     column    = left_populated_column
                           IMPORTING value     = DATA(current_value) ).

        "I think the current value for column 2 is XXX
        "and the iterated value proposed for column 3 is YYY
        "We need to see if this combination is already in the all pairs table
        "If so great, if not add
        DATA(combination_exists) = combination_exists( variable_1   = full_column_variable
                                                       first_value  = current_value
                                                       variable_2   = right_blank_column_variable
                                                       second_value = proposed_value ).

        IF combination_exists EQ abap_true.
          CONTINUE."With next possible value in blank column
        ELSE.
          "Add a proper pair to the test case line and also add to all pairs table
          set_test_case_value( EXPORTING column    = left_populated_column
                                         value     = current_value
                                         arbitrary = abap_false           "In case it currently is arbitrary
                               CHANGING  test_case = <ls_test_case> ).

          set_test_case_value( EXPORTING column    = right_blank_column
                                         value     = proposed_value
                                         arbitrary = abap_false
                               CHANGING  test_case = <ls_test_case> ).
          "Add to all pairs table
          DATA(ls_all_pairs) = VALUE m_typ_pairs( test_case = <ls_test_case>-case_number
                                                  variable_1 = full_column_variable
                                                  value_1    = current_value
                                                  variable_2 = right_blank_column_variable
                                                  value_2    = proposed_value ).
          INSERT ls_all_pairs INTO TABLE mt_pairs.
          "Move on to next test case
          EXIT.
        ENDIF.

      ENDDO."Possible values in blank column

    ENDLOOP."Test Cases

*--------------------------------------------------------------------*
* We have added all possible pairs for these two columns. Now we
* fill the remaining blank values with arbitrary values
*--------------------------------------------------------------------*
    LOOP AT mt_test_cases ASSIGNING <ls_test_case>.

      get_current_value( EXPORTING test_case = <ls_test_case>
                                   column    = right_blank_column
                         IMPORTING value     = current_value ).

      IF current_value IS NOT INITIAL.
        "No need to add a value, one is already there
        CONTINUE.
      ENDIF.

      current_new_value_index = current_new_value_index + 1.

      IF current_new_value_index GT number_of_new_values.
        current_new_value_index = 1.
      ENDIF.

      "What value is it we are trying to insert?
      proposed_value = get_proposed_value( variable_name = right_blank_column_variable
                                           index         = current_new_value_index ).

      set_test_case_value( EXPORTING column    = right_blank_column
                                     value     = proposed_value
                                     arbitrary = abap_true
                           CHANGING  test_case = <ls_test_case> ).

    ENDLOOP.

  ENDMETHOD."Populate Blank Column

  METHOD get_variable_for_column.
*--------------------------------------------------------------------*
* IMPORTING column          TYPE i
* RETURNING VALUE(variable) TYPE string.
*--------------------------------------------------------------------*

    READ TABLE mt_mapping INTO DATA(mapping)
    WITH KEY column_number = column.

    ASSERT sy-subrc EQ 0.

    variable = mapping-variable_name.

  ENDMETHOD.

  METHOD get_column_for_variable.
*--------------------------------------------------------------------*
* IMPORTING variable        TYPE string
* RETURNING VALUE(column)   TYPE i
*--------------------------------------------------------------------*

    READ TABLE mt_mapping INTO DATA(mapping)
    WITH KEY variable COMPONENTS variable_name = variable.

    ASSERT sy-subrc EQ 0.

    column = mapping-column_number.

  ENDMETHOD.

  METHOD get_number_of_new_values_for.
*--------------------------------------------------------------------*
* IMPORTING variable_name TYPE string
* RETURNING VALUE(no_of_values) TYPE i.
*--------------------------------------------------------------------*
    LOOP AT mt_mapping ASSIGNING FIELD-SYMBOL(<mapping>) USING KEY variable WHERE variable_name = variable_name.
      no_of_values = <mapping>-value_count.
    ENDLOOP.

  ENDMETHOD.

  METHOD get_proposed_value.
*--------------------------------------------------------------------*
* IMPORTING variable_name TYPE string
*           index         TYPE i
* RETURNING VALUE(value)  TYPE string
*--------------------------------------------------------------------*
    READ TABLE mt_configuration INTO DATA(configuration)
    WITH KEY variable_name = variable_name
             count         = index.

    ASSERT sy-subrc EQ 0.

    value = configuration-possible_value.

  ENDMETHOD.

  METHOD get_current_value.
*--------------------------------------------------------------------*
* IMPORTING column    TYPE i
* EXPORTING value     TYPE sring
*           arbitrary TYPE abap_bool
*--------------------------------------------------------------------*
    DATA: column_name    TYPE string,
          arbitrary_name TYPE string,
          number         TYPE n LENGTH 2.

    CLEAR: value,
           arbitrary.

    number = column.

    column_name    = 'COLUMN_' && number.
    arbitrary_name = 'ARBITRARY_' && number.

    FIELD-SYMBOLS: <value>          TYPE any,
                   <arbitrary_flag> TYPE any.

    UNASSIGN <value>.
    ASSIGN COMPONENT column_name OF STRUCTURE test_case TO <value>.
    IF sy-subrc NE 0.
      RETURN.
    ENDIF.
    IF <value> IS NOT ASSIGNED.
      RETURN.
    ENDIF.

    UNASSIGN <arbitrary_flag>.
    ASSIGN COMPONENT arbitrary_name OF STRUCTURE test_case TO <arbitrary_flag>.
    IF sy-subrc NE 0.
      RETURN.
    ENDIF.
    IF <arbitrary_flag> IS NOT ASSIGNED.
      RETURN.
    ENDIF.

    value     = <value>.
    arbitrary = <arbitrary_flag>.

  ENDMETHOD.

  METHOD set_test_case_value.
*--------------------------------------------------------------------*
* IMPORTING column    TYPE i
*           value     TYPE string
*           arbitrary TYPE abap_bool
* CHANGING  test_case TYPE m_typ_test_case,l
*--------------------------------------------------------------------*
    DATA: column_name    TYPE string,
          arbitrary_name TYPE string,
          number         TYPE n LENGTH 2.

    "Preconditions
    IF column IS INITIAL.
      RETURN.
    ENDIF.
    IF value IS INITIAL.
      RETURN.
    ENDIF.

    number = column.

    column_name    = 'COLUMN_' && number.
    arbitrary_name = 'ARBITRARY_' && number.

    FIELD-SYMBOLS: <value>          TYPE any,
                   <arbitrary_flag> TYPE any.

    UNASSIGN <value>.
    ASSIGN COMPONENT column_name OF STRUCTURE test_case TO <value>.
    IF sy-subrc NE 0.
      RETURN.
    ENDIF.
    IF <value> IS NOT ASSIGNED.
      RETURN.
    ENDIF.

    UNASSIGN <arbitrary_flag>.
    ASSIGN COMPONENT arbitrary_name OF STRUCTURE test_case TO <arbitrary_flag>.
    IF sy-subrc NE 0.
      RETURN.
    ENDIF.
    IF <arbitrary_flag> IS NOT ASSIGNED.
      RETURN.
    ENDIF.

    <value>          = value.
    <arbitrary_flag> = arbitrary.

  ENDMETHOD."Set Test Case Value

  METHOD combination_exists.
*--------------------------------------------------------------------*
* IMPORTING first_value        TYPE string
*           second_value       TYPE string
* RETURNING VALUE(yes_it_does) TYPE abap_bool
*--------------------------------------------------------------------*

    IF line_exists( mt_pairs[ variable_1 = variable_1
                              value_1    = first_value
                              variable_2 = variable_2
                              value_2    = second_value ] ).
      yes_it_does = abap_true.
    ELSE.
      yes_it_does = abap_false.
    ENDIF.

  ENDMETHOD.

ENDCLASS."LCL_ALL_PAIRS

CLASS lcl_application IMPLEMENTATION.

  METHOD main.
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
    mo_model->fill_field_texts( ).

    "It is bad news to pass system variables as parameters
    DATA(repid) = sy-repid.

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
      id_report_name        = repid
      if_start_in_edit_mode = abap_false
      id_edit_control_field = mo_model->md_edit_control_field
      is_layout             = mo_model->ms_layout
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
          id_report_name     = repid
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

    p_file = ip_file.

  ENDMETHOD.                    "constructor

ENDCLASS."Local Selections

*----------------------------------------------------------------------*
*       CLASS lcl_persistency_layer IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_persistency_layer IMPLEMENTATION.

  METHOD get_data.
*--------------------------------------------------------------------*
* EXPORTING et_output_data TYPE g_tt_output_data.
*--------------------------------------------------------------------*
* Really, we should use ABAP2XLSX to read spreadsheets
* This is a good example of how to extract the contents from an
* EXCEL file and do whatever you want with the data
*--------------------------------------------------------------------*
* Local variables
    DATA: extension     TYPE string,
          reader        TYPE REF TO zif_excel_reader,
          column        TYPE zexcel_cell_column VALUE 1,
          row           TYPE int4               VALUE 1,
          configuration LIKE LINE OF et_configuration.

    FIND REGEX '(\.xlsx|\.xlsm)\s*$' IN go_selections->p_file SUBMATCHES extension.
    extension = to_upper( extension ).

    CASE extension.

      WHEN '.XLSX'.
        reader = NEW zcl_excel_reader_2007( ).
        DATA(excel) = reader->load_file( go_selections->p_file ).

      WHEN '.XLSM'.
        reader = NEW zcl_excel_reader_xlsm( ).
        excel = reader->load_file( go_selections->p_file ).

      WHEN OTHERS.
        "Unsupported File Type
        MESSAGE i016(z4monsters).
        RETURN.

    ENDCASE.

    DATA(worksheet)      = excel->get_active_worksheet( ).
    DATA(highest_column) = worksheet->get_highest_column( ).
    DATA(highest_row)    = worksheet->get_highest_row( ).

    WHILE row <= highest_row.
      WHILE column <= highest_column."#EC CI_NESTED
        DATA(col_str) = zcl_excel_common=>convert_column2alpha( column ).
        worksheet->get_cell(
          EXPORTING ip_column = col_str
                    ip_row    = row
          IMPORTING ep_value  = DATA(value) ).
        IF column = 1.
          configuration-variable_name  = value.
        ELSEIF column = 2.
          configuration-possible_value = value.
        ELSE.
          EXIT."From looking at columns
        ENDIF.
        column = column + 1.
      ENDWHILE.
      APPEND configuration TO et_configuration.
      column = 1.
      row = row + 1.
    ENDWHILE.

    DATA: current_variable TYPE string,
          current_value    TYPE sy-tabix.

    LOOP AT et_configuration ASSIGNING FIELD-SYMBOL(<configuration>).

      IF current_variable IS INITIAL.
        current_variable = <configuration>-variable_name.
        current_value    = 0.
      ELSEIF current_variable NE <configuration>-variable_name.
        current_variable = <configuration>-variable_name.
        current_value    = 0.
      ENDIF.

      current_value = current_value + 1.

      <configuration>-count = current_value.

    ENDLOOP.

  ENDMETHOD.                                               "get_data

ENDCLASS.                    "lcl_persistency_layer IMPLEMENTATION

*----------------------------------------------------------------------*
*       CLASS lcl_model IMPLEMENTATION
*----------------------------------------------------------------------*
* When creating the model for real we do not fill the import parameter
* and thus the data is read for real
* When creating the model within a unit test, we pass in a reference to
* the fake database access class. Actually we prefer INJECTION
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

    fill_editable_fields( ).

    fill_checkbox_fields( ).

    set_edit_control_field( ).

  ENDMETHOD.                                               "constructor

  METHOD data_retrieval.

    TRY.
        mt_configuration = mo_persistency_layer->get_data( ).
      CATCH zcx_excel.
        "Oh Dear!
        MESSAGE e015(z4monsters).
    ENDTRY.

  ENDMETHOD.                                               "data_retrieval

**********************************************************************
* METHOD prepare_data_for_output
**********************************************************************
* Get text names of objects, mapping, etc etc
*----------------------------------------------------------------------*
  METHOD prepare_data_for_ouput.

    mo_all_pairs = NEW lcl_all_pairs( ).

    mt_output_data = mo_all_pairs->main( mt_configuration ).

  ENDMETHOD.                                               "prepare_data_for_ouput

  METHOD fill_user_commands.

    CLEAR mt_user_commands.

  ENDMETHOD.                                               "fill_user_commands

  METHOD fill_layout_data.

    ms_layout = VALUE #( list_header       = 'All Pairs Test Cases'(004)
                         colwidth_optimize = abap_true
                         striped_pattern   = abap_true
                         no_cell_merging   = abap_true ).

  ENDMETHOD.

  METHOD fill_editable_fields ##needed.

  ENDMETHOD.                    "fill_editable_fields

  METHOD fill_hidden_fields ##needed.
    "No Hidden Fields
  ENDMETHOD.                    "fill_hidden_fields

  METHOD fill_technical_fields ##needed.

  ENDMETHOD.                    "fill_technical_fields

  METHOD fill_hotspot_fields ##needed.

  ENDMETHOD.                    "fill_hotspot_fields

  METHOD fill_subtotal_fields ##needed.
    "No Subtotals
  ENDMETHOD.                    "fill_subtotal_fields

  METHOD fill_field_texts.
*--------------------------------------------------------------------*
* This has to be done "late in the day" as it were because we have to
* wait till the model is aware of the mapping, before we can say
* what the column names are going to be
*--------------------------------------------------------------------*
* Local Variables
    DATA: column_number TYPE n LENGTH 2,
          field_texts   LIKE LINE OF mt_field_texts.

    "Preconditions
    IF mo_all_pairs->mt_mapping[] IS INITIAL.
      RETURN.
    ENDIF.

    CLEAR mt_field_texts.

    field_texts-field_name = 'CASE_NUMBER'.
    field_texts-long_text  = 'Case Number'(005).
    APPEND field_texts TO mt_field_texts.

    LOOP AT mo_all_pairs->mt_mapping ASSIGNING FIELD-SYMBOL(<mapping_entry>).

      IF line_exists( mt_field_texts[ long_text = <mapping_entry>-variable_name ] ) ##WARN_OK.
        "We have already handled this variable, move on
        CONTINUE.
      ENDIF.

      column_number = column_number + 1.

      IF column_number GT 6.
        "Only six possible variables for this example. Feel free to add more...
        EXIT.
      ENDIF.

      CLEAR field_texts.

      field_texts-field_name = 'COLUMN_' && column_number.
      field_texts-long_text  = <mapping_entry>-variable_name.
      APPEND field_texts TO mt_field_texts.

      field_texts-field_name = 'ARBITRARY_' && column_number.
      field_texts-long_text  = 'A'.
      field_texts-tooltip    = 'Arbitrary Value?'(006).
      APPEND field_texts TO mt_field_texts.

    ENDLOOP.

  ENDMETHOD.                    "fill_field_texts

  METHOD fill_checkbox_fields ##needed.

  ENDMETHOD.                    "fill_checkbox_fields

  METHOD user_command ##needed.

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
* Listing 10.32 - User Command to Make a SALV Grid Editable
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
