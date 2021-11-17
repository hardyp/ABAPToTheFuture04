class ZCL_4_VILLAGE_MONSTER_QUERY definition
  public
  inheriting from /BOBF/CL_LIB_Q_SUPERCLASS
  final
  create public .

public section.

  methods /BOBF/IF_FRW_QUERY~QUERY
    redefinition .
protected section.
private section.
ENDCLASS.



CLASS ZCL_4_VILLAGE_MONSTER_QUERY IMPLEMENTATION.


  METHOD /bobf/if_frw_query~query.
* Local Variables
    DATA: lr_village_number TYPE RANGE OF z4de_village_address_number.

    CLEAR: eo_message,
           et_key,
           es_query_info,
           et_data.

   "Preconditions
   IF it_selection_parameters[] IS INITIAL.
     RETURN.
   ENDIF.

*--------------------------------------------------------------------*
* First we convert the incoming data into something an SQL Query
* can understand
*--------------------------------------------------------------------*
* IT_SELECTION_PARAMETERS contains what you might expect...
    LOOP AT it_selection_parameters ASSIGNING FIELD-SYMBOL(<selection_parameter>).
      CASE <selection_parameter>-attribute_name.
        WHEN 'VILLAGE_NUMBER'.
          lr_village_number = VALUE #( (
          option = <selection_parameter>-option
          sign   = <selection_parameter>-sign
          low    = |{ <selection_parameter>-low ALPHA = IN }|
          high   = |{ <selection_parameter>-high ALPHA = IN }| ) ).
        WHEN 'DESIRED_REVULSION_LEVEL'.
          DATA(desired_revulsion_level) = <selection_parameter>-low.
        WHEN 'DESIRED_NO_OF_SCREAMS'.
          DATA(desired_no_of_screams) = <selection_parameter>-low.
        WHEN OTHERS.
          CONTINUE.
      ENDCASE.
    ENDLOOP.

*--------------------------------------------------------------------*
* Now we have to find the target villages
*--------------------------------------------------------------------*
    SELECT *
      FROM z4t_villages
      INTO TABLE @DATA(village_list)
      WHERE village_number IN @lr_village_number.

    IF sy-subrc NE 0.
      RETURN.
    ENDIF.

*--------------------------------------------------------------------*
* Next Step is to get a list of filtered monsters
* They have to be scary enough to satisfy the screaming
* They have to be smelly enough to satisfy the revulsion level
*--------------------------------------------------------------------*
    DATA: lr_evilness  TYPE RANGE OF z4de_monster_evilness,
          lr_scariness TYPE RANGE OF z4de_monster_scariness.

    IF desired_revulsion_level GE 2.
      APPEND VALUE #(
      option = 'EQ'
      sign   = 'I'
      low    = 'EVIL' ) "Just Evil in General
      TO lr_evilness.
    ENDIF.

    IF desired_revulsion_level GE 4.
      APPEND VALUE #(
      option = 'EQ'
      sign   = 'I'
      low    = 'VERY' ) "Very Evil
      TO lr_evilness.
    ENDIF.

    IF desired_revulsion_level GE 6.
      APPEND VALUE #(
      option = 'EQ'
      sign   = 'I'
      low    = 'JOLY' ) "Jolly Evil (He's Come to Tea)
      TO lr_evilness.
    ENDIF.

    IF desired_revulsion_level GE 8.
      APPEND VALUE #(
      option = 'EQ'
      sign   = 'I'
      low    = 'INEV' ) "Incredibly Evil
      TO lr_evilness.
    ENDIF.

    IF desired_revulsion_level = 10.
      APPEND VALUE #(
      option = 'EQ'
      sign   = 'I'
      low    = 'BANK' ) "Works in a Bank
      TO lr_evilness.
    ENDIF.

* Now we do the same for the desired scariness
    APPEND VALUE #(
    option = 'EQ'
    sign   = 'I'
    low    = 'NORM' ) "Normal Monster
    TO lr_scariness.

    IF desired_no_of_screams GE 3.
      APPEND VALUE #(
      option = 'EQ'
      sign   = 'I'
      low    = 'SLIG' ) "Slightly Scary
      TO lr_scariness.
    ENDIF.

    IF desired_no_of_screams GE 5.
      APPEND VALUE #(
      option = 'EQ'
      sign   = 'I'
      low    = 'REAL' ) "Really Scary
      TO lr_scariness.
    ENDIF.

    IF desired_no_of_screams GE 7.
      APPEND VALUE #(
      option = 'EQ'
      sign   = 'I'
      low    = 'UNBL' ) "Unbelivably Scary
      TO lr_scariness.
    ENDIF.

    IF desired_no_of_screams GE 10.
      APPEND VALUE #(
      option = 'EQ'
      sign   = 'I'
      low    = 'BANK' ) "Works in a Bank
      TO lr_scariness.
    ENDIF.

    SELECT *
      FROM z4t_monster_head
      INTO TABLE @DATA(monster_list)
      WHERE evilness  IN @lr_evilness
      AND   scariness IN @lr_scariness.

    IF sy-subrc NE 0.
      RETURN.
    ENDIF.

*--------------------------------------------------------------------*
* Now we filter out unsuitable monsters.
* Villages where they are all have asthma are no good for making
* the villagers scream no matter how scary the monster is.
* Villages where they have no noses are no good for getting the villagers
* revolted by the monsters smell no matter how evil it is.
* The rule is, if the monster cannot do the job for all requested
* villages, exclude it from the result. We are presuming only one or
* two villages will be passed in. We have to be realistic about this.
*--------------------------------------------------------------------*
    LOOP AT village_list ASSIGNING FIELD-SYMBOL(<village_information>).
* Villagers with big noses can smell all sorts of monsters
* as their nose size decreases so does their ability to smell
* different types of monster and hence be revolted by them
      IF <village_information>-avg_nose_size LT 10.
        DELETE monster_list WHERE evilness EQ 'EVIL'.
      ENDIF.

      IF <village_information>-avg_nose_size LT 8.
        DELETE monster_list WHERE evilness EQ 'VERY'.
      ENDIF.

      IF <village_information>-avg_nose_size LT 6.
        DELETE monster_list WHERE evilness EQ 'JOLY'."Jolly Evil
      ENDIF.

      IF <village_information>-avg_nose_size LT 4.
        DELETE monster_list WHERE evilness EQ 'INEV'."Incredibly Evil
      ENDIF.

      IF <village_information>-avg_nose_size EQ 0.
        DELETE monster_list WHERE evilness EQ 'BANK'."Makes Bankers look like Saints
      ENDIF.

* Likewise the more asthma the villagers have the less they are able to
* scream. So no point sending a really scary monster where the asthma
* level is high as they would not be able to scream enough

* Everyone is able to manage at least on scream for a normal monster

      IF <village_information>-avg_asthma_level GE 8.
        DELETE monster_list WHERE scariness = 'BANK'."Works in a Bank
      ENDIF.

      IF <village_information>-avg_asthma_level GE 6.
        DELETE monster_list WHERE scariness = 'UNBL'."Unbelievably Scary
      ENDIF.

      IF <village_information>-avg_asthma_level GE 4.
        DELETE monster_list WHERE scariness = 'REAL'."Really Scary
      ENDIF.

      IF <village_information>-avg_asthma_level GE 2.
        DELETE monster_list WHERE scariness = 'SLIG'."Slightly Scary
      ENDIF.
    ENDLOOP."Villages

    IF monster_list[] IS INITIAL.
      "The Query Result will be empty
      RETURN.
    ENDIF.

*--------------------------------------------------------------------*
* Lastly we convert the result list into a format the BOPF can understand
*--------------------------------------------------------------------*
    DATA: result_table TYPE z4tt_village_monster_results.

    TRY.
        DATA(lo_factory) = zcl_4_monster_factory=>get_instance( ).
        DATA(pers_layer) = lo_factory->get_monster_bo_pl( ).

        LOOP AT monster_list ASSIGNING FIELD-SYMBOL(<monster_details>).

          DATA(monster_bopf_key) = pers_layer->get_bopf_key_4_monster_number( <monster_details>-monster_number ).

          DATA(key_structure) = VALUE /bobf/s_frw_key( key = monster_bopf_key ).

          APPEND key_structure TO et_key.

          IF iv_fill_data EQ abap_false.
            "Nothing more to do in this loop pass, we are just filling the keys
            CONTINUE.
          ENDIF.

          DATA(result_structure) = VALUE z4sq_village_monster_result( monster_number = <monster_details>-monster_number
                                                                      monster_name   = <monster_details>-name ).

          INSERT result_structure INTO TABLE result_table.
        ENDLOOP.

        et_data = result_table.

      CATCH zcx_4_monster_exceptions INTO DATA(exception_object) ##NEEDED ##NO_HANDLER."#EC EMPTY_CATCH
        "In real life here you would fill up the EO_MESSAGE exporting parameter
    ENDTRY.

  ENDMETHOD.
ENDCLASS.
