*&---------------------------------------------------------------------*
*& Report Z4MONSTER_DATA_GENERATOR
*&---------------------------------------------------------------------*
* When using the boring SFLIGHT model you run SAPBC_DATA_GENERATOR
* in a development system to create some bogus data so you can run
* the example programs and get some sort of result
*----------------------------------------------------------------------*
* As you know the Monster based data model is far more realistic but
* I have had complaints that to truly appreciate the inner beauty of
* the examples in the book the reader would really need a way to easily
* generate some test data like you can do for SFLIGHT
* As a result one reader changed all the Monster examples in the book to
* SFLIGHT examples. The Monsters were horrifed and ate him, and to add
* insult to injury they then went on a flight to celebrate
*----------------------------------------------------------------------*
* To prevent such occurences in the future here we have a Monster Data
* Generator program a la SAPBC_DATA_GENERATOR.
* The ultimate irony is that SAPBC_DATA_GENERATOR mentions Monsters in
* it's SELECTION-SCREEN as well!
*----------------------------------------------------------------------*
* However I am not doing anything clever here, instead it will be all
* FORM routines and WRITE statements. The equivalent in ABAP in the Cloud
* will have to be a bit more OO!
*----------------------------------------------------------------------*
REPORT z4monster_data_generator.

PARAMETERS: p_monst AS CHECKBOX DEFAULT 'X',
            p_ords  AS CHECKBOX DEFAULT 'X',
            p_dels  AS CHECKBOX DEFAULT 'X'.

START-OF-SELECTION.
  PERFORM monster_bo_data.
  PERFORM order_bo_data.
  PERFORM delivery_bo_data.

*--------------------------------------------------------------------*
* FORM Routines
*--------------------------------------------------------------------*
FORM monster_bo_data.
* Local Variables
  DATA: lt_headers TYPE STANDARD TABLE OF z4t_monster_head,
        lt_items   TYPE STANDARD TABLE OF z4t_monster_itms,
        ld_number  TYPE z4de_monster_number.

  IF p_monst EQ abap_false.
    RETURN.
  ENDIF.

  DATA(lo_uuid_generator) = cl_uuid_factory=>create_system_uuid( ).

  "Build Up New Data
  TRY.
*--------------------------------------------------------------------*
* Monster 1
*--------------------------------------------------------------------*
      DATA(ld_parent_key) = lo_uuid_generator->create_uuid_x16( ).
      ld_number           = '0000000001'.
      INSERT VALUE #(
      db_key         = ld_parent_key
      monster_number = ld_number
      name           = 'JIMMY'
      model          = 'BTNK'
      monster_usage  = 'NORM' "Scares Peasants
      )
      INTO TABLE lt_headers.

* Items
      DATA(ld_child_key) = lo_uuid_generator->create_uuid_x16( ).
      INSERT VALUE #(
      db_key         = ld_child_key
      parent_key     = ld_parent_key
      monster_number = ld_number
      monster_item   = '000010'
      part_category  = 'HD' "Head
      part_quantity  = 1 ) INTO TABLE lt_items.

      ld_child_key = lo_uuid_generator->create_uuid_x16( ).
      INSERT VALUE #(
      db_key         = ld_child_key
      parent_key     = ld_parent_key
      monster_number = ld_number
      monster_item   = '000020'
      part_category  = 'TO' "Torso
      part_quantity  = 1 ) INTO TABLE lt_items.

      ld_child_key = lo_uuid_generator->create_uuid_x16( ).
      INSERT VALUE #(
      db_key         = ld_child_key
      parent_key     = ld_parent_key
      monster_number = ld_number
      monster_item   = '000030'
      part_category  = 'LG' "Leg
      part_quantity  = 2 ) INTO TABLE lt_items.

      ld_child_key = lo_uuid_generator->create_uuid_x16( ).
      INSERT VALUE #(
      db_key         = ld_child_key
      parent_key     = ld_parent_key
      monster_number = ld_number
      monster_item   = '000040'
      part_category  = 'AR' "Arm
      part_quantity  = 2 ) INTO TABLE lt_items.

      ld_child_key = lo_uuid_generator->create_uuid_x16( ).
      INSERT VALUE #(
      db_key         = ld_child_key
      parent_key     = ld_parent_key
      monster_number = ld_number
      monster_item   = '000050'
      part_category  = 'TA' "Tail
      part_quantity  = 2 ) INTO TABLE lt_items.

*--------------------------------------------------------------------*
* Monster 2
*--------------------------------------------------------------------*
      ld_parent_key = lo_uuid_generator->create_uuid_x16( ).
      ld_number     = '0000000002'.
      INSERT VALUE #(
            db_key         = ld_parent_key
            monster_number = '0000000002'
            name           = 'ROLF'
            model          = 'BLLN'
            monster_usage  = 'BALL'	"Ballroom Dancer
            )
            INTO TABLE lt_headers.

* Items
      ld_child_key = lo_uuid_generator->create_uuid_x16( ).

      INSERT VALUE #(
      db_key         = ld_child_key
      parent_key     = ld_parent_key
      monster_number = ld_number
      monster_item   = '000010'
      part_category  = 'HD' "Head
      part_quantity  = 1 ) INTO TABLE lt_items.

      ld_child_key = lo_uuid_generator->create_uuid_x16( ).
      INSERT VALUE #(
      db_key         = ld_child_key
      parent_key     = ld_parent_key
      monster_number = ld_number
      monster_item   = '000020'
      part_category  = 'TO' "Torso
      part_quantity  = 1 ) INTO TABLE lt_items.

      ld_child_key = lo_uuid_generator->create_uuid_x16( ).
      INSERT VALUE #(
      db_key         = ld_child_key
      parent_key     = ld_parent_key
      monster_number = ld_number
      monster_item   = '000030'
      part_category  = 'LG' "Leg
      part_quantity  = 2 ) INTO TABLE lt_items.

      ld_child_key = lo_uuid_generator->create_uuid_x16( ).
      INSERT VALUE #(
      db_key         = ld_child_key
      parent_key     = ld_parent_key
      monster_number = ld_number
      monster_item   = '000040'
      part_category  = 'AR' "Arm
      part_quantity  = 2 ) INTO TABLE lt_items.

      ld_child_key = lo_uuid_generator->create_uuid_x16( ).
      INSERT VALUE #(
      db_key         = ld_child_key
      parent_key     = ld_parent_key
      monster_number = ld_number
      monster_item   = '000050'
      part_category  = 'TA' "Tail
      part_quantity  = 2 ) INTO TABLE lt_items.

*--------------------------------------------------------------------*
* Monster 3
*--------------------------------------------------------------------*
      ld_parent_key = lo_uuid_generator->create_uuid_x16( ).
      ld_number     = '0000000003'.

      INSERT VALUE #(
          db_key         = ld_parent_key
          monster_number = ld_number
          name           = 'JOHNATHON'
          model          = 'KLKL'
          monster_usage  = 'MORT' "Mortgage Salesman
          )
          INTO TABLE lt_headers.

* Items
      ld_child_key = lo_uuid_generator->create_uuid_x16( ).
      INSERT VALUE #(
      db_key         = ld_child_key
      parent_key     = ld_parent_key
      monster_number = ld_number
      monster_item   = '000010'
      part_category  = 'HD' "Head
      part_quantity  = 0 ) INTO TABLE lt_items.

      ld_child_key = lo_uuid_generator->create_uuid_x16( ).
      INSERT VALUE #(
      db_key         = ld_child_key
      parent_key     = ld_parent_key
      monster_number = ld_number
      monster_item   = '000020'
      part_category  = 'TO' "Torso
      part_quantity  = 1 ) INTO TABLE lt_items.

      ld_child_key = lo_uuid_generator->create_uuid_x16( ).
      INSERT VALUE #(
      db_key         = ld_child_key
      parent_key     = ld_parent_key
      monster_number = ld_number
      monster_item   = '000030'
      part_category  = 'LG' "Leg
      part_quantity  = 2 ) INTO TABLE lt_items.

      ld_child_key = lo_uuid_generator->create_uuid_x16( ).
      INSERT VALUE #(
      db_key         = ld_child_key
      parent_key     = ld_parent_key
      monster_number = ld_number
      monster_item   = '000040'
      part_category  = 'AR' "Arm
      part_quantity  = 2 ) INTO TABLE lt_items.

      ld_child_key = lo_uuid_generator->create_uuid_x16( ).
      INSERT VALUE #(
      db_key         = ld_child_key
      parent_key     = ld_parent_key
      monster_number = ld_number
      monster_item   = '000050'
      part_category  = 'TA' "Tail
      part_quantity  = 2 ) INTO TABLE lt_items.

*--------------------------------------------------------------------*
* Monster 4
*--------------------------------------------------------------------*
      ld_parent_key = lo_uuid_generator->create_uuid_x16( ).
      ld_number     = '0000000004'.
      INSERT VALUE #(
          db_key         = ld_parent_key
          monster_number = ld_number
          name           = 'JEFFREY'
          model          = 'BLOB'
          monster_usage  = 'PLUM' "It's the PLUMBER!
          )
          INTO TABLE lt_headers.

* Items
      ld_child_key = lo_uuid_generator->create_uuid_x16( ).
      INSERT VALUE #(
      db_key         = ld_child_key
      parent_key     = ld_parent_key
      monster_number = ld_number
      monster_item   = '000010'
      part_category  = 'HD' "Head
      part_quantity  = 3 ) INTO TABLE lt_items.

      ld_child_key = lo_uuid_generator->create_uuid_x16( ).
      INSERT VALUE #(
      db_key         = ld_child_key
      parent_key     = ld_parent_key
      monster_number = ld_number
      monster_item   = '000020'
      part_category  = 'TO' "Torso
      part_quantity  = 1 ) INTO TABLE lt_items.

      ld_child_key = lo_uuid_generator->create_uuid_x16( ).
      INSERT VALUE #(
      db_key         = ld_child_key
      parent_key     = ld_parent_key
      monster_number = ld_number
      monster_item   = '000030'
      part_category  = 'LG' "Leg
      part_quantity  = 2 ) INTO TABLE lt_items.

      ld_child_key = lo_uuid_generator->create_uuid_x16( ).
      INSERT VALUE #(
      db_key         = ld_child_key
      parent_key     = ld_parent_key
      monster_number = ld_number
      monster_item   = '000040'
      part_category  = 'AR' "Arm
      part_quantity  = 2 ) INTO TABLE lt_items.

      ld_child_key = lo_uuid_generator->create_uuid_x16( ).
      INSERT VALUE #(
      db_key         = ld_child_key
      parent_key     = ld_parent_key
      monster_number = ld_number
      monster_item   = '000050'
      part_category  = 'TA' "Tail
      part_quantity  = 2 ) INTO TABLE lt_items.

*--------------------------------------------------------------------*
* Monster 5
*--------------------------------------------------------------------*
      ld_parent_key = lo_uuid_generator->create_uuid_x16( ).
      ld_number     = '0000000005'.
      INSERT VALUE #(
          db_key         = ld_parent_key
          monster_number = ld_number
          name           = 'GARY'
          model          = 'ISDD'
          monster_usage  = 'MORI' "Morris Dancer
          )
          INTO TABLE lt_headers.

* Items
      ld_child_key = lo_uuid_generator->create_uuid_x16( ).
      INSERT VALUE #(
      db_key         = ld_child_key
      parent_key     = ld_parent_key
      monster_number = ld_number
      monster_item   = '000010'
      part_category  = 'HD' "Head
      part_quantity  = 1 ) INTO TABLE lt_items.

      ld_child_key = lo_uuid_generator->create_uuid_x16( ).
      INSERT VALUE #(
      db_key         = ld_child_key
      parent_key     = ld_parent_key
      monster_number = ld_number
      monster_item   = '000020'
      part_category  = 'TO' "Torso
      part_quantity  = 1 ) INTO TABLE lt_items.

      ld_child_key = lo_uuid_generator->create_uuid_x16( ).
      INSERT VALUE #(
      db_key         = ld_child_key
      parent_key     = ld_parent_key
      monster_number = ld_number
      monster_item   = '000030'
      part_category  = 'LG' "Leg
      part_quantity  = 2 ) INTO TABLE lt_items.

      ld_child_key = lo_uuid_generator->create_uuid_x16( ).
      INSERT VALUE #(
      db_key         = ld_child_key
      parent_key     = ld_parent_key
      monster_number = ld_number
      monster_item   = '000040'
      part_category  = 'AR' "Arm
      part_quantity  = 2 ) INTO TABLE lt_items.

      ld_child_key = lo_uuid_generator->create_uuid_x16( ).
      INSERT VALUE #(
      db_key         = ld_child_key
      parent_key     = ld_parent_key
      monster_number = ld_number
      monster_item   = '000050'
      part_category  = 'TA' "Tail
      part_quantity  = 2 ) INTO TABLE lt_items.


    CATCH cx_uuid_error INTO DATA(uuid_error).
      DATA(ld_message) = uuid_error->get_text( ).
      WRITE: / ld_message.
      RETURN.
  ENDTRY.

  "Delete Existing Data
  IF lt_headers[] IS INITIAL OR
     lt_items[]   IS INITIAL.
    RETURN.
  ENDIF.

  DELETE FROM z4t_monster_head.
  IF sy-subrc EQ 0.
    COMMIT WORK.
  ENDIF.

  DELETE FROM z4t_monster_itms.
  IF sy-subrc EQ 0.
    COMMIT WORK.
  ENDIF.

  WRITE:/ 'Existing Monster BO Data Deleted'.
  COMMIT WORK.

  "Insert New Data
  INSERT z4t_monster_head FROM TABLE lt_headers.
  IF sy-subrc NE 0.
    WRITE: / 'Error Inserting Headers'.
    RETURN.
  ENDIF.

  INSERT z4t_monster_itms FROM TABLE lt_items.
  IF sy-subrc NE 0.
    WRITE: / 'Error Inserting Items'.
    RETURN.
  ENDIF.

  WRITE:/ 'New Monster BO Data Inserted'.
  COMMIT WORK.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form DELIVERY_BO_DATA
*&---------------------------------------------------------------------*
FORM delivery_bo_data.
* Local Variables
  DATA: lt_headers TYPE STANDARD TABLE OF z4t_deliveries,
        ld_number  TYPE z4de_monster_delivery_number.

  IF p_dels EQ abap_false.
    RETURN.
  ENDIF.

  DATA(lo_uuid_generator) = cl_uuid_factory=>create_system_uuid( ).

  TRY.
      DATA(ld_key) = lo_uuid_generator->create_uuid_x16( ).

      INSERT VALUE #(
      db_key           = ld_key
      delivery_number  = '0000000001'
      order_number     = '0000000001'
      order_item       = '000010'
      monster_number   = '0000000001'
      monster_name     = 'JIMMY'
      castle_number    = '1000'
      village_number   = '0000000001'
      task_description = 'Kill Peasants'
      due_date         = sy-datum + 5
      due_time         = '160000'
      actual_date      = sy-datum + 5
      actual_time      = '160000'
      current_status   = 'C'
       )
      INTO TABLE lt_headers.

    CATCH cx_uuid_error INTO DATA(uuid_error).
      DATA(ld_message) = uuid_error->get_text( ).
      WRITE: / ld_message.
      RETURN.
  ENDTRY.

  "Delete Existing Data
  IF lt_headers[] IS INITIAL.
    RETURN.
  ENDIF.

  DELETE FROM z4t_deliveries.
  IF sy-subrc EQ 0.
    COMMIT WORK.
  ENDIF.

  WRITE:/ 'Existing Monster Delivery BO Data Deleted'.
  COMMIT WORK.

  "Insert New Data
  INSERT z4t_deliveries FROM TABLE lt_headers.
  IF sy-subrc NE 0.
    WRITE: / 'Error Inserting Headers'.
    RETURN.
  ENDIF.

  WRITE:/ 'New Monster Delivery BO Data Inserted'.
  COMMIT WORK.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form ORDER_BO_DATA
*&---------------------------------------------------------------------*
FORM order_bo_data.
* Local Variables
  DATA: lt_headers TYPE STANDARD TABLE OF z4t_order_header,
        lt_items   TYPE STANDARD TABLE OF z4t_order_items,
        ld_number  TYPE z4de_monster_order_number,
        ld_time    TYPE timestampl.

  IF p_monst EQ abap_false.
    RETURN.
  ENDIF.

  GET TIME STAMP FIELD ld_time.

  DATA(lo_uuid_generator) = cl_uuid_factory=>create_system_uuid( ).

  TRY.
*--------------------------------------------------------------------*
* Order 1
*--------------------------------------------------------------------*
      DATA(ld_parent_key) = lo_uuid_generator->create_uuid_x16( ).
      ld_number           = '0000000001'.
      INSERT VALUE #(
      db_key           = ld_parent_key
      order_number     = ld_number
      customer         = '0000000001'
      delivery_address = '0000000001'
      createdat        = ld_time
      createdby        = sy-uname
      )
      INTO TABLE lt_headers.

* Items
      DATA(ld_child_key) = lo_uuid_generator->create_uuid_x16( ).
      INSERT VALUE #(
      db_key                = ld_child_key
      parent_key            = ld_parent_key
      order_number          = ld_number
      order_item            = '000010'
      task_description      = 'Scare Peasants'
      castle_number         = '1000'
      fd_target_date        = sy-datum + 5
      fd_target_time        = '160000'
      reservation_number    = '0000000001'
      foul_deed_status      = 'A'
      missng_monster_status = 'R' "Ready
      max_sanity_desired    = 1
      usage_desired         = 'NORM'  "Scares Peasants
      scariness_desired     = 'REAL'  "Really Scary
      brain_size_desired    = 'MICRO' "Microscopic
      model_desired         = 'BTNK'  "Bolts Through Neck
      createdat             = ld_time
      createdby             = sy-uname
       )
      INTO TABLE lt_items.

    CATCH cx_uuid_error INTO DATA(uuid_error).
      DATA(ld_message) = uuid_error->get_text( ).
      WRITE: / ld_message.
      RETURN.
  ENDTRY.

  "Delete Existing Data
  IF lt_headers[] IS INITIAL OR
     lt_items[]   IS INITIAL.
    RETURN.
  ENDIF.

  DELETE FROM z4t_order_header.
  IF sy-subrc EQ 0.
    COMMIT WORK.
  ENDIF.

  DELETE FROM z4t_order_items.
  IF sy-subrc EQ 0.
    COMMIT WORK.
  ENDIF.

  WRITE:/ 'Existing Monster Order BO Data Deleted'.
  COMMIT WORK.

  "Insert New Data
  INSERT z4t_order_header FROM TABLE lt_headers.
  IF sy-subrc NE 0.
    WRITE: / 'Error Inserting Headers'.
    RETURN.
  ENDIF.

  INSERT z4t_order_items FROM TABLE lt_items.
  IF sy-subrc NE 0.
    WRITE: / 'Error Inserting Items'.
    RETURN.
  ENDIF.

  WRITE:/ 'New Monster Order BO Data Inserted'.
  COMMIT WORK.

ENDFORM.
