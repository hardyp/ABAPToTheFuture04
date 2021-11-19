class ZCL_4_MONSTER_DATABASE_EXAMPLE definition
  public
  create public .

public section.

  types:
    mtt_green_monsters TYPE STANDARD TABLE OF zv4_monsters WITH DEFAULT KEY .
  types:
    BEGIN OF m_typ_scariness,
        monster_number    TYPE z4de_monster_number,
        name              TYPE z4de_monster_name,
        scariness_string  TYPE string,
        strength          TYPE z4de_monster_strength,
        sanity_percentage TYPE z4de_monster_sanity,
        scariness_ratio   TYPE decfloat34,
      END OF m_typ_scariness .
  types:
    mtt_scariness TYPE STANDARD TABLE OF m_typ_scariness WITH DEFAULT KEY .
  types:
    mtt_deliveries TYPE STANDARD TABLE OF z4t_deliveries WITH DEFAULT KEY .
  types:
    mtt_villages TYPE STANDARD TABLE OF z4t_villages WITH DEFAULT KEY .

  methods DERIVE_OLD_FASHIONED_MONSTERS
    importing
      !ID_MONSTER_NUMBER type Z4DE_MONSTER_NUMBER .
  methods DERIVE_MONSTER_SCARINESS
    importing
      !ID_MONSTER_NUMBER type Z4DE_MONSTER_NUMBER
    returning
      value(RT_SCARINESS) type MTT_SCARINESS .
  methods DERIVE_MONSTER_BUSINESS_V1
    importing
      !IS_ORDER_ITEM type Z4T_ORDER_ITEMS .
  methods DERIVE_MONSTER_BUSINESS_V2
    importing
      !IS_ORDER_ITEM type Z4T_ORDER_ITEMS .
  methods DERIVE_SCARY_RATIO_OLD
    returning
      value(RT_SCARINESS) type MTT_SCARINESS .
  methods DERIVE_SCARY_RATIO_NEW .
  methods DERIVE_VIA_COOL_WHERE_CLAUSES .
  methods DERIVE_STRINGY_MONSTERS .
  methods DERIVE_MONSTER_INITIALS .
  methods DERIVE_MONSTER_DANCE_FLOOR .
  methods DERIVE_TENTACLED_MONSTERS .
  methods DERIVE_VILLAGES_BY_DESCRIPTION
    importing
      !ID_DESCRIPTION type STRING
    returning
      value(RT_VILLAGES) type MTT_VILLAGES .
  methods DERIVE_ADJACENT_VILLAGES .
  methods UPDATE_GET_SET_GO .
  methods DERIVE_MANUAL_MONSTERS .
  methods DERIVE_AUTOMATIC_MONSTERS .
  methods DERIVE_MONSTER_EXISTENCE
    importing
      !ID_MONSTER_NUMBER type Z4DE_MONSTER_NUMBER
    returning
      value(RF_MONSTER_EXISTS) type ABAP_BOOL .
  methods DERIVE_RES_THE_HARD_WAY .
  methods DERIVE_RES_THE_EASY_WAY .
  methods DERIVE_VIA_COOL_ON_CLAUSE .
  methods DERIVE_UNION_MEMBERSHIP .
  methods DERIVE_BY_AGGREGATION .
  methods DERIVE_DELS_BY_NO_OF_HEADS
    importing
      !ID_NO_OF_HEADS type Z4DE_MONSTER_HEADS
      !IT_DATE_RANGE type /GC1/TAB_RNG_DATE
    returning
      value(RT_ORDERS) type MTT_DELIVERIES .
  methods DERIVE_BLANK_LOOKS .
  methods DERIVE_MONSTER_FILLINGS .
  methods DERIVE_GLOBAL_MONSTERS .
  methods DERIVE_TABLED_MOTION .
  methods DERIVE_GREEN_MONSTERS
    returning
      value(RT_GREEN_MONSTERS) type MTT_GREEN_MONSTERS .
protected section.
private section.
ENDCLASS.



CLASS ZCL_4_MONSTER_DATABASE_EXAMPLE IMPLEMENTATION.


  METHOD derive_adjacent_villages.
*--------------------------------------------------------------------*
* Listing 06.13: LEAD and LAG in Action
*--------------------------------------------------------------------*
* This only works as of ABAP 7.55 (2020)
*--------------------------------------------------------------------*
*    SELECT delivery_number,
*           monster_name,
*           village_address,
*           due_date,
*           due_time,
*    ( LAG( village_address )
*      OVER( PARTITION BY monster_number ORDER BY due_date, due_time ) )
*      AS last_village,
*    ( LEAD( village_address )
*      OVER( PARTITION BY monster_number ORDER BY due_date, due_time ) )
*      AS next_village
*    FROM z4t_deliveries
*    WHERE monster_number EQ '0000000001'
*    AND   due_date       EQ '20221231'
*    ORDER BY due_date, due_time
*    INTO TABLE @DATA(delivery_list).

  ENDMETHOD.


  METHOD derive_automatic_monsters.
*--------------------------------------------------------------------*
* Listing 06.16 Defining Internal Table Based on SQL Query
*--------------------------------------------------------------------*

    SELECT name, monster_number, sanity_percentage
    FROM z4t_monster_head
    WHERE name = 'FRED'
    INTO TABLE @DATA(table_of_monsters).

  ENDMETHOD.


  METHOD derive_blank_looks.

    DATA: lr_village  TYPE RANGE OF z4t_deliveries-village_number,
          lr_castle   TYPE RANGE OF z4t_deliveries-castle_number,
          lr_due_date TYPE RANGE OF z4t_deliveries-due_date.
*--------------------------------------------------------------------*
* Listing 06.25: - Looking for NOT INITIAL Values in date fields
*--------------------------------------------------------------------*
* NB - need to make sure they are Goods Issued i.e. ACTUAL_DATE is not INITIAL
    SELECT  delivery_number
      FROM  z4t_deliveries
      WHERE village_number IN @lr_village       "Ship-to
      AND   castle_number  IN @lr_castle        "Plant
      AND   due_date       IN @lr_due_date      "Delivery date
      AND   current_status EQ @space            "Billing block - not cancelled
      AND   actual_date <> @( VALUE #( ) )      "Delivery has been goods issued
      INTO TABLE @DATA(lt_all_deliveries).
*--------------------------------------------------------------------*
* Or in 7.53+
*--------------------------------------------------------------------*
* NB - need to make sure they are Goods Issued i.e. ACTUAL_DATE is not INITIAL
*    SELECT  delivery_number
*      FROM  z4t_deliveries
*      WHERE village_number IN @lr_village           "Ship-to
*      AND   castle_number  IN @lr_castle            "Plant
*      AND   due_date       IN @lr_due_date          "Delivery date
*      AND   current_status IS INITIAL               "Billing block - not cancelled
*      AND   actual_date    IS NOT INITIAL           "Delivery has been goods issued
*      INTO TABLE @DATA(lt_all_deliveries).

  ENDMETHOD.


  METHOD derive_by_aggregation.
*--------------------------------------------------------------------*
* Listing 06.22: Populating Aggregated Database Table
*--------------------------------------------------------------------*
* You want want to sum up people scared by reading the
* delivery table, as there can be multiple rows per monster
*--------------------------------------------------------------------*
    INSERT z4t_scared_tots FROM
    ( SELECT
    FROM z4t_deliveries
    FIELDS monster_number,
    SUM( actual_no_scared ) AS people_scared
    GROUP BY mandt, monster_number ) ##NULL_VALUES.

  ENDMETHOD.


  METHOD derive_dels_by_no_of_heads.
*--------------------------------------------------------------------*
* Demonstration of Common Table Expressions
*--------------------------------------------------------------------*
    IF 1 = 2.
*--------------------------------------------------------------------*
* Listing 06.23: Traditonal Way of Doing Subqueries in Open SQL
*--------------------------------------------------------------------*
      SELECT *
      FROM z4t_deliveries
      INTO TABLE @DATA(lt_deliveries_old)
      WHERE due_date IN @it_date_range
      AND   monster_number IN ( SELECT monster_number FROM  z4t_monster_head
                                                      WHERE no_of_heads = @id_no_of_heads ).
    ELSE.
*--------------------------------------------------------------------*
* Listing 06.24: Subqueries from 7.51 Onward
*--------------------------------------------------------------------*
      WITH +monsters_with_heads AS
         ( SELECT monster_number FROM z4t_monster_head
           WHERE  no_of_heads = @id_no_of_heads )

       SELECT *
       FROM z4t_deliveries
       WHERE due_date       IN @it_date_range
       AND   monster_number IN ( SELECT monster_number FROM +monsters_with_heads )
       INTO TABLE @DATA(lt_deliveries_new).

    ENDIF.

  ENDMETHOD.


  METHOD derive_global_monsters.
*--------------------------------------------------------------------*
* Listing 06.27: Doing a JOIN on a GTT rather than using FAE
*--------------------------------------------------------------------*
  DATA: selection_list TYPE STANDARD TABLE OF z4gtt_monsters.

* (1) Fill Selection List & Insert into Database
  SELECT monster_number ##too_many_itab_fields
    FROM z4t_monster_head
    INTO CORRESPONDING FIELDS OF TABLE selection_list
    WHERE color EQ 'GREN'."Secret code for "Green"

  IF sy-subrc NE 0.
    RETURN.
  ELSE.
    INSERT z4gtt_monsters FROM TABLE @selection_list.
    IF sy-subrc NE 0.
      "Should be impossible
      RETURN.
    ENDIF.
  ENDIF.

* (2) Join instead of a FAE
  SELECT z4t_deliveries~monster_number, delivery_number
    FROM z4t_deliveries
    INNER JOIN z4gtt_monsters
    ON z4t_deliveries~monster_number EQ z4gtt_monsters~monster_number
    WHERE due_date EQ @sy-datum
    INTO TABLE @DATA(todays_green_deliveries).

  IF sy-subrc NE 0.
    "Do not RETURN - you have work to do
  ENDIF.

* (3) Remove the data from the GTT
  DELETE FROM z4gtt_monsters.

* (4) Get a Clue
* Jimmy is a GREEN Monster
  ENDMETHOD.


  METHOD derive_green_monsters.
*--------------------------------------------------------------------*
* Listing 06.40: - Reading Data from CDS View Entity in ABAP
*--------------------------------------------------------------------*
    "Read the view into an internal table
    SELECT * FROM zcds_4_monsters_join
    INTO CORRESPONDING FIELDS OF TABLE @rt_green_monsters ##too_many_itab_fields.

    "Or just display the contents directly
    cl_salv_gui_table_ida=>create_for_cds_view( 'ZCDS_4_MONSTERS_JOIN' )->fullscreen( )->display( ).

  ENDMETHOD.


  METHOD derive_manual_monsters.
*--------------------------------------------------------------------*
* Listing 06.15 : Defining an Internal Table Manually
*--------------------------------------------------------------------*
    TYPES: BEGIN OF l_typ_monsters,
             name              TYPE z4t_monster_head-name,
             monster_number    TYPE z4t_monster_head-monster_number,
             sanity_percentage TYPE z4t_monster_head-sanity_percentage,
           END OF l_typ_monsters.

    DATA: table_of_monsters TYPE STANDARD TABLE OF l_typ_monsters.

    SELECT name monster_number sanity_percentage
    FROM z4t_monster_head
    INTO CORRESPONDING FIELDS OF TABLE table_of_monsters
    WHERE name = 'FRED'.

  ENDMETHOD.


  METHOD derive_monster_business_v1.
*--------------------------------------------------------------------*
* Listing 06.03: Looking for a Business Data Record in an Internal Table
*--------------------------------------------------------------------*
    DATA: lc_header_posnr TYPE posnr VALUE '000000'.

    SELECT ordernumber, itemnumber, paymentterms
      FROM z4tmonster_bdata
      INTO TABLE @DATA(monster_bd_table)
      WHERE ordernumber EQ @is_order_item-order_number.

    IF sy-subrc EQ 0.
      DATA(monster_bd_record) =
      VALUE          #( monster_bd_table[ ordernumber = is_order_item-order_number
                                          itemnumber  = is_order_item-order_item ]
      DEFAULT VALUE  #( monster_bd_table[ ordernumber = is_order_item-order_number
                                          itemnumber  = lc_header_posnr ] ) ).
    ENDIF.

  ENDMETHOD.


  METHOD derive_monster_business_v2.
*-------------------------------------------------------------------------*
* Listing 06.04: COALESCE in Action
*-------------------------------------------------------------------------*
* This reflects table VBKD in the standard SAP ERP system
* You pass in item 50 for example
* If an item record exists for item 50 then that is returned
* If no item record exists for item 50 then the header record is returned
*-------------------------------------------------------------------------*
    DATA: lc_header_posnr TYPE posnr VALUE '000000'.

    SELECT SINGLE paymentterms
      FROM z4tmonster_bdata
      INTO @DATA(payment_terms)
      WHERE ordernumber EQ @is_order_item-order_number
      AND   itemnumber  EQ (
    "Sub-Query to Get Correct Business Data item
    SELECT
    coalesce( item~itemnumber , header~itemnumber )
    FROM z4t_order_items
    LEFT OUTER JOIN z4tmonster_bdata AS header
      ON header~ordernumber = z4t_order_items~order_number AND
         header~itemnumber  = @lc_header_posnr
    LEFT OUTER JOIN z4tmonster_bdata AS item
        ON item~ordernumber = z4t_order_items~order_number AND
           item~itemnumber  = z4t_order_items~order_item
    WHERE z4t_order_items~order_number EQ @is_order_item-order_number
    AND   z4t_order_items~order_item   EQ @is_order_item-order_item ).

  ENDMETHOD.


  METHOD derive_monster_dance_floor.
*--------------------------------------------------------------------*
* Listing 06.10: SQL Function – Using FLOOR to round down values
*--------------------------------------------------------------------*

    SELECT order_number, order_item,
           floor( max_sanity_desired ) as rounded_sanity
      FROM z4t_order_items
      WHERE fd_target_date EQ @sy-datum
      INTO TABLE @DATA(order_sanity_table).

* Each of the five Suspect Monsters has a different SUPER-POWER. The Baron only ever gives each monster one
* super-power so that they do not become too big for their boots.
  ENDMETHOD.


  METHOD derive_monster_existence.
*--------------------------------------------------------------------*
* Listing 06.17 Existence Check with No Database Rows Transported
*--------------------------------------------------------------------*
    SELECT SINGLE @abap_true
    FROM z4t_monster_head
    WHERE monster_number = @id_monster_number
    INTO @rf_monster_exists.

  ENDMETHOD.


  METHOD derive_monster_fillings.
*--------------------------------------------------------------------*
* Listing 06.26: - Filling a Range Table
*--------------------------------------------------------------------*
    DATA: lr_village  TYPE RANGE OF z4t_deliveries-village_number.

    SELECT FROM z4t_deliveries
      FIELDS
      'I'            AS sign,
      'EQ'           AS option,
      village_number AS low,
      ' '            AS high
      WHERE monster_name EQ 'JIMMY'
      INTO CORRESPONDING FIELDS OF TABLE @lr_village.

  ENDMETHOD.


  METHOD derive_monster_initials.
*--------------------------------------------------------------------*
* Listing 06.09: Using IS INITIAL in WHERE Clause
*--------------------------------------------------------------------*
* Only works from ABAP 1809 (7.53) Upwards
*--------------------------------------------------------------------*
*    SELECT monster_number
*    FROM z4t_monster_head
*    WHERE hat_size IS INITIAL
*    INTO TABLE @DATA(monsters_without_hats)."They don’t dance

  ENDMETHOD.


  METHOD derive_monster_scariness.
*--------------------------------------------------------------------*
* Listing 06.02: - CASE Statement within SQL Statement
*--------------------------------------------------------------------*
    CONSTANTS: lc_scary1 TYPE char20 VALUE 'SLIGHTLY SCARY',
               lc_scary2 TYPE char20 VALUE 'REALLY SCARY',
               lc_scary3 TYPE char20 VALUE 'NORMAL'.

    SELECT name, monster_number,
      CASE
        WHEN sanity_percentage <= 10 AND strength >= 75 THEN @lc_scary2
        WHEN sanity_percentage <= 25 AND strength >= 50 THEN @lc_scary1
        ELSE @lc_scary3
      END AS scariness_string
      FROM z4t_monster_head
      WHERE monster_number = @id_monster_number
      INTO CORRESPONDING FIELDS OF TABLE @rt_scariness ##too_many_itab_fields.

  ENDMETHOD.


  METHOD derive_old_fashioned_monsters.
*--------------------------------------------------------------------*
* Listing 06.01: - ABAP SQL
*--------------------------------------------------------------------*
    DATA: monster_table TYPE STANDARD TABLE OF z4t_monster_head.

    SELECT monster_number name
      FROM z4t_monster_head
      INTO CORRESPONDING FIELDS OF TABLE monster_table
      WHERE monster_number = id_monster_number ##too_many_itab_fields.

    IF sy-subrc NE 0.
      RETURN.
    ENDIF.

* One Haunted Castle can be found at 1.9 degrees Longtitude
  ENDMETHOD.


  METHOD derive_res_the_easy_way.
*--------------------------------------------------------------------*
* Listing 06.19: - Reading Everything from One Table during Inner Join
*--------------------------------------------------------------------*
    SELECT z4t_monster_head~*,
           z4t_reservations~reservation_number
    FROM z4t_monster_head
    INNER JOIN z4t_reservations
    ON z4t_monster_head~monster_number = z4t_reservations~monster_number
    WHERE name = 'JIMMY'
    INTO TABLE @DATA(table_of_monsters).

  ENDMETHOD.


  METHOD derive_res_the_hard_way.
*--------------------------------------------------------------------*
* Listing 06.18: - Lots of Fields from Main Table
*--------------------------------------------------------------------*
* Sometimes this list (of fields) can go on for pages for really wide tables
    TYPES: BEGIN OF l_typ_monsters,
             reservation_number TYPE z4de_monster_reservation_no,
             name               TYPE z4t_monster_head-name,
             monster_number     TYPE z4t_monster_head-monster_number,
             sanity_percentage  TYPE z4t_monster_head-sanity_percentage,
             strength           TYPE z4t_monster_head-strength,
             hat_size           TYPE z4t_monster_head-hat_size,
             no_of_heads        TYPE z4t_monster_head-no_of_heads,
           END OF l_typ_monsters.

    DATA: table_of_monsters TYPE STANDARD TABLE OF l_typ_monsters.

    SELECT
    z4t_reservations~reservation_number
    z4t_monster_head~name              z4t_monster_head~monster_number
    z4t_monster_head~sanity_percentage z4t_monster_head~strength
    z4t_monster_head~hat_size          z4t_monster_head~no_of_heads
    FROM z4t_monster_head
    INNER JOIN z4t_reservations
    ON z4t_monster_head~monster_number = z4t_reservations~monster_number
    INTO CORRESPONDING FIELDS OF TABLE table_of_monsters
    WHERE z4t_monster_head~name = 'JIMMY'.

* One Haunted Castle has an Oubliette where the owners would would drop guests through the trap door to be impaled
* on the spikes 8 feet below
  ENDMETHOD.


  METHOD derive_scary_ratio_new.
*--------------------------------------------------------------------*
* Listing 06.06: Operations inside SQL Statements: After
*--------------------------------------------------------------------*

    SELECT name, monster_number,
    CAST( strength AS FLTP ) / CAST( sanity_percentage AS FLTP )
    AS scariness_ratio
    FROM z4t_monster_head
    INTO TABLE @DATA(scariness_table).

  ENDMETHOD.


  METHOD derive_scary_ratio_old.
*--------------------------------------------------------------------*
* Listing 06.05: Operations inside SQL Statements: Before 7.4
*--------------------------------------------------------------------*
* Local Variables
    DATA: converted_strength TYPE decfloat34,
          converted_sanity   TYPE decfloat34.

    FIELD-SYMBOLS: <monster_details>
    LIKE LINE OF rt_scariness.

    SELECT name monster_number strength sanity_percentage
    ##too_many_itab_fields "in the world
    FROM z4t_monster_head
    INTO CORRESPONDING FIELDS OF TABLE rt_scariness.

    LOOP AT rt_scariness ASSIGNING <monster_details>.
      converted_strength = <monster_details>-strength.
      converted_sanity   = <monster_details>-sanity_percentage.
      <monster_details>-scariness_ratio =
      converted_strength / converted_sanity.
    ENDLOOP.

  ENDMETHOD.


  METHOD derive_stringy_monsters.
*--------------------------------------------------------------------*
* Listing 06.08: String Functions in SELECT Statements
*--------------------------------------------------------------------*
    DATA(helper_function) = NEW lcl_monster_functions( ).

    SELECT
    concat( monster_number, replace( 'EVIL', 'VERY_EVIL' , name ) )
    AS monster_description,
    length( weapon ) AS weapon_length
    FROM z4t_monster_head
    WHERE hat_size = @( helper_function->hat_size_of_the_day( ) )
    INTO TABLE @DATA(evilized_monster_weapons).

  ENDMETHOD.


  METHOD derive_tabled_motion.
*--------------------------------------------------------------------*
* Listing 06.28: - Doing a JOIN on an Internal Table
*--------------------------------------------------------------------*
* (1) Fill Selection List
    SELECT monster_number
      FROM z4t_monster_head
      INTO TABLE @DATA(selection_list)
      WHERE color EQ 'GREN'."Secret code for "Green"

    IF sy-subrc NE 0.
      RETURN.
    ENDIF.

    IF cl_abap_dbfeatures=>use_features(
        requested_features =
          VALUE #( ( cl_abap_dbfeatures=>itabs_in_from_clause ) ) ).

* (2) Join instead of a FAE
    SELECT z4t_deliveries~monster_number, delivery_number
      FROM @selection_list AS selection_list
      INNER JOIN z4t_deliveries
      ON z4t_deliveries~monster_number EQ selection_list~monster_number
      WHERE due_date EQ @sy-datum
      INTO TABLE @DATA(todays_green_deliveries) ##DB_FEATURE_MODE[ITABS_IN_FROM_CLAUSE].

    IF sy-subrc NE 0.
      RETURN.
    ENDIF.

    ELSE.
      "Your database does not support reading from an Internal table
      RETURN.
    ENDIF.

  ENDMETHOD.


  METHOD derive_tentacled_monsters.
*--------------------------------------------------------------------*
* Listing 06.11: - SQL Function as Selection Condition
*--------------------------------------------------------------------*
* This only works as of ABAP 7.55 (2020)
*--------------------------------------------------------------------*
*    DATA: count_to_four TYPE c LENGTH 10 VALUE '1234'.
*
*    SELECT *
*      FROM z4t_monster_itms
*      WHERE part_category EQ 'TN' "Tentacle
*      AND   part_quantity GT length( @count_to_four )
*      INTO TABLE @DATA(tentacled_monsters).

  ENDMETHOD.


  METHOD derive_union_membership.
*--------------------------------------------------------------------*
* Listing 06.21: - SELECT from Different Tables Using UNION
*--------------------------------------------------------------------*
* Oh my love, you've been so much on my mind
* I've been waiting for this day to arrive
* It feels so long, I watch the hours go by
* It really doesn't matter when you're here by my side
* UNION - together we're strong
* UNION - forever be mine
* (c) Erasure (1986)
*--------------------------------------------------------------------*
    SELECT monster_number AS monster_number, tax_number AS tax_number
    FROM z4t_monster_head
    UNION DISTINCT
    SELECT monster_number AS monster_number, ' ' AS tax_number
    FROM z4t_mon_op_items
    UNION DISTINCT
    SELECT monster_number AS monster_number, ' ' AS tax_number
    FROM z4t_mon_cl_items
    INTO TABLE @DATA(monster_tax_no_list).

  ENDMETHOD.


  METHOD derive_via_cool_on_clause.
*--------------------------------------------------------------------*
* Listing 06.20: - Joining Two Tables in Newly Possible Ways
*--------------------------------------------------------------------*

    SELECT z4t_reservations~reservation_number,
           z4t_monster_head~*
    FROM z4t_reservations
    INNER JOIN z4t_monster_head
    ON    z4t_monster_head~monster_number EQ z4t_reservations~monster_number
    AND   z4t_monster_head~weapon         LIKE 'AXE%'
    AND   z4t_monster_head~evilness       IN  ('EVIL','VERY','JOLY')
    WHERE z4t_monster_head~name           EQ   'JIMMY'
    INTO TABLE @DATA(table_of_monsters).

  ENDMETHOD.


  METHOD derive_via_cool_where_clauses.
*--------------------------------------------------------------------*
* Listing 06.07: New Possibilities in WHERE Clauses
*--------------------------------------------------------------------*
    SELECT monster_number, name
    FROM z4t_monster_head
    WHERE eas_days + sanity_percentage > 100
    INTO TABLE @DATA(old_sane_monster_list).

    DATA(helper_function) = NEW lcl_monster_functions( ).

    SELECT monster_number, name
    FROM z4t_monster_head
    WHERE hat_size = @( helper_function->hat_size_of_the_day( ) )
    INTO TABLE @DATA(fashionable_monster_list).

  ENDMETHOD.


  METHOD derive_villages_by_description.
*--------------------------------------------------------------------*
* Listing 06.12: - Case-Insensitive Database Query
*--------------------------------------------------------------------*
* Preconditions
    CHECK id_description IS NOT INITIAL.

    DATA(upper_case_query) = `%` && to_upper( id_description ) && `%`.

    SELECT *
    FROM z4t_villages
    WHERE upper( village_address_description ) LIKE @upper_case_query
    ORDER BY village_number
    INTO TABLE @rt_villages.

  ENDMETHOD.


  METHOD update_get_set_go.
*--------------------------------------------------------------------*
* Listing 06.14: Set Indicators in Action
*--------------------------------------------------------------------*
* This only works in ABAP 7.55 (2020)
*--------------------------------------------------------------------*
*    TYPES: item_update  TYPE z4t_monster_itms WITH INDICATORS itemsx TYPE abap_bool.
*    DATA:  item_updates TYPE STANDARD TABLE OF item_update.
*
*    item_updates = VALUE #(
*  ( monster_number = '0000000001' monster_item = '00010'
*    part_quantity = 3 itemsx-part_quantity = abap_true )
*  ( monster_number = '0000000001' monster_item = '00020'
*    part_quantity = 4 itemsx-part_quantity = abap_true  )
*  ( monster_number = '0000000001' monster_item = '00030'
*    part_quantity = 5 itemsx-part_quantity = abap_true ) ).
*
*    UPDATE z4t_monster_itms FROM TABLE @item_updates
*                            INDICATORS SET STRUCTURE itemsx.

  ENDMETHOD.
ENDCLASS.
