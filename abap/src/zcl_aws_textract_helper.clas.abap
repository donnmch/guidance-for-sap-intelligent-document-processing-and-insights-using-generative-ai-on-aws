class ZCL_AWS_TEXTRACT_HELPER definition
  public
  final
  create public .

public section.

  types:
    BEGIN OF ty_word,
        page TYPE string,
        word TYPE string,
      END   OF ty_word .
  types:
    tt_word TYPE STANDARD TABLE OF ty_word WITH NON-UNIQUE KEY word .
  types:
    BEGIN OF ty_expense_item,
        posnr            TYPE posnr_acc,
        quantity         TYPE string,
        unit_price       TYPE bapidoccur,
        price            TYPE bapidoccur,
        description      TYPE string,
        product_code     TYPE string,
        confidence_score TYPE /aws1/texpercent,
        page             TYPE /aws1/texuinteger,
      END OF ty_expense_item .
  types:
    BEGIN OF ty_form,
        key   TYPE string,
        value TYPE string,
        page  TYPE string,
      END OF ty_form .
  types:
    tt_form TYPE STANDARD TABLE OF ty_form WITH NON-UNIQUE KEY key .
  types:
    BEGIN OF ty_expense_summary,
        expense_type TYPE string,
        value        TYPE string,
        score        TYPE /aws1/texpercent,
        page         TYPE /aws1/texuinteger,
        currency     type string,
        noreceipt    type string,
      END OF ty_expense_summary .
  types:
    BEGIN OF ty_expense,
        expense_index   TYPE /aws1/texuinteger,
        expense_summary TYPE STANDARD TABLE OF ty_expense_summary WITH DEFAULT KEY,
        expense_items   TYPE STANDARD TABLE OF ty_expense_item   WITH DEFAULT KEY,
        bucket          TYPE string,
        bucket_key      TYPE string,
        aedtm           TYPE timestamp,
        aenam           TYPE sy-uname,
        status          TYPE string,
        belnr           type belnr_d,
      END  OF ty_expense .
  types:
    BEGIN OF ty_table,
        table_id TYPE string,
        page     TYPE string,
        row_indx TYPE sy-tabix,
        col_indx TYPE sy-tabix,
        cell_val TYPE string,
      END OF ty_table .
  types:
    tt_expense_item    TYPE STANDARD TABLE OF ty_expense_item WITH DEFAULT KEY .
  types:
    tt_table TYPE STANDARD TABLE OF ty_table WITH NON-UNIQUE KEY table_id .
  types:
    tt_expense_summary TYPE STANDARD TABLE OF ty_expense_summary WITH DEFAULT KEY .
  types:
    tt_expenses        TYPE STANDARD TABLE OF ty_expense WITH DEFAULT KEY .

  data GO_TEXTRACT type ref to /AWS1/IF_TEX .
  data GO_SESSION type ref to /AWS1/CL_RT_SESSION_BASE .
  data AT_FEATURETYPES type /AWS1/CL_TEXFEATURETYPES_W=>TT_FEATURETYPES .
  constants CO_STATUS type STRING value 'SUCCEEDED' ##NO_TEXT.
  data:
    at_expsummary_types TYPE STANDARD TABLE OF char80 WITH DEFAULT KEY .
  data:
    at_expitem_types TYPE STANDARD TABLE OF char80 WITH DEFAULT KEY .

  methods CONSTRUCTOR
    importing
      !IV_PROFILE type /AWS1/RT_PROFILE_ID
    raising
      /AWS1/CX_RT_TECHNICAL_GENERIC
      /AWS1/CX_RT_NO_AUTH_GENERIC
      /AWS1/CX_RT_SERVICE_GENERIC .
  methods SET_FEATURE_TYPES
    importing
      !IM_TT_FEATURES type STRINGTAB optional .
  methods ANALYZE_DOCUMENT_ASYNCHRONOUS
    importing
      !IV_BUCKET type STRING
      !IV_KEY type STRING
    returning
      value(RE_JOBID) type STRING
    raising
      /AWS1/CX_TEXACCESSDENIEDEX
      /AWS1/CX_TEXBADDOCUMENTEX
      /AWS1/CX_TEXDOCUMENTTOOLARGEEX
      /AWS1/CX_TEXIDEMPOTENTPRMMIS00
      /AWS1/CX_TEXINTERNALSERVERERR
      /AWS1/CX_TEXINVALIDKMSKEYEX
      /AWS1/CX_TEXINVALIDS3OBJECTEX
      /AWS1/CX_TEXLIMITEXCEEDEDEX
      /AWS1/CX_TEXPROVTHRUPUTEXCDEX
      /AWS1/CX_TEXTHROTTLINGEX
      /AWS1/CX_TEXUNSUPPORTEDDOCEX
      /AWS1/CX_TEXCLIENTEXC
      /AWS1/CX_TEXSERVEREXC
      /AWS1/CX_RT_TECHNICAL_GENERIC
      /AWS1/CX_RT_SERVICE_GENERIC .
  methods GET_DOCUMENT_RESULTS_ASYNCH
    importing
      !IM_JOBID type STRING
    returning
      value(RE_STATUS) type STRING
    raising
      /AWS1/CX_TEXACCESSDENIEDEX
      /AWS1/CX_TEXINTERNALSERVERERR
      /AWS1/CX_TEXINVALIDJOBIDEX
      /AWS1/CX_TEXINVALIDKMSKEYEX
      /AWS1/CX_TEXINVALIDPARAMETEREX
      /AWS1/CX_TEXINVALIDS3OBJECTEX
      /AWS1/CX_TEXPROVTHRUPUTEXCDEX
      /AWS1/CX_TEXTHROTTLINGEX
      /AWS1/CX_TEXCLIENTEXC
      /AWS1/CX_TEXSERVEREXC
      /AWS1/CX_RT_TECHNICAL_GENERIC
      /AWS1/CX_RT_SERVICE_GENERIC .
  methods GET_TABLE_DATA
    returning
      value(RE_DATA) type TT_TABLE .
  methods GET_FORM_DATA
    returning
      value(RE_DATA) type TT_FORM .
  methods GET_WORDS
    returning
      value(RE_DATA) type TT_WORD .
  methods GET_LINES
    returning
      value(RE_DATA) type TT_WORD .
  methods ANALYZE_EXPENSE
    importing
      !IV_BUCKET type STRING
      !IV_KEY type STRING
    returning
      value(RT_EXPENSE) type TT_EXPENSES
    raising
      /AWS1/CX_TEXACCESSDENIEDEX
      /AWS1/CX_TEXBADDOCUMENTEX
      /AWS1/CX_TEXDOCUMENTTOOLARGEEX
      /AWS1/CX_TEXINTERNALSERVERERR
      /AWS1/CX_TEXINVALIDPARAMETEREX
      /AWS1/CX_TEXINVALIDS3OBJECTEX
      /AWS1/CX_TEXPROVTHRUPUTEXCDEX
      /AWS1/CX_TEXTHROTTLINGEX
      /AWS1/CX_TEXUNSUPPORTEDDOCEX
      /AWS1/CX_TEXCLIENTEXC
      /AWS1/CX_TEXSERVEREXC
      /AWS1/CX_RT_TECHNICAL_GENERIC
      /AWS1/CX_RT_SERVICE_GENERIC .
  methods SET_MIN_SCORE
    importing
      !IV_MIN_SCORE type /AWS1/TEXPERCENT .
  methods ANALYZE_EXPENSE_ASYNCH
    importing
      !IV_BUCKET type STRING
      !IV_KEY type STRING
    returning
      value(RE_JOBID) type STRING
    raising
      /AWS1/CX_TEXACCESSDENIEDEX
      /AWS1/CX_TEXBADDOCUMENTEX
      /AWS1/CX_TEXDOCUMENTTOOLARGEEX
      /AWS1/CX_TEXIDEMPOTENTPRMMIS00
      /AWS1/CX_TEXINTERNALSERVERERR
      /AWS1/CX_TEXINVALIDKMSKEYEX
      /AWS1/CX_TEXINVALIDS3OBJECTEX
      /AWS1/CX_TEXLIMITEXCEEDEDEX
      /AWS1/CX_TEXPROVTHRUPUTEXCDEX
      /AWS1/CX_TEXTHROTTLINGEX
      /AWS1/CX_TEXUNSUPPORTEDDOCEX
      /AWS1/CX_TEXCLIENTEXC
      /AWS1/CX_TEXSERVEREXC
      /AWS1/CX_RT_TECHNICAL_GENERIC
      /AWS1/CX_RT_SERVICE_GENERIC .
  methods GET_EXPENSE_RESULTS_ASYNCH
    importing
      !IM_JOBID type STRING
    exporting
      !RE_STATUS type STRING
      !RT_EXPENSES type TT_EXPENSES
    raising
      /AWS1/CX_TEXACCESSDENIEDEX
      /AWS1/CX_TEXINTERNALSERVERERR
      /AWS1/CX_TEXINVALIDJOBIDEX
      /AWS1/CX_TEXINVALIDKMSKEYEX
      /AWS1/CX_TEXINVALIDPARAMETEREX
      /AWS1/CX_TEXINVALIDS3OBJECTEX
      /AWS1/CX_TEXPROVTHRUPUTEXCDEX
      /AWS1/CX_TEXTHROTTLINGEX
      /AWS1/CX_TEXCLIENTEXC
      /AWS1/CX_TEXSERVEREXC
      /AWS1/CX_RT_TECHNICAL_GENERIC
      /AWS1/CX_RT_SERVICE_GENERIC .
protected section.
private section.

  types:
    BEGIN OF ty_block,
           block_id TYPE string,
           block    TYPE REF TO /aws1/cl_texblock,
         END   OF ty_block .
  types:
    tt_block TYPE STANDARD TABLE OF ty_block .

  data AT_BLOCKS type /AWS1/CL_TEXBLOCK=>TT_BLOCKLIST .
  data AT_KEY_BLOCK type TT_BLOCK .
  data AT_VAL_BLOCK type TT_BLOCK .
  data AT_BLOCK_MAP type TT_BLOCK .
  data AT_TAB_BLOCK type TT_BLOCK .
  data GV_MIN_SCORE type /AWS1/TEXPERCENT .
  data AT_DOCUMENTS type /AWS1/CL_TEXEXPENSEDOCUMENT=>TT_EXPENSEDOCUMENTLIST .
  data GV_BUCKET type STRING .
  data GV_KEY type STRING .

  methods GET_KEY_VALUE_BLOCK_MAPS .
  methods GET_TABLE_BLOCKS .
  methods GET_BLOCKS
    importing
      !IM_JOBID type STRING
    raising
      /AWS1/CX_TEXACCESSDENIEDEX
      /AWS1/CX_TEXINTERNALSERVERERR
      /AWS1/CX_TEXINVALIDJOBIDEX
      /AWS1/CX_TEXINVALIDKMSKEYEX
      /AWS1/CX_TEXINVALIDPARAMETEREX
      /AWS1/CX_TEXINVALIDS3OBJECTEX
      /AWS1/CX_TEXPROVTHRUPUTEXCDEX
      /AWS1/CX_TEXTHROTTLINGEX
      /AWS1/CX_TEXCLIENTEXC
      /AWS1/CX_TEXSERVEREXC
      /AWS1/CX_RT_TECHNICAL_GENERIC
      /AWS1/CX_RT_SERVICE_GENERIC .
  methods GET_VALUEBLOCK_FOR_KEY
    importing
      !IM_KEY_BLOCK type TY_BLOCK
    returning
      value(RE_VAL_BLOCK) type TY_BLOCK .
  methods GET_TEXT_BLOCK
    importing
      !IM_BLOCK type TY_BLOCK
    returning
      value(RE_VALUE) type STRING .
  methods GET_ROWS_TABLE_BLOCK
    importing
      !IM_TABLEID type STRING
      !IM_BLOCK type TY_BLOCK
    returning
      value(RE_TABLE) type TT_TABLE .
  methods SET_EXPSUMMARY_FIELD_TYPES .
  methods SET_EXPITEM_FIELDS_TYPES .
  methods GET_EXPENSE_SUMMARY
    importing
      !IO_EXPENSE type ref to /AWS1/CL_TEXEXPENSEDOCUMENT
    returning
      value(RT_EXPENSE_SUMMARY) type TT_EXPENSE_SUMMARY .
  methods GET_EXPENSE_ITEMS
    importing
      !IO_EXPENSE type ref to /AWS1/CL_TEXEXPENSEDOCUMENT
    returning
      value(RT_ITEMS) type TT_EXPENSE_ITEM .
  methods GET_EXPENSE_DOCUMENTS
    importing
      !IM_JOBID type STRING
    raising
      /AWS1/CX_TEXACCESSDENIEDEX
      /AWS1/CX_TEXINTERNALSERVERERR
      /AWS1/CX_TEXINVALIDJOBIDEX
      /AWS1/CX_TEXINVALIDKMSKEYEX
      /AWS1/CX_TEXINVALIDPARAMETEREX
      /AWS1/CX_TEXINVALIDS3OBJECTEX
      /AWS1/CX_TEXPROVTHRUPUTEXCDEX
      /AWS1/CX_TEXTHROTTLINGEX
      /AWS1/CX_TEXCLIENTEXC
      /AWS1/CX_TEXSERVEREXC
      /AWS1/CX_RT_TECHNICAL_GENERIC
      /AWS1/CX_RT_SERVICE_GENERIC .
  methods GET_EXPENSE_SUMMARY_MULTIPAGE
    returning
      value(RT_EXPENSE_SUMMARY) type TT_EXPENSE_SUMMARY .
  methods GET_EXPENSE_ITEMS_MULTIPAGE
    returning
      value(RT_ITEMS) type TT_EXPENSE_ITEM .
ENDCLASS.



CLASS ZCL_AWS_TEXTRACT_HELPER IMPLEMENTATION.


  METHOD ANALYZE_DOCUMENT_ASYNCHRONOUS.

    DATA(lo_s3object) = NEW /aws1/cl_texs3object( iv_bucket = iv_bucket
                                                iv_name   = iv_key  ).

    DATA(lo_documentlocation) = NEW /aws1/cl_texdocumentlocation( io_s3object = lo_s3object ).

* Start document analysis
   re_jobid = go_textract->startdocumentanalysis(
        EXPORTING
          io_documentlocation    =  lo_documentlocation
          it_featuretypes        =  at_featuretypes
      )->get_jobid( ).


  ENDMETHOD.


  METHOD constructor.
    go_session  =  /aws1/cl_rt_session_aws=>create( iv_profile_id =  iv_profile ).
    go_textract =  /aws1/cl_tex_factory=>create( go_session ).
    set_feature_types( ).
    set_expitem_fields_types( ).
    set_expsummary_field_types( ).
  ENDMETHOD.


  METHOD get_blocks.


    IF ( go_textract->getdocumentanalysis( EXPORTING
            iv_jobid = im_jobid )->has_blocks( ) = abap_true ).
      at_blocks = go_textract->getdocumentanalysis(
                     EXPORTING iv_jobid = im_jobid )->get_blocks( ).

      IF go_textract->getdocumentanalysis(
                                        EXPORTING
                                        iv_jobid = im_jobid
                                        )->has_nexttoken( ) = abap_true.

        DATA(lv_token) = go_textract->getdocumentanalysis(
                                         EXPORTING iv_jobid = im_jobid
                                         )->get_nexttoken( ).
      ENDIF.

      WHILE lv_token IS NOT INITIAL.
        DATA(lt_next_blocks) =   go_textract->getdocumentanalysis(
                                      EXPORTING iv_jobid = im_jobid
                                                iv_nexttoken = lv_token
                                                )->get_blocks(  ).
        APPEND LINES OF lt_next_blocks TO at_blocks.

        IF go_textract->getdocumentanalysis(
                                        EXPORTING
                                        iv_jobid = im_jobid
                                        iv_nexttoken = lv_token
                                        )->has_nexttoken( ) = abap_true.

          lv_token = go_textract->getdocumentanalysis(
                                       EXPORTING
                                        iv_jobid = im_jobid
                                        iv_nexttoken = lv_token
                                        )->get_nexttoken( ).
        ELSE.

          EXIT.
        ENDIF.
      ENDWHILE.
    ENDIF.

  ENDMETHOD.


  METHOD get_document_results_asynch.

    WHILE go_textract->getdocumentanalysis( EXPORTING iv_jobid = im_jobid )->get_jobstatus( ) <> 'SUCCEEDED'.

      cl_progress_indicator=>progress_indicate(
       i_text = |Watiing for Textract to finish Processing Job|
       i_output_immediately = abap_true ).
*
      WAIT UP TO 10 SECONDS.

      IF sy-index = 10.
        DATA(lv_status) = go_textract->getdocumentanalysis( EXPORTING
                             iv_jobid = im_jobid )->get_jobstatus( ).

        EXIT.
      ENDIF.
    ENDWHILE.

    IF lv_status IS NOT INITIAL.
      re_status = lv_status.
      EXIT.
    ENDIF.

    get_blocks( im_jobid ).

    get_key_value_block_maps( ).

    get_table_blocks( ).


    re_status = go_textract->getdocumentanalysis( EXPORTING iv_jobid = im_jobid )->get_jobstatus( ).

  ENDMETHOD.


  method GET_FORM_DATA.

 data: ls_form type ty_form.

      LOOP AT at_key_block INTO DATA(ls_key_block).

       data(ls_val_block) = get_valueblock_for_key( im_key_block = ls_key_block ).

       ls_form-key = get_text_block( im_block = ls_key_block ).
       ls_form-value = get_text_block( im_block = ls_val_block ).
       ls_form-page = ls_val_block-block->get_page( ).

       append ls_form to re_data.

      endloop.

  endmethod.


  METHOD get_key_value_block_maps.

    CLEAR: at_block_map, at_key_block, at_val_block.

    LOOP AT AT_blocks INTO DATA(ls_blocks).
* get key, value, block maps
      AT_block_map = VALUE #( BASE At_block_map ( block_id =
      ls_blocks->get_id( ) block = ls_blocks ) ).

      IF ls_blocks->get_blocktype( ) EQ 'KEY_VALUE_SET'.
        DATA(lt_entitytypes) = ls_blocks->get_entitytypes( ).
        IF lt_entitytypes[] IS NOT INITIAL.
          LOOP AT lt_entitytypes INTO DATA(ls_entitytypes).
            IF ls_entitytypes->get_value( ) EQ 'KEY'.
              At_key_block = VALUE #( BASE At_key_block ( block_id =
              ls_blocks->get_id( ) block = ls_blocks ) ).
            ELSE.
              At_val_block = VALUE #( BASE At_val_block ( block_id =
              ls_blocks->get_id( ) block = ls_blocks ) ).
            ENDIF.
          ENDLOOP.
        ENDIF.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.


  method GET_LINES.

    LOOP AT at_block_map INTO DATA(ls_block_map).

      IF ls_block_map-block->get_blocktype( ) EQ 'LINE'.

        re_data = VALUE #( BASE re_data (
        page = ls_block_map-block->get_page( )
        word = ls_block_map-block->get_text( ) ) ).
      ENDIF.

    ENDLOOP.


  endmethod.


  METHOD get_rows_table_block.

    DATA: ls_table TYPE ty_table.
    DATA: lt_table TYPE tt_table.

    DATA(lt_relationships) = im_block-block->get_relationships( ).

    LOOP AT lt_relationships INTO DATA(ls_relationship).
      IF ls_relationship->get_type( ) EQ 'CHILD'.
        LOOP AT ls_relationship->get_ids( ) INTO DATA(ls_id).
          READ TABLE at_block_map WITH KEY block_id = ls_id->get_value( )
                                 INTO DATA(ls_cell_block).


          IF sy-subrc = 0 AND
             ls_cell_block-block->get_blocktype( ) EQ 'CELL'.

            CLEAR: ls_table.

            ls_table-cell_val = get_text_block( im_block = ls_cell_block  ).
            ls_table-row_indx = ls_cell_block-block->get_rowindex( ).
            ls_table-col_indx = ls_cell_block-block->get_columnindex( ).
            ls_table-page = ls_cell_block-block->get_page( ).
            ls_table-table_id = im_tableid.
            APPEND ls_table TO lt_table.

          ENDIF.
        ENDLOOP.
      ENDIF.
    ENDLOOP.

    re_table = lt_table.

  ENDMETHOD.


  method GET_TABLE_BLOCKS.


  LOOP AT at_block_map INTO DATA(ls_block_map).
    IF ls_block_map-block->get_blocktype( ) EQ 'TABLE'.
      APPEND ls_block_map TO at_tab_block.
    ENDIF.
  ENDLOOP.

  endmethod.


  METHOD get_table_data.

    DATA lt_table TYPE tt_table.

    LOOP AT at_tab_block INTO DATA(ls_table_block).

      DATA(lv_tableid) = |table_{ sy-tabix }|.

      CLEAR: lt_table.
      get_rows_table_block(
        EXPORTING
          im_tableid =  lv_tableid
          im_block   = ls_table_block
        RECEIVING
          re_table   = lt_table
      ).

      APPEND LINES OF lt_table TO re_data.

    ENDLOOP.

  ENDMETHOD.


  METHOD get_text_block.

    DATA(lt_relationships) = im_block-block->get_relationships( ).

    LOOP AT lt_relationships INTO DATA(ls_relationship).
      IF ls_relationship->get_type( ) EQ 'CHILD'.
        LOOP AT ls_relationship->get_ids( ) INTO DATA(ls_id).
          READ TABLE at_block_map WITH KEY block_id = ls_id->get_value( )
                                 INTO DATA(ls_word_block).
          IF sy-subrc = 0.
            CASE ls_word_block-block->get_blocktype( ).

              WHEN 'WORD'.
                DATA(lv_text) = ls_word_block-block->get_text( ).
                CONCATENATE re_value lv_text INTO re_value SEPARATED BY space.
              WHEN 'SELECTION_ELEMENT'.
                IF ls_word_block-block->get_selectionstatus( ) EQ
                'SELECTED'.

                  CONCATENATE re_value 'X ' INTO re_value.
                ENDIF.
              WHEN OTHERS.
            ENDCASE.
          ENDIF.
        ENDLOOP.
      ENDIF.
    ENDLOOP.


  ENDMETHOD.


  method GET_VALUEBLOCK_FOR_KEY.

  DATA(lt_relationships) = im_key_block-block->get_relationships( ).


  LOOP AT lt_relationships INTO DATA(ls_relationship).
    IF ls_relationship->get_type( ) EQ 'VALUE'.
      LOOP AT ls_relationship->get_ids( ) INTO DATA(ls_id).
        READ TABLE at_val_block WITH KEY block_id = ls_id->get_value( )
                                INTO re_val_block.
      ENDLOOP.
    ENDIF.
  ENDLOOP.

  endmethod.


  method GET_WORDS.

    LOOP AT at_block_map INTO DATA(ls_block_map).

      IF ls_block_map-block->get_blocktype( ) EQ 'WORD'.

        re_data = VALUE #( BASE re_data (
        page = ls_block_map-block->get_page( )
        word = ls_block_map-block->get_text( ) ) ).
      ENDIF.

    ENDLOOP.


  endmethod.


  METHOD set_feature_types.

    CLEAR: at_featuretypes.

    IF im_tt_features IS INITIAL.
      at_featuretypes = VALUE #( (  NEW /aws1/cl_texfeaturetypes_w( iv_value = 'FORMS'  ) )
                                 (  NEW /aws1/cl_texfeaturetypes_w( iv_value = 'TABLES' ) )
                                ).
    ELSE.
      LOOP AT im_tt_features INTO DATA(ls_feature).
        at_featuretypes = VALUE #( BASE at_featuretypes
                                  ( NEW /aws1/cl_texfeaturetypes_w( iv_value = ls_feature ) )
                                ).
      ENDLOOP.
    ENDIF.

  ENDMETHOD.


  METHOD analyze_expense_asynch.

    DATA(lo_s3object) = NEW /aws1/cl_texs3object( iv_bucket = iv_bucket
                                                iv_name   = iv_key  ).

    gv_bucket = iv_bucket.
    gv_key    = iv_key.

    DATA(lo_documentlocation) = NEW /aws1/cl_texdocumentlocation( io_s3object = lo_s3object ).

* Start document analysis
    re_jobid = go_textract->startexpenseanalysis(
         EXPORTING
           io_documentlocation    =  lo_documentlocation
       )->get_jobid( ).


  ENDMETHOD.


  METHOD get_expense_documents.


    IF ( go_textract->getexpenseanalysis( EXPORTING
            iv_jobid = im_jobid )->has_expensedocuments( ) = abap_true ).

      at_documents = go_textract->getexpenseanalysis(
                     EXPORTING iv_jobid = im_jobid )->get_expensedocuments( ).

      IF go_textract->getexpenseanalysis(
                                        EXPORTING
                                        iv_jobid = im_jobid
                                        )->has_nexttoken( ) = abap_true.

        DATA(lv_token) = go_textract->getexpenseanalysis(
                                         EXPORTING iv_jobid = im_jobid
                                         )->get_nexttoken( ).
      ENDIF.

      WHILE lv_token IS NOT INITIAL.
        DATA(lt_next_documents) =   go_textract->getexpenseanalysis(
                                      EXPORTING iv_jobid = im_jobid
                                                iv_nexttoken = lv_token
                                                )->get_expensedocuments(  ).
        APPEND LINES OF lt_next_documents TO at_documents.

        IF go_textract->getexpenseanalysis(
                                        EXPORTING
                                        iv_jobid = im_jobid
                                        iv_nexttoken = lv_token
                                        )->has_nexttoken( ) = abap_true.

          lv_token = go_textract->getexpenseanalysis(
                                       EXPORTING
                                        iv_jobid = im_jobid
                                        iv_nexttoken = lv_token
                                        )->get_nexttoken( ).
        ELSE.

          EXIT.
        ENDIF.
      ENDWHILE.
    ENDIF.

  ENDMETHOD.


  METHOD get_expense_items.

    DATA: lv_value         TYPE string.
    DATA: lv_score         TYPE /aws1/texpercent.
    DATA: lv_avg_count     TYPE i.


    DATA(lt_line_items) = VALUE /aws1/cl_texlineitemfields=>tt_lineitemlist(
                    FOR lo_itemgroup  IN io_expense->get_lineitemgroups( )
                    FOR lo_item   IN lo_itemgroup->get_lineitems( )
                      ( lo_item ) ) .


    LOOP AT lt_line_items INTO DATA(lo_lineitem).
      CLEAR: lv_avg_count.

      APPEND INITIAL LINE TO rt_items ASSIGNING FIELD-SYMBOL(<fs_item>).
      <fs_item>-posnr = lines( rt_items ).


      LOOP AT lo_lineitem->get_lineitemexpensefields( ) INTO DATA(lo_exp_item).

        CLEAR: lv_value, lv_score.
        lv_value = lo_exp_item->get_valuedetection( )->get_text( ).
        lv_score = lo_exp_item->get_valuedetection( )->get_confidence( ).

        IF gv_min_score > 0
          AND lv_score < gv_min_score.

          CONTINUE.
        ENDIF.

        CASE lo_exp_item->get_type( )->get_text( ).
          WHEN 'ITEM'.
            <fs_item>-description  = lv_value.
            <fs_item>-confidence_score = <fs_item>-confidence_score + lv_score.
            lv_avg_count = lv_avg_count + 1.
          WHEN 'PRICE'.          " total
            REPLACE ALL OCCURRENCES OF REGEX  '[^\d.]' IN lv_value WITH space.
            CONDENSE lv_value.
            <fs_item>-price = CONV bapidoccur( lv_value ).
            <fs_item>-confidence_score = <fs_item>-confidence_score + lv_score.
            lv_avg_count = lv_avg_count + 1.
          WHEN 'UNIT_PRICE'.
            REPLACE ALL OCCURRENCES OF REGEX  '[^\d.]' IN lv_value WITH space.
            CONDENSE lv_value.
            <fs_item>-unit_price = CONV bapidoccur( lv_value )..
            <fs_item>-confidence_score = <fs_item>-confidence_score + lv_score.
            lv_avg_count = lv_avg_count + 1.
          WHEN 'QUANTITY'.
            <fs_item>-quantity = lv_value.
            <fs_item>-confidence_score = <fs_item>-confidence_score + lv_score.
            lv_avg_count = lv_avg_count + 1.
          WHEN 'PRODUCT_CODE'.
            <fs_item>-product_code = lv_value.
            <fs_item>-confidence_score = <fs_item>-confidence_score + lv_score.
            lv_avg_count = lv_avg_count + 1.
          WHEN  OTHERS.
* custom type handling
        ENDCASE.
      ENDLOOP.
* calculate the overall confidence score for this line
      IF <fs_item>-confidence_score IS NOT INITIAL.
        <fs_item>-confidence_score =  <fs_item>-confidence_score / lv_avg_count.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.


  METHOD get_expense_items_multipage.

    DATA: lv_value         TYPE string.
    DATA: lv_score         TYPE /aws1/texpercent.
    DATA: lv_avg_count     TYPE i.


    DATA(lt_line_items) = VALUE /aws1/cl_texlineitemfields=>tt_lineitemlist(
                    FOR lo_expense    IN at_documents
                    FOR lo_itemgroup  IN lo_expense->get_lineitemgroups( )
                    FOR lo_item   IN lo_itemgroup->get_lineitems( )
                      ( lo_item ) ) .


    LOOP AT lt_line_items INTO DATA(lo_lineitem).
      CLEAR: lv_avg_count.

      APPEND INITIAL LINE TO rt_items ASSIGNING FIELD-SYMBOL(<fs_item>).
      <fs_item>-posnr = lines( rt_items ).


      LOOP AT lo_lineitem->get_lineitemexpensefields( ) INTO DATA(lo_exp_item).

        CLEAR: lv_value, lv_score.
        lv_value = lo_exp_item->get_valuedetection( )->get_text( ).
        lv_score = lo_exp_item->get_valuedetection( )->get_confidence( ).

        IF gv_min_score > 0
          and lv_score < gv_min_score.

          CONTINUE.
        ENDIF.

        CASE lo_exp_item->get_type( )->get_text( ).
          WHEN 'ITEM'.
            <fs_item>-description  = lv_value.
            <fs_item>-confidence_score = <fs_item>-confidence_score + lv_score.
            lv_avg_count = lv_avg_count + 1.
          WHEN 'PRICE'.          " total
            REPLACE ALL OCCURRENCES OF REGEX  '[^\d.]' IN lv_value WITH space.
            CONDENSE lv_value.
            <fs_item>-price = CONV bapidoccur( lv_value ).
            <fs_item>-confidence_score = <fs_item>-confidence_score + lv_score.
            lv_avg_count = lv_avg_count + 1.
          WHEN 'UNIT_PRICE'.
            REPLACE ALL OCCURRENCES OF REGEX  '[^\d.]' IN lv_value WITH space.
            CONDENSE lv_value.
            <fs_item>-unit_price = CONV bapidoccur( lv_value )..
            <fs_item>-confidence_score = <fs_item>-confidence_score + lv_score.
            lv_avg_count = lv_avg_count + 1.
          WHEN 'QUANTITY'.
            <fs_item>-quantity = lv_value.
            <fs_item>-confidence_score = <fs_item>-confidence_score + lv_score.
            lv_avg_count = lv_avg_count + 1.
          WHEN 'PRODUCT_CODE'.
            <fs_item>-product_code = lv_value.
            <fs_item>-confidence_score = <fs_item>-confidence_score + lv_score.
            lv_avg_count = lv_avg_count + 1.
          WHEN  OTHERS.
* custom type handling
        ENDCASE.
      ENDLOOP.
* calculate the overall confidence score for this line
      IF <fs_item>-confidence_score IS NOT INITIAL.
        <fs_item>-confidence_score =  <fs_item>-confidence_score / lv_avg_count.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.


  METHOD get_expense_results_asynch.

    SELECT SINGLE low from tvarv INTO @DATA(lv_seconds)
                      WHERE name = 'TEXTRACT_ASYNCH_WAIT'.
    IF sy-subrc <> 0.
      lv_seconds = 2.
    ENDIF.

    WHILE go_textract->getexpenseanalysis( EXPORTING iv_jobid = im_jobid )->get_jobstatus( ) <> 'SUCCEEDED'.

      WAIT UP TO lv_seconds SECONDS.

      IF sy-index = 10.
        DATA(lv_status) = go_textract->getexpenseanalysis( EXPORTING
                             iv_jobid = im_jobid )->get_jobstatus( ).

        EXIT.
      ENDIF.
    ENDWHILE.

    IF lv_status IS NOT INITIAL AND lv_status <> co_status.
      re_status = lv_status.
      EXIT.
    ENDIF.

    get_expense_documents( im_jobid ).


* get summary
    DATA(lt_expense_summary) = me->get_expense_summary_multipage( ).
    DATA(lt_expense_items)   = me->get_expense_items_multipage( ).

    CONVERT DATE sy-datum TIME sy-uzeit
            INTO TIME STAMP DATA(lv_time_stamp)
            TIME ZONE 'UTC'.


* get expenses response
    rt_expenses = VALUE #( BASE rt_expenses ( expense_summary = lt_expense_summary
                                            expense_items   = lt_expense_items
                                            bucket          = gv_bucket
                                            bucket_key      = gv_key
                                            aedtm           = lv_time_stamp
                                            aenam           = sy-uname
                                            expense_index   = 1

                                            ) ).


    re_status = go_textract->getexpenseanalysis( EXPORTING iv_jobid = im_jobid )->get_jobstatus( ).

  ENDMETHOD.


  METHOD get_expense_summary.

    DATA lv_currency TYPE string.

    DATA(lt_exp_summary) = VALUE /aws1/cl_texexpensefield=>tt_expensefieldlist(
                           FOR lo_summary    IN io_expense->get_summaryfields( )
                           ( lo_summary ) ).

    LOOP AT lt_exp_summary INTO DATA(lo_exp_summary).

      DATA(lo_currency) = lo_exp_summary->get_currency( ).

      IF lo_currency IS BOUND.
        IF lo_currency->has_code( ).
          lv_currency = lo_currency->get_code( ).
        ENDIF.
      ENDIF.

      rt_expense_summary = VALUE #( BASE rt_expense_summary (
                                    expense_type = lo_exp_summary->get_type( )->get_text( )
                                    value = lo_exp_summary->get_valuedetection( )->get_text( )
                                    score = lo_exp_summary->get_valuedetection( )->get_confidence( )
                                     page = lo_exp_summary->get_pagenumber( )
                                     currency = lv_currency
                                             ) ).
    ENDLOOP.

    IF gv_min_score > 0.
      DELETE rt_expense_summary WHERE score < gv_min_score.
    ENDIF.

  ENDMETHOD.


  METHOD get_expense_summary_multipage.

    DATA lv_currency TYPE string.

    DATA(lt_exp_summary) = VALUE /aws1/cl_texexpensefield=>tt_expensefieldlist(
                           FOR lo_expense_doc  IN at_documents
                           FOR lo_summary    IN lo_expense_doc->get_summaryfields( )
                           ( lo_summary ) ).

    LOOP AT lt_exp_summary INTO DATA(lo_exp_summary).

      DATA(lo_currency) = lo_exp_summary->get_currency( ).

      IF lo_currency IS BOUND.
        IF lo_currency->has_code( ).
          lv_currency = lo_currency->get_code( ).
        ENDIF.
      ENDIF.

      rt_expense_summary = VALUE #( BASE rt_expense_summary (
                                    expense_type = lo_exp_summary->get_type( )->get_text( )
                                    value = lo_exp_summary->get_valuedetection( )->get_text( )
                                    score = lo_exp_summary->get_valuedetection( )->get_confidence( )
                                     page = lo_exp_summary->get_pagenumber( )
                                     currency = lv_currency
                                             ) ).
    ENDLOOP.

    IF gv_min_score > 0.
      DELETE rt_expense_summary WHERE score < gv_min_score.
    ENDIF.

  ENDMETHOD.


  METHOD analyze_expense.

*-------------------------------------------------------------------------------*
    DATA(lo_s3object) = NEW /aws1/cl_texs3object( iv_bucket = iv_bucket
                                                  iv_name   = iv_key  ).

    DATA(lo_document) = NEW /aws1/cl_texdocument( io_s3object = lo_s3object ).

    TRY.
        DATA(lo_expresp) =  go_textract->analyzeexpense(
            EXPORTING
              io_document    =  lo_document
          ).
      CATCH /aws1/cx_texunsupporteddocex INTO DATA(lo_ex).

    ENDTRY.

    IF lo_expresp IS BOUND.

      LOOP AT lo_expresp->get_expensedocuments( ) INTO DATA(lo_expensedoc).
* get summary
        DATA(lt_expense_summary) = me->get_expense_summary( io_expense = lo_expensedoc ).
        DATA(lt_expense_items)   = me->get_expense_items( io_expense =  lo_expensedoc ).

        CONVERT DATE sy-datum TIME sy-uzeit
                INTO TIME STAMP DATA(lv_time_stamp)
                TIME ZONE 'UTC'.


* get expenses response
        rt_expense = VALUE #( BASE rt_expense ( expense_summary = lt_expense_summary
                                                expense_items   = lt_expense_items
                                                bucket          = iv_bucket
                                                bucket_key      = iv_key
                                                aedtm           = lv_time_stamp
                                                aenam           = sy-uname
                                                expense_index   = lo_expensedoc->get_expenseindex( )

                                                ) ).
      ENDLOOP.

    ENDIF.

  ENDMETHOD.


  METHOD set_expitem_fields_types.


    at_expitem_types = VALUE #( ( 'ITEM' )
                                ( 'QUANTITY' )
                                ( 'PRICE' )
                                ( 'UNIT_PRICE' )
                                ( 'PRODUCT_CODE' )
                                ( 'OTHER' )
                               ).
  ENDMETHOD.


  METHOD set_expsummary_field_types.

    at_expsummary_types = VALUE #( ( 'ACCOUNT_NUMBER' )
                                   ( 'VENDOR_NAME'    )
                                   ( 'INVOICE_RECEIPT_ID' )
                                   ( 'INVOICE_RECEIPT_DATE' )
                                   ( 'VENDOR_ADDRESS' )
                                   ( 'DUE_DATE'      )
                                   ( 'PO_NUMBER' )
                                   ( 'TOTAL'    )
                                   ( 'TAX'    )
                                   ( 'DISCOUNT' )
                                   ( 'DUE_DATE' )
                                   ( 'OTHER' ) ).

  ENDMETHOD.


  method SET_MIN_SCORE.

    gv_min_score = iv_min_score.
  endmethod.
ENDCLASS.
