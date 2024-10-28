FUNCTION ZSEND_INVOICEDATA_S3.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(IE_FILENAME) TYPE  RLGRAP-FILENAME
*"     VALUE(IE_PROFILE) TYPE  /AWS1/RT_PROFILE_ID
*"----------------------------------------------------------------------

    DATA lv_body TYPE xstring.
*   DATA lv_file_name TYPE rlgrap-filename
*          value '/usr/sap/tmp/invoicedata/invoiceMaster_withheader.csv'.
*
* Instantiate the s3 helper
    DATA(lo_s3_helper) = NEW zcl_aws_s3_helper( im_profile = ie_profile ).

** Try creating if not exists
    TRY.
        lo_s3_helper->create_bucket(
             im_bucket = |invoicekb-{ to_lower( sy-uname ) }|
             ).
      CATCH /aws1/cx_s3_bucketalrdyexists.
       CATCH /aws1/cx_s3_bktalrdyownedbyyou.
    ENDTRY.

    SPLIT ie_filename AT '/' INTO TABLE DATA(lt_slug).
** read the sample csv data and write to s3
    OPEN DATASET ie_filename FOR INPUT IN BINARY MODE.
    IF sy-subrc = 0.
      READ DATASET ie_filename INTO lv_body.
      if sy-subrc = 0.

        lo_s3_helper->put_object( im_bucket = |invoicekb-{ to_lower( sy-uname ) }|
                          im_key    = lt_slug[ lines( lt_slug ) ]
                          im_data   = lv_body ).

          CLOSE DATASET ie_filename.
      endif.
    ENDIF.
ENDFUNCTION.
