*FREE
       // Control Options
       Ctl-opt Option(*srcStmt: *nodebugio);
       // *****************************************************************
       // This program is the driver for WIBUNIPRO6.DSPF                  *
       //                                                                 *
       // It chains to the MYINSTP data file to access instructor records *
       // for Wibaux University, using instructor SSN as a primary key.   *
       //                                                                 *
       // Users are able to update, delete, or add a record depending on  *
       // what selection they make from the main menu screen.             *
       //                                                                 *
       // Written by Justin Becker on 02/21/2024                          *
       // *****************************************************************

       // Declare files
       dcl-f MYINSTP disk keyed usage(*update: *output: *delete); // instructor file
       dcl-f WIBUNIPRO6 Workstn indds(indicators); // display file

       // Indicator DS
       dcl-ds indicators Len(99);
         Exit ind pos(3);            // F3
         Cancel ind pos(12);         // F12
         RecordAlreadyExists ind pos(30);  // Redirects invalid actions with displayed messages
         RecordNotFound ind pos(31);       // Redirects invalid actions with displayed messages
         ActionCodeEmpty ind pos(32);      // Redirects invalid actions with displayed messages
         DateError ind pos(33);            // Redirects invalid actions with displayed messages
         MARSTSError ind pos(34);          // Redirects invalid actions with displayed messages
         ActionCodeInvalid ind pos(35);    // Redirects invalid actions with displayed messages
         YesOrNoFieldInvalid ind pos(36);  // Redirects invalid actions with displayed messages
         SSNInvalid ind pos(37);           // Redirects invalid actions with displayed messages
         SexFieldInvalid ind pos(38);      // Redirects invalid actions with displayed messages
         AddMode ind pos(40);         // indicate which edit mode to swap to
         UpdateMode ind pos(41);      // indicate which edit mode to swap to
         DeleteMode ind pos(42);      // indicate which edit mode to swap to
         FKey ind pos(25);           // VLDCMDKEY
       end-ds;

       // Standalone variables
       dcl-s HIRDATIN date(*ISO) inz; // needed to convert HIRDAT
       dcl-s RANKIN zoned(1:0) inz;   // converts RANK to char
       dcl-s TITLEIN zoned(1:0) inz;  // converts TITLE to char
       dcl-s IsValid ind inz;         // flag to indicate input validation success


       // ------------- MAIN ------------- //

       DOU EXIT;

         EXFMT MAINMENU; // show main screen

         ACTIONCODE = %TRIM(ACTIONCODE);  // get action code entry

         SELECT; // determine what sr to use based on ACTIONCODE

           WHEN EXIT;

             LEAVE; // exit program when user presses <F3> on main screen

           WHEN ACTIONCODE = '';

             ActionCodeEmpty = *ON; // disallows an empty ACTIONCODE

           WHEN ACTIONCODE = 'A';

             EXSR AddRecord; // allows user to add a record

           WHEN ACTIONCODE = 'C';

             EXSR UpdateRecord; // opens an existing record for update

           WHEN ACTIONCODE = 'D';

             EXSR DeleteRecord; // deletes an existing record

           OTHER;

             ActionCodeInvalid = *ON; // only allow A, C, or D to be entered for ACTIONCODE

         ENDSL;

       ENDDO; // end do until EXIT

       *INLR = *ON;

       Return;

       // ------------ END MAIN ----------- //

       // Subroutines

       // ADD RECORD
       BEGSR AddRecord;

         EXSR SetMode;

         Chain(n) INSTNO MYINSTP; // search MYINSTP by SSN

         RecordAlreadyExists = %found(MYINSTP); // do not allow user to add duplicate INSTNO


         IF NOT RecordAlreadyExists; // only proceed if INSTNO doesn't exist already

           EXFMT INFOSCREEN; // show info entry screen

           IF Fkey; // If Fkey is not pressed

             RESET INSTNO; // reset search key if user cancels

           ELSE;

             EXSR ValidateEntries; // validate sex, marital status, and tenure fields

             IF IsValid; // only proceed if all fields are valid

               RANK = %CHAR(RANKIN);

               TITLE = %CHAR(TITLEIN);

               HIRDAT = %DEC(HIRDATIN);

               WRITE INSTREC;

               RESET ACTIONCODE;

             ENDIF; // end if IsValid

           ENDIF; // end if Fkey

         ENDIF; // end if not RecordAlreadyExists

         EXSR ResetFields; // clear all fields

       ENDSR; // end AddRecord

       // UPDATE RECORD
       BEGSR UpdateRecord;

         EXSR SetMode; // set mode to update

         Chain INSTNO MYINSTP; // search MYINSTP by SSN

         RecordNotFound = NOT %FOUND(MYINSTP); // can't update a record that doesn't exist

         IF NOT RecordNotFound; // only proceed if INSTNO does not exist

           HIRDATIN = %DATE(HIRDAT : *ISO); // convert date for display

           RANKIN = %DEC(RANK : 1:0); // convert rank for display

           TITLEIN = %DEC(TITLE : 1:0); // convert title for display

           EXFMT INFOSCREEN; // show info entry screen

             IF FKey; // If Fkey is pressed

               RESET INSTNO; // reset search key if user cancels

             ELSE;

               EXSR ValidateEntries; // validate sex, marital status, and tenure fields

               IF IsValid; // only proceed if all fields are valid

                 HIRDAT = %DEC(HIRDATIN);

                 RANK = %CHAR(RANKIN);

                 TITLE = %CHAR(TITLEIN);

                 UPDATE INSTREC; // write updates to file

                 RESET ACTIONCODE; // clear action code

               ENDIF; // end if IsValid

             ENDIF; // end if Fkey

         ENDIF; // end if not RecordNotFound

         EXSR ResetFields; // clear all fields

       ENDSR; // end UpdateRecord

       // DELETE MODE
       BEGSR DeleteRecord;

         EXSR SetMode; // set mode to delete

         Chain INSTNO MYINSTP; // search MYINSTP by SSN

         IF %FOUND(MYINSTP); // only proceed if INSTNO exists

           HIRDATIN = %DATE(HIRDAT : *ISO); // convert date for display

           RANKIN = %DEC(RANK : 1:0);

           TITLEIN = %DEC(TITLE : 1:0);

           EXFMT INFOSCREEN; // show info entry screen

           DOU EXIT OR CANCEL OR YNDELETE = 'Y'; // loop until user cancels or confirms delete

               IF FKey;

                 RESET INSTNO; // reset search key if user cancels

               ELSE;

                 EXFMT CONFIRMDLT; // show confirmation screen

                 SELECT;

                 WHEN CANCEL; // if user cancels, reset fields and exit

                   YNDELETE = '';

                   EXFMT INFOSCREEN;

                 WHEN YNDELETE = 'Y'; // if user confirms delete, delete record

                   DELETE INSTREC; // delete record

                   RESET ACTIONCODE; // clear action code

                   EXSR ResetFields; // clear all fields

                 WHEN YNDELETE = 'N'; // if user refuses delete, reset field and exit

                   YNDELETE = '';

                   EXFMT INFOSCREEN;

                 OTHER;

                   YesOrNoFieldInvalid = *ON; // only allow Y or N to be entered for YNDELETE

                 ENDSL; // end select

               ENDIF; // end if FKey

             ENDDO; // end do until EXIT or CANCEL or YNDELETE = 'Y'

         ELSE;

           RecordNotFound = *ON; // do not allow user to delete a record that doesn't exist

         ENDIF; // end if %FOUND(MYINSTP)

         EXSR ResetFields; // clear all fields

       ENDSR; // end DeleteRecord

       // SET MODE
       BEGSR SetMode; // this determines what options and messages are shown in DSPF

         AddMode    = (ACTIONCODE = 'A'); // set mode to add
         UpdateMode = (ACTIONCODE = 'C'); // set mode to update
         DeleteMode = (ACTIONCODE = 'D'); // set mode to delete

       ENDSR;

       BEGSR ValidateEntries; // verifies that input is within acceptable parameters

         // Check SEX
         DOU IsValid OR Fkey;

           IF %CHECK('MFmf' : SEX) > 0; // checks for any character other than M or F

             IsValid = *OFF; // if invalid, set flag to off for false
             SexFieldInvalid = *ON; // set indicator to show error message

             EXFMT INFOSCREEN; // show info entry screen

             ITER; // re-loop so user can correct entry

           ELSE;

             IsValid = *ON; // if valid, set flag to on for true
             SexFieldInvalid = *OFF; // set indicator to off to hide error message

           ENDIF; // end if %CHECK('MFmf' : SEX) > 0;

           // Check Marital Status
           IF %CHECK('SMDsmd' : MARSTS) > 0; // checks for any character other than S, M, or D

             IsValid = *OFF; // if invalid, set flag to off for false
             MARSTSError = *ON; // set indicator to show error message

             EXFMT INFOSCREEN; // show info entry screen

             ITER; // re-loop so user can correct entry

           ELSE;

             IsValid = *ON; // if valid, set flag to on for true
             MARSTSError = *OFF; // set indicator to off to hide error message

           ENDIF; // end if %CHECK('SMDsmd' : MARSTS) > 0

           // Check TENURE
           IF %CHECK('YNyn' : TENURE) > 0; // checks for any character other than Y or N

             IsValid = *OFF; // if invalid, set flag to off for false
             YesOrNoFieldInvalid = *ON; // set indicator to show error message

             EXFMT INFOSCREEN; // show info entry screen

             ITER; // re-loop so user can correct entry

           ELSE;

             IsValid = *ON; // if valid, set flag to on for true
             YesOrNoFieldInvalid = *OFF; // set indicator to off to hide error message

           ENDIF; // end if %CHECK('YNyn' : TENURE) > 0

         ENDDO; // end do until IsValid OR Fkey

       ENDSR; // end ValidateEntries

       // RESET FIELDS
       BEGSR ResetFields; // clears all fields and reinitializes HIRDATIN, RANKIN, and TITLEIN

         RESET INSTNO;
         RESET IFNAME;
         RESET ILNAME;
         RESET DEPT;
         RESET SALARY;
         RESET RANK;
         RESET SEX;
         RESET HIRDAT;
         RESET MARSTS;
         RESET DEPEND;
         RESET TENURE;
         RESET TITLE;

         HIRDATIN = *LOVAL;
         RANKIN = 0;
         TITLEIN = 0;

       ENDSR; // end ResetFields
 