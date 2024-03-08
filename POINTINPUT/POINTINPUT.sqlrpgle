*FREE
       // * Control Options Section
       Ctl-opt Option(*srcStmt: *nodebugio);
       // *****************************************************************
       // This is program is the driver for the POINTINPUT.DSPF file.     *
       //                                                                 *
       // It uses embedded SQL to access student data contained in WUSTDP *
       // using the student number (STUNO) entered by the user.           *
       //                                                                 *
       // On a successful retrieval, the user is prompted to enter an     *
       // assignment description, the points the student received, and    *
       // the total points possible for the assignment.                   *
       //                                                                 *
       // The values entered by the user are written to the POINTSP file, *
       // and the program loops back to the STUNO entry screen.           *
       //                                                                 *
       // The entry loop continues until the user exits the program using *
       // the F3 key.                                                     *
       //                                                                 *
       // NOTE: SQL JOURNALING MUST BE ENABLED FOR THE FILE FOR THE SQL   *
       // INSERT STATEMENTS TO WORK CORRECTLY.                            *
       //                                                                 *
       // Written by Justin Becker on 01/24/2024                          *
       // *****************************************************************

       //*********** DECLARE FILES ***********//
       dcl-f POINTINPUT Workstn Indds(Indicators);

       //*********** DECLARE VARIABLES ***********//
       dcl-s STUNOIN zoned(9:0) inz;    // student num input
       dcl-s HWDESCIN char(20) inz;     // homework description input
       dcl-s PTSRCVIN zoned(3:0) inz;   // points received input
       dcl-s PTSPOSIN zoned(3:0) inz;   // points possible input
       dcl-s FULLNAME char(26) inz;     // holds concatenated student name
       dcl-s IsValid ind inz(*OFF);     // boolean for input validation
       dcl-s PERCENTAGE char(6) inz;    // holds grade percentage string
       dcl-s PERCENTNUM zoned(3:2) inz; // holds the calculated percentage
       dcl-s PASSORFAIL char(14) inz;   // shows passorfail message

       //*********** DECLARE DATA STRUCTURES ***********//

       // indicators
       dcl-ds Indicators Len(99);
         Exit Ind Pos(3);
         InvalidID Ind Pos(90);
         OutOfRange Ind Pos(95);
       END-DS;

       // host ds for sql results
       dcl-ds StudentData;
        STUSSN zoned(9:0);
        FirstName char(10);
        LastName char(15);
       END-DS;

       //*********** MAIN ***********//
       DoW not Exit; // continue loop until user presses F3

         Exfmt SCRN1; // show Student ID entry screen

         if not Exit; // continue to screen 2 if user has not pressed F3

           // select fields from WUSTDP into DS
           Exec SQL Select STUSSN, SFNAME, SLNAME
             into :StudentData
             from WUSTDP
             where STUSSN = :STUNOIN;

             // handle if STUSSN found
             if SQLCODE >= 0 and SQLCODE < 100;

               exsr BuildName; // construct full name

               // clear fields, but leave Points Possible populated
               // with previous entry to expedite data entry
               HWDESCIN = '';
               PTSRCVIN = 0;


               Exfmt SCRN2; // display screen 2

               exsr ProcessEntries; // process user entries from screen 2

               if IsValid;

                 exsr AddRecord; // add the record to MYPOINTSP

                 if not exit;

                   Exfmt SCRN3; // display screen 3

                 ENDIF; // end if not exit

               ENDIF; // end if IsValid

             // handle if STUSSN not found
             elseif SQLCODE < 0 or SQLCODE = 100;

               InvalidID = *ON;

             ENDIF; // end if SQLCODE

         endif; // end if not exit

       enddo; // end DOW NOT EXIT

       *inlr = *on;

       return; // end program

       //*********** SUBROUTINES ***********//

       // inserts new row in MYPOINTSP for assignment
       begsr AddRecord;

       Exec SQL insert
         into MYPOINTSP (STUNO, ASSIGNDESC, PTSRECEIVE, PTSPOSSIBL)
         values (:STUNOIN, :HWDESCIN, :PTSRCVIN, :PTSPOSIN);

       ENDSR;

       // concatenates first and last name
       begsr BuildName;

         FULLNAME = %TRIM(FirstName) + ' ' + %TRIM(LastName);

       ENDSR;

       // validates input, calculates percentage for assignment,
       // and displays either a pass or fail message
       begsr ProcessEntries;

         IsValid = *OFF;

         // verify Points Possible is greater than 0
         DOW not IsValid and not exit;

           if PTSPOSIN <= 0;

             OutOfRange = *ON; // display error message

             exfmt SCRN2; // reload screen

           else;

             OutOfRange = *OFF; // turn error indicator off if already on

             IsValid = *ON; // exit loop if entry is valid

           ENDIF;

         ENDDO;

         if not exit; // only execute expressions if user has not tried to exit

           // calculate grade percentage and assign pass or fail
           PERCENTNUM = PTSRCVIN / PTSPOSIN;

           // convert to string to format output
           PERCENTAGE = %char( %dec(PERCENTNUM * 100 : 3:0) ) + '%';

           if PERCENTNUM >= 0.6;

             PASSORFAIL = 'Student Passed';

           else;

             PASSORFAIL = 'Student Failed';

           ENDIF; // end if PERCENTNUM

         ENDIF; // end if not exit

       ENDSR; 