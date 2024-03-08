*FREE
       // * Control Options Section
       Ctl-opt Option(*srcStmt: *nodebugio);
       // *****************************************************************
       // This program reads the grades from the GRADEBOOK file and       *
       // assigns letter grades based on their values. It also calculates *
       // an average of all student grades, and generates a report.       *
       // Written by Justin Becker on 01/24/2024                          *
       // *****************************************************************

       //*** Declare Files ***//
       dcl-f GRADEBOOK disk keyed usage(*input);
       dcl-f WEXPRTF printer usage(*output) oflind(EndOfPage); // need to design still

       //*** Declare Variables ***//
       dcl-s FullName char(40) inz;
       dcl-s AvgGrade zoned(4:0) inz;
       dcl-s LetGrade char(2) inz;  // stores the assigned letter grade
       dcl-s ScoreString char(75) inz; // stores string from GRADEBOOK SCORES field
       dcl-s StringPosition int(3) inz; // holds position in ScoreString
       dcl-s TempScore zoned(3:0) inz;  // holds each score before storing it in array
       dcl-s TempString char(3) inz; // holds a score string that is converted to dec
       dcl-s index int(3) inz;
       dcl-s terminator int(3) inz;
       dcl-s EndOfPage ind inz(*off);

       //*** Declare Data Structures and Arrays ***//
       dcl-s ScoresArray zoned(3:0) DIM(25); // holds each score for a student
       dcl-s GradeMap zoned(2:0) DIM(12) Ctdata descend;
       dcl-s Letters char(2) DIM(12) Alt(GradeMap);

       //*** Main ***//

         Read GRADEBOOK; // read the first record

         Write Header;

         DOW not %EOF(GRADEBOOK);

           EXSR GetScores; // store the student's scores in a runtime array

           EXSR AverageGrades; // average the student's grades

           EXSR AssignGradeLetter; // assign the letter grade based on the average

           EXSR BuildName; // concatenate student first and last

           IF EndOfPage;      // check for end of page
                              // and write header if needed
             Write Header;

             EndOfPage = *off;

           ENDIF;

           Write Detail; // write record to report

           Read GRADEBOOK; // read the next student's info

         ENDDO;

       *INLR = *on;

       Return; // end program

       //*** Subroutines ***//

       BEGSR GetScores;

         ScoreString = SCORES; // Scores is externally-described

         StringPosition = 1; // reset position for each student loop

         terminator = (%len(ScoreString) / 3); // loop runs 25 times

         // advances through the string and extracts 3-integer
         // values which are each stored in an array element
         FOR index = 1 to terminator by 1;

           TempString = %subst(ScoreString : StringPosition : 3);

           TempScore = %dec(TempString : 3 : 0);

           ScoresArray(index) = TempScore; // store score in array

           StringPosition += 3; // advance to next score in string

         ENDFOR;

       ENDSR;


       BEGSR AverageGrades;

         // use %XFOOT to sum each student's grades
         AvgGrade = %XFOOT(ScoresArray);

         // then divide by the length of the array to get the average grade
         eval(h) AvgGrade = AvgGrade / %Elem(ScoresArray);

       ENDSR;

       BEGSR AssignGradeLetter;

         // match the average grade with the lowest possible score
         // that will earn a letter grade
         index = %LookUpLe(AvgGrade : GradeMap);

         IF index <> 0;

           // assign the letter grade which corresponds to the index
           LetGrade = Letters(index);

         ELSE;

           // if the score is less than 59, the lookup will return 0
           // so assign the lowest score
           LetGrade = Letters(12);

         ENDIF;

       ENDSR;

       BEGSR BuildName;

         // assemble the full student name for report
         FullName = %TRIM(SFNAME) + ' ' + %TRIM(SLNAME);

       ENDSR;

       //*** **CTData Values ***//
**CTDATA GradeMap
93A
90A-
87B+
83B
80B-
77C+
73C
70C-
67D+
63D
60D-
59E 