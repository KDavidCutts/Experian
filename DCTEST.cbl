       IDENTIFICATION DIVISION.
       PROGRAM-ID. DCTEST.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       77 WS-AMOUNT 				PIC 9(5) VALUE 0.
       77 WS-PRICE  				PIC 9(5) VALUE 0.

       01 WS-WORKING-AMOUNTS.
           03 WS-CHANGE           	PIC S9(5) VALUE 0.
           03 WS-CHANGE-REMAINDER 	PIC S9(5) VALUE 0.
           03 WS-SUB1             	PIC 99    VALUE 0.
		   03 WS-VALUE-1          	PIC S9(8) VALUE 0.
		   
       01 WS-DISP-STRINGS.
           03 WS-DISP-1        		PIC X(15) VALUE "Your change is:".
           03 WS-DISP-2.
              05 FILLER        		PIC X(7) VALUE "       ".
              05 WS-DISP-NUM   		PIC X.
              05 FILLER        		PIC X(3) VALUE " X ".
              05 WS-DISP-MONEY 		PIC X(4).

       01 WS-CHANGE-BREAKDOWN.
           03 WS-NUM-50N   			PIC 9 VALUE 0.
		   03 WS-NUM-20N   			PIC 9 VALUE 0.
           03 WS-NUM-10N   			PIC 9 VALUE 0.
           03 WS-NUM-5N    			PIC 9 VALUE 0.
           03 WS-NUM-2N    			PIC 9 VALUE 0.
           03 WS-NUM-1N    			PIC 9 VALUE 0.
           03 WS-NUM-50P   			PIC 9 VALUE 0.
           03 WS-NUM-20P   			PIC 9 VALUE 0.
           03 WS-NUM-10P   			PIC 9 VALUE 0.
           03 WS-NUM-5P    			PIC 9 VALUE 0.
           03 WS-NUM-2P    			PIC 9 VALUE 0.
           03 WS-NUM-1P    			PIC 9 VALUE 0.
 
       01 WS-CHANGE-BREAKDOWN-TAB REDEFINES WS-CHANGE-BREAKDOWN.
          03 WS-CHANGE-NUM 			PIC 9 OCCURS 12.

       01 WS-MONEY-DISP-STRINGS.
		   03 WS-50N           		PIC X(4) VALUE "£50".
           03 WS-20N    		    PIC X(4) VALUE "£20".
           03 WS-10N   		        PIC X(4) VALUE "£10".
           03 WS-5N     		    PIC X(4) VALUE "£5 ".
           03 WS-2N            		PIC X(4) VALUE "£2 ".
           03 WS-1N            		PIC X(4) VALUE "£1 ".
           03 WS-50P           		PIC X(4) VALUE "50P".
           03 WS-20P           		PIC X(4) VALUE "20P".
           03 WS-10P           		PIC X(4) VALUE "10P".
           03 WS-5P            		PIC X(4) VALUE "5P ".
           03 WS-2P            		PIC X(4) VALUE "2P ".
           03 WS-1P            		PIC X(4) VALUE "1P ".

       01 WS-MONEY-DISP-TAB REDEFINES WS-MONEY-DISP-STRINGS.
           03 WS-MONEY-DISP    		PIC X(4) OCCURS 12.
           
       PROCEDURE DIVISION.
       MAIN-PARA.
            MOVE ZEROES TO 	WS-WORKING-AMOUNTS
							WS-CHANGE-BREAKDOWN.
							
            DISPLAY "ENTER AMOUNT TENDERED IN PENCE E.G. £20 AS 2000 :".
            ACCEPT WS-AMOUNT

            IF WS-AMOUNT> 50000
                DISPLAY "ERROR:MAX AMOUNT OF £500"
                STOP RUN
            END-IF.
            
            DISPLAY "ENTER PRICE IN PENCE E.G. £17.49 AS 1749 :".
            ACCEPT WS-PRICE.
       
       CHANGE-PARA.

            SUBTRACT WS-PRICE FROM WS-AMOUNT GIVING WS-CHANGE.

            IF WS-CHANGE  = 0
                DISPLAY "NO CHANGE"
                STOP RUN
            ELSE
                IF WS-CHANGE < 0
                    DISPLAY "ERROR:PRICE GEATER THAN AMOUNT TENDERED"
                    STOP RUN
            END-IF.

            DIVIDE WS-CHANGE BY 5000 GIVING WS-NUM-50N REMAINDER WS-VALUE-1.

			MOVE WS-VALUE-1 TO WS-CHANGE-REMAINDER.	
			
			DIVIDE WS-CHANGE-REMAINDER BY 2000 GIVING WS-NUM-20N REMAINDER WS-VALUE-1.
			
			MOVE WS-VALUE-1 TO WS-CHANGE-REMAINDER.	

            DIVIDE WS-CHANGE-REMAINDER BY 1000 GIVING WS-NUM-10N REMAINDER WS-VALUE-1.
           
			MOVE WS-VALUE-1 TO WS-CHANGE-REMAINDER.
	
			DIVIDE WS-CHANGE-REMAINDER BY 500 GIVING WS-NUM-5N REMAINDER WS-VALUE-1.
           
			MOVE WS-VALUE-1 TO WS-CHANGE-REMAINDER.

            DIVIDE WS-CHANGE-REMAINDER BY 200 GIVING WS-NUM-2N REMAINDER WS-VALUE-1.
           
			MOVE WS-VALUE-1 TO WS-CHANGE-REMAINDER.
			
			DIVIDE WS-CHANGE-REMAINDER BY 100 GIVING WS-NUM-1N REMAINDER WS-VALUE-1.
           
			MOVE WS-VALUE-1 TO WS-CHANGE-REMAINDER.
			
			DIVIDE WS-CHANGE-REMAINDER BY 50 GIVING WS-NUM-50P REMAINDER WS-VALUE-1.
           
			MOVE WS-VALUE-1 TO WS-CHANGE-REMAINDER.
			
			DIVIDE WS-CHANGE-REMAINDER BY 20 GIVING WS-NUM-20P REMAINDER WS-VALUE-1.
           
			MOVE WS-VALUE-1 TO WS-CHANGE-REMAINDER.
			
			DIVIDE WS-CHANGE-REMAINDER BY 10 GIVING WS-NUM-10P REMAINDER WS-VALUE-1.
           
			MOVE WS-VALUE-1 TO WS-CHANGE-REMAINDER.
			
			DIVIDE WS-CHANGE-REMAINDER BY 5 GIVING WS-NUM-5P REMAINDER WS-VALUE-1.
           
			MOVE WS-VALUE-1 TO WS-CHANGE-REMAINDER.
			
			DIVIDE WS-CHANGE-REMAINDER BY 2 GIVING WS-NUM-2P REMAINDER WS-VALUE-1.
           
			MOVE WS-VALUE-1 TO WS-NUM-1P.
			
        DISP-PARA.
            
            DISPLAY "Your change is:"
            
            MOVE 1 TO WS-SUB1.
            
            PERFORM UNTIL WS-SUB1 > 12
                IF WS-CHANGE-NUM(WS-SUB1) > 0
                    MOVE WS-CHANGE-NUM(WS-SUB1) TO WS-DISP-NUM
                    MOVE WS-MONEY-DISP(WS-SUB1) TO WS-DISP-MONEY
                    DISPLAY WS-DISP-2
                END-IF
                ADD 1 TO WS-SUB1
            END-PERFORM.

            STOP RUN.