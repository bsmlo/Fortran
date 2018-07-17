   
      SUBROUTINE WRITETOFILE( M, MM)
      IMPLICIT NONE
      CHARACTER(LEN=100) :: M, MM
      
      
      OPEN (unit = 15, file = "wyniki.txt")
      
      WRITE(15,*)
      WRITE(15,*) M
      WRITE(15,*) MM
      WRITE(15,*)
      
      CLOSE (unit = 15)
      
      WRITE(*,*)
      WRITE(*,*) 'Zapisano!!!'
      WRITE(*,*) '- Wcisnij by kontynuowac -'
      READ(*,*)
      WRITE(*,*)
      WRITE(*,*)
      
      RETURN
      END SUBROUTINE
      
      
      
      SUBROUTINE OBLICZ( A, B, C)
      IMPLICIT NONE
      
          INTEGER :: CHOSE
          REAL :: A, B, C, DELTA, X, XX
          CHARACTER(LEN=100) :: MESSAGE1, MESSAGE2
          
          DELTA = (B**2)-(4*A*C)
          
          WRITE(*,*)
          WRITE(*,*)       
          WRITE(*,*) !'DELTA = ', DELTA
          WRITE(*,*)
          
          IF(DELTA > 0) THEN
              X = (-B-(DELTA**(0.5)))/(2*A)
              XX = (-B+(DELTA**(0.5)))/(2*A)
              
              WRITE(MESSAGE1,*) 'ROWNANIE: ',A,'X^2+ ',B,'X+ ',C,'=0'
              WRITE(MESSAGE2,*) 'DELTA = ', DELTA, 'X1=',X, 'X2=', XX
              
              WRITE(*,*) MESSAGE1
              WRITE(*,*) MESSAGE2
              
              CONTINUE
              
          ELSE IF(DELTA == 0) THEN
              X = (-B-(DELTA**(0.5)))/(2*A)
              XX = X
              
              WRITE(MESSAGE1,*) 'ROWNANIE: ',A,'X^2+ ',B,'X+ ',C,'=0'
              WRITE(MESSAGE2,*) 'DELTA = ', DELTA, 'X1 = X2 =',X

              WRITE(*,*) MESSAGE1
              WRITE(*,*) MESSAGE2
              
              CONTINUE
              
          ELSE 
              WRITE(MESSAGE1,*) 'ROWNANIE: ',A,'X^2+ ',B,'X+ ',C,'=0'
              WRITE(MESSAGE2,*) 'DELTA = ', DELTA, 'Brak pierwiastkow'

              WRITE(*,*) MESSAGE1
              WRITE(*,*) MESSAGE2
              
              CONTINUE
          
          END IF
      
1         WRITE(*,*)
          WRITE(*,*) 'Czy zapisac do pliku?'
          WRITE(*,*)  '1  ---  TAK'
          WRITE(*,*)  '2  ---  NIE - wyjdz do menu'
          WRITE(*,*)
          READ(*,*) CHOSE
          WRITE(*,*)
          
          IF (CHOSE == 1) THEN
             CALL WRITETOFILE( MESSAGE1, MESSAGE2)
             
          ELSE IF (CHOSE == 2) THEN
             GOTO 10
             
          ELSE
              WRITE(*,*) 'BLAD' 
              GOTO 1  
                
          END IF
          
10    RETURN
      END SUBROUTINE
          
          
      PROGRAM ROWNANIA
      IMPLICIT NONE
      
      REAL :: A, B, C
      INTEGER :: CHOSE
      
      WRITE(*,*)  '*** OBLICZANIE PIERWIASTKOW ROWNANIA KWADRATOWEGO ****'
      WRITE(*,*)
      
      DO
          WRITE(*,*)  '**   MENU   **'
          WRITE(*,*)  '1  ---  WPROWADZ WSPOLCZYNNIKI'
          WRITE(*,*)  '2  ---  ZAMKNIJ PROGRAM'
        
          READ(*,*) CHOSE
        
          IF(CHOSE == 1) THEN
              WRITE(*,*)
              WRITE(*,*)  'PODAJ A, B, C, DLA ROWNANIA: AX^2 + BX + C = 0'
              READ(*,*) A, B, C
              CALL OBLICZ( A, B, C )
            
          ELSE IF (CHOSE == 2) THEN
              GOTO 100
            
          ELSE                                                                  
              WRITE(*,*)  'ZLE DANE!!! -- WCISNIJ ABY KONTYNUOWAC'
              READ(*,*)
              WRITE(*,*)
              WRITE(*,*)
              WRITE(*,*)
              WRITE(*,*)
          END IF
      END DO
      
      PAUSE
100   END PROGRAM
      