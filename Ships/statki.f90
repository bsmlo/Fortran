subroutine shipsCount(aray, x, y, ships)
   INTEGER :: i, j, x, y, c, ships
   CHARACTER, DIMENSION(5, 5) :: aray
   c = 0
   
   do i=2, x
      do j=2, y
         IF(aray(i, j) == 'O') THEN
            c = c+1
            !print*, c
            continue
         ELSE
            c = c
            continue   
         END IF 
      end do      
   end do
   !print*, c
   ships = c
   !print*, ships

   RETURN 
end subroutine shipsCount


subroutine arrayPrint(aray, x, y)
   INTEGER :: x, y, i, j 
   CHARACTER, DIMENSION(x, y) :: aray
   
   do i=1, x
      do j=1, y
         write(*, '(A3,X)', advance='no') aray(i, j)
      end do
      write(*, *)
      write(*, *)
   end do

   RETURN
end subroutine arrayPrint


INTEGER FUNCTION Losowanie()
IMPLICIT NONE
    INTEGER, DIMENSION (1, 2) :: ship      
    INTEGER :: x, y
    
      x = (rand()+1)*1000000000/1
      x = MOD(x, 3)+2

      Losowanie = x 
    
END FUNCTION Losowanie


!************* MAIN *************!
    
program statki


IMPLICIT NONE
INTEGER, external :: Losowanie 
CHARACTER, DIMENSION(5, 5) :: fieldPC, fieldPL
INTEGER, DIMENSION (1, 2) :: kA, kB, kC, gA, gB, Gc, shot
INTEGER :: i, j, r, t, y, PCships, PlayerShips
CHARACTER :: s, chose

! Umiejscowienie statkow gracza
print*,'SHIP 1, TYPE X Y int cordinates 1-4'
  read (*,*) i,j
  gA(1, 1) = i+1
  gA(1, 2) = j+1

print*,'SHIP 2, TYPE X Y int cordinates 1-4'
  read (*,*) i,j
  gB(1, 1) = i+1
  gB(1, 2) = j+1
    
print*,'SHIP 3, TYPE X Y int cordinates 1-4'
  read (*,*) i,j
  gC(1, 1) = i+1
  gC(1, 2) = j+1
 
 
! Losowanie umiejscowienia statkow komputera
do j=1, 2
   kA(1, j) = Losowanie()
   kB(1, j) = Losowanie()
   kC(1, j) = Losowanie()
end do     


! Tworzenie pol gry

fieldPC(1, 1)='X'
fieldPC(1, 2)='1'
fieldPC(1, 3)='2'
fieldPC(1, 4)='3'
fieldPC(1, 5)='4'
fieldPC(2, 1)='1'
fieldPC(3, 1)='2'
fieldPC(4, 1)='3'
fieldPC(5, 1)='4'

fieldPL(1, 1)='X'
fieldPL(1, 2)='1'
fieldPL(1, 3)='2'
fieldPL(1, 4)='3'
fieldPL(1, 5)='4'
fieldPL(2, 1)='1'
fieldPL(3, 1)='2'
fieldPL(4, 1)='3'
fieldPL(5, 1)='4'


! Wczytywanie statkow na pole gry
! Statki Komputera
fieldPC(kA(1, 1), kA(1, 2))='O'
fieldPC(kB(1, 1), kB(1, 2))='O' 
fieldPC(kC(1, 1), kC(1, 2))='O'

! Statki Gracza
fieldPL(gA(1, 1), gA(1, 2))='O'
fieldPL(gB(1, 1), gB(1, 2))='O' 
fieldPL(gC(1, 1), gC(1, 2))='O'
    
    
    
!CZYSZCZENIE EKRANU    
DO i=0, 50
  write(*,*)
END DO




!YSWIETLANIE POLA GRY
!print*, 'POLE KOMPUTERA'
!call arrayPrint(fieldPC, 5, 5)

print*, 'POLE GRACZA'
call arrayPrint(fieldPL, 5, 5)


!LICZENIE STATKOW

print*, 'Statki komputera: '
call shipsCount(fieldPC, 5, 5, PCships) 
print*, PCships                              

print*, 'Statki gracza: '
call shipsCount(fieldPL, 5, 5, PlayerShips)
print*, PlayerShips 
 
print*, 'TYPE TO START '
read (*,*) !chose 


!CZYSZCZENIE EKRANU    
DO i=0, 50
  write(*,*)
END DO

print*, ' START '
 
continue
!************** GAME LOOP ****************** 

print*, chose, PCships>0.and.PlayerShips>0

DO WHILE ( PCships>0.and.PlayerShips>0) 
 
!STRZAL KOMPUTERA
shot(1, 1) = Losowanie()
shot(1, 2) = Losowanie()
print*, 'SHOT! ', shot

IF (fieldPL(shot(1, 1), shot(1, 2)) == 'O' .and. fieldPC(shot(1, 1), shot(1, 2)) == 'O') THEN
   print*, 'PC DESTROYED! '
   fieldPL(shot(1, 1), shot(1, 2)) = ''
    continue
    
ELSE IF (fieldPL(shot(1, 1), shot(1, 2)) == 'O' .and. fieldPC(shot(1, 1), shot(1, 2)) /= 'O') THEN
   print*, 'PC DESTROYED! '
   fieldPL(shot(1, 1), shot(1, 2)) = ''
   fieldPC(shot(1, 1), shot(1, 2)) = 'd'
    continue
    
ELSE IF (fieldPL(shot(1, 1), shot(1, 2)) /= 'O' .and. fieldPC(shot(1, 1), shot(1, 2)) == 'O') THEN
   print*, 'PC MISSED ! '
    continue

ELSE IF (fieldPL(shot(1, 1), shot(1, 2)) /= 'O' .and. fieldPC(shot(1, 1), shot(1, 2)) /= 'O') THEN
   print*, 'PC MISSED ! '
   fieldPC(shot(1, 1), shot(1, 2)) = 'm'
   continue
   
END IF


print*, 'TYPE TO CONTINUE '
read (*,*) !chose 


!CZYSZCZENIE EKRANU    
DO i=0, 50
  write(*,*)
END DO


!YSWIETLANIE POLA GRY
!print*, 'POLE KOMPUTERA'
!call arrayPrint(fieldPC, 5, 5)

print*, 'POLE GRACZA'
call arrayPrint(fieldPL, 5, 5)


!LICZENIE STATKOW

print*, 'Statki komputera: '
call shipsCount(fieldPC, 5, 5, PCships) 
print*,  PCships

print*, 'Statki gracza: '
call shipsCount(fieldPL, 5, 5, PlayerShips)
print*,  PlayerShips


!Sprawdzanie warunku petli
IF ( PCships == 0 .or. PlayerShips == 0) THEN
   EXIT
END IF



! STRZAL GRACZA
print*, 'TYPE YOUR SHOT!'
read (*,*) i, j
shot(1, 1) = i+1
shot(1, 2) = j+1

!CZYSZCZENIE EKRANU    
DO i=0, 50
  write(*,*)
END DO



IF (fieldPC(shot(1, 1), shot(1, 2)) == 'O' .and. fieldPL(shot(1, 1), shot(1, 2)) == 'O') THEN
   print*, 'YOU DESTROYED! '
   fieldPC(shot(1, 1), shot(1, 2)) = ''

    continue
    
ELSE IF (fieldPC(shot(1, 1), shot(1, 2)) == 'O' .and. fieldPL(shot(1, 1), shot(1, 2)) /= 'O') THEN
   print*, 'YOU DESTROYED! '
   fieldPC(shot(1, 1), shot(1, 2)) = ''
   fieldPL(shot(1, 1), shot(1, 2)) = 'd'
    continue
    
ELSE IF (fieldPC(shot(1, 1), shot(1, 2)) /= 'O' .and. fieldPL(shot(1, 1), shot(1, 2)) == 'O') THEN
   print*, 'YOU MISSED ! '
    continue

ELSE IF (fieldPC(shot(1, 1), shot(1, 2)) /= 'O' .and. fieldPL(shot(1, 1), shot(1, 2)) /= 'O') THEN
   print*, 'YOU MISSED ! '

   fieldPL(shot(1, 1), shot(1, 2)) = 'm'
   continue
   
END IF

print*, 'TYPE TO CONTINUE!'
read (*,*)

!CZYSZCZENIE EKRANU    
DO i=0, 50
  write(*,*)
END DO



!YSWIETLANIE POLA GRY
!print*, 'POLE KOMPUTERA'
!call arrayPrint(fieldPC, 5, 5)

print*, 'POLE GRACZA'
call arrayPrint(fieldPL, 5, 5)



!Sprawdzanie warunku petli
IF ( PCships == 0 .or. PlayerShips == 0) THEN
   EXIT
END IF


print*, 'TYPE TO CONTINUE '
read (*,*)


!CZYSZCZENIE EKRANU
DO i=0, 50
  write(*,*)
END DO


END DO
continue


DO i=0, 50
  write(*,*)
END DO


IF ( PCships == 0 .and. PlayerShips > 0) THEN
  print*, 'You WIN!! '
  continue 
  
ELSE IF ( PCships > 0 .and. PlayerShips == 0) THEN
  print*, 'You LOSE!! '
  continue

ELSE
  print*, 'Something is wrong! '
  continue

END IF

print*, 'TYPE TO END '
read (*,*)


pause

end program