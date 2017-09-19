! This is a program which generates a grid with random obstacles based on user specified dimensions
! difficulty parameter and then finds all paths from the chosen start point to the chosen end point.

! Ideas -
! 1. More intricate structure obstacles:
! Node more likely impassable if only one node next to it is impassable? (creates maze-like structure)


PROGRAM pathfinder
    IMPLICIT NONE

    INTEGER :: rows, columns
    INTEGER :: startrows, startcolumns, endrows, endcolumns
    INTEGER :: dangerlevel


11  WRITE (6, *) 'Please enter map dimensions in the following form with spaces: rows, columns (both 9 or smaller)'
    READ (5, "(I1, 2X, I1)") rows, columns
    WRITE (6, '(A32, I2, A3, I2)') 'Your chosen grid dimensions are ',  rows, ' X ', columns

    WRITE (6, *) NEW_LINE('A')
    CALL generategraph(rows, columns, 0, 0, 0, 0, 0)

    WRITE (6, *) 'Please enter your startpoint with format "x, y"'
    READ (5, "(I1, 2X, I1)") startrows, startcolumns
    WRITE (6, *) NEW_LINE('A')//'Please enter your endpoint with format "x, y"'
    READ (5, "(I1, 2X, I1)") endrows, endcolumns

    WRITE (6, *) "Enter difficulty level of your hero's journey (0-9)"
    READ (5, *) dangerlevel

    WRITE (6, *) NEW_LINE('A')
    CALL generategraph(rows, columns, startrows, startcolumns, endrows, endcolumns, dangerlevel)

    WRITE (6, *) "For new grid, press enter"
    READ (*,*)
    GO TO 11

END PROGRAM pathfinder


! *******************************************************

! Defines a subroutines which generates a grid with random obstacles (impassable nodes)
! Subroutine also calls countpaths, which counts the number of paths from user specified start to end point.

SUBROUTINE generategraph(row, column, startrow, startcolumn, endrow, endcolumn, danger)
    IMPLICIT NONE
    CHARACTER (len = 150) :: graph, pillars
    CHARACTER (len = 1) :: format_string
    INTEGER, INTENT(IN) :: row, column
    INTEGER, INTENT(IN) :: startrow, startcolumn, endrow, endcolumn
    INTEGER, INTENT(IN) :: danger
    INTEGER :: i, j, k, l, y, z, a
    LOGICAL, DIMENSION(row, column) :: roadblock, traverse
    REAL, DIMENSION(row*column) :: luck
    INTEGER, DIMENSION(row*column) :: length
    INTEGER :: counter
    REAL :: finish, start

    CALL init_random_seed()
    CALL RANDOM_NUMBER(luck)
    luck = luck *20

   ! DO a = 1 , column*row
   ! WRITE (6, *) "Luck for (", a/column, ", ", MOD(a, column), ")", luck(a)
   ! END DO

    DO i = 1, row - 1
        graph = "" !Initialise to blank
        pillars = "  ."

        DO j = 1, column-1
            pillars = trim(pillars)//"           ."
            !print *, luck(i*column + j)

            IF (startrow == i .AND. startcolumn == j) THEN
                graph = trim(graph)//"| S |"
                roadblock(i, j) = .TRUE.
            ELSE IF (endrow == i .AND. endcolumn == j) THEN
                graph = trim(graph)//"| E |"
            ELSE IF (luck((i-1)*column +j) >= danger) THEN
                graph = trim(graph)//"["
                WRITE (format_string, "(I1)") i
                graph = trim(graph)//format_string//","
                WRITE (format_string, "(I1)") j
                graph = trim(graph)//format_string
                graph = trim(graph)//"]"

                roadblock(i, j) = .FALSE.
            ELSE
                graph = trim(graph)//"(   )"
                roadblock(i, j) = .TRUE.
            END IF
                graph = trim(graph)//". . . ."

            traverse(i,j) = .FALSE.
            traverse(i,column) = .FALSE.
            traverse(row,j) = .FALSE.
            traverse(row,column) = .FALSE.

            length((i-1)*column + j) = 0
            length((i-1)*column + column) = 0
            length((row-1)*column + j) = 0
            length(row*column) = 0
        END DO


        ! Last column
        IF (startrow == i .AND. startcolumn == column) THEN
            graph = trim(graph)//"| S |"
            roadblock(i, column) = .TRUE.
        ELSE IF (endrow == i .AND. endcolumn == column) THEN
            graph = trim(graph)//"| E |"
            roadblock(i, column) = .FALSE.
        ELSE IF (luck((i-1)*column +j)  >= danger) THEN
            graph = trim(graph)//"["
            WRITE (format_string, "(I1)") i
            graph = trim(graph)//format_string//","
            WRITE (format_string, "(I1)") column
            graph = trim(graph)//format_string
            graph = trim(graph)//"]"
            roadblock(i, column) = .FALSE.
        ELSE
            graph = trim(graph)//"(   )"
            roadblock(i, column) = .TRUE.
        END IF

        WRITE (6, *) graph
        WRITE (6, *) trim(pillars)
        WRITE (6, *) trim(pillars)
        WRITE (6, *) trim(pillars)
        WRITE (6, *) trim(pillars)
    END DO

    graph = "" !Initialise to blank

    ! Last row

    DO j = 1, column -1
        IF (startrow == row .AND. startcolumn == j) THEN
            graph = trim(graph)//"| S |"
            graph = trim(graph)//". . . ."
            roadblock(row, j) = .TRUE.
        ELSE IF (endrow == row .AND. endcolumn == j) THEN
            graph = trim(graph)//"| E |"
            graph = trim(graph)//". . . ."
            roadblock(row, j) = .FALSE.
        ELSE IF (luck((row-1)*column +j)  >= danger) THEN
            graph = trim(graph)//"["
            WRITE (format_string, "(I1)") row
            graph = trim(graph)//format_string//","
            WRITE (format_string, "(I1)") j
            graph = trim(graph)//format_string//"]. . . ."
            roadblock(row, j) = .FALSE.
        ELSE
            graph = trim(graph)//"(   ). . . ."
            roadblock(row, j) = .TRUE.
        END IF
    END DO


    !Last entry
        IF (startrow == row .AND. startcolumn == column) THEN
            graph = trim(graph)//"| S |"
            roadblock(row, column) = .TRUE.
        ELSE IF (endrow == row .AND. endcolumn == column) THEN
            graph = trim(graph)//"| E |"
            roadblock(row, column) = .FALSE.
        ELSE IF (luck(row*column)  >= danger) THEN
            graph = trim(graph)//"["
            WRITE (format_string, "(I1)") row
            graph = trim(graph)//format_string//","
            WRITE (format_string, "(I1)") column
            graph = trim(graph)//format_string
            graph = trim(graph)//"]"
            roadblock(row, column) = .FALSE.
        ELSE
            graph = trim(graph)//"(   )"
            roadblock(row, column) = .TRUE.
        END IF

    WRITE (6, *) graph

    WRITE (6, *) NEW_LINE('A')

    ! WRITE (6, *) roadblock
    ! WRITE (6, *) length

    ! Initialise Values
    counter = 0
    DO y = 1, row
        DO z = 1, column
            traverse(y, z) = .FALSE.
            length((y-1)*column + z) = 0
        END DO
    END DO

    CALL cpu_time(start)
    CALL countpaths(roadblock, traverse, row, column, startrow, startcolumn, endrow, endcolumn, length, counter)
    CALL cpu_time(finish)

    l = 0
    DO k = 1, row*column

        IF (length(k) /= 0) THEN
            WRITE (6, "(A16, I2, A2, I10)") "Paths of length ", k, ": ", length(k)

        ELSE
            l = l + 1
        END IF
    END DO

    IF ((l >= row*column) .AND. (startrow /=0 .OR. startcolumn /=0)) THEN
        WRITE (6, *) "There are no paths. Our hero will never reach his destination."
        WRITE (6, *) "Time taken for search:", finish - start
    ELSE IF (startrow /=0 .OR. startcolumn /=0) THEN
        WRITE (6, *) "Time taken for search:", finish - start, " seconds."
    END IF



END SUBROUTINE generategraph


!***************************************************************


! Call a recursive function that systematically traverses all paths. Backtracks if stuck or finds endpoint.
! If finds endpoint, increment counter for particular path length

RECURSIVE SUBROUTINE countpaths(roadblocked, traversed, rowed, columned, &
    currentrowed, currentcolumned, endrowed, endcolumned, lengthed, lengthcount)
    IMPLICIT NONE
    INTEGER, INTENT(IN) :: rowed, columned
    INTEGER:: currentrowed, currentcolumned, endrowed, endcolumned
    LOGICAL, DIMENSION(rowed, columned) :: roadblocked, traversed
    INTEGER, DIMENSION(rowed*columned) :: lengthed
    INTEGER :: lengthcount



    !WRITE(6,"(A15, I1)") "Number of rows = ", rowed
    !WRITE(6, "(A18, I1)") "Number of columns = ", columned
    ! Check if at end
    IF (currentrowed == endrowed .AND. currentcolumned == endcolumned .AND. (currentrowed /=0 .OR. currentcolumned /=0)) THEN
          GO TO 10
          WRITE (6, "(A25, I2)") "Hit the Jackpot! Path of length ", lengthcount
          WRITE (6, "(A23, I2, A2, I5)") "Number of paths of length ", lengthcount, ": ", lengthed(lengthcount)
          WRITE (6, *) "All paths encountered: "
          WRITE (6, *) lengthed

10        lengthed(lengthcount) = lengthed(lengthcount) + 1

        ELSE
        GO TO 9
        WRITE (6, *) "All paths encountered: "
        WRITE (6, *) lengthed

         ! Try left
9        IF ((currentrowed >= 1) .AND. (currentrowed <= rowed) .AND. (currentcolumned > 1) .AND. (currentcolumned <= columned) &
        .AND. (traversed(currentrowed,currentcolumned - 1) .EQV. .FALSE.) &
        .AND. (roadblocked(currentrowed,currentcolumned - 1) .EQV. .FALSE.)) THEN

            traversed(currentrowed,currentcolumned - 1) = .TRUE.
            lengthcount = lengthcount + 1

            !Debugging printouts
            GO TO 1
            WRITE (6, *) "*************************************"
            WRITE (6,*) "Moved Left"
            WRITE (6,"(A20, I1, A1, I1, A1)") "CURRENT LOCATION (", currentrowed, ",", currentcolumned - 1, ")"
            WRITE (6,"(A24, I2)") "Current Length of path: ", lengthcount


1            CALL countpaths(roadblocked, traversed, rowed, columned, currentrowed, &
            currentcolumned-1, endrowed, endcolumned, lengthed, lengthcount)

            lengthcount = lengthcount -1
            traversed(currentrowed,currentcolumned - 1) = .FALSE.

            GO TO 2
            WRITE (6, *) "*************************************"
            WRITE (6,*) "Backstepped Right"
            WRITE (6,"(A20, I1, A1, I1, A1)") "CURRENT LOCATION (", currentrowed, ",", currentcolumned, ")"
            WRITE (6,"(A24, I2)") "Current Length of path: ", lengthcount
            WRITE (6, *) "All paths encountered: "
            WRITE (6, *) lengthed


2        END IF

        ! Try Up
        IF((currentrowed > 1) .AND. (currentrowed <= rowed) .AND. (currentcolumned >= 1) .AND. (currentcolumned <= columned) &
        .AND. (traversed(currentrowed - 1,currentcolumned) .EQV. .FALSE.) &
        .AND. (roadblocked(currentrowed - 1,currentcolumned) .EQV. .FALSE.)) THEN

            traversed(currentrowed - 1,currentcolumned) = .TRUE.
            lengthcount = lengthcount + 1

            GO TO 3
            WRITE (6, *) "*************************************"
            WRITE (6,*) "Moved Up"
            WRITE (6,"(A20, I1, A1, I1, A1)") "CURRENT LOCATION (", currentrowed - 1, ",", currentcolumned, ")"
            WRITE (6,"(A24, I2)") "Current Length of path: ", lengthcount


3            CALL countpaths(roadblocked, traversed, rowed, columned, currentrowed-1, &
            currentcolumned, endrowed, endcolumned, lengthed, lengthcount)

            lengthcount = lengthcount -1
            traversed(currentrowed - 1,currentcolumned) = .FALSE.

            GO TO 4
            WRITE (6, *) "*************************************"
            WRITE (6,*) "Backstepped Down"
            WRITE (6,"(A20, I1, A1, I1, A1)") "CURRENT LOCATION (", currentrowed, ",", currentcolumned, ")"
            WRITE (6,"(A24, I2)") "Current Length of path: ", lengthcount
            WRITE (6, *) "All paths encountered: "
            WRITE (6, *) lengthed


4        END IF

        ! Try right
        IF((currentrowed >= 1) .AND. (currentrowed <= rowed) .AND. (currentcolumned >= 1) .AND. (currentcolumned < columned) &
        .AND. (traversed(currentrowed,currentcolumned + 1) .EQV. .FALSE.) &
        .AND. (roadblocked(currentrowed,currentcolumned + 1) .EQV. .FALSE.)) THEN

            traversed(currentrowed,currentcolumned + 1) = .TRUE.
            lengthcount = lengthcount + 1

            GO TO 5
            WRITE (6, *) "*************************************"
            WRITE (6,*) "Moved Right"
            WRITE (6,"(A20, I1, A1, I1, A1)") "CURRENT LOCATION (", currentrowed, ",", currentcolumned + 1, ")"
            WRITE (6,"(A24, I2)") "Current Length of path: ", lengthcount


5            CALL countpaths(roadblocked, traversed, rowed, columned, currentrowed, &
            currentcolumned + 1, endrowed, endcolumned, lengthed, lengthcount)

            lengthcount = lengthcount -1
            traversed(currentrowed,currentcolumned + 1) = .FALSE.

            GO TO 6
            WRITE (6, *) "*************************************"
            WRITE (6,*) "Backstepped Left"
            WRITE (6,"(A20, I1, A1, I1, A1)") "CURRENT LOCATION (", currentrowed, ",", currentcolumned, ")"
            WRITE (6,"(A24, I2)") "Current Length of path: ", lengthcount
            WRITE (6, *) "All paths encountered: "
            WRITE (6, *) lengthed


6        END IF

        ! Try down
        IF((currentrowed >= 1) .AND. (currentrowed < rowed) .AND. (currentrowed < rowed) .AND. (currentcolumned <= columned) &
        .AND. (traversed(currentrowed + 1,currentcolumned) .EQV. .FALSE.) &
        .AND. (roadblocked(currentrowed + 1, currentcolumned) .EQV. .FALSE.)) THEN

            traversed(currentrowed + 1,currentcolumned) = .TRUE.
            lengthcount = lengthcount + 1

            GO TO 7
            WRITE (6, *) "*************************************"
            WRITE (6,*) "Moved Down"
            WRITE (6,"(A20, I1, A1, I1, A1)") "CURRENT LOCATION (", currentrowed + 1, ",", currentcolumned, ")"
            WRITE (6,"(A24, I2)") "Current Length of path: ", lengthcount


7            CALL countpaths(roadblocked, traversed, rowed, columned, currentrowed + 1, &
            currentcolumned, endrowed, endcolumned, lengthed, lengthcount)
            lengthcount = lengthcount -1
            traversed(currentrowed + 1,currentcolumned) = .FALSE.

            GO TO 8
            WRITE (6, *) "*************************************"
            WRITE (6,*) "Backstepped Up"
            WRITE (6,"(A20, I1, A1, I1, A1)") "CURRENT LOCATION (", currentrowed, ",", currentcolumned, ")"
            WRITE (6,"(A24, I2)") "Current Length of path: ", lengthcount

            WRITE (6, *) "All paths encountered: "
            WRITE (6, *) lengthed

8        END IF
    END IF

END SUBROUTINE countpaths


! Initialises new random seed each time
SUBROUTINE init_random_seed

      INTEGER :: i, n, clock
      INTEGER, DIMENSION(:), ALLOCATABLE :: seed

      CALL RANDOM_SEED(size = n)
      ALLOCATE(seed(n))

      CALL SYSTEM_CLOCK(COUNT=clock)

      seed = clock + 37 * (/ (i - 1, i = 1, n) /)
      CALL RANDOM_SEED(PUT = seed)

      DEALLOCATE(seed)
END SUBROUTINE init_random_seed


!    WRITE (6,*) traversed(currentrowed,currentcolumned + 1)
!           IF(currentrowed >= 1) THEN
!              WRITE (6,*) "Cond 1 satisfied"
!            END IF
!            IF(currentrowed < rowed) THEN
!              WRITE (6,*) "Cond 2 satisfied"
!            IF (currentrowed < rowed) THEN
!              WRITE (6,*) "Cond 3 satisfied"
!            END IF
!            IF (currentcolumned <= columned) THEN
!              WRITE (6,*) "Cond 4 satisfied"
!            END IF
!             IF (traversed(currentrowed + 1,currentcolumned) .EQV. .FALSE.) THEN
!              WRITE (6,*) "Cond 5 satisfied"
!            END IF
!             IF(roadblocked(currentrowed + 1, currentcolumned) .EQV. .FALSE.) THEN
!              WRITE (6,*) "Cond 6 satisfied"
!            END IF

