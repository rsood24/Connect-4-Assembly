TITLE connect_4.asm
;Ritesh Sood
;Description: This program is a connect three game. The game can
;             be played player vs player, player vs computer, or computer vs computer.
;             It creates a 4x4 matrix which holds disks in slots selected by a user.
;             Whoever gets to the first three disks connected in a row, column, or diagonal wins
;05/07/2017


INCLUDE Irvine32.inc


.data
    matr BYTE 20h, 20h, 20h, 20h            ;matr holds the game board matrix
	RowSize = ($ - matr)
    BYTE 20h, 20h, 20h, 20h                 ;it is filled with 20h in each slot to begin with
		 BYTE 20h, 20h, 20h, 20h
		 BYTE 20h, 20h, 20h, 20h
	turnCount BYTE 0h	 
    colNums BYTE " 0  1  2  3  ", 0ah, 0dh, 0h              ;colNums is used to display the column names for the user
	Rules BYTE "The game is played by dropping", 0ah, 0dh,
               "a disk into a slot at the top of the board.", 0ah, 0dh,     ;Rules holds the game rules to be displayed at the beginning of the prgoram
			   "That disk will drop to the bottom if there", 0ah, 0dh,
			   "are no other disks in the game. Each player", 0ah, 0dh,
			   "in turn will drop a disk until a player connects three.", 0ah, 0dh, 0h
	Winner_1 BYTE "Player 1 wins!", 0ah , 0dh, 0h
	Winner_2 BYTE "Player 2 wins!", 0ah, 0dh, 0h
	Loser BYTE "You lose", 0ah, 0dh, 0h
	a_menu BYTE "1. Player vs Player", 0ah, 0dh,
                "2. Player vs Computer", 0ah, 0dh,                      ;a_menu holds the program menu
				"3. Computer vs Computer", 0ah, 0dh,
				"4. Exit Program", 0ah, 0dh, 0h
    No_Winner BYTE "There was no winner.", 0ah, 0dh, 0h                 ;No_Winner is displayed when the matrix is full with disks but there is no winner
	
.code
main PROC
	
	mov eax, 0
	mov esi, 0
	mov ecx, 0
	mov edx, 0
    call Randomize                              ;a call to Randomize before the program begins to seed RandomRange
	mov edx, offset Rules
    call WriteString                            ;display the rules to the screen
	call WaitMsg
	StartHere:
    call Clrscr                                 ;a call to Clrscr before the main menu is displayed
		mov esi, offset matr
		mov al, RowSize
        call cleanUp                            ;cleanUp takes in RowSize through al. cleanUp clears any values the matrix may hold from previous runs
		mov ECX, 0
		mov EDX, offset a_menu
        call WriteString                        ;display the menu
        call ReadDec                            ;a call to ReadDec to retrieve a menu option from the user
        cmp eax, 1                              ;menu option 1 represents player vs player
		je pvp
        cmp eax, 2                              ;menu option 2 represents player vs computer
		je pve
        cmp eax, 3                              ;menu option 3 represents computer vs computer
        je cvc
        cmp eax, 4                              ;menu option 4 exits the program
        je itsAllOver                           ;an invalid input will display the menu again
		jmp StartHere
		
		
        pve:                                    ;the pve block runs player vs computer
			call Clrscr
			mov esi, offset matr
			mov al, RowSize
			mov EDX, offset colNums
            call WriteString                    ;WriteString is used here to display the column numbers above the columns
            call DisplayBoard                   ;DisplayBoard displays the matrix to the screen
			mov esi, offset matr
			mov al, RowSize
            call Player                         ;Player PROC takes in which column the user wants to drop their disk
            mov al, RowSize                     ;Player PROC uses the value of ECX to determine which players turn it is
            mov esi, offset matr                ;if ecx is 0 then it is player one's turn, if ecx is fffff, it is player 2's turn'
			call CheckMatr                      ;CheckMatr checks the matrix to see if there are 3 disks connected in a row, column, or diagonally
			cmp al, 17h                         ;If there is a winner, CheckMatr returns a 17h within the al register
			je winnaa                           ;CheckMatr only checks for one players disk when it is called. it does this based on the value in ecx
			not ecx                             ;not ecx is used to indicate a switch in turns
			mov ESI, offset matr
			mov al, RowSize
			call Computer                       ;Computer randomly chooses a column and drops a disk in it
			mov al, RowSize
			mov esi, offset matr
			call CheckMatr                      ;CheckMatr is called to see if the computer has won
			cmp al, 17h                         ;if the player has won, jmp to winnaa, if the computer has won, jmp to loossa
			je loossa
			not ECX
			jmp pve
		
			winnaa:                             ;The winnaa block displays the matr and displays the string Winner_1
				call Clrscr
				mov esi, offset matr
				mov al, RowSize
				mov EDX, offset colNums
				call WriteString
				call DisplayBoard
				mov EDX, offset Winner_1
				call WriteString
				call WaitMsg
				jmp StartHere                   ;jmps back to the main menu
				
				loossa:                         ;The loossa block displays the matr and displays the string Loser
					call Clrscr
					mov esi, offset matr
					mov al, RowSize
					mov EDX, offset colNums
					call WriteString
					call DisplayBoard
					mov EDX, offset Loser
					call WriteString
					call WaitMsg
					jmp StartHere               ;jmps back to the main menu
		pvp:                                    ;The pvp block is for player vs player
			call Clrscr
			mov esi, offset matr
			mov al, RowSize
			mov EDX, offset colNums
			call WriteString
			call DisplayBoard                   ;Display the gameboard on the screen
			mov esi, offset matr
			mov al, RowSize
			call Player                         ;Player PROC takes in which column the user wants to drop their disk
			mov al, RowSize                     ;Player PROC uses the value of ECX to determine which players turn it is
			mov esi, offset matr
			call CheckMatr                      ;CheckMatr checks the matrix to see if there are 3 disks connected in a row, column, or diagonally
			cmp al, 17h                         ;If there is a winner, CheckMatr returns a 17h within the al register
			je a_win                            ;CheckMatr only checks for one players disk when it is called. it does this based on the value in ecx
			not ecx                             ;not ecx is used to switch turns
			jmp pvp
			
			a_win:                              ;a_win block is used for when ChekMatr determines there is a winner
				call Clrscr
				mov esi, offset matr
				mov al, RowSize
				mov EDX, offset colNums
				call WriteString
				call DisplayBoard               ;display the gameboard
				cmp ecx, 0h                     ;if ecx is 0 then player 1 has won, if not then player 2 has won
				je play_1
				mov EDX, offset Winner_2
				call WriteString
				call WaitMsg
				jmp StartHere
					
					play_1:                         ;play_1 block is used for when player 1 has won the game
						mov EDX, offset Winner_1
						call WriteString
						call WaitMsg
						jmp StartHere
		cvc:
			call Clrscr                         ;the cvc block is used for computer vs computer
			mov esi, offset matr
			mov al, RowSize
			mov EDX, offset colNums
			call WriteString
			call DisplayBoard
			mov eax, 0
			mov eax, 1000
			call Delay                          ;a call to Delay is used to delay the screen output by a second every time the computer takes a turn
			mov eax, 0
			mov esi, offset matr
			mov al, RowSize
			call Computer
			mov al, RowSize
			mov esi, offset matr
			call CheckMatr                      ;if CheckMatr determines that the matrix if full without any winners, it returns 42h within eax
			cmp eax, 42h
			je noWinner
			cmp al, 17h                         ;if CheckMatr determines that there is a winner it returns 17h within al
			je comp_Win
			not ecx
			jmp cvc
			
			comp_Win:                           ;comp_Win displays the winner between computer vs computer
				call Clrscr
				mov esi, offset matr            ;if the blue disks win, it displays player 1 has won
				mov al, RowSize                 ;if the yellow disks win, it displays player 2 has won
				mov EDX, offset colNums
				call WriteString
				call DisplayBoard
				cmp ecx, 0h                     ;if ecx contains a 0 then blue disks have won
				je comp_1                       ;otherwise yellow disks have won
				mov EDX, offset Winner_2
				call WriteString
				call WaitMsg
				jmp StartHere                   ;jmp to main menu
				
					comp_1:                         ;comp_1 is used if blue disks have won
						mov EDX, offset Winner_1
						call WriteString
						call WaitMsg
						jmp StartHere
				
			noWinner:                           ;noWinner is used when the matrix is full without any winners
				call Clrscr
				mov esi, offset matr
				mov al, RowSize
				mov EDX, offset colNums
				call WriteString
				call DisplayBoard               ;displays the matrix
				mov EDX, offset No_Winner       ;displays the No_Winner prompt
				call WriteString
				call WaitMsg
				jmp StartHere
itsAllOver:                                     ;itsAllOver is used to end the program
exit
main ENDP


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Desc:  DisplayBoard displays the matrix in a specific format to the screen
;Recieves:  ESI - holds the offset to matr
;           EAX - holds the RowSize for the matr
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
DisplayBoard PROC uses ECX
.data
	dash BYTE "------------", 0ah, 0dh, 0h          ;dash is used to display a seperation between each row
	I_0 BYTE "|", 0h                                ;I_0 is used to diplay a seperation between each block
	roSize BYTE 0h                                  ;roSize holds the size of each row
	roIndex BYTE 0h                                 ;roIndex stores the index of the row being stepped through
	anOff DWORD 0h                                  ;anOff holds the offset to matr
	endl BYTE 0ah, 0dh, 0h
	
.code
	mov ECX, 0
	mov roIndex, 0h                                 ;roIndex starts off at 0
	mov roSize, al                                  ;roSize stores the RowSize passed through using al
	mov anOff, ESI                                  ;mov the offset of matr into anOff using ESI
	mov EDX, offset dash                            ;display the first dash line for the matrix
	call WriteString
	mov EDX, 0
	
	L1:
		mov EAX, 0
		mov al, I_0                                 ;display I_0 between each block
		call WriteChar
		mov EAX, 0
		mov ESI, anOff
		mov al, roSize
		mul roIndex
		add ESI, EAX
		mov cl, [ESI + EDX]                         ;mov into cl the value in the current matrix element
		cmp cl, 20h                                 ;if the value is 20h jmp to Print Space
		je prSpa
		cmp cl, 14                                  ;if the value is 14d jmp to Print Yellow
		je prYel
		cmp cl, 1                                   ;if the value is 1d jmp to Print Blue
		je prBl
		
		prSpa:
			mov eax, 0
			mov al, cl
			call WriteChar
			mov al, I_0
			call WriteChar
			cmp dl, 3                               ;if edx is at the 4th column jmp to nextRo
			je nextRo
			inc EDX                                 ;inc EDX to get to the next element within the row
			jmp L1
			
		prYel:
			mov eax, 0
			mov eax, 14 + (14*16)
			call SetTextColor                   ;Sets the text color to yellow for the yellow blocks to be displayed
			mov eax, 0
			mov al, cl
			call WriteChar
			mov al, 15
			call SetTextColor                   ;Sets the text color back to white after the disk has been printed
			mov al, I_0
			call WriteChar
			cmp dl, 3                           ;if edx is at the 4th column jmp to nextRo
			je nextRo
			inc EDX                             ;inc EDX to get to the next element within the row
			jmp L1
		
		prBl:
			mov eax, 0
			mov eax, 17                         ;Sets the text color to blue for the blue blocks to be displayed
			call SetTextColor
			mov eax, 0
			mov al, cl
			call WriteChar
			mov al, 15
			call SetTextColor                   ;Sets the text color back to white after the disk has been printed
			mov al, I_0
			call WriteChar
			cmp dl, 3                           ;if edx is at the 4th column jmp to nextRo
			je nextRo
			inc EDX                             ;inc EDX to get to the next element within the row
			jmp L1
			
		nextRo:
			push EDX
			mov EDX, offset endl
			call WriteString                    ;if a row has been printed, use endl to get to the next line
			mov EDX, offset dash                ;print a dash to seperate between the rows
			call WriteString
			pop EDX
			mov cl, roIndex
			cmp cl, 3                           ;if roIndex is at the 4th row, the entire matrix has been printed
			je End_n
			inc roIndex                         ;otherwise inc roIndex, mov EDX back to the 0 column and jmp to L1
			mov EDX, 0
			jmp L1
		
End_n:
ret
DisplayBoard ENDP


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Desc:  This PROC has a player choose which column
;       they would like to drop a disk in, and drops it
;       to the lowest most available spot in that column
;Recieves:  EAX - holds the RowSize of matr
;           ESI - holds the offset to matr
;           ECX - holds which players turn it is
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
Player PROC
.data
	Pla_Num BYTE 0h                             ;Pla_Num holds a 1 or 14 depending on which players turn it is
	a_OFF DWORD 0h
	ro_Size BYTE 0h
	ro_Index BYTE 3h
	Player_1 BYTE "Player 1 please enter a column number: ", 0ah, 0dh, 0h           ;Prompt for player 1 to enter a column number
	Player_2 BYTE "Player 2 please enter a column number: ", 0ah, 0dh, 0h           ;Prompt for player 2 to enter a column number
	a_Error BYTE "Error: That column is full. You lose your turn.", 0ah, 0dh, 0h    ;for when a player enters a number for a column which is full
	
.code
	push ecx
	mov ro_Index, 3h                                        ;ro_Index starts off at the very last row and moves up
	mov a_OFF, ESI
	mov ro_Size, al
	mov EDX, 0												;0 represents player 1 and 1 represents player 2
	cmp cl, 0												;Player number is being passed in through ECX
	ja pla_2
	
	mov cl, 1												;the 1 that is moved into cl, then Pla_Num represents blue
	mov Pla_Num, cl
	mov EDX, offset Player_1                                ;displays the prompt asking player 1 to enter a column number
	call WriteString
	mov EDX, 0
	jmp begin
	
	pla_2:
		mov cl, 14											;the 14 that is moved into cl, then Pla_Num represents yellow
		mov Pla_Num, cl
		mov EDX, offset Player_2                            ;displays the prompt asking player 2 to enter a column number
		call WriteString
		
	begin:
		mov EAX, 0
		mov ECX, 0
		mov EDX, 0
		call ReadDec                                        ;Read in a column number from a player
		cmp al, 0                                           ;if 0 jmp to col1
		je col1
		cmp al, 1                                           ;if 1 jmp to col2
		je col2
		cmp al, 2                                           ;if 2 jmp to col3
		je col3
		cmp al, 3                                           ;if 3 jmp to col4
		je col4
		
		col1:
			mov ESI, a_OFF                                  ;mov into ESI the offset of matr
			mov al, ro_Size
			mul ro_Index
			add ESI, EAX
			mov cl, [ESI + EDX]                             ;mov into cl the current element of the matr
			cmp cl, 20h                                     ;if the element does not hold a value yet jmp good
			je good
			mov al, ro_Index
			cmp al, 0h                                      ;check to see if ro_Index is pointing to the very first row
			je noGood                                       ;if it is, then the column is full and jmp to noGood
			dec ro_Index                                    ;otherwise dec ro_Index to step to the next slot up
			jmp col1                                        ;jmp to the beginning of the block
			
		col2:
			mov ESI, a_OFF
			mov al, ro_Size                                 ;col2 block works exactly the same way as col1, but EDX points to the second column
			mul ro_Index
			add ESI, EAX
			mov EDX, 1
			mov cl, [ESI + EDX]
			cmp cl, 20h
			je good
			mov al, ro_Index
			cmp al, 0h
			je noGood
			dec ro_Index
			jmp col2
			
		col3:
			mov ESI, a_OFF
			mov al, ro_Size                                 ;col3 block works exactly the same way as col1, but EDX points to the third column
			mul ro_Index
			add ESI, EAX
			mov EDX, 2
			mov cl, [ESI + EDX]
			cmp cl, 20h
			je good
			mov al, ro_Index
			cmp al, 0h
			je noGood
			dec ro_Index
			jmp col3
			
		col4:
			mov ESI, a_OFF
			mov al, ro_Size
			mul ro_Index
			add ESI, EAX                                    ;col4 block works exactly the same way as col1, but EDX points to the fourth column
			mov EDX, 3
			mov cl, [ESI + EDX]
			cmp cl, 20h
			je good
			mov al, ro_Index
			cmp al, 0h
			je noGood
			dec ro_Index
			jmp col4
			
		good:
			 mov cl, Pla_Num                        ;if the slot is good to go, mov in Pla_Num into that slot
			 mov [ESI + EDX], cl
			 jmp here
			 
		noGood:
			mov EDX, offset a_Error                 ;if the entire column is filled, display error and end PROC
			call WriteString
			
here:
pop ECX
ret
Player ENDP


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Desc:  This PROC checks matr for any three of the same blocks
;       that may be connected in a row, column, or diagonally
;Recieves:  ESI - offset of matr
;           EAX - holds RowSize of matr
;           ECX - holds a value which lets the PROC know which block color to look for
;Returns:   EAX - holds a 17 if there is a winner
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
CheckMatr PROC
.data
	holdOff DWORD 0h
	Playa_Num BYTE 0h
	Row_Size BYTE 0h
	Row_Index BYTE 0h
	
.code
	push ECX
	mov Row_Index, 0
	mov EDX, 1h                 ;EDX starts holding a 1h, pointing to the second column
	mov EBX, 0h
	mov holdOff, ESI
	mov Row_Size, al
	cmp cl, 0
	jne Playa2
	mov Playa_Num, 1h
	jmp rws
	
	Playa2:
		mov Playa_Num, 14d

    ;The rws block will look for any combination of three values next to each other in a row
    ;it checks the value in the second slot to see if it matches the Playa_Num
    ;if it does not match the Playa_Num then there is no combo of 3 in that row, if it does match
    ;It checks the values in the second and third slots of a row to see if they match
    ;if they dont match that means there in no combo of three values within that row and
    ;steps into the next row. if the second and third values of a row match, it checks the
    ;first and the fourth slots in the row for the value located in the middle two rows
    ;if either the first or the fourth slot match the values in the middle two slots
    ;we have a winner.

	rws:
		mov ESI, holdOff
		mov al, Row_Size
		mul Row_Index
		add ESI, EAX
		mov cl, [ESI + EDX]         ;mov the value in the second slot of a certain row into cl
		mov al, Playa_Num
		cmp cl, al                  ;cmp cl to the Playa_Num, if they match jmp to nextCheck
		je nextCheck
		inc Row_Index               ;if they dont match mov to the next row
		mov bl, Row_Index
		cmp bl, 4h                  ;if Row_Index equals 4h there were no row combos in the matrix, so jmp to clean Values
		je cleanValues
		jmp rws
		
		nextCheck:                  ;nextCheck is for when the second slot matches Playa_Num
			mov eax, 0
			mov ESI, holdOff
			inc EDX
			mov al, Row_Size
			mul Row_Index
			add ESI, EAX
			mov cl, [ESI + EDX]
			mov al, Playa_Num       ;cmp the third slot ot Playa_Num, if they match jmp to rightCheck
			cmp cl, al
			je rightCheck
			inc Row_Index           ;if they dont match inc Row_Index to step to the next row
			dec EDX                 ;dec EDX to step back to the second slot from the third
			mov bl, Row_Index
			cmp bl, 4h              ;if Row_Index is 4h the whole matrix has been checked. jmp to cleanValues
			je cleanValues
			jmp rws
			
			rightCheck:                 ;rightCheck checks the 4th slot in a row for Playa_Num
				mov eax, 0
				mov ESI, holdOff
				inc EDX
				mov al, Row_Size
				mul Row_Index
				add ESI, EAX
				mov cl, [ESI + EDX]
				mov al, Playa_Num       ;if the slot matches Playa_Num we have a winner
				cmp cl, al
				je YouWin
				jne leftCheck           ;otherwise check the first slot in the row
				
			leftCheck:
				mov eax, 0              ;leftCheck checks the 1st slot in a row for Playa_Num
				mov ESI, holdOff
				mov EDX, 0              ;mov EDX, 0 to go the first slot the row
				mov al, Row_Size
				mul Row_Index
				add ESI, EAX
				mov cl, [ESI + EDX]
				mov al, Playa_Num       ;if the slot matches Playa_Num we have a winner
				cmp cl, al
				je YouWin
				inc EDX                 ;otherwise inc EDX to go to the second slot
				inc Row_Index           ;inc Row_Index to mov to the next row
				mov bl, Row_Index
				cmp bl, 4h
				je cleanValues          ;if Row_Index is 4h the whole matrix has been checked. jmp to cleanValues
				jmp rws
				
	cleanValues:                    ;cleanValues resets all values before jumping to columns to check for
		mov Row_Index, 1h           ;a combo of 3 in columns
		mov EAX, 0h
		mov EDX, 0h                 ;here Row_Index starts off with a 1h instead of EDX
		mov EBX, 0h                 ;Row_Index will be pointing to the second row
		mov ECX, 0h


        ;columns does the exact same thing as rws
        ;the only difference here is instead of checking EDX = 1 and EDX = 2 slots of a certain row
        ;it checks the slots where the Row_Index = 1 and Row_Index = 2 of a certain column


    columns:
		mov ESI, holdOff
		mov al, Row_Size
		mul Row_Index
		add ESI, EAX                    ;here Row_Index and EDX switch roles compared to the rws block
		mov cl, [ESI + EDX]
		mov al, Playa_Num
		cmp cl, al
		je CheckAgain
		inc EDX
		cmp dl, 4h
		je cleanMore
		jmp columns
			
			CheckAgain:
				mov eax, 0
				mov ESI, holdOff
				inc Row_Index
				mov al, Row_Size
				mul Row_Index
				add ESI, EAX
				mov cl, [ESI + EDX]
				mov al, Playa_Num
				cmp cl, al
				je bottomCheck
				inc EDX
				dec Row_Index
				cmp dl, 4h
				je cleanMore
				jmp columns
				
					bottomCheck:
						mov eax, 0
						mov ESI, holdOff            ;in rws it is the rightcheck, but here we check the bottom slot
						inc Row_Index               ;of a coloumn to see if it matches Playa_Num
						mov al, Row_Size
						mul Row_Index
						add ESI, EAX
						mov cl, [ESI + EDX]
						mov al, Playa_Num
						cmp cl, al
						je YouWin
						jne topCheck
						
					topCheck:
						mov eax, 0
						mov ESI, holdOff            ;in rws it is the leftcheck, but here we check the top slot
						mov Row_Index, 0            ;of a column to see if it matches Playa_Num
						mov al, Row_Size
						mul Row_Index
						add ESI, EAX
						mov cl, [ESI + EDX]
						mov al, Playa_Num
						cmp cl, al
						je YouWin
						inc Row_Index
						inc EDX
						cmp dl, 4h              ;if there are no combos within the columns jmp to cleanMore
						je cleanMore
						jmp columns
						
	cleanMore:
		mov Row_Index, 2h                       ;cleanMore resets the values again but now for the matr
		mov EAX, 0h                             ;to be checked for any diagonal combos
		mov EDX, 0h
		mov EBX, 0h
		mov ECX, 0h


        ;Diagonal_1 checks for any combos going from the bottom left to the top right
        ;it uses basically the same concept as rws and columns. it checks the first
        ;slot in the second row for Playa_Num, it the value holds then it steps back
        ;a row and check the third slot of that row, which would be diagonal to the
        ;slot last checked. if both of these slots hold true, then it appropriately
        ;checks the diagonal bottom and top to those slots for Playa_Num. I say
        ;appropriately because depending on the first slot checked there may not be
        ;a bottom or a top slot to the diagonal.

	Diagonal_1:
		mov ESI, holdOff
		mov al, Row_Size
		mul Row_Index
		add ESI, EAX
		mov cl, [ESI + EDX]
		mov al, Playa_Num
		cmp cl, al
		je anotherCheck
		inc EDX
		cmp dl, 4h
		je a_Cleaning
		jmp Diagonal_1
			
			anotherCheck:
				mov eax, 0
				mov ESI, holdOff
				inc EDX
				dec Row_Index
				mov al, Row_Size
				mul Row_Index
				add ESI, EAX
				mov cl, [ESI + EDX]
				mov al, Playa_Num
				cmp cl, al
				je b_check
				inc Row_Index
				jmp Diagonal_1
				
				b_check:
					cmp dl, 01h
					je t_check
					dec EDX
					dec EDX
					inc Row_Index
					inc Row_Index
					mov eax, 0
					mov ESI, holdOff
					mov al, Row_Size
					mul Row_Index
					add ESI, EAX
					mov cl, [ESI + EDX]
					mov al, Playa_Num
					cmp cl, al
					je YouWin
					inc EDX
					inc EDX
					dec Row_Index
					dec Row_Index
					
				t_check:
					cmp dl, 03h
					je Diagonal_1
					inc EDX
					dec Row_Index
					mov EAX, 0
					mov ESI, holdOff
					mov al, Row_Size
					mul Row_Index
					add ESI, EAX
					mov cl, [ESI + EDX]
					mov al, Playa_Num
					cmp cl, al
					je YouWin
					cmp dl, 3h
					je a_Cleaning
					inc Row_Index
					inc Row_Index
					jmp Diagonal_1
	
	a_Cleaning:
		mov Row_Index, 1h
		mov EAX, 0h
		mov EDX, 0h
		mov EBX, 0h
		mov ECX, 0h


        ;Diagonal_2 does the same thing as diagonal_1 but
        ;instead doing top left to bottom right.

	Diagonal_2:
		mov ESI, holdOff
		mov al, Row_Size
		mul Row_Index
		add ESI, EAX
		mov cl, [ESI + EDX]
		mov al, Playa_Num
		cmp cl, al
		je lastCheck
		inc EDX
		cmp dl, 4h
		je NoWinner
		jmp Diagonal_2
		
			lastCheck:
				mov eax, 0
				mov ESI, holdOff
				inc EDX
				inc Row_Index
				mov al, Row_Size
				mul Row_Index
				add ESI, EAX
				mov cl, [ESI + EDX]
				mov al, Playa_Num
				cmp cl, al
				je bd_check
				dec Row_Index
				jmp Diagonal_2
				
				bd_check:
					cmp dl, 03h
					je td_check
					inc Row_Index
					inc EDX
					mov eax, 0
					mov ESI, holdOff
					mov al, Row_Size
					mul Row_Index
					add ESI, EAX
					mov cl, [ESI + EDX]
					mov al, Playa_Num
					cmp cl, al
					je YouWin
					dec Row_Index
					dec EDX
					cmp dl, 01h
					jne td_check
					dec Row_Index
					jmp Diagonal_2
				
				td_check:
					dec Row_Index
					dec Row_Index
					dec EDX
					dec EDX
					mov EAX, 0
					mov ESI, holdOff
					mov al, Row_Size
					mul Row_Index
					add ESI, EAX
					mov cl, [ESI + EDX]
					mov al, Playa_Num
					cmp cl, al
					je YouWin
					
	NoWinner:
		mov eax, 0
		jmp itsover
	
	YouWin:
		mov eax, 17h
		
itsover:
pop ecx
ret
CheckMatr ENDP


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Desc:   This PROC randomly chooses a column and drops in a
;        a certain value depending on what ECX holds in it.
;Recieves:  ESI - offset to matr
;           EAX - RowSize of matr
;           ECX - holds a value which lets the PROC know which block color to drop in
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
Computer PROC
.data
	Comp_Num BYTE 0h
	aa_off DWORD 0h
	Size_row BYTE 0h
	index_row BYTE 0h
	
.code
	push ecx
	mov index_row, 3h               ;index_row is set to 3h to point to the very bottom row
	mov aa_off, ESI
	mov Size_row, al
	mov EDX, 0
	cmp cl, 0
	ja cmp_2                        ;depending on what ECX holds mov the appropriate value
                                    ;into Comp_Num
	
	mov cl, 1
	mov Comp_Num, cl
	jmp startit
	
	cmp_2:
		mov cl, 14
		mov Comp_Num, cl
		
	startit:
		mov EAX, 0
		mov ECX, 0
		mov EDX, 0
		mov eax, 500
		call RandomRange           ;RandomRange generates a random number up to 500
		mov cl, 3h
		DIV cl                     ;This random number is DIV by 3h to randomly get a 0, 1, 2, or 3 in the ah register
		cmp ah, 0
		je first_col
		cmp ah, 1                  ;this value in the ah register lets the PROC know which column to drop the Comp_Num in
		je second_col
		cmp ah, 2
		je third_col
		cmp ah, 3
		je fourth_col
		
		first_col:
			mov EAX, 0
			mov ESI, aa_off
			mov al, Size_row
			mul index_row
			add ESI, EAX
			mov cl, [ESI + EDX]
			cmp cl, 20h
			je thatsit
			mov al, index_row
			cmp al, 0h
			je nope                 ;if the column is full jmp to nope
			dec index_row
			jmp first_col
			
		second_col:
			mov ESI, aa_off
			mov al, Size_row
			mul index_row
			add ESI, EAX
			mov EDX, 1
			mov cl, [ESI + EDX]
			cmp cl, 20h 
			je thatsit              ;if a slot is empty jmp to thatsit
			mov al, index_row
			cmp al, 0h
			je nope_2               ;if the second column is full jmp to nope_2
			dec index_row
			jmp second_col
			
		third_col:
			mov ESI, aa_off
			mov al, Size_row
			mul index_row
			add ESI, EAX
			mov EDX, 2
			mov cl, [ESI + EDX]
			cmp cl, 20h 
			je thatsit              ;if a slot is empty jmp to thatsit
			mov al, index_row
			cmp al, 0h
			je nope_3               ;if the third column is full jmp to nope_3
			dec index_row
			jmp third_col
			
		fourth_col:
			mov ESI, aa_off
			mov al, Size_row
			mul index_row
			add ESI, EAX
			mov EDX, 3
			mov cl, [ESI + EDX]
			cmp cl, 20h
			je thatsit              ;if a slot is empty jmp to thatsit
			mov al, index_row
			cmp al, 0h
			je nope_4               ;if the fourth column is full jmp to nope_4
			dec index_row
			jmp fourth_col
			
		thatsit:
			mov cl, Comp_Num        ;stores Comp_Num in the empty slot that was found
			mov [ESI + EDX], cl
			jmp itsin               ;jmp to itsin ends the PROC
nope:
	mov index_row, 3h               ;if the first column is full mov index_row back to the last row
	jmp second_col                  ;and jmp to second_col

nope_2:
	mov index_row, 3h               ;if the second column is full mov index_row back to the last row
	jmp third_col                   ;and jmp to third_col
	
nope_3:
	mov index_row, 3h               ;if the third column is full mov index_row back to the last row
	jmp fourth_col                  ;and jmp to fourth_col
	
nope_4:
	mov ESI, aa_off
	mov al, Size_row
	mov index_row, 0h
	mul index_row
	add ESI, EAX
	mov EDX, 1h
	mov cl, [ESI + EDX]
	mov index_row, 3h
	cmp cl, 20h
	je second_col
	mov EAX, 0                      ;if all columns are full, mov 42h into EAX and end the PROC
	mov EAX, 42h
itsin:
pop ECX
ret
Computer ENDP


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Desc:  This PROC removes any values that are not 20h
;       from the gameboard
;Recieves:  ESI - offset to matr
;           EAX - RowSize of matr
;Returns:   a matr with only 20h for values
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
cleanUp PROC
.data 
	a_rowSize BYTE 0h
	a_rowIndex BYTE 0h
	an_Offset DWORD 0h

.code
	mov a_rowSize, 0h
	mov a_rowIndex, 0h
	mov an_Offset, ESI
	mov a_rowSize, al
	mov EDX, 0
	mov EAX, 0
	mov ECX, 0
	
	L_1:
		mov ESI, an_Offset
		mov al, a_rowSize
		mul a_rowIndex
		add ESI, EAX
		mov cl, [ESI + EDX]
		cmp cl, 20h
		jne fillin
		inc EDX
		cmp dl, 4h
		jne L_1
		inc a_rowIndex
		mov EDX, 0
		mov cl, a_rowIndex
		cmp cl, 4h
		je finished
		jmp L_1
		
		fillin:
			mov cl, 20h
			mov [ESI + EDX], cl
			inc EDX
			cmp dl, 4h
			jne L_1
			inc a_rowIndex
			mov EDX, 0
			mov cl, a_rowIndex
			cmp cl, 4h
			je finished
			jmp L_1
finished:
ret
cleanUp ENDP
				
END main
