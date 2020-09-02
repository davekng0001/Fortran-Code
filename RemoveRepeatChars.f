
c input: 10 character string
c removerepeats(): take input and remove all repeating characters and print new character string


        program stringfun
        
            integer :: i,l,cnt
            character(Len=10) string
             l = 10

            print*,"input an 10 character string:"
            read(*,*) string
            print*,"You choose: ",string
            print*,"Removing repeated characters from string"
            
            call removerepeats(string,l)


        end program stringfun


        subroutine removerepeats (string, l)

            
            integer :: i,l, cnt
            character (Len=l)string
            character (Len=l) save

            cnt = 1;

            do i= 1,l
                if(i > 1 .and. i < l) then

                     if( string(i-1:i-1) /= string(i:i)) then
    
                        if (string(i+1:i+1) /= string(i:i)) then

                            save(cnt:cnt) = string(i:i)

                            cnt = cnt + 1
                        end if

                     end if

                end if

                if (i == l .and. string(i:i) /= string(i-1:i-1)) then
                    save(cnt:cnt) = string(i:i)
                end if

                if ( i == 1 .and. string(i:i) /= string(i+1:i+1) ) then
                    save(cnt:cnt) = string(i:i)
                    cnt = cnt +1

                end if


            end do

            print*,"This is your new character string"
            print*," ", save



        end subroutine removerepeats
