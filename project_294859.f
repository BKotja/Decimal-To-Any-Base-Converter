        program main
              integer i, b

              print *, "This is the program for converting decimal"
              print *, "integer numbers into any base from 2 to 32"
              print *, "Created by Vsevolod Panasov (294859)"
              print *, "Press Ctrl-D to exit"
              print *, ''

              do while(.True.) 
                print *,"Please enter integer number in decimal system:"
                read (*,*,end=1), i
                print *, "Please enter new base (2 - 32): "
                read (*,*,end=1), b
                call ConvertBase(i, b)
              enddo
1       endprogram main
       
        subroutine ConvertBase(Num, Base)
                integer Num, tmpNum
                integer Base, tmpBase
                character(len=26) :: Letters
                character(len=512) :: strBin
                character, dimension(:), allocatable :: Res(:)
                character(len=4) :: tmpStr
                integer, dimension (512) :: tmpRes
                integer MaxBit
                logical isNegative
                integer zeroCount

                zeroCount = 0
                isNegative = .False.
                MaxBit = 512
                Letters = 'ABCDEFGHIJKLMNOPQRSTUVQXYZ'
                strBin = ''
                
                tmpNum = Num
                tmpBase = Base
                

                if (tmpNum == 0) then
                    print *, "Result: 0"
                    print *, ''
                    print *, ''
                    return
                else if (tmpNum < 0) then
                        isNegative = .True.
                        tmpNum = tmpNum * (-1)
                endif
                

                do while (tmpNum > 0)
                        rem = Mod(tmpNum, tmpBase)
                        tmpRes(MaxBit) = rem
                        MaxBit = MaxBit - 1
                        tmpNum = tmpNum / tmpBase
                enddo
                

                do i=512, MaxBit+1, -1
                   if(tmpRes(i) >= 10) then
                     write(tmpStr,10)Letters(tmpRes(i)-9:tmpRes(i)-9)
10                   format (A4)
                     strBin=trim(adjustl(tmpStr))//trim(adjustl(strBin))
                   else
                     write(tmpStr , 20) tmpRes(i)
20                   format (I4)
                     strBin=trim(adjustl(tmpStr))//trim(adjustl(strBin))
                   endif
                enddo
                
              if(isNegative.eqv..True.)strBin='-'//trim(adjustl(strBin))

                print *, "Result: ", strBin
                
        endsubroutine
