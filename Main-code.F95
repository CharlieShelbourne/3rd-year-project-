program Main
  
    implicit none

    
    INTEGER, DIMENSION(:, :, :), ALLOCATABLE :: array
    integer :: i=0,j=0,k=0,n=0,loopArray=0,p=0,q=0,s=0,h=0,t=0,j0=0,j1=0,x=0
    integer :: zTop=0, zBottom=0, heartBottom=0, heartTop=0, heartBottom_1=0
    integer :: leftLungTop=0,leftLungBottom=0,rightLungBottom=0, rightLungTop=0, rightLungBottom_1=0,leftLungBottom_1=0
    integer :: heartBottomOriginal=0, jOrg=0, diaphragmLeft =0, diaphragmRight =0, diaphragm =0;
    integer :: liverTop=0, liverBottom =0, orgNum=0, jright=0, jleft=0
    integer :: kleft_1=0, kright_1=0,kleft_2=0, kright_2=0, ileft =0 ,iright =0
    integer :: OrigLeftLungPix =0, OrigRightLungPix =0,NewRightLungPix =0, NewLeftLungPix=0, RightLungVol =0, LeftLungVol =0
    real :: r=0
    
 
    integer :: mill=45
    
    integer :: ni,nj,nk,err
    ni=265
    nj=490
    nk=1000
    
    ALLOCATE (array(ni,nj,nk))
    !print*, 'Enter reduction amount in mm'
    !read*, mill
    
    include '3D-array-2.f95'
go to 100
    
    !i=150
    do i=1,ni-1
        print*,'slice number',i

       
!--------------------------------------------------------------------------------------------------------------
    !FIND HEART AND COUNT PIXELS IN LUNGS
!--------------------------------------------------------------------------------------------------------------
        h=0 
        do k=1,nk 
            do j=1,nj
                if(array(i,j,k)==24) then    !NEW
                    h=1
                else if(array(i,j,k)== 85)then
                    OrigLeftLungPix = OrigLeftLungPix +1
                else if(array(i,j,k)== 27)then
                    OrigRightLungPix =  OrigRightLungPix +1
                end if 
            end do
        end do
        
        
        
!-------------------------------------------------------------------------------------------------------------
!FIND LEFT LUNG BOTTOM
!-------------------------------------------------------------------------------------------------------------
        
        orgNum = 85
        include 'org-size.f95'
        leftLungTop = zTop
        leftLungBottom_1=zBottom
       ! print*, 'leftlung z top', zTop
       ! print*, 'rightlung z bottom', zBottom
!-------------------------------------------------------------------------------------------------------------
!FIND RIGHT LUNG BOTTOM
!-------------------------------------------------------------------------------------------------------------
        orgNum = 27
        include 'org-size.f95'
        rightLungTop = zTop
        rightLungBottom_1=zBottom
       ! print*, 'leftlung z top', zTop
       ! print*, 'rightlung z bottom', zBottom
              

    
        if (h/=0)then
    
            orgNum = 24
            include 'org-size.f95'
                
            heartBottomOriginal = zBottom
            heartBottom = zBottom
            heartBottom_1=zBottom
            heartTop = zTop 
            

    
!-----------------------------------------------------------------------------------------------------------  
! MOVE HEART UP 20mm
!----------------------------------------------------------------------------------------------------------   
     
        
            zTop = heartTop
            zBottom = heartBottom
            orgNum = 24
     
            include 'org-move.f95'
                
    
!-------------------------------------------------------------------------------------------------------
!FIND HEART 
!----------------------------------------------------------------------------------------------------------
             
            orgNum = 24 
          
            include 'org-size.f95'
            heartBottom = zBottom
            heartTop = zTop 
              

!-------------------------------------------------------------------------------------------------------------
!FIND LEFT LUNG 
!-------------------------------------------------------------------------------------------------------------
          
            orgNum = 85
            include 'org-size.f95'
            leftLungTop = zTop
            leftLungBottom = zBottom

!-------------------------------------------------------------------------------------------------
!MOVE LEFT LUNG UP
!--------------------------------------------------------------------------------------------------
                
            
            orgNum = 85
            if(array(i,jOrg,heartBottomOriginal+3)==85)then          !NEW
                zTop= leftLungTop
                zBottom = leftLungBottom
                include 'decrease-lung.f95'

            endif 

   
        end if
 
!-------------------------------------------------------------------------------------------------------------
!FIND LEFT LUNG BOTTOM
!-------------------------------------------------------------------------------------------------------------

!            orgNum = 85
!            include 'org-size.f95'
!            leftLungTop = zTop
!            leftLungBottom = zBottom
!            leftLungBottom_1=zBottom
!----------------------------------------------------------------------------------------------------
!Move Left lung up 
!----------------------------------------------------------------------------------------------------
   
    !If lung is present 
!           if(leftLungBottom_1/= 0 .and. leftLungBottom_1 <= 375)then
!                print*,'4'
!     !if(leftLungBottom==leftLungBottom_1)then
!                do n=1, mill
!                    do  j=480,10,-1
!                        do k= nk, 1, -1
!                            if (array(i,j,k) == 85)then
!                                array(i,j,k)= 48
!                                exit
!                            end if
!                        end do
!                    end do
!                end do
    !end if
        
!            endif

!-------------------------------------------------------------------------------------------------------------
!FIND RIGHT LUNG BOTTOM
!-------------------------------------------------------------------------------------------------------------
!            orgNum = 27
!            include 'org-size.f95'
!            rightLungTop = zTop
!            rightLungBottom = zBottom
!            rightLungBottom_1=zBottom
!-----------------------------------------------------------------------------------------------------        
!Move Right lung up
!-----------------------------------------------------------------------------------------------------
!            if(rightLungBottom_1 /=0 .and. rightLungBottom_1 <= 375)then  
   !if(rightLungBottom==rightLungBottom_1)then
!                print*,'6'
!                do n = 1,mill
!                    do  j=10,480  
!                        do k= nk, 1, -1
!                            if (array(i,j,k) == 27)then
!                                array(i,j,k)= 48
!                                exit
!                            end if
!                        end do
!                    end do
!                end do
   !end if
!           end if   
        
        !end if
    

!--------------------------------------------------------------------------------------------------------------
!find edges of diaphragm
!-------------------------------------------------------------------------------------------------------------
        
    
    orgNum = 55
    !include 'find-diaphragm.f95'
    diaphragm =0
    !Far left
    do j=1,nj
         
            do k=nk,2,-1
         
                if(array(i,j,k)==55) then
               
                    jleft=j
                    kleft_1=k
                    diaphragm = 1  !1 if diaphragm is apparent in slice 
                    if(jleft>245 .and. jleft<490)then
                        diaphragmLeft =1
                    else 
                        diaphragmLeft =0
                    end if 
                    exit
                else 
                    diaphragm =0  ! o if diaphragm not apparent in slice 
                end if
            end do
            if(array(i,j,k)==55)then
                exit
            end if
        end do
       ! print*, k,jleft, 'diphragm left'
  
 
  
!Far right
        do j=nj,1,-1
            do k=nk,2,-1
                if(array(i,j,k)==55) then
                    jright = j
                    kright_1 =k
                    if(jright>245 .and. jright<490)then
                        diaphragmRight =1
                        kright_2 =k
                    else 
                        diaphragmRight =0
                    end if
                    exit
                end if
            end do
            if(array(i,j,k)==55)then
                exit
            end if
        end do
       ! print*, k,jright, 'diphragm right'
    
       
 

!--------------------------------------------------------------------------------------------------------
 !Reduce back of LEFT lung
 !---------------------------------------------------------------------------------------------------------
    
 if((kleft_1 /=0 .or. kright_1/=0))then
     
    orgNum = 85
    include 'org-size.f95'
    leftLungTop = zTop
    leftLungBottom = zBottom
  
  !  print*, leftLungTop,j , 'left lung top'
  !  print*, leftLungBottom,j,'left lung bottom'
    
    if(i>178 .and. (leftLungBottom >= (kleft_1-(mill))))then 
    !if(leftLungBottom >= (kleft_1-mill))then

     !if(leftLungBottom==leftLungBottom_1)then
     do n=1, mill
       do  j=480,10,-1
        do k= nk, 1, -1
                if (array(i,j,k) == 85)then
                    array(i,j,k)= 48
                    exit
                end if
            end do
        end do
     end do
    !end if
  
    endif

!-----------------------------------------------------------------------------------------------------------------------
 !Reduce back RIGHT lung
!----------------------------------------------------------------------------------------------------------------------
    orgNum = 27
    include 'org-size.f95'
    rightLungTop = zTop
    rightLungBottom = zBottom
   
 !  print*, leftLungTop,j , 'left lung top'
 !   print*, rightLungBottom,j,'right lung bottom'
    
   
  if(i>178 .and. rightLungBottom >= (kright_2-(mill-10)))then  
   !if(rightLungBottom==rightLungBottom_1)then
  
    do n = 1,mill
      do  j=10,480  
       do k= nk,1, -1
            if (array(i,j,k) == 27)then
                array(i,j,k)= 48
                exit
            end if
        end do
      end do
    end do
   !end if
  end if   
 end if 
 
 !diaphragm = 0 
 
 
   
    
    

!---------------------------------------------------------------------------------------------------------- 
!un-flatten diaphragm  
!----------------------------------------------------------------------------------------------------------

  
    do n=1,mill
        do k=10,nk
            do j=5,nj
            if(array(i,j,k) == 55)then 
                array(i,j,k-1)= 55
                array(i,j,k)=48
            end if

            end do
        end do
    end do

    
  
    !ADD diaphragm left
    if((jleft/=0 .and. jright/=0).and.(jleft/=490 .and. jright/=490))then
      do k=kleft_1-mill,kleft_1
            CALL RANDOM_NUMBER(r)
            t=r*10 
        
        if(t<3)then
            array(i,jleft,k) = 55    
      
        elseif(t>=3 .and. t<6)then
            array(i,jleft,k)= 55    
            array(i,jleft-1,k) = 55
         
        elseif(t>=6)then     
            array(i,jleft,k)= 55    
            array(i,jleft+1,k) = 55
         
           
          
        endif
  
      end do
    
      
! Add diaphragm right       
    do k=kright_1-mill,kright_1
        CALL RANDOM_NUMBER(r)
        t=r*10 
        if(t<3)then
            array(i,jright,k) = 55
          
            
        elseif(t>=3 .and. t<6)then
            array(i,jright,k)= 55    
            array(i,jright+1,k) = 55
           
        elseif(t>=6)then 
            array(i,jright,k) = 55
            array(i,jright-1,k) = 55
        
      
        end if      
    end do
    
    end if 
    
 

 !-------------------------------------------------------------------------------------------------------------
 !FIND SIZE OF LIVER 
 !--------------------------------------------------------------------------------------------------------------
   
  
    orgNum = 26
    include 'org-size.f95'
    liverTop = zTop
    liverBottom = zBottom
    
   ! print*, liverTop,j , 'liver top'
   ! print*, liverBottom,j,'liver bottom'

 
 
   
!------------------------------------------------------------------------------------------------------------
!MOVE LIVER UP
!-------------------------------------------------------------------------------------------------------------
  
    if(liverTop /=0 .and. liverBottom /=0)then
    
    zTop = liverTop
    zBottom = liverBottom
    orgNum = 26
     
    !include 'org-move.f95' 
    do n=1,mill
    !do k=zTop-1,zBottom+2
     do k=2,nk 
       do j=1,nj

           if(array(i,j,k)==orgNum) then
               if((array(i,j,k-1).NE.55) .and. (j.GE.jleft) .and. (array(i,j,k-1).NE.45))then
                array(i,j,k-1)= array(i,j,k)
               end if



           else if(array(i,j,k) .NE. orgNum .and. array(i,j,k-1)==orgNum) then
                 if(array(i,j,k)==23 .or.array(i,j,k)==18 .or. array(i,j,k)==33)then
                    array(i,j,k-1)=48
                 
                else
                    array(i,j,k-1) = array(i,j,k)
             
               end if
              
            end if
        end do
    end do
    !zTop = zTop -1
    !zBottom = zBottom - 1
   end do
   end if
 
 
 !----------------------------------------------------------------------------------------------------------------
 !Move Stomach Organs 
 !----------------------------------------------------------------------------------------------------------------
   
do n=1,mill
    do k=2,nk  !k starts from 2 to avoid an access outside of memory
        do j=1,nj
            !if(array(i,j,k-1).NE.45)then        
           !Move Stomach, Spleen, Pancreas
                if(array(i,j,k)==33 .or. array(i,j,k)==32 .or. array(i,j,k)==29 .or. array(i,j,k)==20) then
                    array(i,j,k-1)= array(i,j,k)
                     
           !Move small intestine, large intestine 
                elseif((array(i,j,k)==31 .or. array(i,j,k)==18) .and. k.LE.900)then
                    array(i,j,k-1)= array(i,j,k)
                !elseif((array(i,j,k)==31 .or. array(i,j,k)==18) .and. (k>700 .and. (j>300 .or. j<200)))then
                !    array(i,j,k-1)= array(i,j,k)
            
           !Move left kidney, right kidney,    
                else if((array(i,j,k)==84 .or. array(i,j,k)==25) .and. n.LT.(mill*0.7))then
                    array(i,j,k-1)= array(i,j,k)
         
            !Move bottom of organs     
                elseif(array(i,j,k-1)==33 .or. array(i,j,k-1)==32 .or. array(i,j,k-1)==29 .or. array(i,j,k-1)==20) then
                    array(i,j,k-1) = array(i,j,k)
            
                elseif((array(i,j,k-1)==31 .or. array(i,j,k-1)==18) .and. (k.LE.900))then
                    array(i,j,k-1)= 48
                
                !elseif((array(i,j,k-1)==31 .or. array(i,j,k-1)==18).and.(k.GT.700 .and. (j.GT.300 .or. j.LT.200)))then
                !    array(i,j,k-1)= array(i,j,k)
                
                elseif((array(i,j,k-1)==84 .or. array(i,j,k-1)==25) .and. n.LT.(mill*0.7))then
                    array(i,j,k-1) = array(i,j,k)
                 
         
                end if
            !end if   
        end do
    end do
end do
   

 

  

   
   
 !--------------------------------------------------------------------------------------------------------
 !Fix LEFT lung
 !---------------------------------------------------------------------------------------------------------
    
 if(diaphragm == 1 .and. leftLungBottom /=0)then
     
 !   orgNum = 85
 !   include 'org-size.f95'
 !   leftLungTop = zTop
 !   leftLungBottom = zBottom
  
  !  print*, leftLungTop,j , 'left lung top'
  !  print*, leftLungBottom,j,'left lung bottom'
    
 !   if(i>178 .and. (leftLungBottom >= (kleft_1-mill)))then 
    !if(leftLungBottom >= (kleft_1-mill))then
 !       print*,'4'
     !if(leftLungBottom==leftLungBottom_1)then
 !    do n=1, mill
       
        do k= nk, leftLungBottom_1-mill, -1
            do  j=480,1,-1
                if (array(i,j,k) == 85)then
                    array(i,j,k)= 48
 !                   exit
                end if
            end do
        end do
 !    end do
    !end if
  
!   endif

!-----------------------------------------------------------------------------------------------------------------------
   !Fix RIGHT lung
!----------------------------------------------------------------------------------------------------------------------
 !   orgNum = 27
 !   include 'org-size.f95'
 !   rightLungTop = zTop
 !   rightLungBottom = zBottom
   
 !  print*, leftLungTop,j , 'left lung top'
 !   print*, rightLungBottom,j,'right lung bottom'
    
   
 ! if(i>178 .and.(rightLungBottom-5) >= (kright_1-mill))then  
   !if(rightLungBottom==rightLungBottom_1)then
 !     print*,'6'
 !   do n = 1,mill
        
       do k= nk,rightLungBottom, -1
           do  j=1,480
            if (array(i,j,k) == 27)then
                array(i,j,k)= 48
 !               exit
            end if
        end do
      end do
 !   end do
   !end if
 ! end if   
 end if 

 !diaphragm = 0 
 
!--------------------------------------------------------------------------------------------------------------
!REDUCE LUNG Lateral
!------------------------------------------------------------------------------------------------------------- 
 
 
! do n=1,mill-40    
!   do k=1,nk 
!      do j=490,-1,245
!         if (array(i,j,k)==85)then
!                array(i,j,k)=48
!                exit
!            end if 
!        end do
!    end do
!    do k=1,nk 
!      do j=1,245
!         if (array(i,j,k)==27)then
!                array(i,j,k)=48
!                exit
!            end if 
!       end do
!    end do
   
!  end do

 end do
 
 
 
!----------------------------------------------------------------------------------------------------------------
!Start of Sagittal Movement 
!--------------------------------------------------------------------------------------------------------------- 
   do j=1, nj
       
       
     
       
!--------------------------------------------------------------------------------------------------------------
!REDUCE LUNG FROM CHEST WALL 
!------------------------------------------------------------------------------------------------------------- 
!  do k=1,nk  
!    do i=1,ni 
!        if(array(i,j,k)==85)then 
!            leftLungTop = array(i,j,k)
!            x=1
!            exit
!        else if(array(i,j,k)==27)then 
!            rightLungTop = array(i,j,k)
!            x=1
!            exit
!        end if
!    end do
!    if(x==1)then 
!        x=0
!        exit 
!    end if 
!  end do
        
!if(leftLungTop/=0)then                  
!   do n=1,5  
!    do k=leftLungTop,nk  
!      do i=1,130 
!         if (array(i,j,k)==85)then
!                array(i,j,k)=48
!                exit
!            end if 
!        end do
!    end do
!  end do
!  leftLungTop =0
  
  
!else if(rightLungTop/=0)then
 
!   do n=1,5    
!    do k=rightLungTop,nk  
!      do i=1,130 
!         if (array(i,j,k)==27)then
!                array(i,j,k)=48
!                exit
!            end if 
!        end do
!    end do
!  end do
!  rightLungTop =0
!end if   
  
!-------------------------------------------------------------------------------------------------------------
!COUNT LUNG PIXELS  
!--------------------------------------------------------------------------------------------------------------

   do k=1,nk 
    do i=1,ni
      
        if(array(i,j,k)== 85)then
            NewLeftLungPix = NewLeftLungPix +1
        else if(array(i,j,k)== 27)then
            NewRightLungPix =  NewRightLungPix +1
        end if 
    end do
   end do

 
 
      
!-------------------------------------------------------------------------------------------------------------- 
!find edges of diaphragm
!-------------------------------------------------------------------------------------------------------------
 

     
     

    orgNum = 55
    !include 'find-diaphragm.f95'
    
    !Far left
    do i=10,130
      do k=nk,2,-1
            if(array(i,j,k)==55) then
                kleft_2=k
                ileft=i
                exit
            end if
        end do
        if(array(i,j,k)==55)then
            exit
        end if
    end do
 ! print*, i,jleft, 'diphragm left'
  
!Far right
   do k=nk,2,-1
      do i=90,264   
            if(array(i,j,k)==55) then
                kright_2 = k
                iright = i
                exit
            end if
      end do
        if(array(i,j,k)==55)then
            exit
        end if
    end do
    
  !  print*, k,iright,'diaphragm right'
!---------------------------------------------------------------------------------------------------------- 
!un-flatten diaphragm  
!----------------------------------------------------------------------------------------------------------
    
   
 
    
    !ADD diaphragm left
    if(kleft_2<500)then 
        if((ileft/=0 .and. iright/=0).and.(ileft/=265 .and. iright/=265))then
            do k=kleft_2,kleft_2+mill
            if(array(ileft,j,k)==26)then     !Add diaphragm to outside of liver 
                do i=1,130
                if(array(i,j,k)==26 .and. array(i-1,j,k)/=26)then
                  array(i-1,j,k)=55
                  !array(i-2,j,k)=55
                end if
                end do
  
            else 
                CALL RANDOM_NUMBER(r)
                t=r*10 
        
                if(t<3)then
                    array(ileft,j,k) = 55    

                elseif(t>=3 .and. t<6)then
                    array(ileft,j,k)= 55    
                    array(ileft-1,j,k) = 55
         
                elseif(t>=6)then     
                    array(ileft,j,k)= 55    
                    array(ileft+1,j,k) = 55
                endif
            end if
        end do
        
        
      
    
    !  do n=kright,kright+mill
     !     do k=kright-2,kright
      !        do i=130,265
       !       if(array(i,j,k)==55 .and.array(i-1,j,k)/=55)then
        !          array(i-1,j,k)=55
         !     endif 
          !    if(array(i,j,k)==55 .and.array(i,j,k+1)/=55)then
           !       array(i,j,k+1)=55
      !        end if 
      !      end do
       !   end do
     
   ! end do

       end if   
    end if
   

end do

!------------------------------------------------------------------------------------------------------------------------ 
!Write documents
!------------------------------------------------------------------------------------------------------------------------
100 continue 
do n=2017,2022  
    if(n>=2021)then
          write(n,'(a)')'P2'
          write(n,'(a)')'# test'
          write(n,'(a)')'265 1687'
          write(n,'(a)')'94'
    else
        write(n,'(a)')'P2'
        write(n,'(a)')'# test'
        write(n,'(a)')'490 1687'
        write(n,'(a)')'94'
  end if

end do

    do k=1,nk
        do j=1,nj

            write(2017,*) int(array(50,j,k)) !Coronel Depth 5cm 

        end do
    end do
   

  !Coronel 50
   do k=1,nk
        do j=1,nj

            write(2018,*) int(array(100,j,k))  !Coronel Depth 10 cm

  !Coronel 100                 
        end do
    end do
    
    
     do k=1,nk
        do j=1,nj

            write(2019,*) int(array(150,j,k)) !Coronel Depth 15cm

        end do
    end do
    
   
   

     do k=1,nk
        do j=1,nj

            write(2020,*) int(array(190,j,k)) !Coronel Depth 19cm

        end do
    end do
    
  
  
 
    do k=1,nk
       do i=1,ni

            write(2021,*) int(array(i,160,k))  !Sagittal depth 16cm 

        end do
    end do
    
    do k=1,nk
       do i=1,ni

            write(2022,*) int(array(i,350,k))   !Sagittal depth 35cm 

        end do
    end do

    
    
   print*,'Left lung original volume ' ,OrigLeftLungPix
   print*, 'Right Lung original volume ',OrigRightLungPix
    
   
        
   print*,'Left lung new volume ' , NewLeftLungPix
   print*,'Right lung new volume' , NewRightLungPix   
        
  
  

end program Main




