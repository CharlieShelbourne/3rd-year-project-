!----------------------------------------------------------------------
!Decrease Left Lung only 
!----------------------------------------------------------------------
do n=1,mill
    do k=heartBottomOriginal-(15),zBottom
       do j=1,nj

!check for lung tissue
           if(array(i,j,k)==orgNum) then
!copy lung tissue into pixel above
               if(array(i,j,k-1)/= 45)then
               array(i,j,k-1)= array(i,j,k)
               end if
!To replace the lung tissues at the bottom of the lung, check tissue above is lung tissue 
           else if(array(i,j,k)/=orgNum .and. array(i,j,k-1)==orgNum) then  
!if diaphragm replace pixel above with fat tissues, else replace with tissues below  
               if(array(i,j,k)==55)then 
                array(i,j,k-1)=48
               else
!else replace lung tissue with tissues below 
                array(i,j,k-1) = 48
               end if
            end if
       end do
    end do
    heartBottomOriginal = heartBottomOriginal-1
    zBottom=ZBottom-1
end do

!-------------------------------------------------------------------------------------------------------------
!Smooth lung 
!------------------------------------------------------------------------------------------------------------

do n=1,2
    do k=heartBottomOriginal-(mill-28), zBottom
        do j=490,1,-1
            if(array(i,j,k)==45)then 
                exit
            else if (array(i,j,k)==orgNum)then
                array(i,j,k)=48
                exit
            end if 
        end do
    end do
end do
        



               
		





  

