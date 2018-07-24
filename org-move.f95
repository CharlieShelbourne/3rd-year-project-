   do n=1,mill
    do k=zTop-1,zBottom+2
       do j=1,nj

           if(array(i,j,k)==orgNum) then
               array(i,j,k-1)= array(i,j,k)
               !print*, array(i,j,k),array(i-1,j,k)


           else if(array(i,j,k)/=orgNum .and. array(i,j,k-1)==orgNum) then
                 if( array(i,j,k)==18 .or. array(i,j,k)==23 .or. array(i,j,k)==33 .or. array(i,j,k)==55)then
                    array(i,j,k-1)=48
               else
                    array(i,j,k-1) = array(i,j,k)
             
               end if
              
            end if
        end do
    end do
    zTop = zTop -1
    zBottom = zBottom - 1
   end do
