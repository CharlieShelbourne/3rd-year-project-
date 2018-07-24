 
!TOP
do k=nk,1,-1
       do j=1,nj
            if(array(i,j,k)==orgNum) then
            zTop=k
            exit
            end if
        end do

    end do
  
!BOTTOM
     do k=1,nk
       do j=1,nj
            if(array(i,j,k)==orgNum) then
            zBottom=k
            jOrg=j
            exit
            end if
        end do

    end do