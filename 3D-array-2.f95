!--------------------------------------------------------------------------------------------------------------------
 !EXTRACT DATA TO ARRAY
 !----------------------------------------------------------------------------------------------------------------------
    
    !open file with riken 3D coronel slice
    open(unit=2014,             &
           file='fort.2014',         &
           status="old",        &
           iostat=err)

    if (err.NE.0) then
        print *, "Error reading file: 'fort.2014'"
        stop
    endif

    

    !read data into a 3D array
  do k=1,nk 
        do j=1,nj
            do i=1,ni
            read(2014,*) array(i,j,k)
        end do
    end do
    s=s+1
    print*,s
  end do
  
   
    close(unit=2014)
    

    


