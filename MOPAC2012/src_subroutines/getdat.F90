      subroutine getdat(input, output) 
!-----------------------------------------------
!   M o d u l e s 
!-----------------------------------------------
      use molkst_C, only : natoms, jobnam, run, ijulian, verson, &
      gui, line, ncomments, is_PARAM
      use chanel_C, only : iw0, job_fn, input_fn, iw
      use common_arrays_C, only : all_comments
!...Translated by Pacific-Sierra Research 77to90  4.4G  22:53:42  03/15/06  
!...Switches: -rl INDDO=2 INDIF=2 
!-----------------------------------------------
!   I n t e r f a c e   B l o c k s
!----------------------------------------------- 
      use mopend_I 
      implicit none
!-----------------------------------------------
      integer, intent (in) :: input, output
!-----------------------------------------------
!   L o c a l   V a r i a b l e s
!-----------------------------------------------
      integer, parameter :: from_data_set = 7
      integer :: i, j, io_stat, l, nlines
      logical :: exists, arc_file, comments = .true.
      character :: text*90, line1*400
      character, allocatable :: tmp_comments(:)*80
      external getarg
      integer, external :: iargc
      save i 
!-----------------------------------------------
!
!***********************************************************************
!
!   GETDAT READS IN ALL THE DATA USING CHANEL "from_data_set", AND WRITES IT
!   TO SCRATCH CHANNEL "input".  THIS WAY THE ORIGINAL DATA-SET IS
!   FREED UP AS SOON AS THE JOB STARTS.
!******************************************************************** 
      natoms = 0 
      call to_screen("To_file: getdat")
      if (gui) then
        jobnam = "MOPAC input"
        natoms = 1
      else
        if (run /= 2 .or.jobnam ==" ") then
          i = iargc()
          if (i >= run) then
            call getarg (run, jobnam)
            natoms = 1
            do i = len_trim(jobnam), 1, -1   !  Remove any unprintable characters from the end of the file-name
              if (ichar(jobnam(i:i)) > 39 .and. ichar(jobnam(i:i)) < 126) exit
            end do
            jobnam(i + 1:) = " "
          else if (i == 0) then
            if (is_PARAM) then
              write(line,'(2a)')" PARAM is the parameter optimization program for use with MOPAC"
              write(0,'(//10x,a,/)')trim(line)
              call mopend(trim(line))
              write(0,'(10x,a)')" It uses a single argument, the PARAM data-set"
              write(0,'(10x,a)')" The command to run PARAM is 'PARAM.exe <data-set>.dat'"
             ! call web_message(0,"running_MOPAC.html")
              write(0,'(10x,a)')" Press (return) to continue"
              read(5,*, iostat=i) 
              return
            else
              write(line,'(2a)')" MOPAC is a semiempirical quantum chemistry program"
              write(0,'(//10x,a,/)')trim(line)
              call mopend(trim(line))
              write(0,'(10x,a)')" It uses a single argument, the MOPAC data-set"
              write(0,'(10x,a)')" The command to run MOPAC is 'MOPAC2012.exe <data-set>.mop'"
              call web_message(0,"running_MOPAC.html")
              write(0,'(10x,a)')" Press (return) to continue"
              read(5,*, iostat=i) 
              return
            end if            
          end if
        else
       natoms = 1
      end if
      end if
      if (natoms == 0) return
!
! Check for the data set in the order: <file>.mop, <file>.dat, <file>
!
      line = jobnam
      exists = .false.
      call upcase(line, len_trim(line))
      i = Index(line,".MOP") + Index(line,".DAT") + Index(line, ".ARC")
      arc_file = (Index(line, ".ARC") > 0)
      if (i > 0) then  ! User has supplied a suffix - use it     
        line = jobnam(:len_trim(jobnam))
        inquire(file=line, exist=exists)
        jobnam(i:i+3) = " "
      else  ! No suffix supplied, try the file, then .mop, then .dat
        line = jobnam(:len_trim(jobnam))
        inquire(file=line, exist=exists)
        if (exists) then ! Check that it is not a folder
          open(unit=from_data_set, file=line, status='OLD', position=&
          'asis', iostat=io_stat) 
          if (io_stat == 9) then
            exists = .false.
          else
            close (from_data_set)
          end if
        end if
        if ( .not. exists) then
          line = jobnam(:len_trim(jobnam))//'.mop'
          inquire(file=line, exist=exists)
        end if
        if ( .not. exists) then
          line = jobnam(:len_trim(jobnam))//'.dat'
          inquire(file=line, exist=exists)
        end if
      end if
  98  if (exists) then 
         if (iw0 > -1) then           
           if (ijulian < 365) then
             call to_screen("********************************************************************************")
             write (text,"(a,a,a,i4,a)") &
!            " <-Start of text                                                              End of text->"
             "**                          MOPAC (",verson,")        Days remaining",ijulian,"         **"
             call to_screen(text)
             call to_screen("********************************************************************************")
           end if
           call to_screen("Preparing to read the following MOPAC file: ")
           i = min(len_trim(line), 241)
           if (i > 160) call to_screen(line(161:i))
           i = min(i,160)
           if (i > 80)  call to_screen(line(81:i))
           i = min(i,80)
           call to_screen(line(:i))
         end if
         job_fn = line(:len(job_fn))
        open(unit=from_data_set, file=job_fn, status='OLD', position='asis', iostat=io_stat) 
        if (io_stat /= 0) then
          write(line,'(2a)')" Data file: '"//trim(job_fn)//"' exists, but it cannot be opened."
          write(0,'(//10x,a,//)')trim(line)
          call to_screen(" File '"//job_fn(:len_trim(job_fn))//"' cannot be opened")
          call mopend (" File '"//job_fn(:len_trim(job_fn))//"' cannot be opened")
          return
        end if
!
!  Now that the name of the data-set is known, set up all the other file-names
!
        call init_filenames
      else 
        write (line, '(a)') ' The input data file "'//jobnam(:len_trim(jobnam))//'" does not exist.'
        open(unit=iw, file=jobnam(:len_trim(jobnam))//'.out') 
        write(iw,"(a)")line
        if( .not. gui) write(0,"(///10x,a)")line
        call to_screen(line)
        call mopend ('The input data file does not exist') 
        return  
      endif 
!
!  CLOSE UNIT IFILES(5) IN CASE IT WAS ALREADY PRE-ASSIGNED.
!
      close(input) 
      open(unit=input, file=input_fn, status='UNKNOWN', &
        position='asis', iostat = io_stat) 
      if (io_stat /= 0) then
        if (io_stat == 30) then
          call to_screen(" The file'"//input_fn(:len_trim(input_fn))//"' is busy")
          call to_screen(" Correct this problem, and re-submit job")
          call mopend(" Temporary file '"//input_fn(:len_trim(input_fn))//"' is unavailable for use")
          return
        end if
      end if
      rewind input 
      rewind from_data_set 
      if (arc_file) then
        do 
          read (from_data_set, '(A241)', iostat = i) line
          if (i /= 0) then
            write(0,"(a)")"The data set was defined as an ARC file"
            write(0,"(a)")"but it does not appear to an archive file"
            write(0,"(a)")"An attempt will be made to read it as a normal data set"
            i = 0
            ncomments = 0
            rewind(from_data_set)
            exit
          end if 
          if (index(line, "FINAL GEOMETRY OBTAINED") > 0) exit   
          if (index(line, "GEOMETRY IN CARTESIAN COORDINATE") > 0) exit 
          if (index(line, "GEOMETRY IN MOPAC Z-MATRIX") > 0) exit            
        end do    
      end if
      nlines = 0 
      ncomments = 0
      if (.not. allocated(tmp_comments)) allocate(tmp_comments(10000))
!
!  The size of the comments is not known, so set up a temporary array of size 10000 lines
!
      do        
        read (from_data_set, '(A241)', end=30, err=30) line 
        nlines = nlines + 1 
        if (nlines == 1) then
          j = len_trim(line1)
          line1(:j) = line(:j)
          call upcase(line1, j)
          j = Index(line1,"DATA=")
          if (j > 0) exit
        end if
        if (line(1:1) /= '*') then
            line1 = line     
            i = 0
            do j = 1, len_trim(line1)
              if (ichar(line1(j:j)) == 9) then
      !
      ! convert tab to space(s).  Align with 8 character boundary
      !
                i = i + 1
                l = mod(i,8)
                line(i:) = " "
                if (l /= 0) i = i + 8 - l                    
              else
                i = i + 1
                line(i:i) = line1(j:j)
              end if
            end do
          write (input, '(A241)', iostat=io_stat) line 
          if (io_stat /= 0) then
            write (line, '(a)') ' The run-time temporary file "'//jobnam(:len_trim(jobnam))//'.temp" cannot be written to.'
            open(unit=iw, file=jobnam(:len_trim(jobnam))//'.out') 
            write(iw,"(a)")line
            if( .not. gui) write(0,"(///10x,a)")line
            call to_screen(line)
            call mopend (trim(line)) 
            return
          end if
          comments = .false.
        else if (comments) then
          ncomments = ncomments + 1
          tmp_comments(ncomments) = line(:80)
        end if
      end do
   30 continue 
      if (nlines == 1) then   !  Data set points to a MOPAC data set.        
        i = Len_trim(line1)
        job_fn = line(j+5:i)
        if(line1(i:i) == "+") then
!
!  File name runs onto two lines
!
           read (input, "(A120)", end=1000, err=1000) line
           i = i - j - 6
           job_fn(i:) = line
        end if
        i = Len_trim(line)
        if(line(i:i) == "+") then
!
!  File name runs onto three lines
!
           read (input, "(A120)", end=1000, err=1000) line
           i = Len_trim(job_fn) - 1
           job_fn(i:) = line
        end if
!
!  Make sure that the file ends in ".mop", ".dat", or "arc"
!
        i = Len_trim(job_fn) - 1
        line = job_fn
        call upcase(line,i)
        i = index(line,".MOP") + index(line,".DAT") + index(line,".ARC")
        if (i /= 0) job_fn(i+4:) = " "              
        do j = 1,Len_trim(job_fn) 
          if (job_fn(j:j) == char(92)) job_fn(j:j) = "/"
        end do
!
!  Clean up path before continuing
!
        do j = 1, Len_trim(jobnam)
          if (jobnam(j:j) == char(92)) jobnam(j:j) = "/"
        end do
        l = Index(job_fn,"/")
        if (l == 0)then
          call mopend ("INPUT FILE PATH MUST BE DEFINED IN ""DATA=<file plus path>"" ")
          return
        end if                       
        call to_screen ("Preparing to read the following MOPAC file: ")
        j = Len_trim (job_fn)
        call to_screen ("  ")
        if (j > 160) then
          call to_screen ("'"//job_fn(1:80))
          call to_screen (job_fn(81:160))
          call to_screen (job_fn(161:j)//"'")
        else if (j> 80) then
          call to_screen ("'"//job_fn(1:80))
          call to_screen (job_fn(81:j)//"'")
        else
          call to_screen ("'"//job_fn(1:j)//"'")
        end if
        call to_screen ("  ")
        line = job_fn
        goto 98
    end if   ! Line was one of first 5 lines
!
! Now the size of the comments list is known.  Allocate all_comments
!
      allocate(all_comments(ncomments + 3))
      all_comments(:ncomments) = tmp_comments(:ncomments)
      deallocate(tmp_comments)
      line = ' ' 
      write (input, '(A241)') line 
      rewind input 
 1000 if (nlines < 3 .and. .not. is_PARAM) then 
        write (output, '(A)') ' INPUT FILE MISSING OR EMPTY' 
        call mopend ('INPUT FILE MISSING OR EMPTY') 
        return  
      endif 
      natoms = nlines
      close(from_data_set) 
      return  
      end subroutine getdat 
 
