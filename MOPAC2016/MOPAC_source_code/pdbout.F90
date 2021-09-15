subroutine pdbout (mode1)
    use molkst_C, only: numat, natoms, ncomments, verson, line, nbreaks, &
      maxtxt, keywrd
    use chanel_C, only: iw, input_fn
    use elemts_C, only: elemnt
    use common_arrays_C, only: txtatm, coord, nat, all_comments, p, &
      breaks, txtatm1
    USE parameters_C, only : tore
    implicit none
    integer, intent (in) :: mode1
!
    character :: ele_pdb*2, idate*24
    integer :: i, ii, iprt, k, nline, iii
    logical :: ter, html
    intrinsic Abs, Char, Ichar
    double precision, allocatable :: q2(:)
!
    html = (index(keywrd, " HTML") /= 0)
    allocate (q2(numat))
    if (html .and. allocated(p)) then
      call chrge (p, q2)      
      q2(:numat) = tore(nat(:numat)) - q2(:numat) 
    else
      q2 = 0.d0
    end if
    nline = 0
    if (mode1 == 1) then
      iprt = iw
    else
      iprt = Abs (mode1)
    end if 
    call fdate (idate) 
    i = len_trim(input_fn)
    if (ncomments > 0) then
      if (index(all_comments(1),"HEADER") == 0) then
        line = "HEADER  data-set: "//input_fn(:i - 5)
        if(len_trim(line) > 80) then
          do
            k = index(line, "\")
            if (k == 0) exit
            ii = index(line,"data-set:") + 9
            line = line(:ii)//line(k + 1:)
          end do
        end if
        line(81:) = " "
        write (iprt, "(A)") trim(line)
        line = "REMARK  MOPAC 2012, Version: "//verson//" Date: "//idate(5:11)//idate(21:)//idate(11:16)
        write (iprt, "(A)") trim(line)
      end if 
      do i = 1, ncomments
        line = all_comments(i)(:7)
        if (index(line,"ATOM  ") + index(line,"HETATM") + index(line,"TITLE ") + index(line,"HEADER") + &
            index(line,"ANISOU") + index(line,"COMPND") + index(line,"SOURCE") + index(line,"KEYWDS") + &
            index(line,"HELIX ") + index(line,"SHEET ") + index(line,"REMARK") + index(line,"USER  ") + &
            index(line,"EXPDTA") + index(line,"AUTHOR") + index(line,"REVDAT") + index(line,"JRNL  ") + &
            index(line,"DBREF ") + index(line,"SEQRES") + index(line,"HET   ") + index(line,"HETNAM") + &
            index(line,"LINK  ") + index(line,"CRYST1") + index(line,"SCALE" ) + index(line,"ORIGX" ) + &
            index(line,"FORMUL") + index(line,"SEQRES") == 0) cycle
        write(iprt,"(a)")all_comments(i)(2:len_trim(all_comments(i)))
      end do
    else
      line = "HEADER  data-set: "//input_fn(:i - 5)
      if(len_trim(line) > 80) then
        do
          k = index(line, "\")
          if (k == 0) exit
          ii = index(line,"data-set:") + 9
          line = line(:ii)//line(k + 1:)
        end do
      end if
      line(81:) = " "
      write (iprt, "(A)") trim(line)
      line = "REMARK  MOPAC 2012, Version: "//verson//" Date: "//idate(5:11)//idate(21:)//idate(11:16)
      write (iprt, "(A)") trim(line)
    end if
    if (maxtxt == 0 .and. txtatm(1) /= " ") then
      txtatm1(:natoms) = txtatm(:natoms)
    end if
    ii = 0
    iii = 0
    nbreaks = 1
    do i = 1, numat
      ii = ii + 1
      iii = iii + 1
      if (txtatm(iii)(14:14) == "X") iii = iii + 1
      nline = nline + 1
      ter = (i == breaks(nbreaks))
      if (ter) nbreaks = nbreaks + 1
      if (elemnt(nat(i)) (1:1) == " ") then
        ele_pdb(1:1) = " "
        ele_pdb(2:2) = elemnt(nat(i)) (2:2)
      else
        ele_pdb(1:1) = elemnt(nat(i)) (1:1)
        ele_pdb (2:2) = Char (Ichar ("A")-Ichar ("a")+Ichar (elemnt(nat(i)) (2:2)))
      end if
      write (iprt, "(a,i5,a,f12.3,f8.3,f8.3,a,f5.2,a, a2,a)") txtatm(iii)(1:6),ii,txtatm(iii)(12:26), &
        & (coord(k, i), k=1, 3), "  1.00 ",q2(i),"      PROT", ele_pdb, " "
      if (ter) then
        ii = ii + 1
        write(iprt, "(a,i8,a)")"TER",ii,"      "//txtatm(iii)(18:26)  !  Write out the TERMINAL marker
      end if
    end do
    write(iprt, "(a)")"END" 
    if (html) call write_html
  end subroutine pdbout
  subroutine write_html
    use chanel_C, only: input_fn
    use molkst_C, only : line, koment, title, numat, keywrd, numcal, geo_ref_name, geo_dat_name
    use common_arrays_C, only : txtatm, p
    use mozyme_C, only : tyres, tyr
    implicit none
    integer :: nres, i, j, k, nprt, ncol, biggest_res, iprt=27, icalcn = -1, it
    character :: res_txt(4000)*10, l_res*10, n_res*11, wrt_res*11, num*1, line_1*400
    integer, parameter :: limres = 260
    logical :: exists, l_prt_res, l_geo_ref
    double precision, external :: reada
    save :: icalcn
    if (icalcn == numcal) return
    icalcn = numcal
    i = index(keywrd, " HTML")
    j = i + index(keywrd(i + 1:)," ")
    l_prt_res = (index(keywrd(i + 1:j),"NORES") == 0)
    line = input_fn(:len_trim(input_fn) - 4)//"html"
    open(unit=iprt, file=trim(line)) 
    nres = 0
    if (l_prt_res) then   
!
!   Identify all residues
!

      do i = 1, numat
        if (txtatm(i)(18:20) == "HOH") cycle
        if (txtatm(i)(18:20) == "SO4") cycle
        j = nint(reada(txtatm(i), 23))
        if (j < 0) then
           write(l_res,"(a3,i4.3,a)")txtatm(i)(18:20),j,":"//txtatm(i)(22:22)
        else
           write(l_res,"(a3,i4.4,a)")txtatm(i)(18:20),j,":"//txtatm(i)(22:22)
        end if  
        do k = 1, 6
          if (l_res(k:k) /= " ") exit
        end do        
        if (l_res(k:k) >= "0" .and. l_res(k:k) <= "9") l_res(k:k) = "Q"
        do k = 1, nres
          if (res_txt(k) == l_res) exit
        end do
        if (k > nres) then
          nres = nres + 1
          res_txt(nres) = l_res
        end if     
      end do
      do i = 1, nres
        do j = 1, 20
          if (res_txt(i)(1:3) == tyres(j)) exit
        end do
        if (j < 21) exit
      end do
      num = txtatm(1)(22:22)
      do i = 1, nres
        if (res_txt(i)(9:9) /= num) exit
      end do
      if (i > nres) then
        res_txt(:)(8:9) = "  "      
      end if
    end if
!
!  Heading
!
    if (len_trim(koment) == 0 .or. (index(koment(:8), " NULL") /= 0)) then
      write(iprt,"(a)") "<HTML><HEAD><TITLE>"//input_fn(:len_trim(input_fn) - 5)//"</TITLE></HEAD>"
    else
      do it = 1, len_trim(koment)
        if (koment(it:it) /= " ") exit
      end do
      write(iprt,"(a)") "<HTML><HEAD><TITLE>"//trim(koment(it:))//"</TITLE></HEAD>"
    end if
    write(iprt,"(a)")"<style type=""text/css"">"
    write(iprt,"(a)")".auto-style4 {"
    write(iprt,"(a)")"margin-top: 0px;"
    write(iprt,"(a)")"text-align: center;"
    write(iprt,"(a)")"line-height: 85%;"
    write(iprt,"(a)")"}"
    write(iprt,"(a)")".auto-style5 {"
    write(iprt,"(a)")"text-decoration: none;"
    write(iprt,"(a)")"}"
    write(iprt,"(a)")"</style>"
    write(iprt,"(a)") "<!--"," ","   Start of JSmol script"," ", "-->"
    write(iprt,"(a)") "<meta charset=""utf-8""> <script type=""text/javascript"" src=""../jsmol/JSmol.min.js""></script>  "
    write(iprt,"(a)") "<script type=""text/javascript"">"
    write(iprt,"(/a)") "$(document).ready(function() {Info = {"
    write(iprt,"(10x,a)") "width: 1500,", "height: 1000,", "color: ""0xB0B0B0"",", &
    "disableInitialConsole: true, ", "addSelectionOptions: false,", "j2sPath: ""../jsmol/j2s"",", &
    "jarPath: ""../jsmol/java"",", "use: ""HTML5"", script: ", " "
    write(iprt,"(a)")"// Data set to be loaded", " "
    line = input_fn(:len_trim(input_fn) - 4)//"pdb"
    do i = len_trim(line), 1, -1
      if (line(i:i) == "/" .or. line(i:i) == "\") exit
    end do
    write(iprt,"(a)") """load \"
    l_geo_ref = (index(keywrd, " 0SCF") /= 0 .and. index(keywrd, " GEO_REF") /= 0)
    if (l_geo_ref) then
      line = geo_dat_name(:len_trim(geo_dat_name) -3)//"pdb"//"' '"// &
      geo_ref_name(:len_trim(geo_ref_name) -3)//"pdb"//"'; \"
      write(iprt,"(10x,a)")"FILES '"//trim(line)
    else      
      write(iprt,"(10x,a)")"'"//trim(line(i + 1:))//"'; \"
    end if
    write(iprt,"(10x,a)")"set measurementUnits ANGSTROMS; \"
    if (l_geo_ref) then
      write(iprt,"(10x,a)")"set bondRadiusMilliAngstroms (25); \", "spacefill 10%; \"
    else
      write(iprt,"(10x,a)")"set bondRadiusMilliAngstroms (50); \", "spacefill 15%; \"
    end if
    write(iprt,"(10x,a)")"set display selected; \", "hBonds calculate; \"
    write(iprt,"(10x,a)")"set defaultDistanceLabel '%0.3VALUE %UNITS'; \"
    if (index(keywrd, " 0SCF") /= 0 .and. index(keywrd, " GEO_REF") /= 0) then
      write(iprt,"(10x,a)")"select */2.1; color bonds green;  select off;\"
    else
      write(iprt,"(10x,a)")"select off; \"
    end if
    write(iprt,"(10x,a)")"set perspectivedepth off; \", &
    "connect 0.8  1.5 (hydrogen) (phosphorus) create; \"
    if (l_geo_ref) then
      write(iprt,"(10x,a)")"set zoomLarge false; frame 0;"" "
    else      
      write(iprt,"(10x,a)")"set zoomLarge false;"" "
    end if
    write(iprt,"(a)") "} "
    write(iprt,"(a)") "$(""#mydiv"").html(Jmol.getAppletHtml(""jmolApplet0"",Info))}); "
    write(iprt,"(a)") "</script>"
    write(iprt,"(a)") "<!--"," ","   End of JSmol script"," ", "-->"
    if (len_trim(koment) == 0 .or. (index(koment(:8), " NULL") /= 0) ) then
      write(iprt,"(a)") "<h1 align=""center"">"//input_fn(:len_trim(input_fn) - 5)//"</h1>"
    else
      write(iprt,"(a)") "<h1 align=""center"">"//trim(koment(it:))//"</h1>"
    end if
    if (len_trim(title) /= 0 .and. (index(title(:8), " NULL") == 0) ) &
      write(iprt,"(a)") '<h2 align="center">'//trim(title)//'</h2>'
    if (index(keywrd, " 0SCF") /= 0 .and. index(keywrd, " GEO_REF") /= 0) then
      write(iprt,"(a)")"<h2 align=""center"">Compare """//trim(geo_dat_name)// &
      & """ and ""<span style=""color:green"">"//trim(geo_ref_name)//"</span>""</h2>"
    end if
    write(iprt,"(a)") "<BODY  BGCOLOR=""#ffffff"">"
!
!  Start of table.  The table consists of two side-by-side cells
!  The first entry in the first cell is a table of size three rows, two columns
!
    write(iprt,"(a)")"<TABLE>", "<TD>", "<TABLE>", "<TR>"
    if (nres < limres) then
!
!   Element(1,1) and (2,1)
!
      write(iprt,"(a)") "<TD colspan=""2"">"
      call write_data_to_html(iprt)
      write(iprt,"(a)") "</TD></TR><TR>"
    end if
!
!   Element(1,2)
!
    if (index(keywrd, " 0SCF") /= 0 .and. index(keywrd, " GEO_REF") /= 0) then
      write(iprt,"(a)") "<TD>Toggle display<br><a href=""javascript:Jmol.script(jmolApplet0,'"
    else
      write(iprt,"(a)") "<TD><a href=""javascript:Jmol.script(jmolApplet0,'"
    end if
    write(iprt,"(a)") "if (isOK1);  Display *; zoom 0; isOK1 = FALSE; else hide *; "
    line = " "
    biggest_res = 0
    do i = 1, nres
      n_res = "["//res_txt(i)(:3)//"]"
      j = nint(reada(res_txt(i),4))
      biggest_res = max(biggest_res, j)
      num = char(Int(log10(j + 1.0)) + ichar("1"))
      if (j < 0) then
        num = char(Int(log10(-j + 1.0)) + ichar("1"))
        write(n_res(6:),'(a1,i'//num//',a)')"_", -j, res_txt(i)(8:9)
      else
        num = char(Int(log10(-j + 1.0)) + ichar("2"))
        write(n_res(6:),'(i'//num//',a)')j, res_txt(i)(8:9)
      end if   
      wrt_res = n_res(2:4)//n_res(6:)
      wrt_res(2:2) = char(ichar(wrt_res(2:2)) + ichar("a") - ichar("A"))
      wrt_res(3:3) = char(ichar(wrt_res(3:3)) + ichar("a") - ichar("A"))
      j = len_trim(line)
      if (j > 120) then
        j = 0
        write(iprt,"(a)")trim(line)
        line = " "
      end if
      l_res = res_txt(i)(:7)//res_txt(i)(9:9)
      if (l_res(1:1) >= "0" .and. l_res(1:1) <= "9") &
        l_res(1:1) = char(ichar("A") + ichar(l_res(1:1)) - ichar("0"))
      k = index(l_res, "-")
      if (k > 0) l_res(k:k) = "_"
      write(line(j + 2:),"(a)") l_res//" = FALSE;" 
    end do
    write(iprt,"(a)")trim(line)    
    write(iprt,"(a)") "isOK1 = TRUE; isOK2 = FALSE; lzoom = TRUE; lcenter = TRUE;"
    if (index(keywrd, " 0SCF") /= 0 .and. index(keywrd, " GEO_REF") /= 0) then
    write(iprt,"(a)") "endif;')""> 1&2</a>"
    write(iprt,"(a)")"<a href=""javascript:Jmol.script(jmolApplet0,'if (isOKone);  Display add */1.1; zoom 0; isOKone = FALSE;", &
      "else hide add */1.1; zoom 0; isOKone = TRUE;  lzoom = TRUE; lcenter = TRUE; endif;')""> 1</a>"
    write(iprt,"(a)")"<a href=""javascript:Jmol.script(jmolApplet0,'if (isOKtwo);  Display add */2.1; zoom 0; isOKtwo = FALSE;", &
      "else hide add */2.1; zoom 0; isOKtwo = TRUE;  lzoom = TRUE; lcenter = TRUE; endif;')""> 2</a>"
    else
      write(iprt,"(a)") "endif;')"">Toggle display all</a>"
    end if    
    write(iprt,"(a)")"</TD>"
!
!   Element(2,2)
!
    write(iprt,"(a)")"<TD><a href=""javascript:Jmol.script(jmolApplet0,'"
    write(iprt,"(a)")"if (lcenter);  lcenter = FALSE; else lcenter = TRUE; center {visible}; end if;"
    write(iprt,"(a)")"')"">Toggle center picture</a> </TD>","</TR> <TR>" 
!
!   Element(1,3)
!
    write(iprt,"(a)")"<TD><a href=""javascript:Jmol.script(jmolApplet0,'console;')"">Console</a> </TD> "
    write(iprt,"(a)") ""
!
!   Element(2,3)
!
!
!   Element(1,4)
!
    write(iprt,"(a)")"<TD> <a href=""javascript:Jmol.script(jmolApplet0,'if (lzoom); zoom 0; "// &
      "select */2.1; color bonds green; select off; lzoom = FALSE; else lzoom = TRUE; "// &
    "end if ')"">Fit to screen</a> </TD></TR><TR>"
!
!   Element(2,4)
!
    write(iprt,"(a)")"<TD><a href=""javascript:Jmol.script(jmolApplet0,'display within(3,visible);"// &
      "select */2.1; color bonds green; select off; if (lzoom); zoom 0; end if ')"">Near Neighbors</a> </TD> "
     write(iprt,"(a)")"<TD><a href=""javascript:Jmol.script(jmolApplet0,'"// &
     "connect (all) (all) delete; connect; display add connected(visible); select *; hbonds calculate; select "// &
      " */2.1; color bonds green; select off; if (lzoom); delay 0.001; zoom 0; end if ')"">Connected Neighbors</a> </TD> "
    if (allocated(p)) then
      write(iprt,"(a)")"<TR><TD>"
      write(iprt,"(a)")"<a href=""javascript:Jmol.script(jmolApplet0,'if (!lcharge_x); " 
      write(iprt,"(a)")"var use = {visible}; var sel = {selected};"
      write(iprt,"(a)")"var z = 0; for (var i IN @sel){z = 3}"
      write(iprt,"(a)")"if (z = 3); use = sel; end if;"
      write(iprt,"(a)")"for (var x IN @use){select @x; var txt =  (x.temperature > 0 ? \'+\':\'\')"// &
        "+format(\'%1.2f\',x.temperature ); label @txt; color label black;"
      write(iprt,"(a)")"set labelOffset 0 0;}  select @sel; lcharge_x= TRUE;"
      write(iprt,"(a)")"else lcharge_x= FALSE; var use = {visible}; var sel = {selected};"
      write(iprt,"(a)")"var z = 0; for (var i IN @sel){z = 3}"
      write(iprt,"(a)")"if (z = 3); use = sel; end if;"
      write(iprt,"(a)")"select @use; label OFF; select @sel; end if ')"">Charges as Nos.</a>"

      write(iprt,"(a)")"</TD><TD>"
      write(iprt,"(a)")"<a href=""javascript:Jmol.script(jmolApplet0,'if (!lcharge_s); "
       write(iprt,"(a)")"var use = {visible}; var sel = {selected};"
      write(iprt,"(a)")"var z = 0; for (var i IN @sel){z = 3}"
      write(iprt,"(a)")"if (z = 3); use = sel; end if;"
      write(iprt,"(a)")"for (var x IN @use)"
      write(iprt,"(a)")"{select @x; var txt =  @x.temperature*0.5;"
      write(iprt,"(a)")"if (@txt > 0){spacefill @txt; color atom deepskyblue;}"
      write(iprt,"(a)")"if (!@txt > 0){txt = -txt; spacefill @txt; color atom deeppink;}}"
      write(iprt,"(a)")"select @sel; lcharge_s= TRUE;"
      write(iprt,"(a)")"else lcharge_s= FALSE; var use = {visible}; var sel = {selected};"
      write(iprt,"(a)")"var z = 0; for (var i IN @sel){z = 3}"
      write(iprt,"(a)")"if (z = 3); use = sel; end if;"
      write(iprt,"(a)")"select @use; spacefill 15%; color cpk; select @sel; end if ')"">Charges as Sizes</a>"
      write(iprt,"(a)")"</TD></TR>"
    end if
    write(iprt,"(a)") "<TR>"
!
!   Element(1,5)
!
    line = input_fn(:len_trim(input_fn) - 4)//"txt"
    line_1 = trim(line)
    call upcase(line_1, len_trim(line_1))
    i = 0
    do j = 1, len_trim(line) - 4
      if (line(j:j) == "/" .or. line(j:j) == "\")then
        i = j
      end if        
    end do
    if (i /= 0) line = line(i + 1:)
    write(iprt,"(a)")"<TD colspan=""2""><a href=""javascript:Jmol.script(jmolApplet0,'script common.txt;')"">"// &
    "<strong style=""font-size:20px"">Common Script</strong>"
    write(iprt,"(a)")"</a>&nbsp; (Read file from:<br> ""<a href=""common.txt""  target=""_blank"">common.txt</a>"")</TD>"
    write(iprt,"(a)")"</TR> <TR>" 
    write(iprt,"(a)")"<TD colspan=""2""><a href=""javascript:Jmol.script(jmolApplet0,'script \'"//trim(line)//"\';')"">"// &
    "<strong style=""font-size:20px"">Specific Script</strong>"
    write(iprt,"(a)")"</a>&nbsp; (Read file from:<br> ""<a href="""//trim(line)// &
    """  target=""_blank"">"//trim(line)//"</a>"")</TD>"
    write(iprt,"(a)")"</TR></TABLE>"
!  
!  The second entry in the first cell is a table of size (nres/5) rows, ncol columns.
!  Write all residues in table form
!
    if (nres > 0) then
      write(iprt,"(a)")"<p align=""center"">Toggle Individual Residues</p>"
      write(iprt,"(a)") "<TABLE>"
      write(iprt,"(a)") "<TR>"
!
! Number of rows is a maximum of 36.  
! If the number of residues is small, put 8 residues on a line 
!
      ncol = max(7, nres/16) + 1
      nprt = 1
      do i = 1, nres
        n_res = "["//res_txt(i)(:3)//"]"
        j = nint(reada(res_txt(i),4))
        if (j < 0) then
          num = char(Int(log10(-j + 1.0)) + ichar("2"))
          write(n_res(6:),'(i'//num//',a)')j, res_txt(i)(8:9)
        else
          num = char(Int(log10(j + 1.0)) + ichar("1"))
          write(n_res(6:),'(i'//num//',a)')j, res_txt(i)(8:9)
        end if
        

        wrt_res = n_res(2:4)//n_res(6:)
        l_res = res_txt(i)(:7)//res_txt(i)(9:9)
  !
  !  Residue names must not start with a number.  If they do, convert them into a letter
  !  0 => A, 1 => B, etc.
  !
        if (l_res(1:1) >= "0" .and. l_res(1:1) <= "9") &
          l_res(1:1) = char(ichar("A") + ichar(l_res(1:1)) - ichar("0"))
        do j = 1, 23
          if (n_res(2:4) == tyres(j)) exit
        end do
        if (j > 23) then
          n_res(:5) = " "
        !  j = index(n_res, ":")
        !  if (j > 0) n_res(j:) = n_res(j + 1:)
          wrt_res(1:1) = "X"
        else
          wrt_res(1:1) = tyr(j)
          if (wrt_res(1:1) == "?") wrt_res(1:1) = "X"
        end if
        j = index(l_res, "-")
        if (j > 0) l_res(j:j) = "_"
        write(iprt,"(2a)") "<TD> <a href=""javascript:Jmol.script(jmolApplet0,'if (!"//l_res, &
        ");   display ADD "//trim(n_res)//";  "//l_res//" = TRUE; else "
        write(iprt,"(2a)") " hide ADD "//trim(n_res)//";  "//l_res//" = FALSE; end if; ", &
        "if (lcenter); center {visible}; end if; if (lzoom); zoom 0; end if;')"" class=""auto-style5"">  "
        if (index(wrt_res,":") /= 0) then
          j = index(wrt_res,":") + 1
          write(iprt,"(2a)")"<p class=""auto-style4"">"//wrt_res(1:1)//"<br>"//wrt_res(4:j - 2)//"</p> </a></TD>"  
        else        
          write(iprt,"(2a)")"<p class=""auto-style4"">"//wrt_res(1:1)//"<br>"//wrt_res(4:)//"</p> </a></TD>"  
        end if
        if (nprt == ncol) then
          nprt = 1
          write(iprt,'(a)')"</TR> <TR>"
        else
          nprt = nprt + 1
        end if
      end do
      write(iprt,"(a)") "</TR>"
      write(iprt,"(a)") "</TABLE><br> "
    end if
!
!  End of table of residues, and end of the first cell, now to write the JSmol picture (in the cell (1,2))
!
    write(iprt,"(a)") " </TD><TD>"
    if (nres >= limres) then
!
!   Element(1,1) and (2,1)
!
   !   write(iprt,"(a)") "<TD colspan=""2"">"
      call write_data_to_html(iprt)
   !   write(iprt,"(a)") "</TD></TR><TR>"
    end if
    write(iprt,"(a)") "<span id=mydiv></span><a href=""javascript:Jmol.script(jmolApplet0)""></a></TD></TABLE>"
    write(iprt,"(a)") "</BODY>"
    write(iprt,"(a)") "</HTML>"
!
!  Write out a simple script file
!
    close (iprt)
    line = line(:len_trim(line) - 3)
    call add_path(line)
    inquire (file=trim(line)//"txt", exist = exists)
    if (.not. exists) then
      open(unit=iprt, file=trim(line)//"txt") 
      i = 0
      do j = 1, len_trim(line)
        if (line(j:j) == "/" .or. line(j:j) == "\") i = j
      end do
      if (i /= 0) line = line(i + 1:)
      write(iprt,"(a)")"#","# Script for use with the HTML file """//trim(line)//"html""","#"
    end if
    close (iprt)
  end subroutine write_html
  subroutine write_data_to_html(iprt)
    use molkst_C, only : numat, formula, escf, nelecs, keywrd, arc_hof_1, arc_hof_2
    USE parameters_C, only : tore
    use common_arrays_C, only: nat
    use chanel_C, only: log
    implicit none
    integer :: iprt
    integer :: i, j, eqls, colon
    character :: idate*24, line*120
    logical :: store_log
    double precision :: sum   
!
!  Print out information on the system: formula, number of atoms, heat of formation, date, etc.
!
    write(iprt,"(a)")"<TABLE>"
    call fdate (idate) 
    write(iprt,"(a)")  "<TR><TD> Date:</TD><TD> &nbsp;&nbsp; &nbsp;</TD><TD>"// &
      idate(5:11)//idate(21:)//idate(11:16)//"</TD></TR>"
    write(iprt,"(a,i5,a)")  "<TR><TD> No. atoms:</TD><TD> &nbsp;&nbsp; &nbsp;</TD><TD>", &
      numat,  "</TD></TR>"
    store_log = log
    log = .false.
    call empiri()
    log = store_log
    colon = index(formula, ":") + 1
    eqls = index(formula, "=") - 1
    line = " "
    do i = colon, eqls
      j = i + 1
      if ((formula(i:i) < "0" .or. formula(i:i) > "9") .and. formula(j:j) >= "0" .and. formula(j:j) <= "9") then
        line = trim(line)//formula(i:i)
        line = trim(line)//"<sub>"
      else if ((formula(j:j) < "0" .or. formula(j:j) > "9") .and. formula(i:i) >= "0" .and. formula(i:i) <= "9") then
        line = trim(line)//formula(i:i)
        line = trim(line)//"</sub>"
      else
        line = trim(line)//formula(i:i)
      end if
    end do
    write(iprt,"(a)")  "<TR><TD> Formula:</TD><TD> &nbsp;&nbsp; &nbsp;</TD><TD>"//trim(line)//"</TD></TR>"
    if (index(keywrd, " 0SCF") /= 0 .and. index(keywrd, " GEO_REF") /= 0) then
      if (abs(arc_hof_1) > 1.d-4) &
      write(iprt,"(a,f12.3,a)")  "<TR><TD> Dataset:</TD><TD> &nbsp;&nbsp; &nbsp;</TD><TD>",arc_hof_1," kcal/mol</TD></TR>"
      if (abs(arc_hof_2) > 1.d-4) &
      write(iprt,"(a,f12.3,a)")  "<TR><TD> GEO_REF:</TD><TD> &nbsp;&nbsp; &nbsp;</TD><TD>",arc_hof_2," kcal/mol</TD></TR>"
      i = index(keywrd, " RMS_DIFF")
      if (i > 0) then
        i = index(keywrd(i:), "F=") + i + 1
        j = index(keywrd(i:), " ") + i - 2
        write(iprt,"(a)")  "<TR><TD> RMS Diff.:</TD><TD> &nbsp;&nbsp; &nbsp;</TD><TD>"//keywrd(i:j)//"&Aring;</TD></TR>"
      end if
    end if        
    if (index(line, "H") /= 0 .and. nelecs > 0) then
!
!  Net charge has to be worked out the hard way
!
      sum = -nelecs
      do i = 1, numat
        sum = sum + tore(nat(i))
      end do
      i = nint(sum) 
      if (i /= 0) then
        write(iprt,"(a,SP,i6,SS,a)")"<TR><TD> Net charge:</TD><TD> &nbsp;&nbsp; &nbsp;</TD><TD>",i,"</TD></TR>"
      else
          write(iprt,"(a)")"<TR><TD> Net charge:</TD><TD> &nbsp;&nbsp; &nbsp;</TD><TD>Zero</TD></TR>"        
      end if
    end if
    if (abs(escf) > 1.d-10) then
      if (abs(escf) > 1.d-10) &
        write(iprt,"(a, f12.3)")"<TR><TD> Heat of Formation:</TD><TD> &nbsp;&nbsp; &nbsp;</TD><TD>", &
        escf," Kcal/mol</TD></TR>"
    end if
    write(iprt,"(a)")  "</TABLE>"
  end subroutine write_data_to_html
  
