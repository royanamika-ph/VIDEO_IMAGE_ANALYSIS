          implicit doubleprecision(a-h,o-z)
          parameter (a=1,n=463,sc=1)
          dimension r(926,926),rhex(n,n),xpoint(n,n),ypoint(n,n)
          dimension label(2*n,2*n),ii(n*n),jj(n*n)
          dimension iiw(n*n),jjw(n*n)
          dimension check1(n,n),check2(n,n)
          dimension check4(n,n),check5(n,n)
          dimension check6(n,n) ,check3(n,n)
          dimension wcheck1(n,n),wcheck2(n,n)
          dimension wcheck4(n,n),wcheck5(n,n)
          dimension  wcheck6(n,n) ,wcheck3(n,n)
          dimension bcheck1(n,n),bcheck2(n,n)
          dimension pointvalue(n,n),mod_val(n*n)
          dimension nbn(n*n),nwn(n*n),p(20)
          dimension euler(20), eulerno(20),normeuler(20)
          dimension sizeb(n*n),sizew(n*n),trialno(n*n)
          dimension clustersize(n*n) ,num(n*n),numf(n*n)
          dimension trno(n*n), tr(n*n),u(20),alab(n*n)
          dimension nx(216,6),ny(216,6),maxn(6)
          dimension modx(n*n),mody(n*n),mn(6)
          real i1,j1,j2,inc
          integer blacklabel ,nn ,nnw,sno,wsno ,totn ,totnw,sp,spw
          integer b_count,k_sort,l_sort,sort_lab
          
          integer whitelabel ,m1,m2,b1,b2 ,nb,nw,wl euln ,itrial,nbt,nwt
          integer lp,vp,tot,isz ,t1,trial,i_mod
          integer ix,jy,temp_l,tempx,tempy,ll,ks,ms,scount
          !data increx/-1,-1,0,1,1,1,0,-1/,increy/0,1,1,1,0,-1,-1,-1/

          open(3,file='14.15_eul_sc_up7.dat',status='unknown')
          open(9,file='14.15sc7.dat',status='unknown')
          open(7,file='14.15m0d7.dat',status='unknown')
          open(5,file='14.15sc6_up.dat',status='unknown')
          open(11,file='out7_14.15.dat',status='unknown')
         
          open(1,file='out6_14.15.dat',status='unknown')

         !p(1)=0.15
          !inc=0.40
         do k=1,1
          nbt=0
          nwt=0
          tot=1
          do isz=1,n*n
          num(isz)=0
          enddo
          trial=1
          tl=1
       do itrial =1,trial
          ! write(*,*)itrial
         ! write(*,*),p

          j2=0.0

          j1=0.0
          blacklabel=0
          whitelabel=0

           do i=1,n
           read(1,*)(r(i,j),j=1,n)
           enddo

          do i=1,n
           i1=0.5*(FLOAT(i-1)*SQRT(3.0)*a)

           j1=j2
          
           do j=1,n
           j1=j1+a
          !point(i,j)=10*i+j
           ypoint(i,j)=i1
           xpoint(i,j)=j1
             
          
          !write(9,*)xpoint(i,j),ypoint(i,j),rhex(i,j)
          enddo
          
          j2=0.5*(FLOAT(i)*a)
        
          enddo
          
           do i=1,n
           do j=1,n
           if (r(i,j).eq.0)then
           pointvalue(i,j)=0
           !write(*,*)'hi'
           else
           pointvalue(i,j)=1
           endif
           !write(*,*)i,j ,'!!!!!'
           enddo
           
           enddo

          
          
          
           do i=1,n
           do j=1,n
           
           label(i,j)=0
           write(5,*)xpoint(i,j),ypoint(i,j),pointvalue(i,j)
           enddo
           enddo

           do i=2,n-1
           do j=2,n-1
           ! write(*,*)i,j

           if ((pointvalue(i,j).eq.1 ).and. (label(i,j).eq.0))then

           blacklabel=blacklabel+1

           label(i,j)=blacklabel
           ! write(*,*)'label',label(i,j),point(i,j),i,j
           nn=0
           sno=0
           ii(sno)=i
           jj(sno)=j
           !write(*,*)ii(0),jj(0)
           sp=0


11        totn=nn



           do sno=sp,totn
           !if(ii(sno).gt.1 .and. ii(sno).lt.n )then
           !if(jj(sno).gt.1 .and. jj(sno).lt.n) then



           check1(ii(sno),jj(sno))=pointvalue(ii(sno),jj(sno)-1)
           check2(ii(sno),jj(sno))=pointvalue(ii(sno),jj(sno)+1)
           check3(ii(sno),jj(sno))=pointvalue(ii(sno)-1,jj(sno))
           check4(ii(sno),jj(sno))=pointvalue(ii(sno)-1,jj(sno)+1)
           check5(ii(sno),jj(sno))=pointvalue(ii(sno)+1,jj(sno)-1)
           check6(ii(sno),jj(sno))=pointvalue(ii(sno)+1,jj(sno))
           if (check1(ii(sno),jj(sno)).eq.1 )then
           if(label(ii(sno),jj(sno)-1).eq.0)then
           label(ii(sno),jj(sno)-1)=label(ii(sno),jj(sno))
            if(ii(sno).gt.1 .and. ii(sno).lt.n )then
          if((jj(sno)-1).gt.1 .and. (jj(sno)-1).lt.n) then
           ii(nn+1)=ii(sno)
           jj(nn+1)=jj(sno)-1
           nn=nn+1


          ! goto 11
          endif
          endif
          endif
          endif
       if(check2(ii(sno),jj(sno)).eq.1)then
       if(label(ii(sno),jj(sno)+1).eq.0)then
           label(ii(sno),jj(sno)+1)=label(ii(sno),jj(sno))

        if(ii(sno).gt.1 .and. ii(sno).lt.n )then
        if((jj(sno)+1).gt.1 .and. (jj(sno)+1).lt.n) then
           ii(nn+1)=ii(sno)
           jj(nn+1)=jj(sno)+1
           nn=nn+1

           !goto 11
           endif
           endif
           endif
           endif

           if (check3(ii(sno),jj(sno)).eq.1 )then
           if(label(ii(sno)-1,jj(sno)).eq.0)then
           label(ii(sno)-1,jj(sno))=label(ii(sno),jj(sno))

           if((ii(sno)-1).gt.1 .and. (ii(sno)-1).lt.n )then
          if(jj(sno).gt.1 .and. jj(sno).lt.n) then
          ii(nn+1)=ii(sno)-1
          jj(nn+1)= jj(sno)
          nn=nn+1
           ! write(*,*)ii(sno),jj(sno)
           !goto 11
           endif
           endif
           endif
           endif

           if (check4(ii(sno),jj(sno)).eq.1 )then
           if(label(ii(sno)-1,jj(sno)+1).eq.0)then
           label(ii(sno)-1,jj(sno)+1)=label(ii(sno),jj(sno))

          if((ii(sno)-1).gt.1 .and. (ii(sno)-1).lt.n )then
          if((jj(sno)+1).gt.1 .and. (jj(sno)+1).lt.n) then
           ii(nn+1)=ii(sno)-1
           jj(nn+1)=jj(sno)+1
           nn=nn+1
           !goto 11
           endif
           endif
           endif
           endif
           if (check5(ii(sno),jj(sno)).eq.1 )then
           if(label(ii(sno)+1,jj(sno)-1).eq.0)then
           label(ii(sno)+1,jj(sno)-1)=label(ii(sno),jj(sno))

            if((ii(sno)+1).gt.1 .and. (ii(sno)+1).lt.n )then
          if((jj(sno)-1).gt.1 .and.(jj(sno)-1).lt.n) then
           ii(nn+1)=ii(sno)+1
           jj(nn+1)=jj(sno)-1
           nn=nn+1
           !goto 11
           endif
           endif
           endif
           endif
           if (check6(ii(sno),jj(sno)).eq.1 )then
           if(label(ii(sno)+1,jj(sno)).eq.0)then
           label(ii(sno)+1,jj(sno))=label(ii(sno),jj(sno))

            if((ii(sno)+1).gt.1 .and. (ii(sno)+1).lt.n )then
          if(jj(sno).gt.1 .and. jj(sno).lt.n) then
           ii(nn+1)=ii(sno)+1
           jj(nn+1)=jj(sno)
           nn=nn+1

           !goto 11
           endif
           endif
           endif
           endif



          if (sno.eq.0)then
           nbn(sno)=nn
           !write(*,*)'jjj' ,nn,sno
           else
           nbn(sno)=nn-nbn(sno-1)

           endif
           if (nbn(sno).gt.0)then
           !write(*,*)nbn(sno),sno,point(i,j)
           sp=sp+1
           !write(*,*)'kkk'
           goto 11

         endif


          !endif
          !endif


          enddo
          endif
          enddo
          enddo

   
           
           
               

          
           do i=2,n-1
           do j=2,n-1
          ! write(*,*)i,j

           if ((pointvalue(i,j).eq.0 ).and. (label(i,j).eq.0))then

           whitelabel=whitelabel-1

           label(i,j)=whitelabel
           !write(*,*)'hi',point(i,j)
           !write(*,*)'label',label(i,j),point(i,j),i,j
           nnw=0
           wsno=0
           iiw(wsno)=i
           jjw(wsno)=j
           !write(*,*)ii(0),jj(0)
           spw=0


 33      totnw=nnw



           do wsno=spw,totnw
        ! if(iiw(wsno).gt.1 .and. iiw(wsno).lt.n )then
         ! if(jjw(wsno).gt.1 .and. jjw(wsno).lt.n) then



        wcheck1(iiw(wsno),jjw(wsno))=pointvalue(iiw(wsno),jjw(wsno)-1)
        wcheck2(iiw(wsno),jjw(wsno))=pointvalue(iiw(wsno),jjw(wsno)+1)
        wcheck3(iiw(wsno),jjw(wsno))=pointvalue(iiw(wsno)-1,jjw(wsno))
        wcheck4(iiw(wsno),jjw(wsno))=pointvalue(iiw(wsno)-1,jjw(wsno)+1)
        wcheck5(iiw(wsno),jjw(wsno))=pointvalue(iiw(wsno)+1,jjw(wsno)-1)
        wcheck6(iiw(wsno),jjw(wsno))=pointvalue(iiw(wsno)+1,jjw(wsno))
        if (wcheck1(iiw(wsno),jjw(wsno)).eq.0)then
        if(label(iiw(wsno),jjw(wsno)-1).eq.0)then
        label(iiw(wsno),jjw(wsno)-1)=label(iiw(wsno),jjw(wsno))

        if(iiw(wsno).gt.1 .and. iiw(wsno).lt.n )then
          if((jjw(wsno)-1).gt.1 .and. (jjw(wsno)-1).lt.n) then


        iiw(nnw+1)=iiw(wsno)
        jjw(nnw+1)=jjw(wsno)-1
         nnw=nnw+1


          ! goto 11
        endif
        endif
        endif
        endif
       if(wcheck2(iiw(wsno),jjw(wsno)).eq.0)then
       if(label(iiw(wsno),jjw(wsno)+1).eq.0)then
       label(iiw(wsno),jjw(wsno)+1)=label(iiw(wsno),jjw(wsno))

       if(iiw(wsno).gt.1 .and. iiw(wsno).lt.n )then
          if((jjw(wsno)+1).gt.1 .and. (jjw(wsno)+1).lt.n) then

        iiw(nnw+1)=iiw(wsno)
        jjw(nnw+1)=jjw(wsno)+1
        nnw=nnw+1

           !goto 11
        endif
        endif
        endif
        endif
        if (wcheck3(iiw(wsno),jjw(wsno)).eq.0 )then
        if(label(iiw(wsno)-1,jjw(wsno)).eq.0)then
        label(iiw(wsno)-1,jjw(wsno))=label(iiw(wsno),jjw(wsno))
        if((iiw(wsno)-1).gt.1 .and. (iiw(wsno)-1).lt.n )then
          if(jjw(wsno).gt.1 .and. jjw(wsno).lt.n) then

           iiw(nnw+1)=iiw(wsno)-1
           jjw(nnw+1)= jjw(wsno)
           nnw=nnw+1
           !write(*,*)ii(sno),jj(sno)
           !goto 11
        endif
        endif
        endif
        endif
        if (wcheck4(iiw(wsno),jjw(wsno)).eq.0 )then
        if(label(iiw(wsno)-1,jjw(wsno)+1).eq.0)then
           label(iiw(wsno)-1,jjw(wsno)+1)=label(iiw(wsno),jjw(wsno))
           if((iiw(wsno)-1).gt.1 .and. (iiw(wsno)-1).lt.n )then
          if((jjw(wsno)+1).gt.1 .and. (jjw(wsno)+1).lt.n) then

           iiw(nnw+1)=iiw(wsno)-1
           jjw(nnw+1)=jjw(wsno)+1
           nnw=nnw+1
           !goto 11
           endif
           endif
           endif
           endif
           if (wcheck5(iiw(wsno),jjw(wsno)).eq.0 )then
           if(label(iiw(wsno)+1,jjw(wsno)-1).eq.0)then
           label(iiw(wsno)+1,jjw(wsno)-1)=label(iiw(wsno),jjw(wsno))
           if((iiw(wsno)+1).gt.1 .and. (iiw(wsno)+1).lt.n )then
          if((jjw(wsno)-1).gt.1 .and. (jjw(wsno)-1).lt.n) then

           iiw(nnw+1)=iiw(wsno)+1
           jjw(nnw+1)=jjw(wsno)-1
           nnw=nnw+1
           !goto 11
           endif
           endif
           endif
           endif
           if (wcheck6(iiw(wsno),jjw(wsno)).eq.0 )then
           if(label(iiw(wsno)+1,jjw(wsno)).eq.0)then
           label(iiw(wsno)+1,jjw(wsno))=label(iiw(wsno),jjw(wsno))
           if((iiw(wsno)+1).gt.1 .and. (iiw(wsno)+1).lt.n )then
          if(jjw(wsno).gt.1 .and. jjw(wsno).lt.n) then

           iiw(nnw+1)=iiw(wsno)+1
           jjw(nnw+1)=jjw(wsno)
           nnw=nnw+1

           !goto 11
           endif
           endif
           endif
           endif



          if (wsno.eq.0)then
           nwn(wsno)=nnw
           !write(*,*)'jjj' ,nn,sno
           else
           nwn(wsno)=nnw-nwn(wsno-1)
          ! write(*,*),'gg',nwn(wsno)

           endif
          if (nwn(wsno).gt.0)then
         ! write(*,*)nbn(sno),sno,point(i,j)
          spw=spw+1
          !write(*,*)'kkk'
          goto 33
         ! write(*,*)'hi'

          endif


          !endif
          !endif


          enddo
          endif
          enddo
          enddo


! boundary points
             do i=1,1
             do j=1,n
             b1=0
             b2=0
              if(label(i,j).eq.0)then


              if (j.lt.n)then
              bcheck1(i,j)=pointvalue(i,j+1)
              endif
              if (j.gt.1) then
              bcheck2(i,j)=pointvalue(i,j-1)
              endif
              if (bcheck1(i,j).eq.pointvalue(i,j))then
              if(label(i,j+1).ne.0)then
              label(i,j)=label(i,j+1)
              b1=1
              endif
              endif
             if ( bcheck2(i,j).eq.pointvalue(i,j))then
             if(label(i,j-1).ne.0)then
             label(i,j)=label(i,j-1)
             b2=1
             endif
             endif
             if((b1.eq.1).and.(b2.eq.1)) then
             if(label(i,j-1).ne.label(i,j+1))then
             m1=min(label(i,j+1),label(i,j+1))
             label(i,j)=m1
             label(i,j-1)=m1
             label(i,j+1)=m1
             endif
             endif

             if (b1.eq.0 .and. b2.eq.0)then
             if (pointvalue(i,j).eq.1)then
             blacklabel=blacklabel+1
             label(i,j)=blacklabel

             else
             whitelabel=whitelabel-1
             label(i,j)=whitelabel
             endif
             endif
             endif
             enddo
             enddo







             do i=n,n
             do j=1,n
             b1=0
             b2=0
              if(label(i,j).eq.0)then


              if (j.lt.n)then
              bcheck1(i,j)=pointvalue(i,j+1)
              endif
              if (j.gt.1) then
              bcheck2(i,j)=pointvalue(i,j-1)
              endif
              if (bcheck1(i,j).eq.pointvalue(i,j))then
              if(label(i,j+1).ne.0)then
              label(i,j)=label(i,j+1)
              b1=1
              endif
              endif
             if ( bcheck2(i,j).eq.pointvalue(i,j))then
             if(label(i,j-1).ne.0)then
             label(i,j)=label(i,j-1)
             b2=1
             endif
             endif
             if((b1.eq.1).and.(b2.eq.1))then
             if(label(i,j-1).ne.label(i,j+1))then
             m1=min(label(i,j+1),label(i,j+1))
             label(i,j)=m1
             label(i,j-1)=m1
             label(i,j+1)=m1
             endif
             endif

             if (b1.eq.0 .and. b2.eq.0)then
             if (pointvalue(i,j).eq.1)then
             blacklabel=blacklabel+1
             label(i,j)=blacklabel

             else
             whitelabel=whitelabel-1
             label(i,j)=whitelabel
             endif
             endif
             endif
             enddo
             enddo



               do i=1,n
              j=1
             b1=0
             b2=0
              if(label(i,j).eq.0)then


              if (i.lt.n)then
              bcheck1(i,j)=pointvalue(i+1,j)
              endif
              if (i.gt.1) then
              bcheck2(i,j)=pointvalue(i-1,j)
              endif
              if (bcheck1(i,j).eq.pointvalue(i,j))then
              if(label(i+1,j).ne.0)then
              label(i,j)=label(i+1,j)
              b1=1
              endif
              endif
             if ( bcheck2(i,j).eq.pointvalue(i,j))then
             if(label(i-1,j).ne.0)then
             label(i,j)=label(i-1,j)
             b2=1
             endif
             endif
             if((b1.eq.1).and.(b2.eq.1))then
             if(label(i-1,j).ne.label(i+1,j))then
             m2=min(label(i-1,j),label(i+1,j))
             label(i,j)=m2
             label(i-1,j)=m2
             label(i+1,j)=m2
             endif
             endif

             if (b1.eq.0 .and. b2.eq.0)then
             if (pointvalue(i,j).eq.1)then
             blacklabel=blacklabel+1
             label(i,j)=blacklabel

             else
             whitelabel=whitelabel-1
             label(i,j)=whitelabel
             endif
             endif
             endif
             enddo



               do i=1,n
              j=n
              b1=0
              b2=0
              if(label(i,j).eq.0)then


              if (i.lt.n)then
              bcheck1(i,j)=pointvalue(i+1,j)
              endif
              if (i.gt.1) then
              bcheck2(i,j)=pointvalue(i-1,j)
              endif
              if (bcheck1(i,j).eq.pointvalue(i,j))then
              if(label(i+1,j).ne.0)then
              label(i,j)=label(i+1,j)
              b1=1
              endif
              endif
             if ( bcheck2(i,j).eq.pointvalue(i,j))then
             if(label(i-1,j).ne.0)then
             label(i,j)=label(i-1,j)
             b2=1
             endif
             endif
             if((b1.eq.1).and.(b2.eq.1))then
             if(label(i-1,j).ne.label(i+1,j))then
             m2=min(label(i-1,j),label(i+1,j))
             label(i,j)=m2
             label(i-1,j)=m2
             label(i+1,j)=m2
             endif
             endif

             if (b1.eq.0 .and. b2.eq.0)then
             if (pointvalue(i,j).eq.1)then
             blacklabel=blacklabel+1
             label(i,j)=blacklabel

             else
             whitelabel=whitelabel-1
             label(i,j)=whitelabel
             endif
             endif
             endif
             enddo

   


           !displaying labels
           !do i=2,n-1
           !do j=2,n-1
           !write(3,*)'step1',i,j,pointvalue(i,j),'label',label(i,j)
           !enddo
           !enddo
           
         i_mod=0 
         do i=2,n-1
         do j=2,n-1
         tempx=i
         tempy=j
         do ks=1,sc
         maxn(ks)=6**(ks-1)
         scount=1
            do ms=1,maxn(ks)
            !tempx=nx(m,k-1,i,j)
            !tempy=ny(m,k-1,i,j)
           ! write(*,*)'*******',tempx,tempy
                !write(*,*) maxn(ks) ,'maxn'
            nx(scount,ks)=tempx
            nx(scount+1,ks)=tempx
            nx(scount+2,ks)=tempx-1
            nx(scount+3,ks)=tempx-1
            nx(scount+4,ks)=tempx+1
            nx(scount+5,ks)=tempx+1
            
            ny(scount,ks)=tempy-1
            ny(scount+1,ks)=tempy+1
            ny(scount+2,ks)=tempy
            ny(scount+3,ks)=tempy+1
            ny(scount+4,ks)=tempy-1
            ny(scount+5,ks)=tempy
             !write(*,*) nx(scount+5,ks),ny(scount+5,ks),scount+5
              if(sc.gt.1)then
                scount=scount+6
                tempx=nx(ms,ks-1)
                tempy=ny(ms,ks-1)
                 endif
            enddo
         enddo
          !write(*,*) nx(1,sc),ny(1,sc),'**{{{}}**'
         !enddo
         !enddo
         
     
         !do i=2,n-1
         !do j=2,n-1
         do ks=1,sc
           mn(ks)=6**(ks)
           do ll=1,mn(ks)

              ix=nx(ll,ks)
              jy=ny(ll,ks)
              ! write(*,*)ix,jy,nx(ll,sc),ll
              ! write(*,*)label(ix,jy),ix,jy,label(i,j),i,j 
               if(label(i,j).gt.0)then
               if(ix.gt.1.and.ix.lt.n)then
               if(jy.gt.1.and.jy.lt.n)then
              ! write(3,*)'***',label(ix,jy),ix,jy,label(i,j),i,j
               !write(*,*)label(ix,jy),ix,jy,label(i,j),i,j 
               
               pointvalue(ix,jy)=1
                
                 if(label(ix,jy).lt.0)then
                      i_mod=i_mod+1
                      modx(i_mod)=ix
                      mody(i_mod)=jy
                      mod_val(i_mod)=pointvalue(ix,jy)
                 endif
          
           
           
           
        
           
           
              endif
              endif
             endif
           enddo
         enddo
        enddo
        enddo
          
          do i=1,i_mod
           idx= xpoint(modx(i),mody(i))
           idy= ypoint(modx(i),mody(i))
          write(7,*)xpoint(idx,idy),ypoint(idx,idy),mod_val(i)
          enddo
          
          
           !displaying labels
           do i=1,n
           do j=1,n
        !   write(3,*)'step2',i,j,pointvalue(i,j),'label',label(i,j)
           write(9,*)xpoint(i,j),ypoint(i,j),pointvalue(i,j)
           !write(11,*)pointvalue(i,j)
           enddo
           enddo
           
           
          do i=1,n
          write(11,91)(int(pointvalue(i,j)),j=1,n)
 91       format(463(I1,1X))  
          enddo
       

       
       
       
          
             k_sort=0
            do i=2,n-1
            do j=2,n-1
              k_sort=k_sort+1
             alab(k_sort)=label(i,j)
             
           ! write(5,*)label(i,j),alab(k_sort),k_sort,i,j
           
           
           enddo
           enddo
           
           
           
           
          do k_sort=1,(n-2)*(n-2)
          
         
            !write(5,*)a_label(k_sort),k_sort
          
           enddo
           
           
             
          do k_sort=1,(n-2)*(n-2)
          do l_sort=k_sort+1,(n-2)*(n-2)
          !write(*,*)'$$$$$$'
           if(alab(k_sort).gt.alab(l_sort))then
             !write(5,*)'******',a_label(k_sort),a_label(l_sort),'*****'
            sort_lab=alab(k_sort)
            alab(k_sort)=alab(l_sort)
            alab(l_sort)=sort_lab
            endif
          enddo
          enddo





          do k_sort=1,(n-2)*(n-2)
          
         
            ! write(5,*)alab(k_sort)
            !write(*,*)alab(k_sort)
            enddo


        nb=0
        b_count=0
        do i=1,(n-2)*(n-2)
        if(alab(i).gt.b_count)then
        nb=nb+1
        b_count=alab(i)
       !write(*,*)nb,'nb',label(i,j),i,j,'ij'
        endif
        enddo


        wl=0
       do i=2,n-1
       do j=2,n-1
      
       if (label(i,j).lt.wl)then
       wl=wl-1
       !write(*,*)wl,'wl',label(i,j),i,j,'ij'
       endif
       enddo
       enddo

       nw = abs(wl)

      ! euln=nb-nw

        !write(*,*)p(k),euln


        !write(3,*)p(k),euln





       nbt=nbt+nb
       nwt=nwt+nw






       !do lp=1,nb
       !sizeb(lp)=0
       !do i=1,n
       !do j=1,n
       !if(label(i,j).eq.lp)then
       !sizeb(lp)= sizeb(lp)+1
       !write(*,*),'label',label(i,j)
       !endif
       !enddo
       !enddo
       !enddo

       !do vp=1,nw
        !sizew(vp)=0
        !do i=1,n
        !do j=1,n
        !if(label(i,j).eq.(-vp))then
        !sizew(vp)=sizew(vp)+1
        !endif
        !enddo
        !enddo
        !enddo

        !do lp=1,nb
        !clustersize(tot)=sizeb(lp)
        !trialno(tot)=itrial
        !tot=tot+1
       ! write(*,*)'tot',tot,'clustersize',clustersize(tot-1),sizeb(k)
        !enddo


       enddo  !trial ends
        !write(*,*) trial
       nbf=nbt/trial
       !write(*,*)'nbf', nbf

       nwf=nwt/trial
       euln=nbf-nwf
       euler(tl)=euln
       tl=tl+1
       !write(*,*)tl

       !trial=trial+100
       ! write(*,*) trial
       !write(*,*) trial
       !if (tl.eq.2)then
       !goto 99
       !write(*,*)'tl=2',euler(tl)
       !endif
       !if (abs(euler(tl-2)-euler(tl-1)).le.1 )then
       eulerno(k)=euler(tl-1)
       !else
       !goto 99
       !endif
       u(k)=(eulerno(k))/(n*n)





       !write(2,*)'p=',p,nbf,nbw ,eun
        write(*,*)p(k),nbf,nwf,eulerno(k),u(k)
        write(3,*)nbf,nwf,eulerno(k),u(k)
        !do k=1,tot-1
       ! write(*,*)clustersize(k)
        !enddo



        !do isz=1,n*n
        !i=1
        !tr(isz)=1
        !do m=1,tot-1
        !if (clustersize(m).eq.isz)then
        !num(isz)=num(isz)+1
        !trno(i)=trialno(m)
        !i=i+1
       !if((i.gt.2).and.(trno(i-1).ne.trno(i-2)))then
        !tr(isz)=tr(isz)+1
        !endif
        !write(*,*)'total trial',k,isz
         !endif

        !enddo

        !numf(isz)=num(isz)/tr(isz)
        !if (num(isz).gt.0)then
        !write(*,*)'size',isz,'num',numf(isz),p,num(isz),tr(isz)
        !endif
        !enddo
         p(k+1)=p(k)+inc
         !write(*,*),p

         enddo




           read(*,*)
           end program

