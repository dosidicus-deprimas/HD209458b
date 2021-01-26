program prog
	implicit none
	
	real(8),dimension(:),allocatable :: sumerr,MJD,res
	real(8),dimension(:,:),allocatable :: pld
	real(8) :: xn,yn,apsum,medbkg,bkg,lg,rg,su, day
	integer :: n,i,j,l,k,lef,rig,med
	character(143) :: a

	open(4,FILE="input.dat")!Все данные с новой строки
		read(4,*) n			!Количетво кадров в наблюдении
		read(4,*) lg,rg		!Примерное время начала и конца вторичного затмения
	close(4)
	
l=0
	allocate(MJD(1:n),sumerr(1:n),res(1:n),pld(1:3,0:n-1))
	open(4,FILE="predresult.txt")
	open(8,FILE="result.txt")
		read(4,*) a
a="_______MJD_______|________x________|________y________|______apsum____&
&__|______medakg_____|_______bkg_______|_______res_______|______sumerr_____|"	
	write(8,*) a
		do i=1,n 
			read(4,*) j, xn, yn, apsum, sumerr(i), MJD(i), medbkg, bkg, res(i)
			write(8,"(8(F18.8))")  MJD(i),  xn, yn, apsum, medbkg, bkg, res(i), sumerr(i)
			if (MJD(i)<lg .or. MJD(i)>rg) then
				pld(2,l)=res(i)
				pld(1,l)=MJD(i)
				pld(3,l)=sumerr(i)
				l=l+1
			endif
		enddo
	close(8)
	close(4)
	
	do i=1,l-1
		rig=0
		lef=0
		do k=1,l-1
			if (pld(2,i)>pld(2,k)) lef=lef+1
			if (pld(2,i)<pld(2,k)) rig=rig+1
		enddo
		if (abs(lef-rig)<2) med=i
	enddo
	
	su=pld(2,med)
	
	open(4,FILE="postresult.txt")
		do i=1,n 
			write(4,"(3(F18.8))")  MJD(i)+2400000.5, res(i)/su, sumerr(i)/su
		enddo
	close(4)
day=int(MJD(1))
	open(4,FILE="data.dat")
		write(4,*) '# ',n-1
		do i=1,n
			write(4,"(3(F18.8))")  MJD(i)-day, res(i)/su, 10.0**4/sumerr(i)*su
		enddo
	close(4)
	deallocate(MJD,res,sumerr)
	!Для следующего этапа работы используются файлы data.dat, которые для каждого затмения переименованы в 1.dat, 2.dat и т.д.
	!Из файлов необходимо удалить первую строку, содержащую количество кадров.
end program prog
